## Helper function for data cleaning

# quickly look for episodes with duplicated species
check_for_duplicates <- function(data) {
  duplicated_rows <- data %>%
    dplyr::group_by(episode_id, year, species_id) %>%
    dplyr::select(episode_id:species_id) %>%
    duplicated()

  data %>%
    dplyr::filter(duplicated_rows) %>%
    dplyr::select(episode_id, year) %>%
    dplyr::distinct()
}

# check old polygons
# Here we need a function that takes each individual polygon (each row of the
# episode data) and test it against the join of all past polygons. As is used
# in a mutate call it needs to take de vector of polygons to check, the old
# polygons data and return a vector of equal length of the original one with
# TRUE or FALSE
check_old_polygons_intersect_individually <- function(geometry_col, old_polygons_data) {
  if (nrow(old_polygons_data) > 0) {
    geometry_col %>%
      sf::st_make_valid() %>%
      purrr::map_lgl(
        .f = sf::st_intersects,
        y = sf::st_union(old_polygons_data$geometry), sparse = FALSE
      ) %>%
      magrittr::not()
  } else {
    NA
  }
}

check_old_polygons_distance_individually <- function(geometry_col, old_polygons_data) {
  if (nrow(old_polygons_data) > 0) {
    sf::st_distance(geometry_col, sf::st_union(old_polygons_data$geometry))[,1] %>%
      as.numeric()
  } else {
    NA
  }
}

## yearly_checks

# check  species
#   - duplicated species TRUE means problem, that species is duplicated
#   - new species TRUE means problem, that species is new, didn't exist on previous years
#   - TODO
check_species <- function(episode_data, old_episodes_data) {

  # browser()

  pull_safe <- purrr::possibly(dplyr::pull, otherwise = NULL)

  old_species <- old_episodes_data %>%
    dplyr::filter(episode_id == unique(episode_data$episode_id)) %>%
    dplyr::select(dplyr::any_of(c('episode_id', 'species_id', 'geometry'))) %>%
    dplyr::distinct() %>%
    pull_safe(species_id)

  episode_data %>%
    dplyr::group_by(species_id) %>%
    dplyr::mutate(
      check_duplicated_species_ind = dplyr::if_else(dplyr::n() > 1, TRUE, FALSE),
      check_new_species_ind = dplyr::if_else(
        !rlang::is_null(old_species) & length(old_species) > 0,
        !unique(species_id) %in% old_species,
        FALSE
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(episode_id) %>%
    dplyr::mutate(
      check_duplicated_species = any(check_duplicated_species_ind),
      check_new_species = any(check_new_species_ind)
    )
}

# check polygons
#   - species polygons intersects TRUE means problem, dont intersect
#   - max distance between them are less than 1km TRUE means problem, more than 1km
#   - species polygons intersects with old years polygons TRUE means problem, dont intersects
#   - TODO: all polygons intersects, but as we will have only one polygon in the past years we
#     need an individual column indicating if that specific polygon intersects with the past
#     ones
check_polygons <- function(episode_data, old_episodes_data) {

  old_episode_data <- old_episodes_data %>%
    dplyr::filter(episode_id == unique(episode_data$episode_id)) %>%
    dplyr::select(dplyr::any_of(c('episode_id', 'species_id', 'geometry'))) %>%
    dplyr::distinct()

  episode_data %>%
    dplyr::mutate(
      check_polygon_intersects = !all(sf::st_intersects(geometry, sparse = FALSE)),
      check_polygon_distance_above_1000 = as.numeric(max(sf::st_distance(geometry))) >= 1000,
      check_old_polygon_intersects = dplyr::if_else(
        nrow(old_episode_data) > 0,
        !all(sf::st_intersects(geometry, old_episode_data$geometry, sparse = FALSE)),
        NA
      ),
      check_old_polygon_intersects_ind = check_old_polygons_intersect_individually(geometry, old_episode_data),
      check_old_polygon_distance_ind = check_old_polygons_distance_individually(geometry, old_episode_data)
    )
}

check_error_meaning <- function(data) {
  data %>%
    dplyr::mutate(
      check_meaning = dplyr::case_when(
        # Ok cases:
        #   - all main checks false
        !check_duplicated_species & !check_new_species &
          !check_polygon_intersects & !check_polygon_distance_above_1000 &
          !check_old_polygon_intersects ~ "Ok",
        #   - all main checks false and no old polygon (new episode)
        !check_duplicated_species & !check_new_species &
          !check_polygon_intersects & !check_polygon_distance_above_1000 &
          is.na(check_old_polygon_intersects) ~ "Ok",
        #   - All main checks false, but old polygon dont intersect for all species, but the distance between new and old polygon is less than 1000m in all cases
        !check_duplicated_species & !check_new_species & !check_polygon_intersects &
          !check_polygon_distance_above_1000 & check_old_polygon_intersects &
          all(check_old_polygon_intersects_ind) &
          all(check_old_polygon_distance_ind <= 1000) ~ "Ok",
        #   - All main checks false, but old polygon dont intersect for all species, but there is more than one old polygon but all species polygons intersect with one or more of the old polygons
        !check_duplicated_species & !check_new_species & !check_polygon_intersects &
          !check_polygon_distance_above_1000 & check_old_polygon_intersects &
          !any(check_old_polygon_intersects_ind) ~ "Ok",
        #    - New species, but old polygon dont intersect for all species, but there is more than one old
        #    polygon but all species polygons intersect with one or more of the old polygons polygons AND
        #    the new species IS NOT Populus nigra. This is Ok, because other species are not checked
        !check_duplicated_species & check_new_species & !check_polygon_intersects &
          !check_polygon_distance_above_1000 & check_old_polygon_intersects &
          !any(check_old_polygon_intersects_ind) &
          !any(
            species_id %in% c('Populus nigra') &
              check_new_species_ind
          ) ~ "Ok / New species others",


        # Duplicated species
        #   - All main checks false except for duplicated species
        check_duplicated_species & !check_new_species & !check_polygon_intersects &
          !check_polygon_distance_above_1000 &
          !check_old_polygon_intersects ~ "Duplicated species",
        #   - All main checks false except for duplicated species, new episode
        check_duplicated_species & !check_new_species & !check_polygon_intersects &
          !check_polygon_distance_above_1000 &
          is.na(check_old_polygon_intersects) ~ "Duplicated species",
        #   - Duplicated species, and old polygon don't intersect for all species
        #   but all distances between new and old polygon is less than 1000m
        check_duplicated_species & !check_new_species & !check_polygon_intersects &
          !check_polygon_distance_above_1000 & check_old_polygon_intersects &
          all(check_old_polygon_intersects_ind) &
          all(check_old_polygon_distance_ind <= 1000) ~ "Duplicated species",
        #   - Duplicated species, but old polygon dont intersect for all species,
        #   but there is more than one old polygon and all species polygons
        #   intersect with one or more of the old polygons
        check_duplicated_species & !check_new_species & !check_polygon_intersects &
          !check_polygon_distance_above_1000 & check_old_polygon_intersects &
          !any(check_old_polygon_intersects_ind) ~ "Duplicated species",

        # Duplicated species, but one of the dupes is in the correct polygon
        #    - Duplicated species, old polygon dont intersect for all entries,
        #    but some of the dupes intersect with the old polygon
        check_duplicated_species & !check_new_species & !check_polygon_intersects &
          !check_polygon_distance_above_1000 & check_old_polygon_intersects &
          !all(check_old_polygon_intersects_ind) &
          any(check_old_polygon_intersects_ind) ~ "Duplicated species in wrong poly",
        #    - Duplicated species with no intersecting polygons, but one of the duped polygon intersects
        #    with past polygons, so the right poly is known
        check_duplicated_species & !check_new_species & check_polygon_intersects &
          check_polygon_distance_above_1000 & check_old_polygon_intersects &
          !all(check_old_polygon_intersects_ind) &
          any(check_old_polygon_intersects_ind) &
          any(check_old_polygon_distance_ind < 1) ~ "Duplicated species in wrong poly (known)",
        #    - Duplicated species with no intersecting polygons, but one of the duped polygon intersects
        #    with past polygons, so the right poly is known, also new species
        check_duplicated_species & check_new_species & check_polygon_intersects &
          check_polygon_distance_above_1000 & check_old_polygon_intersects &
          !all(check_old_polygon_intersects_ind) &
          any(check_old_polygon_intersects_ind) &
          any(check_old_polygon_distance_ind < 1) ~ "Duplicated species in wrong poly (known)",
        #    - Duplicated species, dupes dont intersect and distance bewteen them
        #    is bigger than 1000m, new episode (so the right poly is unknown)
        check_duplicated_species & !check_new_species & check_polygon_intersects &
          check_polygon_distance_above_1000 &
          is.na(check_old_polygon_intersects) ~ "Duplicated species in wrong poly (unknown)",

        # New species:
        #    - All main checks false, except for new species AND the new species is
        #    Populus nigra (there is a tendency here to confund Populus nigra with Pinus nigra)
        !check_duplicated_species & check_new_species & !check_polygon_intersects &
          !check_polygon_distance_above_1000 & !check_old_polygon_intersects &
          any(
            species_id %in% c('Populus nigra') &
              check_new_species_ind
          ) ~ 'New species Populus nigra',
        #    - All main checks false, except for new species, AND the new species IS NOT
        #    Populus nigra
        !check_duplicated_species & check_new_species & !check_polygon_intersects &
          !check_polygon_distance_above_1000 & !check_old_polygon_intersects &
          !any(
            species_id %in% c('Populus nigra') &
              check_new_species_ind
          ) ~ 'New species others',
        #    - New species, and old polygon don't intersect for all species, but all distances
        #    between new and old polygon are less than 1000m, AND the new species is
        #    Populus nigra (there is a tendency here to confund Populus nigra with Pinus nigra)
        !check_duplicated_species & check_new_species & !check_polygon_intersects &
          !check_polygon_distance_above_1000 & check_old_polygon_intersects &
          all(check_old_polygon_intersects_ind) &
          all(check_old_polygon_distance_ind <= 1000) &
          any(
            species_id %in% c('Populus nigra') &
              check_new_species_ind
          ) ~ 'New species Populus nigra',
        #    - New species, and old polygon don't intersect for all species, but all distances
        #    between new and old polygon are less than 1000m, AND the new species IS NOT
        #    Populus nigra
        !check_duplicated_species & check_new_species & !check_polygon_intersects &
          !check_polygon_distance_above_1000 & check_old_polygon_intersects &
          all(check_old_polygon_intersects_ind) &
          all(check_old_polygon_distance_ind <= 1000) &
          !any(
            species_id %in% c('Populus nigra') &
              check_new_species_ind
          ) ~ 'New species others',
        #    - New species, new episode, AND the new species is
        #    Populus nigra (there is a tendency here to confund Populus nigra with Pinus nigra)
        !check_duplicated_species & check_new_species &
          !check_polygon_intersects & !check_polygon_distance_above_1000 &
          is.na(check_old_polygon_intersects) &
          any(
            species_id %in% c('Populus nigra') &
              check_new_species_ind
          ) ~ 'New species Populus nigra',
        #    - New species, new episode, AND the new species IS NOT
        #    Populus nigra
        !check_duplicated_species & check_new_species &
          !check_polygon_intersects & !check_polygon_distance_above_1000 &
          is.na(check_old_polygon_intersects) &
          !any(
            species_id %in% c('Populus nigra') &
              check_new_species_ind
          ) ~ 'New species others',
        #    - New species, but also duplicate species, no other problems, AND the new species is
        #    Populus nigra (there is a tendency here to confund Populus nigra with Pinus nigra)
        check_duplicated_species & check_new_species &
          !check_polygon_intersects & !check_polygon_distance_above_1000 &
          !check_old_polygon_intersects &
          any(
            species_id %in% c('Populus nigra') &
              check_new_species_ind
          ) ~ 'New species Populus nigra / Duplicated species',
        #    - New species, but also duplicate species, no other problems, AND the new species IS NOT
        #    Populus nigra
        check_duplicated_species & check_new_species &
          !check_polygon_intersects & !check_polygon_distance_above_1000 &
          !check_old_polygon_intersects &
          !any(
            species_id %in% c('Populus nigra') &
              check_new_species_ind
          ) ~ 'New species others / Duplicated species',
        #    - New species, but old polygon dont intersect for all species, but there is more than one old
        #    polygon but all species polygons intersect with one or more of the old polygons polygons
        #    AND the new species is Populus nigra (there is a tendency here to confund Populus nigra with
        #    Pinus nigra)
        !check_duplicated_species & check_new_species & !check_polygon_intersects &
          !check_polygon_distance_above_1000 & check_old_polygon_intersects &
          !any(check_old_polygon_intersects_ind) &
          any(
            species_id %in% c('Populus nigra') &
              check_new_species_ind
          ) ~ "New species Populus nigra",

        # New episodes coded as existent or errors with no plausible cause
        #    -
        !check_duplicated_species & !check_new_species & !check_polygon_intersects &
          !check_polygon_distance_above_1000 & check_old_polygon_intersects &
          all(check_old_polygon_intersects_ind) &
          all(check_old_polygon_distance_ind > 1000) ~ "New or error",
        #    -
        !check_duplicated_species & !check_new_species & !check_polygon_intersects &
          !check_polygon_distance_above_1000 & check_old_polygon_intersects &
          !all(check_old_polygon_intersects_ind) &
          any(check_old_polygon_intersects_ind) ~ "Error",
        #    -
        check_duplicated_species & !check_new_species & !check_polygon_intersects &
          !check_polygon_distance_above_1000 & check_old_polygon_intersects &
          all(check_old_polygon_intersects_ind) &
          all(check_old_polygon_distance_ind > 1000) ~ "New or error",
        #    - error with new species
        !check_duplicated_species & check_new_species & !check_polygon_intersects &
          !check_polygon_distance_above_1000 & check_old_polygon_intersects &
          all(check_old_polygon_intersects_ind) &
          all(check_old_polygon_distance_ind > 1000) ~ "New or error",


        # Not specified cases
        TRUE ~ 'Not specified'
      )
    )
}

create_data_dirty <- function(data_raw, data_curated) {
  # already existing episodes
  episodes_existing_in_data_curated <-
    data_curated %>% pull(episode_id) %>% unique()
  # data from existing episodes
  data_old <- data_raw %>%
    dplyr::filter(episode_id %in% episodes_existing_in_data_curated) %>%
    dplyr::mutate(
      new_episode = FALSE
    )
  # episodes started in raw data
  episodes_started_in_data_raw <-
    data_raw %>%
    dplyr::filter(!episode_id %in% episodes_existing_in_data_curated) %>%
    dplyr::filter(
      # 5% mortality or 40% affected
      mortality_perc >= 5 | (decoloration_perc + defoliation_perc) >= 50,
      # area more than 3 ha
      episode_area >= 3
    ) %>%
    dplyr::pull(episode_id) %>%
    unique()
  # data from new episodes
  data_new <- data_raw %>%
    dplyr::filter(episode_id %in% episodes_started_in_data_raw) %>%
    dplyr::mutate(
      new_episode = TRUE
    )

  data_dirty <- dplyr::bind_rows(data_new, data_old) %>%
    # distinct will remove any duplicated entries
    dplyr::distinct() %>%
    dplyr::arrange(episode_id, year)

  return(data_dirty)

}

create_polygon_plot <- function(episode, dirty_data, curated_data, future_data) {
  list(dirty_data, curated_data, future_data %>% dplyr::select(-new_episode)) %>%
    purrr::map_dfr(
      .f = ~ dplyr::filter(.x, episode_id == episode)
    ) %>%
    sf::st_as_sf() %>%
    ggplot() +
    geom_sf(aes(fill = forcats::as_factor(year)), alpha = 0.3, show.legend = FALSE) +
    facet_grid(cols = dplyr::vars(year)) +
    ggtitle(episode)
}

create_polygon_plot_2 <- function(episodes, data) {
  data %>%
    filter(episode_id %in% episodes) %>%
    sf::st_as_sf() %>%
    ggplot() +
    geom_sf(aes(fill = episode_id), alpha = 0.1) +
    facet_wrap(vars(year))
}

create_spatial_plot <- function(episode_data) {
  episode_data %>%
    ggplot() +
    geom_sf(aes(fill = episode_id), alpha = 0.1) +
    facet_grid(cols = vars(year)) +
    theme(legend.position = 'bottom')
}

create_affectation_plot <- function(episode_data) {
  episode_data %>%
    tidyr::pivot_longer(
      dplyr::ends_with('perc'), names_to = "affectation", values_to = 'perc'
    ) %>%
    ggplot(aes(x = year, y = perc, fill = species_id, colour = species_id)) +
    geom_line() +
    geom_point(size = 4) +
    facet_wrap(vars(affectation)) +
    scale_x_continuous(breaks = unique(episode_data$year), labels = unique(episode_data$year)) +
    theme(legend.position = 'bottom')
}

create_cicatrization_plot <- function(episode_data) {
  episode_data %>%
    ggplot(aes(x = year, y = cicatrization_index)) +
    geom_line() +
    geom_point(size = 4) +
    scale_x_continuous(breaks = unique(episode_data$year), labels = unique(episode_data$year))
}

episode_diagnostic_plot <- function(episode_data) {
  plot_list <- list(
    affectation_plot = create_affectation_plot(episode_data),
    cicatrization_plot = create_cicatrization_plot(episode_data),
    spatial_plot = create_spatial_plot(episode_data)
  )

  top_plot <- cowplot::plot_grid(
    plotlist = plot_list[1:2], nrow = 1, rel_widths = c(1, 0.3)
  )

  all_plot <- cowplot::plot_grid(
    top_plot, plot_list[[3]], nrow = 2, rel_heights = c(1, 0.6)
  )

  all_plot
}

# assign_county <- function(geometry_col, counties_sf) {
#
#   counties_ref <- sf::st_transform(counties_sf, sf::st_crs(geometry_col))
#
#   county_index <- geometry_col %>%
#     sf::st_union() %>%
#     sf::st_centroid() %>%
#     sf::st_intersects(counties_ref) %>%
#     magrittr::extract2(1)
#
#   res <- counties_ref %>%
#     dplyr::pull(NOMCOMAR) %>%
#     magrittr::extract(county_index) %>%
#     glue::glue_collapse(sep = ' / ')
#
#   return(res)
# }

# data_dirty_deboscat %>%
#   group_by(episode_id, year) %>%
#   mutate(county_name = assign_county(geometry, counties_sf)) %>%
#   pull(county_name)
#
#
# sf::st_join(
#   counties_sf %>% sf::st_transform(sf::st_crs(data_dirty_deboscat %>% st_as_sf())),
#   data_dirty_deboscat %>% st_as_sf(),
#   join = st_contains
# ) -> foo
# foo
