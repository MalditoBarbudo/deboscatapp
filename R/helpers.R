# Call this function with an input (such as `textInput("text", NULL, "Search")`) if you
# want to add an input to the navbar (from dean attali,
# https://github.com/daattali/advanced-shiny)
navbarPageWithInputs <- function(..., inputs) {
  navbar <- shiny::navbarPage(...)
  form <- shiny::tags$form(class = "navbar-form", inputs)

  navbar[[3]][[1]]$children[[1]]$children[[2]] <- htmltools::tagAppendChild(
    navbar[[3]][[1]]$children[[1]]$children[[2]], form
  )
  navbar
}

create_affectation_plot <- function(episodes_data) {
  episodes_data %>%
    dplyr::select(-cover_perc) %>%
    tidyr::pivot_longer(
      dplyr::ends_with('perc'), names_to = "affectation", values_to = 'perc'
    ) %>%
    ggplot(aes(x = year, y = perc, fill = affectation, colour = affectation)) +
    geom_col(data = ~ dplyr::filter(.x, affectation != 'affected_trees_perc'), width = 0.4) +
    geom_line(data = ~ dplyr::filter(.x, affectation == 'affected_trees_perc'), size = 1) +
    geom_point(data = ~ dplyr::filter(.x, affectation == 'affected_trees_perc'), size = 4) +
    facet_grid(rows = dplyr::vars(episode_id), cols = dplyr::vars(species_id)) +
    scale_x_continuous(breaks = unique(episodes_data$year), labels = unique(episodes_data$year)) +
    scale_y_continuous(limits = c(0,100)) +
    theme(legend.position = 'bottom') +
    scale_fill_manual(values = deboscat_palette(4, 'light')) +
    scale_colour_manual(values = deboscat_palette(4, 'light')) +
    theme_minimal() +
    theme(
      plot.background = element_rect(fill = '#1C1C20', colour = '#1C1C20'),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = '#E8EAEB'),
      axis.text = element_text(colour = '#E8EAEB', size = 14),
      axis.title = element_text(colour = '#E8EAEB', size = 14),
      strip.background = element_rect(fill = '#1C1C20', colour = '#E8EAEB'),
      strip.text = element_text(colour = '#E8EAEB', size = 14),
      legend.position = 'bottom',
      legend.text = element_text(colour = '#E8EAEB', size = 14),
      legend.title = element_blank()
    )
}

create_affectation_trend_plot <- function(episodes_data) {
  episodes_data %>%
    ggplot(aes(x = year, y = cicatrization_index)) +
    geom_line(colour = deboscat_palette(3)[1], size = 1) +
    geom_point(size = 4, colour = deboscat_palette(3)[3]) +
    scale_x_continuous(breaks = unique(episodes_data$year), labels = unique(episodes_data$year)) +
    facet_grid(rows = dplyr::vars(episode_id)) +
    scale_y_continuous(limits = c(0,100)) +
    theme_minimal() +
    theme(
      plot.background = element_rect(fill = '#1C1C20', colour = '#1C1C20'),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = '#E8EAEB'),
      axis.text = element_text(colour = '#E8EAEB', size = 14),
      axis.title = element_text(colour = '#E8EAEB', size = 14),
      strip.background = element_rect(fill = '#1C1C20', colour = '#E8EAEB'),
      strip.text = element_text(colour = '#E8EAEB', size = 14),
      legend.position = 'bottom',
      legend.text = element_text(colour = '#E8EAEB', size = 14),
      legend.title = element_blank()
    )
}

create_spatial_plot <- function(episodes_data) {
  episodes_data %>%
    dplyr::select(year, episode_id, episode_area, geom) %>%
    dplyr::group_by(year, episode_id) %>%
    dplyr::slice(1) %>%
    ggplot() +
    geom_sf(aes(fill = episode_area), alpha = 0.9) +
    coord_sf(datum = sf::st_crs(episodes_data)) +
    facet_wrap(vars(year), ncol = 4) +
    theme(legend.position = 'bottom', axis.text.x = element_text(angle = 90)) +
    scale_fill_gradientn(colours = deboscat_palette(3, 'dark')) +
    theme_minimal() +
    theme(
      plot.background = element_rect(fill = '#1C1C20', colour = '#1C1C20'),
      panel.grid = element_line(color = '#E8EAEB', size = 0.2),
      axis.text = element_text(colour = '#E8EAEB', size = 8),
      axis.text.x = element_text(colour = '#E8EAEB', size = 8, angle = 45),
      strip.background = element_rect(fill = '#1C1C20', colour = '#E8EAEB'),
      strip.text = element_text(colour = '#E8EAEB', size = 12),
      legend.position = 'right',
      legend.text = element_text(colour = '#E8EAEB', size = 10),
      legend.title = element_blank()
    )

}


# episode_explorer_plot_helper <- function(episode_data) {
#   plot_list <- list(
#     affectation_plot = create_affectation_plot(episode_data),
#     cicatrization_plot = create_cicatrization_plot(episode_data),
#     spatial_plot = create_spatial_plot(episode_data)
#   )
#
#   top_plot <- cowplot::plot_grid(
#     plotlist = plot_list[1:2], nrow = 1, rel_widths = c(1, 0.3)
#   )
#
#   all_plot <- cowplot::plot_grid(
#     top_plot, plot_list[[3]], nrow = 2, rel_heights = c(1, 0.6)
#   )
#
#   all_plot
# }

#' translate app function
#'
#' translate the app based on the lang selected
translate_app <- function(id, lang) {

  app_translations

  id %>%
    purrr::map_chr(
      ~ app_translations %>%
        dplyr::filter(text_id == .x) %>% {
          data_filtered <- .
          if (nrow(data_filtered) < 1) {
            message(glue::glue("{.x} not found in app thesaurus"))
            .x
          } else {
            dplyr::pull(data_filtered, !! rlang::sym(glue::glue("translation_{lang}")))
          }
        }
    )
}

# cache_selected_choice
# logic is as follows:
#   - if the cached value is in choices provided, then return it, if not,
#     return the default value (first choice if not provided)
cache_selected_choice <- function(choices, cache, key, default = choices[1]) {
  cached_input <- cache$get(key, 'non_existent')
  if (all(cached_input %in% choices)) {
    return(cached_input)
  } else {
    return(default)
  }
}


# palettes ----------------------------------------------------------------------------------------------

deboscat_palette <- function(n, type = 'light') {
  ## deboscat palette
  deboscat_colors_dark <- c(green = '#91A63D', orange = '#F8931D', brown = '#643615')
  deboscat_colors_light <- c(green = '#91C779', orange = '#F37521', brown = '#715138')

  deboscat_palette_dark <- colorRampPalette(deboscat_colors_dark, interpolate = 'spline')
  deboscat_palette_light <- colorRampPalette(deboscat_colors_light, interpolate = 'spline')

  switch(
    type,
    'light' = deboscat_palette_light(n),
    'dark' = deboscat_palette_dark(n)
  )
}

# custom stats functions, capped to perform only with 3 or more
# stat_capped <- function(x, .f, ...) {
#   if (length(x[!is.na(x)]) < 3) {
#     res <- NA_integer_
#   } else {
#     res <- .f(x, ...)
#   }
#   return(res)
# }

# custom standar error function
# se_custom <- function(x) {
#   sd(x[!is.na(x)])/length(x[!is.na(x)])
# }

# raw data grouping, for preset polys or custom ones
# raw_data_grouping <- function(raw_data, data_scale, custom_polygon) {
#
#   # if the scale is one of the presets, group by that and return it
#   if (!data_scale %in% c('file', 'drawn_polygon')) {
#     res <- raw_data %>%
#       dplyr::as_tibble() %>%
#       dplyr::select(-geometry) %>%
#       dplyr::group_by(!! rlang::sym(data_scale))
#     return(res)
#   }
#
#   # if scale is given by the user (file or drawn poly) then we need to
#   # make the intersection of the data and the polygons
#   #
#   # get the custom polygon with the reactive and validate it
#   custom_poly <- custom_polygon()
#   shiny::validate(shiny::need(custom_poly, 'no custom poly'))
#
#   # get only the plots inside the polygons supplied
#   # The logic is as follows:
#   #   - get the indexes of the intersection between them
#   #   - use that indexes to extract the poly_id from the custom poly
#   #   - create a new column in the main data with the poly_id to summarise
#   #     later
#   indexes <- sf::st_intersects(raw_data, custom_poly) %>%
#     as.numeric()
#   polys_names <- custom_poly %>%
#     dplyr::pull(poly_id) %>%
#     as.character() %>%
#     magrittr::extract(indexes)
#
#   res <- raw_data %>%
#     dplyr::as_tibble() %>%
#     dplyr::select(-geometry) %>%
#     dplyr::mutate(poly_id = polys_names) %>%
#     dplyr::filter(!is.na(poly_id)) %>%
#     dplyr::group_by(poly_id)
# }

