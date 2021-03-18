# data_raw_2014_2020
# data_raw_2012_2013


# counties 06, 31, 32 ---------------------------------------------------------------------------------------------
# data_raw_2014_2020 %>%
#   dplyr::filter(episode_id == '06-043')
#
# data_raw_2012_2013 %>%
#   dplyr::filter(episode_id %in% c('06-020', '06-043', '06-047'))
#
# create_polygon_plot_2(c('06-010', '06-018'), data_raw_2012_2013)
# create_polygon_plot_2(c('06-020', '06-043'), data_dirty_2014_2020)

# 06-050, nothing to do


data_raw_2012_2013 <- data_raw_2012_2013 %>%
  # 06-045, remove 2012 episode
  dplyr::filter(!(episode_id == '06-045' & year == 2012)) %>%
  # 06-027, remove 2012 episode
  dplyr::filter(!(episode_id == '06-027' & year == 2012)) %>%
  # 06-048, remove 2013 episode
  dplyr::filter(!(episode_id == '06-048' & year == 2013)) %>%
  # 06-015, remove 2013 episode
  dplyr::filter(!(episode_id == '06-015' & year == 2012)) %>%
  # 06-018, remove 2012 episode
  dplyr::filter(!(episode_id == '06-018' & year == 2012)) %>%
  # 32-007, remove 2012 episode
  dplyr::filter(!(episode_id == '32-007' & year == 2012)) %>%
  # 32-006, remove all episodes
  dplyr::filter(!(episode_id == '32-006')) %>%
  # 32-005, remove all episodes
  dplyr::filter(!(episode_id == '32-005')) %>%
  # 32-004, remove all episodes
  dplyr::filter(!(episode_id == '32-004')) %>%
  # 32-002, remove all episodes
  dplyr::filter(!(episode_id == '32-002')) %>%
  # 32-001, remove all episodes
  dplyr::filter(!(episode_id == '32-001')) %>%
  # episodes code changes
  dplyr::mutate(
    episode_id = dplyr::case_when(
      # change 06-014 and 06-017 in 2012 because they are exchanged. So 2012 06-014 is 06-017 and viceversa
      episode_id == '06-014' & year %in% c(2012) ~ glue::glue('06-017'),
      episode_id == '06-017' & year %in% c(2012) ~ glue::glue('06-014'),

      TRUE ~ episode_id
    )
  )

data_raw_2014_2020 <- data_raw_2014_2020 %>%
  # 06-048, remove 2014 episode
  dplyr::filter(!(episode_id == '06-048' & year == 2014)) %>%
  # 31-011, remove 2014 episode
  dplyr::filter(!(episode_id == '31-011' & year == 2014)) %>%
  # 32-006, remove all episodes
  dplyr::filter(!(episode_id == '32-006')) %>%
  # 32-005, remove all episodes
  dplyr::filter(!(episode_id == '32-005')) %>%
  # 32-004, remove all episodes
  dplyr::filter(!(episode_id == '32-004')) %>%
  # 32-002, remove all episodes
  dplyr::filter(!(episode_id == '32-002')) %>%
  # 32-001, remove all episodes
  dplyr::filter(!(episode_id == '32-001')) %>%
  # episodes code changes
  dplyr::mutate(
    episode_id = dplyr::case_when(
      # change 06-018 to 06-043 in 2019 because is wrong
      episode_id == '06-018' & year %in% c(2019) ~ glue::glue('06-043'),
      TRUE ~ episode_id
    )
  )



# {
#   # spatial changes
#   temp_data <- .
#
#   poly_43 <- data_dirty_2014_2020[which(data_dirty_2014_2020$episode_id == '06-043' & data_dirty_2014_2020$year == 2017), 'geometry'][[1]][1]
#   poly_20 <- data_dirty_2014_2020[which(data_dirty_2014_2020$episode_id == '06-020' & data_dirty_2014_2020$year == 2017), 'geometry'][[1]][1]
#
#   poly_43_minus_20 <- sf::st_difference(poly_43, poly_20)
#   sf::st_centroid(poly_43_minus_20)
#   # there is still a small portion that must be removed
#   poly_06_043_fixed <<- sf::st_intersection(poly_43_minus_20, sf::st_buffer(sf::st_centroid(poly_43_minus_20), 1000))
#
#   temp_data
# } %>%



