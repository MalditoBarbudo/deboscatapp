## All episodes lag check
lag_check <- function(episode_data, keys) {
  episode_data |>
    dplyr::mutate(year_lag = dplyr::lag(year), difference = year - year_lag) |>
    dplyr::filter(difference > 2)
}

episode_with_lags <-
  data_clean_deboscat |>
  dplyr::as_tibble() |>
  dplyr::select(episode_id, year) |>
  dplyr::distinct() |>
  dplyr::group_by(episode_id) |>
  dplyr::group_modify(.f = lag_check) |>
  dplyr::arrange(year, episode_id) |>
  dplyr::mutate(
    old_episode_id = glue::glue("{episode_id}-{year_lag}")
  )
episode_with_lags

episode_id_mutate_exprs <- episode_with_lags |>
  dplyr::mutate(
    expr_mutate = glue::glue(
      "dplyr::if_else(
         episode_id == '{.data$episode_id}' & year < {.data$year}, '{.data$old_episode_id}', episode_id
       )"
    )
  ) |>
  dplyr::pull(expr_mutate) |>
  rlang::parse_exprs()

new_episode_mutate_exprs <- episode_with_lags |>
  # dplyr::group_by(episode_id) |>
  # dplyr::summarise(year = max(year)) |>
  dplyr::mutate(
    expr_mutate = glue::glue(
      "dplyr::if_else(
       episode_id == '{.data$episode_id}' & year == {.data$year}, TRUE, new_episode
     )"
    )
  ) |>
  dplyr::pull(expr_mutate) |>
  rlang::parse_exprs()

test_data <- data_muddy_deboscat_centroids

new_episode_mutate_exprs |>
  purrr::walk(
    .f = \(mutate_expr) {
      test_data <<- test_data |>
        dplyr::mutate(new_episode = !! mutate_expr)
    }
  )

test_data |>
  dplyr::filter(stringr::str_detect(episode_id, "14-019")) |>
  View()


episode_id_mutate_exprs |>
  purrr::walk(
    .f = \(mutate_expr) {
      test_data <<- test_data |>
        dplyr::mutate(episode_id = !! mutate_expr)
    }
  )

test_data |>
  dplyr::filter(stringr::str_detect(episode_id, "14-056")) |>
  View()

