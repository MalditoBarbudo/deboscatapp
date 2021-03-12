#' @title mod_speciesInfoOutput and mod_speciesInfo
#'
#' @description module for creating the astounding viz when click
#'
#' @param id shiny id
#'
#' @export
mod_speciesInfoOutput <- function(id) {
  # ns
  ns <- shiny::NS(id)

  # ui skeleton (rows)
  shiny::tagList(
    shiny::fluidRow(
      shiny::br(),
      shiny::h4(shiny::textOutput(ns('mod_speciesInfo_title'))),
      shiny::uiOutput(ns("mod_speciesInfo_panel"))
    )
  )
}

#' mod_info server function
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param lang lang reactive
#'
#' @param year_explorer_data_reactives,year_explorer_output_reactives data reactives
#'
#' @export
mod_speciesInfo <- function(
  input, output, session,
  lang,
  year_explorer_data_reactives, year_explorer_output_reactives
) {

  # title
  output$mod_speciesInfo_title <- shiny::renderText({
    species_name <- req(year_explorer_data_reactives$species_sel)
    title_phrase <- translate_app("mod_speciesInfo_title_text", lang())
    glue::glue("{species_name} {title_phrase}")
  })

  # uiOutput
  output$mod_speciesInfo_panel <- shiny::renderUI({

    ns <- session$ns

    shiny::tagList(
      shiny::fluidRow(
        shiny::column(
          width = 12, align = 'center',
          shiny::plotOutput(ns('speciesInfo_packing_plot'))
        )
      ),
      shiny::fluidRow(
        shiny::column(
          width = 12, align = 'center',
          shiny::plotOutput(ns('speciesInfo_ts_plot'))
        )
      )
    )

  })

  # packing plot
  output$speciesInfo_packing_plot <- shiny::renderPlot({

    # data
    species_data <- deboscat_species_year_affectation_table %>%
      dplyr::filter(year == req(year_explorer_data_reactives$year_sel))

    # inputs
    species_sel <- shiny::req(year_explorer_data_reactives$species_sel)
    input_var_sel <- shiny::req(year_explorer_data_reactives$var_sel)
    new_episodes_sel <- shiny::req(year_explorer_data_reactives$new_episodes_sel)
    var_sel <- glue::glue("{input_var_sel}_{new_episodes_sel}")

    # packcircles layout and data
    packing_data <- packcircles::circleProgressiveLayout(
      species_data[[var_sel]], sizetype = 'area'
    )
    packing_plot_data <- packcircles::circleLayoutVertices(packing_data, npoints = 100) %>%
      dplyr::mutate(fill_val = species_data[[var_sel]][id])
    packing_plot_data_selected <- packing_plot_data %>%
      dplyr::filter(species_data[['species_id']][id] == species_sel)
    packing_plot_data_unselected <- packing_plot_data %>%
      dplyr::filter(species_data[['species_id']][id] != species_sel)
    text_data <- cbind(species_data, packing_data) %>%
      dplyr::filter(radius/max(radius, na.rm = TRUE) > 0.20 | species_id == species_sel)

    # plot
    ggplot() +
      geom_polygon(
        aes(x, y, group = id, fill = fill_val),
        colour = 'black', alpha = 0.6, show.legend = FALSE,
        data = packing_plot_data_unselected
      ) +
      geom_polygon(
        aes(x, y, group = id, fill = fill_val),
        colour = 'red', alpha = 0.6, show.legend = FALSE, size = 1,
        data = packing_plot_data_selected
      ) +
      geom_text(
        aes(x, y, label = species_id),
        data = text_data
      ) +
      coord_equal() +
      theme_void()
  })

  output$speciesInfo_ts_plot <- shiny::renderPlot({
    # data
    # deboscat_species_year_affectation_table

    # if (year_explorer_data_reactives$species_breakdown) {
    #   deboscat_species_year_affectation_table <-
    #     deboscat_species_year_affectation_table %>%
    #     dplyr::filter(species_id %in% shiny::req(year_explorer_data_reactives$species_sel))
    # }

    # inputs
    species_sel <- shiny::req(year_explorer_data_reactives$species_sel)
    input_var_sel <- shiny::req(year_explorer_data_reactives$var_sel)
    new_episodes_sel <- shiny::req(year_explorer_data_reactives$new_episodes_sel)
    var_sel <- rlang::expr(!!rlang::sym(glue::glue("{input_var_sel}_{new_episodes_sel}")))

    deboscat_species_year_affectation_table_selected <- deboscat_species_year_affectation_table %>%
      dplyr::filter(species_id == species_sel)
    deboscat_species_year_affectation_table_unselected <- deboscat_species_year_affectation_table %>%
      dplyr::filter(species_id != species_sel)

    ggplot() +
      geom_line(
        aes(x = year, y = !!var_sel, colour = species_id), alpha = 0.2,
        data = deboscat_species_year_affectation_table_unselected,
        show.legend = FALSE
      ) +
      geom_point(
        aes(x = year, y = !!var_sel, colour = species_id), alpha = 0.2,
        data = deboscat_species_year_affectation_table_unselected,
        show.legend = FALSE
      ) +
      geom_line(
        aes(x = year, y = !!var_sel), colour = 'red', size = 1,
        data = deboscat_species_year_affectation_table_selected,
        show.legend = FALSE
      ) +
      geom_point(
        aes(x = year, y = !!var_sel), colour = 'red', size = 4,
        data = deboscat_species_year_affectation_table_selected,
        show.legend = FALSE
      )

  })
}
