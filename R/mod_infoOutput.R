#' @title mod_infoOutput and mod_info
#'
#' @description module for creating the astounding viz when click
#'
#' @param id shiny id
#'
#' @export
mod_infoOutput <- function(id) {
  # ns
  ns <- shiny::NS(id)

  # ui skeleton (rows)
  shiny::tagList(
    shiny::fluidRow(
      # shiny::br(),
      shiny::h4(shiny::textOutput(ns('mod_info_title'))),
      shiny::uiOutput(ns("mod_info_panel"))
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
#' @param trigger character with the trigger (species_id or county_name)
#'
#' @export
mod_info <- function(
  input, output, session,
  lang,
  year_explorer_data_reactives, year_explorer_output_reactives, trigger
) {

  # title
  output$mod_info_title <- shiny::renderText({
    year_sel <- shiny::req(year_explorer_data_reactives$year_sel)
    selected_value <- switch(
      trigger,
      'species_id' = shiny::req(year_explorer_data_reactives$species_sel),
      'county_name' = shiny::req(year_explorer_output_reactives$year_explorer_map_shape_click$object$properties$id)
    )
    species_sel <- ""
    if (year_explorer_data_reactives$species_breakdown & trigger == 'county_name') {
      species_sel <- glue::glue(" ({shiny::req(year_explorer_data_reactives$species_sel)})")
    }

    glue::glue(translate_app('mod_info_title', lang()))
  })


  # uiOutput ----------------------------------------------------------------------------------------------
  output$mod_info_panel <- shiny::renderUI({

    ns <- session$ns

    shiny::tagList(
      shiny::fluidRow(
        shiny::column(
          width = 4, align = 'center',
          shiny::plotOutput(ns('info_packing_plot'))
        ),
        shiny::column(
          width = 8, align = 'center',
          shiny::plotOutput(ns('info_ts_plot'))
        )
      )
    )

  })

  # packing plot
  output$info_packing_plot <- shiny::renderPlot({

    if (trigger == 'county_name') {
      create_packing_plot(
        data = shiny::req(year_explorer_data_reactives$county_map_data),
        selected_value = shiny::req(year_explorer_output_reactives$year_explorer_map_shape_click$object$properties$id),
        type_variable = county_name,
        affectation_variable = shiny::req(year_explorer_data_reactives$var_sel),
        new_episodes = shiny::req(year_explorer_data_reactives$new_episodes_sel),
        year = shiny::req(year_explorer_data_reactives$year_sel),
        lang = lang
      )
    } else {
      create_packing_plot(
        data = deboscat_species_year_affectation_table,
        selected_value = shiny::req(year_explorer_data_reactives$species_sel),
        type_variable = species_id,
        affectation_variable = shiny::req(year_explorer_data_reactives$var_sel),
        new_episodes = shiny::req(year_explorer_data_reactives$new_episodes_sel),
        year = shiny::req(year_explorer_data_reactives$year_sel),
        lang = lang
      )
    }
  })

  output$info_ts_plot <- shiny::renderPlot({

    if (trigger == 'county_name') {
      # data
      yearly_report_data <- shiny::req(year_explorer_data_reactives$yearly_report_data)

      if (year_explorer_data_reactives$species_breakdown) {
        yearly_report_data <-
          yearly_report_data |>
          dplyr::filter(species_id %in% shiny::req(year_explorer_data_reactives$species_sel))
      }

      create_info_ts_plot(
        data = yearly_report_data,
        selected_value = shiny::req(year_explorer_output_reactives$year_explorer_map_shape_click$object$properties$id),
        type_variable = county_name,
        affectation_variable = shiny::req(year_explorer_data_reactives$var_sel),
        new_episodes = shiny::req(year_explorer_data_reactives$new_episodes_sel),
        lang = lang
      )
    } else {
      create_info_ts_plot(
        data = deboscat_species_year_affectation_table,
        selected_value = shiny::req(year_explorer_data_reactives$species_sel),
        type_variable = species_id,
        affectation_variable = shiny::req(year_explorer_data_reactives$var_sel),
        new_episodes = shiny::req(year_explorer_data_reactives$new_episodes_sel),
        lang = lang
      )
    }
  })
}
