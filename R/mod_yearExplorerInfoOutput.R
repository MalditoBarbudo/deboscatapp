#' @title mod_yearExplorerInfoOutput and mod_yearExplorerInfo
#'
#' @description module for creating the astounding viz when click
#'
#' @param id shiny id
#'
#' @export
mod_yearExplorerInfoOutput <- function(id) {
  # ns
  ns <- shiny::NS(id)

  # ui skeleton (rows)
  shiny::tagList(
    shiny::fluidRow(
      # shiny::br(),
      shiny::h4(shiny::textOutput(ns('mod_yearExplorerInfo_title'))),
      shiny::uiOutput(ns("mod_yearExplorerInfo_panel"))
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
mod_yearExplorerInfo <- function(
  input, output, session,
  lang,
  year_explorer_data_reactives, year_explorer_output_reactives
) {

  # title
  output$mod_yearExplorerInfo_title <- shiny::renderText({
    county_name <- req(year_explorer_output_reactives$year_explorer_map_shape_click$id)
    title_phrase <- translate_app("mod_yearExplorerInfo_title_text", lang())
    glue::glue("{county_name} {title_phrase}")
  })


  # uiOutput ----------------------------------------------------------------------------------------------
  output$mod_yearExplorerInfo_panel <- shiny::renderUI({

    ns <- session$ns

    shiny::tagList(
      shiny::fluidRow(
        shiny::column(
          width = 4, align = 'center',
          shiny::plotOutput(ns('yearExplorerInfo_packing_plot'))
        ),
        shiny::column(
          width = 8, align = 'center',
          shiny::plotOutput(ns('yearExplorerInfo_ts_plot'))
        )
      )
    )

  })

  # packing plot
  output$yearExplorerInfo_packing_plot <- shiny::renderPlot({
    create_packing_plot(
      data = shiny::req(year_explorer_data_reactives$county_map_data),
      selected_value = shiny::req(year_explorer_output_reactives$year_explorer_map_shape_click$id),
      type_variable = county_name,
      affectation_variable = shiny::req(year_explorer_data_reactives$var_sel),
      new_episodes = shiny::req(year_explorer_data_reactives$new_episodes_sel),
      year = shiny::req(year_explorer_data_reactives$year_sel),
      lang = lang
    )
  })

  output$yearExplorerInfo_ts_plot <- shiny::renderPlot({
    # data
    yearly_report_data <- shiny::req(year_explorer_data_reactives$yearly_report_data)

    if (year_explorer_data_reactives$species_breakdown) {
      yearly_report_data <-
        yearly_report_data %>%
        dplyr::filter(species_id %in% shiny::req(year_explorer_data_reactives$species_sel))
    }

    create_info_ts_plot(
      data = yearly_report_data,
      selected_value = shiny::req(year_explorer_output_reactives$year_explorer_map_shape_click$id),
      type_variable = county_name,
      affectation_variable = shiny::req(year_explorer_data_reactives$var_sel),
      new_episodes = shiny::req(year_explorer_data_reactives$new_episodes_sel),
      lang = lang
    )
  })
}
