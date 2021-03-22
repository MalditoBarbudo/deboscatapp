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
          width = 4, align = 'center',
          shiny::plotOutput(ns('speciesInfo_packing_plot'))
        ),
        shiny::column(
          width = 8, align = 'center',
          shiny::plotOutput(ns('speciesInfo_ts_plot'))
        )
      )
    )

  })

  # packing plot
  output$speciesInfo_packing_plot <- shiny::renderPlot({
    create_packing_plot(
      data = deboscat_species_year_affectation_table,
      selected_value = shiny::req(year_explorer_data_reactives$species_sel),
      type_variable = species_id,
      affectation_variable = shiny::req(year_explorer_data_reactives$var_sel),
      new_episodes = shiny::req(year_explorer_data_reactives$new_episodes_sel),
      year = shiny::req(year_explorer_data_reactives$year_sel),
      lang = lang
    )
  })

  output$speciesInfo_ts_plot <- shiny::renderPlot({
    create_info_ts_plot(
      data = deboscat_species_year_affectation_table,
      selected_value = shiny::req(year_explorer_data_reactives$species_sel),
      type_variable = species_id,
      affectation_variable = shiny::req(year_explorer_data_reactives$var_sel),
      new_episodes = shiny::req(year_explorer_data_reactives$new_episodes_sel),
      lang = lang
    )
  })
}
