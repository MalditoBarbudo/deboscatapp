#' @title mod_saveDataUI and mod_saveData
#'
#' @description module for creating the astounding viz when click
#'
#' @param id shiny id
#'
#' @export
mod_saveDataUI <- function(id) {
  # ns
  ns <- shiny::NS(id)
  # ui
  shiny::uiOutput(ns("mod_save_container"))
}

#' mod_saveData
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param lang language selected
#'
#' @param data_reactives reactives with the needed data to save
#' @param data_type character vector of length 1, with the type of data to save: "episode_explorer" or
#'   "year_explorer"
#'
#' @export
#'
#' @rdname mod_saveDataUI
mod_saveData <- function(
  input, output, session,
  lang,
  data_reactives,
  data_type
) {


  # uiOutput ----------------------------------------------------------------------------------------------
  output$mod_save_container <- shiny::renderUI({

    ns <- session$ns

    # labels, that depend on data_type
    save_spatial_label <- translate_app('save_spatial_label', lang())
    save_spatial_choices <- c(csv = '.csv', spatial = '.gpkg') %>%
      rlang::set_names(~ translate_app(.x, lang()))
    save_all_label <- translate_app('save_all_label_counties', lang())
    save_all_choices <- c(all_years = 'all', selected_years = 'selected') %>%
      rlang::set_names(~ translate_app(.x, lang()))

    if (data_type == 'episode_explorer') {
      save_all_label <- translate_app('save_all_label_episodes', lang())
      save_all_choices <- c(all_episodes = 'all', selected_episodes = 'selected') %>%
        rlang::set_names(~ translate_app(.x, lang()))
    }

    shiny::tagList(
      # row for year_explorer
      shiny::fluidRow(
        shiny::column(
          width = 4, align = 'center',
          shinyWidgets::awesomeRadio(
            ns('save_all'),
            label = save_all_label,
            choices = save_all_choices
          )
        ),
        shiny::column(
          width = 4, align = 'center',
          shinyWidgets::awesomeRadio(
            ns('save_spatial'),
            label = save_spatial_label,
            choices = save_spatial_choices
          )
        ),
        shiny::column(
          width = 4, align = 'center',
          shiny::br(),
          shiny::downloadButton(
            ns('save_btn'),
            label = translate_app('save', lang())
          )
        )
      )
    ) # end of tagList
  }) # end of renderUI


  # download logic ----------------------------------------------------------------------------------------
  output$save_btn <- shiny::downloadHandler(
    filename = function() {

      base_name <- 'deboscat'

      if (data_type == 'episode_explorer') {
        base_name <- glue::glue('{base_name}_episodes')
      } else {
        base_name <- glue::glue('{base_name}_counties_years')
      }

      if (!is.null(data_reactives$species_breakdown) && data_reactives$species_breakdown) {
        base_name <- glue::glue("{base_name}_species")
      }

      glue::glue("{Sys.Date() %>% stringr::str_remove_all('-')}_{base_name}{input$save_spatial}")

    },
    content = function(file) {
      if (data_type == 'episode_explorer') {
        if (input$save_all == 'all') {
          if (input$save_spatial == '.csv') {
            readr::write_csv(deboscat_table, file)
          } else {
            sf::st_write(deboscat_table, file)
          }
        } else {
          if (input$save_spatial == '.csv') {
            readr::write_csv(data_reactives$data, file)
          } else {
            sf::st_write(data_reactives$data, file)
          }
        }
      } else {
        if (input$save_all == 'all') {
          if (input$save_spatial == '.csv') {
            readr::write_csv(data_reactives$yearly_report_data, file)
          } else {
            sf::st_write(data_reactives$yearly_report_data, file)
          }
        } else {
          if (input$save_spatial == '.csv') {
            readr::write_csv(data_reactives$county_map_data, file)
          } else {
            sf::st_write(data_reactives$county_map_data, file)
          }
        }
      }
    }
  )
}
