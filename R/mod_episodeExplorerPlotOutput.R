#' @title mod_episodeExplorerPlotOutput and mod_episodeExplorerPlot
#'
#' @description A shiny module to create and populate the data outputs
#'
#' @param id shiny id
#'
#' @export
mod_episodeExplorerPlotOutput <- function(id) {

  # ns
  ns <- shiny::NS(id)

  # UI ####
  shiny::tagList(
    shiny::br(),
    shiny::uiOutput(
      ns('mod_episodeExplorerPlot_container')
    )
  )
}

#' mod_episodeExplorerplot server function
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param lang lang reactive
#' @param episode_explorer_data_reactives data reactive
#'
#' @export
mod_episodeExplorerPlot <- function(
  input, output, session,
  lang, episode_explorer_data_reactives
) {


  # uiOutput ----------------------------------------------------------------------------------------------
  output$mod_episodeExplorerPlot_container <- shiny::renderUI({

    ns <- session$ns
    year_number <- shiny::req(episode_explorer_data_reactives$data) %>%
      dplyr::pull(year) %>%
      unique() %>%
      length()
    shiny::tabsetPanel(
      id = ns('episode_explorer_tabset'), type = 'pills',
      shiny::tabPanel(
        title = translate_app('plot', lang()),
        value = 'episode_explorer_plot_tab',
        shiny::tagList(
          shiny::fluidRow(
            shiny::h4(translate_app('affectation_general_info', lang())),
            shiny::column(
              width = 4,
              gt::gt_output(ns('epiexp_general_info'))
            ),
            shiny::column(
              width = 8,
              shiny::plotOutput(ns('epiexp_affectation_trend_plot'), height = 200)
            )
          ),
          shiny::h4(translate_app('affectation_plot', lang())),
          shiny::plotOutput(ns('epiexp_affectation_plot'), height = 300),
          shiny::h4(translate_app('spatial_plot', lang())),
          shiny::plotOutput(ns('epiexp_spatial_plot'), height =  280 + (280 * (year_number %/% 4)))
          # shiny::fluidRow(
          #   shiny::column(
          #     width = 9,
          #     shiny::h4(translate_app('affectation_general_info')),
          #     shiny::fluidRow(
          #       shiny::column(
          #         width = 4,
          #         gt::gt_output(ns('epiexp_general_info'))
          #       ),
          #       shiny::column(
          #         width = 8,
          #         shiny::plotOutput(ns('epiexp_affectation_trend_plot'), height = 200)
          #       )
          #     ),
          #     shiny::h4(translate_app('affectation_plot', lang())),
          #     shiny::plotOutput(ns('epiexp_affectation_plot'), height = 300)
          #   ),
          #   shiny::column(
          #     width = 3,
          #     shiny::h4(translate_app('spatial_plot', lang())),
          #     shiny::plotOutput(ns('epiexp_spatial_plot'), height = 200*year_number)
          #   )
          # )
        )


        # shiny::tagList(
        #   shiny::h4(translate_app('affectation_plot', lang())),
        #   shiny::plotOutput(ns('epiexp_affectation_plot'), height = relative_height),
        #   shiny::h4(translate_app('affectation_trend_plot', lang())),
        #   shiny::plotOutput(ns('epiexp_affectation_trend_plot'), height = relative_height),
        #   shiny::h4(translate_app('spatial_plot', lang())),
        #   shiny::plotOutput(ns('epiexp_spatial_plot'), height = relative_height)
        # )
      ),
      shiny::tabPanel(
        title = translate_app('table', lang()),
        value = 'episode_explorer_table_tab',
        DT::DTOutput(ns('episode_explorer_table'))
      )
    )
  })


  # plot --------------------------------------------------------------------------------------------------
  output$epiexp_affectation_plot <- shiny::renderPlot({
    shiny::req(episode_explorer_data_reactives$data) %>%
      create_affectation_plot(lang)
  })

  output$epiexp_affectation_trend_plot <- shiny::renderPlot({
    shiny::req(episode_explorer_data_reactives$data) %>%
      create_affectation_trend_plot(lang)
  })

  output$epiexp_spatial_plot <- shiny::renderPlot({
    shiny::req(episode_explorer_data_reactives$data) %>%
      create_spatial_plot()
  })

  output$epiexp_general_info <- gt::render_gt({
    shiny::req(episode_explorer_data_reactives$data) %>%
      dplyr::as_tibble() %>%
      dplyr::select(-geom) %>%
      dplyr::summarise(
        episode_id = dplyr::first(episode_id),
        species_number = length(unique(species_id)),
        # years_number = length(unique(year)),
        episode_starts = min(year),
        last_year_recorded = max(year)
      ) %>%
      tidyr::pivot_longer(
        cols = !episode_id,
        names_to = 'info_label',
        values_to = .$episode_id
      ) %>%
      dplyr::mutate(info_label = translate_app(info_label, lang())) %>%
      # dplyr::select(!episode_id) %>%
      gt::gt(
        rowname_col = 'info_label',
        groupname_col = 'episode_id'
      ) %>%
      gt::tab_style(
        style = gt::cell_fill(color = '#E8EAEB'),
        locations = list(gt::cells_body(), gt::cells_stub(), gt::cells_row_groups())
      ) %>%
      gt::tab_options(column_labels.hidden = TRUE)

  })


  # table -------------------------------------------------------------------------------------------------
  output$episode_explorer_table <- DT::renderDT({
    shiny::req(episode_explorer_data_reactives$data) %>%
      rlang::set_names(~ translate_app(.x, lang())) %>%
      dplyr::mutate(
        dplyr::across(where(is.numeric), round, digits = 2)
      ) %>%
      dplyr::as_tibble() %>%
      dplyr::select(-geom) %>%
      DT::datatable(
        rownames = FALSE,
        class = 'hover order-column stripe nowrap',
        filter = list(position = 'top', clear = FALSE, plain = FALSE),
        # extensions = 'Buttons',
        options = list(
          pageLength = 25, # counties + 1
          dom = 'ti',
          # buttons = I('colvis'),
          autoWidth = FALSE,
          initComplete = DT::JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'font-family': 'Montserrat'});",
            "$(this.api().table().body()).css({'font-family': 'Montserrat'});",
            "}"
          )
        )
      )

  })

}
