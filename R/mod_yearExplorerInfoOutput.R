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
      shiny::br(),
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

  # uiOutput
  output$mod_yearExplorerInfo_panel <- shiny::renderUI({

    ns <- session$ns

    shiny::tagList(
      shiny::fluidRow(
        shiny::column(
          width = 12, align = 'center',
          shiny::plotOutput(ns('yearExplorerInfo_packing_plot'))
        )
      ),
      shiny::fluidRow(
        shiny::column(
          width = 12, align = 'center',
          shiny::plotOutput(ns('yearExplorerInfo_ts_plot'))
        )
      )
    )

  })

  # packing plot
  output$yearExplorerInfo_packing_plot <- shiny::renderPlot({

    # data
    county_map_data <- shiny::req(year_explorer_data_reactives$county_map_data)

    # inputs
    county_clicked <- shiny::req(year_explorer_output_reactives$year_explorer_map_shape_click$id)
    input_var_sel <- shiny::req(year_explorer_data_reactives$var_sel)
    new_episodes_sel <- shiny::req(year_explorer_data_reactives$new_episodes_sel)
    var_sel <- glue::glue("{input_var_sel}_{new_episodes_sel}")

    # packcircles layout and data
    packing_data <- packcircles::circleProgressiveLayout(county_map_data[[var_sel]], sizetype = 'area')
    packing_plot_data <- packcircles::circleLayoutVertices(packing_data, npoints = 100) %>%
      dplyr::mutate(fill_val = county_map_data[[var_sel]][id])
    packing_plot_data_selected <- packing_plot_data %>%
      dplyr::filter(county_map_data[['county_name']][id] == county_clicked)
    packing_plot_data_unselected <- packing_plot_data %>%
      dplyr::filter(county_map_data[['county_name']][id] != county_clicked)
    text_data <- cbind(county_map_data, packing_data) %>%
      dplyr::filter(radius/max(radius, na.rm = TRUE) > 0.20 | county_name == county_clicked)

    # plot
    ggplot() +
      geom_polygon(
        aes(
          x, y, group = id, fill = fill_val
          # tooltip = glue::glue("{text_data[['county_name']][id]}: {fill_val} ha"),
          # data_id = id
        ),
        colour = 'black', alpha = 0.6, show.legend = FALSE,
        data = packing_plot_data_unselected
      ) +
      geom_polygon(
        aes(
          x, y, group = id, fill = fill_val
          # tooltip = glue::glue("{text_data[['county_name']][id]}: {fill_val} ha"),
          # data_id = id
        ),
        colour = 'red', alpha = 0.6, show.legend = FALSE, size = 1,
        data = packing_plot_data_selected
      ) +
      geom_text(
        aes(x, y, label = county_name),
        data = text_data, colour = '#E8EAEB'
      ) +
      # coord_equal() +
      scale_fill_gradientn(colours = deboscat_palette(3, 'dark')) +
      scale_x_continuous(expand = expansion(mult = 1) ) +
      theme_void() +
      theme(plot.background = element_rect(fill = '#1C1C20'))
  })

  output$yearExplorerInfo_ts_plot <- shiny::renderPlot({
    # data
    yearly_report_data <- shiny::req(year_explorer_data_reactives$yearly_report_data)

    if (year_explorer_data_reactives$species_breakdown) {
      yearly_report_data <-
        yearly_report_data %>%
        dplyr::filter(species_id %in% shiny::req(year_explorer_data_reactives$species_sel))
    }

    # inputs
    county_clicked <- shiny::req(year_explorer_output_reactives$year_explorer_map_shape_click$id)
    input_var_sel <- shiny::req(year_explorer_data_reactives$var_sel)
    new_episodes_sel <- shiny::req(year_explorer_data_reactives$new_episodes_sel)
    var_sel <- rlang::expr(!!rlang::sym(glue::glue("{input_var_sel}_{new_episodes_sel}")))

    yearly_report_data_selected <- yearly_report_data %>%
      dplyr::filter(county_name == county_clicked)
    yearly_report_data_unselected <- yearly_report_data %>%
      dplyr::filter(county_name != county_clicked)

    ggplot() +
      geom_line(
        aes(x = year, y = !!var_sel, colour = county_name), alpha = 0.2,
        data = yearly_report_data_unselected,
        show.legend = FALSE
      ) +
      geom_point(
        aes(x = year, y = !!var_sel, colour = county_name), alpha = 0.2,
        data = yearly_report_data_unselected,
        show.legend = FALSE
      ) +
      scale_colour_manual(palette = deboscat_palette) +
      geom_line(
        aes(x = year, y = !!var_sel), colour = deboscat_palette(1, 'light'), size = 1,
        data = yearly_report_data_selected,
        show.legend = FALSE
      ) +
      geom_point(
        aes(x = year, y = !!var_sel), colour = deboscat_palette(1, 'light'), size = 4,
        data = yearly_report_data_selected,
        show.legend = FALSE
      ) +
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

  })
}
