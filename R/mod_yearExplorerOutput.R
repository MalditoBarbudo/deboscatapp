#' @title mod_yearExplorerOutput and mod_yearExplorer
#'
#' @description A shiny module to create and populate the data outputs
#'
#' @param id shiny id
#'
#' @export
mod_yearExplorerOutput <- function(id) {

  # ns
  ns <- shiny::NS(id)

  # UI ####
  shiny::tagList(
    shiny::uiOutput(ns('year_explorer_container'))
  )
}

#' mod_yearExplorer server function
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param lang lang reactive
#' @param year_explorer_data_reactives data reactive
#'
#' @export
mod_yearExplorer <- function(
  input, output, session,
  lang, year_explorer_data_reactives
) {


  # uiOutput ----------------------------------------------------------------------------------------------
  output$year_explorer_container <- shiny::renderUI({
    ns <- session$ns

    shiny::tabsetPanel(
      id = ns('year_explorer_tabset'), type = 'pills',
      shiny::tabPanel(
        title = translate_app('map', lang()),
        value = 'year_explorer_map_tab',
        mapdeck::mapdeckOutput(ns('year_explorer_map'), height = 600)
      ),
      shiny::tabPanel(
        title = translate_app('table', lang()),
        value = 'year_explorer_table_tab',
        DT::DTOutput(ns('year_explorer_table'))
      )
    )
  })

  # map ---------------------------------------------------------------------------------------------------
  output$year_explorer_map <- mapdeck::renderMapdeck(
    {
      
      session$outputOptions("mod_yearExplorerOutput-year_explorer_map", priority = 1000)
      mapdeck::mapdeck(
        # style = mapdeck::mapdeck_style('dark'),
        style = "https://raw.githubusercontent.com/CartoDB/basemap-styles/refs/heads/master/mapboxgl/dark-matter-nolabels.json",
        location = c(1.744, 41.726), zoom = 7, pitch = 0
      )
    },
    env = parent.frame()
  )

  shiny::observe(
    x = {
      county_map_data <- shiny::req(year_explorer_data_reactives$county_map_data)
      episodes_data <- shiny::req(year_explorer_data_reactives$episodes_data)
      var_sel <- glue::glue(
        "{year_explorer_data_reactives$var_sel}_{year_explorer_data_reactives$new_episodes_sel}"
      )

      if (is.null(shiny::outputOptions(output, "year_explorer_map")$priority)) {
        shiny::invalidateLater(1000)
      }

      map_palette <- scales::col_numeric(
        deboscat_palette(100, "dark"),
        c(
          min(county_map_data[[var_sel]], na.rm = TRUE),
          max(county_map_data[[var_sel]], na.rm = TRUE)
        ),
        na.color = "#FFFFFF00", reverse = FALSE, alpha = TRUE
      )

      county_map_data <- county_map_data |>
        # when aggregating for county and species, we can have NAs due to no
        # episodes when filtering by new or old
        dplyr::mutate(
          !!var_sel := dplyr::if_else(
            is.na(!!rlang::sym(var_sel)),
            0, !!rlang::sym(var_sel)
          ),
          # mapdeck variables
          hex = map_palette(.data[[var_sel]]),
          tooltip = paste0(
            "<p>", county_name, ": ", round(.data[[var_sel]], 2), "</p>"
          )
        )
      
      # custom legend (to be able to show in natural order, high values up)
      legend_js <- mapdeck::legend_element(
        variables = rev(round(seq(
          min(county_map_data[[var_sel]], na.rm = TRUE),
          max(county_map_data[[var_sel]], na.rm = TRUE),
          length.out = 10
        ), 0)),
        colours = scales::col_numeric(
          deboscat_palette(10, "dark"),
          c(min(county_map_data[[var_sel]], na.rm = TRUE), max(county_map_data[[var_sel]], na.rm = TRUE)),
          na.color = "#FFFFFF00", reverse = TRUE, alpha = TRUE
        )(seq(
          min(county_map_data[[var_sel]], na.rm = TRUE),
          max(county_map_data[[var_sel]], na.rm = TRUE),
          length.out = 10
        )),
        colour_type = "fill", variable_type = "gradient",
        title = translate_app(var_sel, lang())
      ) |>
        mapdeck::mapdeck_legend()

      # map update with
      mapdeck::mapdeck_update(map_id = session$ns("year_explorer_map")) |>
        mapdeck::clear_polygon(layer_id = "counties") |>
        mapdeck::clear_polygon(layer_id = "episodes") |>
        mapdeck::add_polygon(
          data = county_map_data, layer_id = "counties",
          tooltip = "tooltip",
          id = "county_name",
          stroke_colour = "hex",
          fill_colour = "hex",
          fill_opacity = 1,
          auto_highlight = TRUE, highlight_colour = "#19ADCB00",
          update_view = FALSE, focus_layer = FALSE,
          legend = legend_js
        ) |>
        # episodes polygons
        mapdeck::add_polygon(
          data = episodes_data, layer_id = "episodes",
          tooltip = "episode_id",
          id = "episode_id",
          stroke_colour = "#ff0000ff", stroke_width = 25,
          fill_colour = "#60606080",
          fill_opacity = 0.5,
          auto_highlight = TRUE, highlight_colour = "#ff000080",
          update_view = FALSE, focus_layer = FALSE
        )
    }
  )

  # observer to launch the floating plot panel when counties are clicked
  shiny::observeEvent(
    eventExpr = input$year_explorer_map_polygon_click,
    handlerExpr = {
      id_click <-
        jsonlite::fromJSON(shiny::req(input$year_explorer_map_polygon_click))$layerId

      if (id_click == 'polygon-counties') {
        shiny::showModal(
          shiny::modalDialog(
            mod_infoOutput('mod_infoOutput_counties'),
            footer = shiny::modalButton(
              translate_app('dismiss', lang())
            ),
            size = 'l', easyClose = TRUE
          )
        )
      }
    }
  )


  # table -------------------------------------------------------------------------------------------------
  output$year_explorer_table <- DT::renderDT({
    county_map_data <- shiny::req(year_explorer_data_reactives$county_map_data)

    county_map_data |>
      rlang::set_names(~ translate_app(.x, lang())) |>
      dplyr::as_tibble() |>
      dplyr::select(-geometry) |>
      DT::datatable(
        rownames = FALSE,
        class = 'hover order-column stripe nowrap',
        filter = list(position = 'top', clear = FALSE, plain = FALSE),
        # extensions = 'Buttons',
        options = list(
          pageLength = 33, # counties + 1
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


  # reactives to return -----------------------------------------------------------------------------------
  year_explorer_output_reactives <- shiny::reactiveValues()
  shiny::observe({
    year_explorer_output_reactives$year_explorer_map_shape_click <-
      jsonlite::fromJSON(shiny::req(input$year_explorer_map_polygon_click))
  })

  return(year_explorer_output_reactives)

}
