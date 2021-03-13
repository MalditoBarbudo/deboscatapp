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
    shiny::br(),
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
        leaflet::leafletOutput(ns('year_explorer_map'))
      ),
      shiny::tabPanel(
        title = translate_app('table', lang()),
        value = 'year_explorer_table_tab',
        DT::DTOutput(ns('year_explorer_table'))
      )
    )
  })

  # map ---------------------------------------------------------------------------------------------------
  output$year_explorer_map <- leaflet::renderLeaflet({
    county_map_data <- shiny::req(year_explorer_data_reactives$county_map_data)
    episodes_data <- shiny::req(year_explorer_data_reactives$episodes_data)
    var_sel <- glue::glue(
      "{year_explorer_data_reactives$var_sel}_{year_explorer_data_reactives$new_episodes_sel}"
    )

    map_palette <- leaflet::colorNumeric(
      palette = deboscat_palette(100, 'dark'),
      domain = county_map_data[[var_sel]],
      na.color = 'transparent'
    )

    map_palette_legend <- leaflet::colorNumeric(
      palette = deboscat_palette(100, 'dark'),
      domain = county_map_data[[var_sel]],
      na.color = 'transparent',
      reverse = TRUE
    )


    leaflet::leaflet() %>%
      leaflet::setView(1.744, 41.726, zoom = 8) %>%
      leaflet::addProviderTiles(
        leaflet::providers$Esri.WorldShadedRelief,
        group = 'Relief' %>% translate_app(lang())
      ) %>%
      # leaflet::addProviderTiles(
      #   leaflet::providers$Esri.WorldImagery,
      #   group = 'Imaginery' %>% translate_app(lang())
      # ) %>%
      leaflet::addMapPane('counties', zIndex = 410) %>%
      leaflet::addMapPane('episodes', zIndex = 420) %>%
      # counties polygons
      leaflet::addPolygons(
        data = county_map_data,
        group = 'counties',
        label = ~county_name,
        layerId = ~county_name,
        weight = 1, smoothFactor = 1,
        opacity = 1.0, fill = TRUE,
        color = '#6C7A89FF',
        fillColor = map_palette(county_map_data[[var_sel]]),
        fillOpacity = 0.7,
        highlightOptions = leaflet::highlightOptions(
          color = "#CF000F", weight = 2,
          bringToFront = FALSE
        ),
        options = leaflet::pathOptions(
          pane = 'counties'
        )
      ) %>%
      # episodes polygons
      leaflet::addPolygons(
        data = episodes_data,
        group = 'episodes',
        label = ~episode_id,
        layerId = ~episode_id,
        weight = 2, smoothFactor = 1,
        opacity = 1.0,
        color = 'black',
        # fill = TRUE, fillColor = 'black',
        highlightOptions = leaflet::highlightOptions(
          color = "#CF000F", weight = 2,
          bringToFront = FALSE
        ),
        options = leaflet::pathOptions(
          pane = 'episodes'
        )
      ) %>%
      leaflet::addLegend(
        pal = map_palette_legend, values = county_map_data[[var_sel]],
        title = translate_app(var_sel, lang()),
        position = 'bottomright', opacity = 1,
        labFormat = leaflet::labelFormat(
          transform = function(x) {sort(x, decreasing = TRUE)}
        )
      )
  })

  # observer to launch the floating plot panel when counties are clicked
  shiny::observeEvent(
    eventExpr = input$year_explorer_map_shape_click,
    handlerExpr = {

      click_info <- shiny::req(input$year_explorer_map_shape_click)

      if (click_info$group == 'counties') {
        shiny::showModal(
          shiny::modalDialog(
            mod_yearExplorerInfoOutput('mod_yearExplorerInfoOutput'),
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

    county_map_data %>%
      rlang::set_names(~ translate_app(.x, lang())) %>%
      dplyr::as_tibble() %>%
      dplyr::select(-geometry) %>%
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
    year_explorer_output_reactives$year_explorer_map_shape_click <- input$year_explorer_map_shape_click
  })

  return(year_explorer_output_reactives)

}
