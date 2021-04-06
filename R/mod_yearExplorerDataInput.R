#' @title mod_yearExplorerDataInput and mod_yearExplorerData
#'
#' @description A shiny module to create and populate the data inputs
#'
#' @param id shiny id
#'
#' @export
mod_yearExplorerDataInput <- function(id) {

  # ns
  ns <- shiny::NS(id)

  # UI ####
  shiny::tagList(
    shiny::br(),
    shiny::uiOutput(
      ns('mod_yearExplorerData_container')
    )
  )
}

#' mod_yearExplorerData server function
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param lang lang reactive
#'
#' @param cache memoryCache object to store the selected inputs and retrieve them when the underlying data
#'   changes.
#'
#' @export
mod_yearExplorerData <- function(
  input, output, session,
  lang,
  cache
) {

  # uiOutput ----------------------------------------------------------------------------------------------
  output$mod_yearExplorerData_container <- shiny::renderUI({

    ns <- session$ns

    # choices
    year_choices <- deboscat_table %>% dplyr::pull(year) %>% unique()
    species_choices <- 'Quercus ilex'
    new_episodes_choices <- c('all', 'old', 'new') %>%
      purrr::set_names(nm = translate_app(c('all_episodes', 'old_episodes', 'new_episodes'), lang()))
    var_choices <- 'affected_area'


    # taglist
    shiny::tagList(
      shiny::h4(translate_app('h4_yearexp_episodes_data_sel', lang())),
      shinyWidgets::pickerInput(
        ns('year_explorer_year_sel'),
        label = translate_app('year_explorer_year_sel', lang()),
        choices = year_choices,
        options = shinyWidgets::pickerOptions(
          actionsBox = FALSE,
          noneSelectedText = translate_app(
            'deselect-all-text', lang()
          ),
          selectedTextFormat =  'count',
          countSelectedText =  translate_app(
            'count-selected-text-value', lang()
          ),
          size = 10,
          liveSearch = TRUE,
          tickIcon = 'glyphicon-tree-deciduous'
        )
      ),
      shinyWidgets::pickerInput(
        ns('year_explorer_new_episodes_sel'),
        label = translate_app('year_explorer_new_episodes_sel', lang()),
        choices = new_episodes_choices,
        options = shinyWidgets::pickerOptions(
          actionsBox = FALSE,
          noneSelectedText = translate_app(
            'deselect-all-text', lang()
          ),
          selectedTextFormat =  'count',
          countSelectedText =  translate_app(
            'count-selected-text-value', lang()
          ),
          size = 10,
          liveSearch = TRUE,
          tickIcon = 'glyphicon-tree-deciduous'
        )
      ),
      shinyWidgets::prettyCheckbox(
        ns('year_explorer_species_breakdown'),
        label = translate_app('year_explorer_species_breakdown_off', lang()),
        value = FALSE,
        shape = 'round',
        status = 'success'
      ),
      # shinyWidgets::prettyToggle(
      #   ns('year_explorer_species_breakdown'),
      #   label_on = translate_app('year_explorer_species_breakdown_on', lang()),
      #   label_off = translate_app('year_explorer_species_breakdown_off', lang()),
      #   value = FALSE,
      #   shape = 'round'
      # ),
      shinyjs::hidden(
        shiny::div(
          id = ns('species_breakdown_warning'), align = 'center',
          style = glue::glue("color: {deboscat_palette(3, 'dark')[2]};"),
          shiny::p(shiny::icon('exclamation-triangle')),
          shiny::p(translate_app('species_breakdown_warning', lang()))
        )
      ),
      shiny::br(),
      # viz
      shiny::h4(translate_app('h4_yearexp_episodes_viz_sel', lang())),
      shinyWidgets::pickerInput(
        ns('year_explorer_var_sel'),
        label = translate_app('year_explorer_var_sel', lang()),
        choices = var_choices,
        selected = cache_selected_choice(
          choices = var_choices,
          cache = cache,
          key = 'varsel',
          default = 'affected_area'
        ),
        options = shinyWidgets::pickerOptions(
          actionsBox = FALSE,
          noneSelectedText = translate_app(
            'deselect-all-text', lang()
          ),
          selectedTextFormat =  'count',
          countSelectedText =  translate_app(
            'count-selected-text-value', lang()
          ),
          size = 10,
          liveSearch = TRUE,
          tickIcon = 'glyphicon-tree-deciduous'
        )
      ),
      shinyjs::hidden(
        shiny::div(
          id = ns('species_div'),
          shinyWidgets::pickerInput(
            ns('year_explorer_species_sel'),
            label = translate_app('year_explorer_species_sel', lang()),
            choices = species_choices,
            selected = cache_selected_choice(
              choices = species_choices,
              cache = cache,
              key = 'speciessel',
              default = 'Quercus ilex'
            ),
            options = shinyWidgets::pickerOptions(
              actionsBox = FALSE,
              noneSelectedText = translate_app(
                'deselect-all-text', lang()
              ),
              selectedTextFormat =  'count',
              countSelectedText =  translate_app(
                'count-selected-text-value', lang()
              ),
              size = 10,
              liveSearch = TRUE,
              tickIcon = 'glyphicon-tree-deciduous'
            )
          ),
          shiny::actionButton(
            ns('show_species_info'),
            label = translate_app('show_species_info', lang()),
            icon = shiny::icon('eye')
          )
        ) # end of species div
      ), # end of hidden species
      shiny::br(),
      shiny::actionButton(
        ns('year_explorer_save_btn'),
        label = translate_app('save', lang())
      )
    )
  })


  # data reactives ----------------------------------------------------------------------------------------
  yearly_report_data <- shiny::reactive({

    shiny::req(!is.null(input$year_explorer_species_breakdown))

    # inputs
    species_breakdown <- input$year_explorer_species_breakdown

    if (isTRUE(species_breakdown)) {
      deboscat_species_year_county_affectation_table
    } else {
      deboscat_counties_year_affectation_table
    }
  })

  county_map_data <- shiny::reactive({
    # inputs
    shiny::req(!is.null(input$year_explorer_species_breakdown))
    shiny::req(!is.null(input$year_explorer_species_sel))
    year_sel <- shiny::req(input$year_explorer_year_sel)
    new_episodes_sel <- shiny::req(input$year_explorer_new_episodes_sel)
    var_sel <- shiny::req(input$year_explorer_var_sel)
    species_breakdown <- input$year_explorer_species_breakdown
    species_sel <- input$year_explorer_species_sel

    yearly_report_data() %>%
      dplyr::filter(year == year_sel) %>% {
        temp_data <- .
        if (isTRUE(species_breakdown)) {
          temp_data %>%
            dplyr::filter(species_id == species_sel)
        } else {
          temp_data
        }
      } %>%
      dplyr::select(
        dplyr::any_of(c('year', 'county_name', 'species_id', glue::glue("{var_sel}_{new_episodes_sel}")))
      )

  })

  # episodes data (for plotting the episodes in the map)
  episodes_data <- shiny::reactive({
    # inputs
    shiny::req(!is.null(input$year_explorer_species_breakdown))
    shiny::req(!is.null(input$year_explorer_species_sel))
    year_sel <- shiny::req(input$year_explorer_year_sel)
    new_episodes_sel <- shiny::req(input$year_explorer_new_episodes_sel)
    var_sel <- shiny::req(input$year_explorer_var_sel)
    species_breakdown <- input$year_explorer_species_breakdown
    species_sel <- input$year_explorer_species_sel

    new_episodes_filter <- switch(
      new_episodes_sel,
      'new' = rlang::expr(new_episode),
      'old' = rlang::expr(!new_episode),
      'all' = rlang::expr(TRUE)

    )

    deboscat_table %>%
      dplyr::filter(year == year_sel, !!new_episodes_filter) %>% {
        temp_data <- .
        if (isTRUE(species_breakdown)) {
          temp_data %>%
            dplyr::filter(species_id == species_sel)
        } else {
          temp_data
        }
      } %>%
      dplyr::select(
        dplyr::any_of(c('episode_id'))
      ) %>%
      dplyr::group_by(episode_id) %>%
      dplyr::slice(1)
  })


  # observers ---------------------------------------------------------------------------------------------
  # observer to update species input
  shiny::observe({

    species_breakdown <- shiny::req(input$year_explorer_species_breakdown)
    year_sel <- shiny::req(input$year_explorer_year_sel)

    if (isTRUE(species_breakdown)) {

        species_choices <- yearly_report_data() %>%
          dplyr::filter(year == year_sel) %>%
          dplyr::pull(species_id) %>%
          unique()

        shinyWidgets::updatePickerInput(
          session, 'year_explorer_species_sel',
          label = translate_app('year_explorer_species_sel', lang()),
          choices = species_choices,
          selected = cache_selected_choice(
            choices = species_choices,
            cache = cache,
            key = 'speciessel',
            default = 'Quercus ilex'
          )
        )
      }
  })
  # observer to show species div and warning
  shiny::observeEvent(
    eventExpr = input$year_explorer_species_breakdown,
    handlerExpr = {
      if (isTRUE(input$year_explorer_species_breakdown)) {
        shinyjs::show('species_div')
        shinyjs::show('species_breakdown_warning')
      } else {
        shinyjs::hide('species_div')
        shinyjs::hide('species_breakdown_warning')
      }
    }
  )
  # observer to update var_choices depending on the years
  shiny::observe({
    year_sel <- shiny::req(input$year_explorer_year_sel)

    if (year_sel %in% c(2012, 2013)) {
      var_choices <- c(
        'number_of_episodes', 'total_episodes_area', 'total_trees_area', 'affected_area'
      ) %>%
        purrr::set_names(
          nm = translate_app(
            c('number_of_episodes', 'total_episodes_area', 'total_trees_area', 'affected_area'),
            lang()
          )
        )
      shinyWidgets::updatePickerInput(
        session, 'year_explorer_var_sel',
        label = translate_app('year_explorer_var_sel', lang()),
        choices = var_choices,
        selected = cache_selected_choice(
          choices = var_choices,
          cache = cache,
          key = 'varsel',
          default = 'affected_area'
        )
      )
    } else {
      var_choices <- c(
        'number_of_episodes', 'total_episodes_area', 'total_trees_area', 'affected_area', 'decolorated_area',
        'defoliated_area', 'dead_area'
      ) %>%
        purrr::set_names(
          nm = translate_app(
            c('number_of_episodes', 'total_episodes_area', 'total_trees_area', 'affected_area', 'decolorated_area',
              'defoliated_area', 'dead_area'),
            lang()
          )
        )
      shinyWidgets::updatePickerInput(
        session, 'year_explorer_var_sel',
        label = translate_app('year_explorer_var_sel', lang()),
        choices = var_choices,
        selected = cache_selected_choice(
          choices = var_choices,
          cache = cache,
          key = 'varsel',
          default = 'affected_area'
        )
      )
    }
  })
  # observer to launch the floating plot panel with the species info
  shiny::observeEvent(
    eventExpr = input$show_species_info,
    handlerExpr = {
      shiny::showModal(
        shiny::modalDialog(
          mod_infoOutput('mod_infoOutput_species'),
          footer = shiny::modalButton(
            translate_app('dismiss', lang())
          ),
          size = 'l', easyClose = TRUE
        )
      )
    }
  )
  # observer to launch save modal
  shiny::observeEvent(
    eventExpr = input$year_explorer_save_btn,
    handlerExpr = {
      shiny::showModal(
        shiny::modalDialog(
          mod_saveDataUI('mod_saveData_year_explorer'),
          footer = shiny::modalButton(
            translate_app('dismiss', lang())
          ),
          size = 'm', easyClose = TRUE
        )
      )
    }
  )

  # cache -------------------------------------------------------------------------------------------------
  # update cache
  shiny::observe({
    var_sel <- shiny::req(input$year_explorer_var_sel)
    cache$set('varsel', var_sel)
  })
  shiny::observe({
    species_sel <- shiny::req(input$year_explorer_species_sel)
    cache$set('speciessel', species_sel)
  })


  # reactives to return -----------------------------------------------------------------------------------  # reactives to return
  year_explorer_data_reactives <- shiny::reactiveValues()
  shiny::observe({
    year_explorer_data_reactives$yearly_report_data <- yearly_report_data()
    year_explorer_data_reactives$county_map_data <- county_map_data()
    year_explorer_data_reactives$episodes_data <- episodes_data()
    year_explorer_data_reactives$var_sel <- input$year_explorer_var_sel
    year_explorer_data_reactives$new_episodes_sel <- input$year_explorer_new_episodes_sel
    year_explorer_data_reactives$species_sel <- input$year_explorer_species_sel
    year_explorer_data_reactives$year_sel <- input$year_explorer_year_sel
    year_explorer_data_reactives$species_breakdown <- input$year_explorer_species_breakdown
    year_explorer_data_reactives$show_species_info <- input$show_species_info
  })

  return(year_explorer_data_reactives)
}

