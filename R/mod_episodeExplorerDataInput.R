#' @title mod_episodeExplorerDataInput and mod_episodeExplorerData
#'
#' @description A shiny module to create and populate the data inputs
#'
#' @param id shiny id
#'
#' @export
mod_episodeExplorerDataInput <- function(id) {

  # ns
  ns <- shiny::NS(id)

  # UI ####
  shiny::tagList(
    shiny::br(),
    shiny::uiOutput(
      ns('mod_episodeExplorerData_container')
    )
  )
}

#' mod_episodeExplorerData server function
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param lang lang reactive
#'
#' @param year_explorer_output_reactives reactives from year report map
#' @param parent_session parent session to update the active tab
#' @param cache cache for storing the episode sel value
#'
#' @export
mod_episodeExplorerData <- function(
  input, output, session,
  lang,
  year_explorer_output_reactives, parent_session, cache
) {
  # renderUI ####
  output$mod_episodeExplorerData_container <- shiny::renderUI({

    ns <- session$ns

    # choices
    year_choices <- deboscat_table |> dplyr::pull(year) |> unique()
    species_choices <- deboscat_table |> dplyr::pull(species_id) |> unique()
    county_choices <- deboscat_table |> dplyr::pull(county_name) |> unique()
    # initial episode choices
    episode_initial_choices <- deboscat_table |> dplyr::pull(episode_id) |> unique()
    # new episodes choices
    new_episodes_choices <- c('all', 'old', 'new') |>
      purrr::set_names(nm = translate_app(c('all_episodes', 'old_episodes', 'new_episodes'), lang()))

    # taglist
    shiny::tagList(
      shiny::h4(translate_app('h4_epiexp_episodes_filter', lang())),
      # county selector
      shinyWidgets::pickerInput(
        ns('episode_explorer_county_sel'),
        label = translate_app('episode_explorer_county_sel', lang()),
        choices = county_choices,
        multiple = TRUE,
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
        ns('episode_explorer_year_sel'),
        label = translate_app('episode_explorer_year_sel', lang()),
        choices = year_choices,
        multiple = TRUE,
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
        ns('episode_explorer_species_sel'),
        label = translate_app('episode_explorer_species_sel', lang()),
        choices = species_choices,
        multiple = TRUE,
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
        ns('episode_explorer_new_episodes_sel'),
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
      shiny::br(),
      shiny::h4(translate_app('h4_epiexp_episode_sel', lang())),
      shinyWidgets::pickerInput(
        ns('episode_explorer_episode_sel'),
        label = NULL,
        choices = episode_initial_choices,
        selected = cache_selected_choice(
          choices = episode_initial_choices,
          cache = cache,
          key = 'episodesel'
        ),
        multiple = FALSE,
        options = shinyWidgets::pickerOptions(
          actionsBox = FALSE,
          noneSelectedText = translate_app('deselect-all-text', lang()),
          selectAllText = translate_app('select-all-text', lang()),
          selectedTextFormat =  'count',
          countSelectedText =  translate_app('count-selected-text-value', lang()),
          size = 10,
          liveSearch = TRUE,
          tickIcon = 'glyphicon-tree-deciduous'
        )
      ),
      shiny::br(),
      shiny::actionButton(
        ns('episode_explorer_save_btn'),
        label = translate_app('save', lang())
      )
    ) # end of tagList
  })

  # update cache
  shiny::observe({
    episode_sel <- shiny::req(input$episode_explorer_episode_sel)
    cache$set('episodesel', episode_sel)
  })

  # episodes list reactive
  episode_list <- shiny::reactive({

    # no filters
    year_filter <- TRUE
    county_filter <- TRUE
    species_filter <- TRUE
    new_episodes_filter <- TRUE

    # depending on the inputs, create the filters
    if (!is.null(input$episode_explorer_year_sel) && input$episode_explorer_year_sel != '') {
      year_filter <- rlang::expr(year %in% input$episode_explorer_year_sel)
    }
    if (!is.null(input$episode_explorer_species_sel) && input$episode_explorer_species_sel != '') {
      species_filter <- rlang::expr(species_id %in% input$episode_explorer_species_sel)
    }
    if (!is.null(input$episode_explorer_county_sel) && input$episode_explorer_county_sel != '') {
      county_filter <- rlang::expr(county_name %in% input$episode_explorer_county_sel)
    }
    if (!is.null(input$episode_explorer_new_episodes_sel) && input$episode_explorer_new_episodes_sel != '') {
      new_episodes_filter <- switch(
        input$episode_explorer_new_episodes_sel,
        'new' = rlang::expr(new_episode),
        'old' = rlang::expr(!new_episode),
        'all' = TRUE
      )
    }

    # obtain the episode_id list
    deboscat_table |>
      dplyr::filter(!! year_filter, !! county_filter, !! species_filter, !!new_episodes_filter) |>
      dplyr::pull(episode_id) |>
      unique()
  })

  shiny::observeEvent(
    priority = 3,
    eventExpr = episode_list(),
    handlerExpr = {
      shinyWidgets::updatePickerInput(
        session = session,
        'episode_explorer_episode_sel',
        label = NULL,
        choices = episode_list(),
        selected = cache_selected_choice(
          choices = episode_list(),
          cache = cache,
          key = 'episodesel'
        )
      )
    }
  )

  # observers to update inputs when map in the other tab is clicked in a episode polygon
  shiny::observeEvent(
    priority = 1,
    eventExpr = year_explorer_output_reactives$year_explorer_map_shape_click,
    handlerExpr = {

      click_info <- shiny::req(year_explorer_output_reactives$year_explorer_map_shape_click)

      if (click_info$group == 'episodes') {
        # update active tab
        shiny::updateTabsetPanel(
          parent_session, 'nav', selected = 'episode_explorer'
        )
      }
    }
  )

  shiny::observeEvent(
    priority = 2,
    eventExpr = year_explorer_output_reactives$year_explorer_map_shape_click,
    handlerExpr = {

      click_info <- shiny::req(year_explorer_output_reactives$year_explorer_map_shape_click)

      if (click_info$group == 'episodes') {
        # reset filter inputs
        shinyjs::reset('episode_explorer_county_sel')
        shinyjs::reset('episode_explorer_year_sel')
        shinyjs::reset('episode_explorer_species_sel')
        shinyjs::reset('episode_explorer_new_episodes_sel')

      }
    }
  )

  shiny::observeEvent(
    priority = 25,
    eventExpr = year_explorer_output_reactives$year_explorer_map_shape_click,
    handlerExpr = {

      click_info <- shiny::req(year_explorer_output_reactives$year_explorer_map_shape_click)

      if (click_info$group == 'episodes') {
        # set cache manually
        cache$set('episodesel', click_info$id)
        # update episode selector with the clicked episode
        shinyWidgets::updatePickerInput(
          session = session,
          'episode_explorer_episode_sel',
          label = translate_app('episode_explorer_episode_sel', lang()),
          choices = episode_list(),
          selected = click_info$id
        )
      }
    }
  )
  # observer to launch save modal
  shiny::observeEvent(
    eventExpr = input$episode_explorer_save_btn,
    handlerExpr = {
      shiny::showModal(
        shiny::modalDialog(
          mod_saveDataUI('mod_saveData_episode_explorer'),
          footer = shiny::modalButton(
            translate_app('dismiss', lang())
          ),
          size = 'm', easyClose = TRUE
        )
      )
    }
  )

  # data reactive
  episode_explorer_data <- shiny::reactive({

    # get the episodes
    episodes <- input$episode_explorer_episode_sel
    # get the data
    deboscat_table |>
      dplyr::filter(episode_id %in% episodes)
  })

  episode_explorer_data_reactives <- shiny::reactiveValues()
  shiny::observe({
    episode_explorer_data_reactives$episode_list <- episode_list()
    episode_explorer_data_reactives$data <- episode_explorer_data()
    episode_explorer_data_reactives$selected_episodes <- input$episode_explorer_episode_sel
  })

  return(episode_explorer_data_reactives)
}

