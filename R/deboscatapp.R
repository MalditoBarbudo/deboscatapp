#' function to launch the deboscatapp
#'
#' @import sf
#' @import ggplot2
#'
#' @export
deboscatapp <- function() {

  ### DB access ################################################################
  # no need

  ### thesauruses ##############################################################
  # no need

  ### Language input ###########################################################
  shiny::addResourcePath(
    'images', system.file('resources', 'images', package = 'deboscatapp')
  )
  lang_choices <- c('cat', 'spa', 'eng')
  lang_flags <- c(
    glue::glue(
      "<img class='flag-image' src='images/cat.png'",
      " width=20px><div class='flag-lang'>%s</div></img>"
    ),
    glue::glue(
      "<img class='flag-image' src='images/spa.png'",
      " width=20px><div class='flag-lang'>%s</div></img>"
    ),
    glue::glue(
      "<img class='flag-image' src='images/eng.png'",
      " width=20px><div class='flag-lang'>%s</div></img>"
    )
  )

  ## JS code needed ############################################################
  keep_alive_script <- shiny::HTML(
    "var socket_timeout_interval;
var n = 0;

$(document).on('shiny:connected', function(event) {
  socket_timeout_interval = setInterval(function() {
    Shiny.onInputChange('alive_count', n++)
  }, 10000);
});

$(document).on('shiny:disconnected', function(event) {
  clearInterval(socket_timeout_interval)
});"
  )

  ## UI ########################################################################
  ui <- shiny::tagList(

    # use shinyjs
    shinyjs::useShinyjs(),

    # use waiter and waitress
    # waiter::use_waiter(),
    # waiter::use_hostess(),

    # css
    shiny::tags$head(
      # js script,
      shiny::tags$script(keep_alive_script),
      # corporative image css
      shiny::includeCSS(
        system.file('apps_css', 'corp_image.css', package = 'lfcdata')
      ),
      # custom css
      shiny::includeCSS(
        system.file('apps_css', 'deboscatapp.css', package = 'lfcdata')
      )
    ),

    # navbar with inputs (custom function, see helpers.R)
    navbarPageWithInputs(
      # opts
      title = 'Deboscat app',
      id = 'nav',
      collapsible = TRUE,

      # navbar with inputs (helpers.R) accepts an input argument, we use it for
      # the lang selector
      inputs = shinyWidgets::pickerInput(
        'lang', NULL,
        choices = lang_choices,
        selected = 'cat',
        width = '100px',
        choicesOpt = list(
          content = c(
            sprintf(lang_flags[1], lang_choices[1]),
            sprintf(lang_flags[2], lang_choices[2]),
            sprintf(lang_flags[3], lang_choices[3])
          )
        )
      ),

      # footer
      footer = shiny::tags$footer(
        shiny::fluidRow(
          shiny::column(
            width = 12, align = "right",
            shiny::HTML(glue::glue(
              '<img src="images/emf_white_logo.svg" width="120px" class="d-inline-block" alt="" loading="lazy">
              <img src="images/creaf_white_logo.svg" width="135px" class="d-inline-block" alt="" loading="lazy">
              <span>({lubridate::year(Sys.Date())})</span>'
            ))
          )
        )
      ),

      # 1. yearly report by county
      # This will be a sidebar layout. SIdebar will allow to select year and to apply or not a species
      # breakdown (warning about calculation of the data). In case of breakdown a selector of species
      # appear.
      # Main panel will have a map with the counties, and the user will select the variable to colour the
      # map.
      shiny::tabPanel(
        title = mod_tab_translateOutput('year_report_tab_translation'),
        value = 'year_explorer',
        shiny::sidebarLayout(
          # options
          position = 'left', fluid = TRUE,
          sidebarPanel = shiny::sidebarPanel(
            width = 3,
            mod_yearExplorerDataInput('mod_yearExplorerDataInput')
          ),
          mainPanel = shiny::mainPanel(
            width = 9,
            mod_yearExplorerOutput('mod_yearExplorerOutput')
          )
        )
      ), # end of year explorer tab
      # 2. episode report
      # This will be a sidebar layout. The sidebar will allow to filter episodes
      # by year, county, species and ¿others?. The main panel will be a graphical
      # report of the episode/s:
      #   a) affectation plots
      #   b) spatial plots
      #   c) affectation trend
      # If more than one episode is selected, the episodes will be faceted in rows for
      # each of the diagnostic plots
      shiny::tabPanel(
        title = mod_tab_translateOutput('episode_report_tab_translation'),
        value = 'episode_explorer',
        shiny::sidebarLayout(
          # options
          position = 'left', fluid = TRUE,
          sidebarPanel = shiny::sidebarPanel(
            # The inputs for filtering will act as accumulative filters, returning a list of episodes
            # (episode_id) to another input to select the ones we want the report about. We will do this
            # in a module
            width = 3,
            mod_episodeExplorerDataInput('mod_episodeExplorerDataInput')
          ),
          mainPanel = shiny::mainPanel(
            # main panel will use another module to show the plots, one that takes the data and create the
            # plots
            width = 9,
            mod_episodeExplorerPlotOutput('mod_episodeExplorerPlotOutput')
          )
        ) # end of sidebar Layout
      ), # End of episode explorer tab
      shiny::tabPanel(
        title = mod_tab_translateOutput('tech_specs_translation'),
        value = 'tech_spec_panel',
        mod_techSpecsOutput('mod_techSpecsOutput')
      )
    ) # end NavBarWithInputs

  ) # end of UI

  ## server ####
  server <- function(input, output, session) {

    # lang reactive ####
    lang <- shiny::reactive({
      input$lang
    })

    # mapbox token
    mapdeck::set_token(Sys.getenv("MAPBOX_TOKEN"))

    # cache ####
    inputs_cache <- cachem::cache_mem(evict = 'fifo')

    # modules ####
    year_explorer_data_reactives <- shiny::callModule(
      mod_yearExplorerData, 'mod_yearExplorerDataInput', lang,
      inputs_cache
    )
    year_explorer_output_reactives <- shiny::callModule(
      mod_yearExplorer, 'mod_yearExplorerOutput', lang,
      year_explorer_data_reactives
    )
    episode_explorer_data_reactives <- shiny::callModule(
      mod_episodeExplorerData, 'mod_episodeExplorerDataInput', lang,
      year_explorer_output_reactives, session, inputs_cache
    )
    episode_explorer_plot_reactives <- shiny::callModule(
      mod_episodeExplorerPlot, 'mod_episodeExplorerPlotOutput', lang,
      episode_explorer_data_reactives
    )
    shiny::callModule(
      mod_info, 'mod_infoOutput_species', lang,
      year_explorer_data_reactives, year_explorer_output_reactives,
      'species_id'
    )
    shiny::callModule(
      mod_info, 'mod_infoOutput_counties', lang,
      year_explorer_data_reactives, year_explorer_output_reactives,
      'county_name'
    )
    shiny::callModule(
      mod_saveData, 'mod_saveData_year_explorer', lang,
      year_explorer_data_reactives, data_type = 'year_explorer'
    )
    shiny::callModule(
      mod_saveData, 'mod_saveData_episode_explorer', lang,
      episode_explorer_data_reactives, data_type = 'episode_explorer'
    )
    shiny::callModule(
      mod_techSpecs, 'mod_techSpecsOutput', lang
    )

    ## tab translations ####
    shiny::callModule(
      mod_tab_translate, 'main_tab_translation',
      'main_tab_translation', lang
    )
    shiny::callModule(
      mod_tab_translate, 'episode_report_tab_translation',
      'episode_report_tab_translation', lang
    )
    shiny::callModule(
      mod_tab_translate, 'year_report_tab_translation',
      'year_report_tab_translation', lang
    )
    shiny::callModule(
      mod_tab_translate, 'tech_specs_translation',
      'tech_specs_translation', lang
    )
    # shiny::callModule(
    #   mod_tab_translate, 'data_translation',
    #   'data_translation', lang
    # )
    # shiny::callModule(
    #   mod_tab_translate, 'viz_translation',
    #   'viz_translation', lang
    # )
    # shiny::callModule(
    #   mod_tab_translate, 'save_translation',
    #   'save_translation', lang
    # )
    # shiny::callModule(
    #   mod_tab_translate, 'help_translation',
    #   'help_translation', lang
    # )
    # shiny::callModule(
    #   mod_tab_translate, 'map_translation',
    #   'map_translation', lang
    # )
    # shiny::callModule(
    #   mod_tab_translate, 'table_translation',
    #   'table_translation', lang
    # )

    ## observers ####
    # modal observer
    # shiny::observeEvent(
    #   eventExpr = map_reactives$fes_map_shape_click,
    #   handlerExpr = {
    #
    #     shiny::showModal(
    #       shiny::modalDialog(
    #         mod_infoUI('mod_infoUI'),
    #         footer = shiny::modalButton(
    #           translate_app('dismiss', lang())
    #         ),
    #         size = 'm', easyClose = TRUE
    #       )
    #     )
    #   }
    # )

  } # end of server

  # Run the application
  deboscatapp_res <- shiny::shinyApp(
    ui = ui, server = server
  )

  return(deboscatapp_res)

}
