## code to prepare `deboscat_table` dataset goes here

library(dplyr)
library(tibble)
library(sf)

## TODO
# Here will be the code to extract the table from the original deboscat db when aailable


one_year_episodes_to_remove <-
  sf::read_sf('data-raw/deboscat_table.gpkg') %>%
  as_tibble() %>%
  group_by(episode_id, year) %>%
  summarise(episode_id = first(episode_id), year = first(year)) %>%
  group_by(episode_id) %>%
  summarise(n = n(), year = first(year)) %>%
  filter(n < 2, year != 2020) %>%
  pull(episode_id)

# temp table
deboscat_table <-
  sf::read_sf('data-raw/deboscat_table.gpkg') %>%
  sf::st_transform(crs = 4326) %>%
  # remove the one years that are not from 2020, as they usually are lingering rests of the cleaning
  dplyr::filter(!(episode_id %in% one_year_episodes_to_remove))

## app thesaurus ####
app_translations <- tibble::tribble(
  ~text_id, ~translation_cat, ~translation_eng, ~translation_spa,
  # episode_explorer
  'h4_epiexp_episodes_filter', "Filtres de episodis", "Episodes filters", "Filtros de episodios",
  "h4_epiexp_episode_sel", "Selecciona els episodis a explorar", "Select the episodes to explore", "Selecciona los episodios a explorar",
  "episode_explorer_year_sel", "Anys que les episodis està actiu", "Years the episodes are active", "Años que los episodios está activo",
  "episode_explorer_county_sel", "Comarques on es localitzan les episodis", "Counties the episodes are located", "Comarcas donde se localizan los episodios",
  "episode_explorer_species_sel", "Espècies que ha de tenir les episodis", "Species the episodes must have", "Especies que debe tener los episodios",

  # data version choices
  # 'h4_data_version', "Fixa l'escala", "Select the scale", "Selecciona la escala",
  'data_version', "Escala temporal", "Temporal scale", "Escala temporal",
  'static', 'Dades estàtics', 'Static data', 'Datos estáticos',
  'dynamic', 'Dades dinamics', 'Dynamic data', 'Datos dinámicos',
  'plot_nfi_2_results', "IFN 2 (~1988-92)", "NFI 2 (~1988-92)", "IFN 2 (~1988-92)",
  'plot_nfi_3_results', "IFN 3 (~2000-01)", "NFI 3 (~2000-01)", "IFN 3 (~2000-01)",
  'plot_nfi_4_results', "IFN 4 (~2014-17)", "NFI 4 (~2014-17)", "IFN 4 (~2014-17)",
  'plot_nfi2_nfi3_results', "Diferències IFN 2 : IFN 3 (1990 ~ 2000)", "Differences NFI 2 : NFI 3 (1990 ~ 2000)", "Diferencias IFN 2 : IFN 3 (1990 ~ 2000)",
  'plot_nfi3_nfi4_results', "Diferències IFN 3 : IFN 4 (2000 ~ 2015)", "Differences NFI 3 : NFI 4 (2000 ~ 2015)", "Diferencias IFN 3 : IFN 4 (2000 ~ 2015)",
  'plot_nfi2_nfi4_results', "Diferències IFN 2 : IFN 4 (1990 ~ 2015)", "Differences NFI 2 : NFI 4 (1990 ~ 2015)", "Diferencias IFN 2 : IFN 4 (1990 ~ 2015)",
  # data scale choices
  'data_scale', 'Escala espacial', 'Spatial scale', 'Escala espacial',
  'local', 'Local', 'Local', 'Local',
  'admin_province', 'Provincia', 'Province', 'Provincia',
  'admin_region', 'Comarca', 'County', 'Comarca',
  'admin_municipality', 'Municipi', 'Municipality', 'Municipio',
  "admin_natural_interest_area", "Àrees d'interès natural", "Natural interest areas", "Áreas de interés natural",
  "admin_special_protection_natural_area", "Àrees naturals de protecció especial", "Special protection natural areas", "Áreas naturales de protección especial",
  "admin_natura_network_2000", "Xarxa Natura 2000", "Natura 2000 network", "Red Natura 2000",
  'drawn_polygon', "Polígon dibuxat", "Drawn polygon", "Polígono dibujado",
  "file", "Arxiu de polìgons", "Polygon file", "Archivo de polígonos",
  "poly_id", "ID polígon", "Polygon ID", "ID polígono",
  # use file selection
  "user_file_sel_label", "Selecciona l'arxiu a carregar", "Select the file to upload", "Selecciona el archivo a cargar",
  "user_file_sel_buttonLabel", "Inspecciona...", "Browse...", "Inspecciona...",
  "user_file_sel_placeholder", "Cap fitxer seleccionat", "No file selected", "Ningún archivo seleccionado",
  "file_text", 'El fitxer pot ser un shapefile (comprimit en un fitxer zip) o un fitxer GeoPackage (.gpkg). Han de tenir un camp anomenat "poly_id" amb els identificadors dels polígons continguts.', 'File can be a shapefile (compressed in a zip file) or GeoPackage file (.gpkg). They must have a field called "poly_id" with the identifiers of the contained polygons.', 'El archivo puede ser un shapefile (comprimido en un archivo zip) o un archivo GeoPackage (.gpkg). Deben tener un campo llamado "poly_id" con los identificadores de los polígonos contenidos.',
  # map
  'Relief', 'Relleu (base)', 'Relief (base)', 'Relieve (base)',
  'Imaginery', 'Satèl·lit (base)', 'Imaginery (base)', 'Satélite (base)',
  "cite_div", "Dades elaborades pel CTFC i el CREAF.", "Data prepared by the CTFC and CREAF.", "Datos elaborados por el CTFC y el CREAF.",
  # tabs translations
  "main_tab_translation", "Explora", "Explore", "Explora",
  "episode_report_tab_tranlation", "Explorador de episodios", "Episode explorer", "Explorador d'episodis",
  "data_translation", "Dades", "Data", "Datos",
  "save_translation", "Guardar", "Save", "Guardar",
  "help_translation", "Ajuda", "Help", "Ayuda",
  "map_translation", "Mapa", "Map", "Mapa",
  "table_translation", "Taula", "Table", "Tabla",
  "tech_specs_translation", "Especificacions tècniques", "Technical specifications", "Especificaciones técnicas",
  # metric choices
  'mean', 'Mitjana', 'Mean', 'Media',
  'min', 'Minim', 'Minimum', 'Mínimo',
  'max', 'Maxim', 'Maximum', 'Máximo',
  'se', 'ES', 'SE', 'ES',
  'q05', 'Quartil 5', 'Quantile 5', 'Cuartil 5',
  'q95', 'Quartil 95', 'Quantile 95', 'Cuartil 95',
  'n', 'Nombre parcel·las', 'Plot number', 'Número de parcelas',
  # _metric
  '_mean', ' mitjana', ' mean', ' media',
  '_min', ' minim ', ' minimum', ' mínimo',
  '_max', ' maxim', ' maximum', ' máximo de ',
  '_se', ' ES', ' SE', ' ES',
  '_q05', ' quartil 5', ' quantile 5', ' cuartil 5',
  '_q95', ' quartil 95', ' quantile 95', ' cuartil 95',
  '_n', ' nombre parcel·las', ' plot number', ' número de parcelas',
  # viz
  "h4_servei", "Fixa el servei", "Select the service", "Selecciona el servicio",
  "h4_viz", "Visualització", "Visualization", "Visualización",
  "deselect-all-text", "Cap", "None selected...", "Ninguno",
  "select-all-text", "Tots", "All selected...", "Todos",
  "count-selected-text-value", "{0} valors seleccionats (de {1})", "{0} values selected (of {1})", "{0} valores seleccionados (de {1})",
  "viz_color_input", "Indicador:", "Indicator:", "Indicador:",
  "viz_statistic_input", "Estadístic:", "Statistic:", "Estadístico:",
  "pal_high", "Discriminar valors alts", "Discriminate higher values", "Discriminar valores altos",
  "pal_low", "Discriminar valors baixos", "Discriminate lower values", "Discriminar valores bajos",
  "pal_normal", "Normal", "Normal", "Normal",
  "viz_pal_config_input", "Configurar paleta", "Config palette", "Configurar paleta",
  "viz_pal_reverse_input", "Invertir la paleta?", "Reverse the palette?", "¿Invertir la paleta?",
  # save
  'save_map_btn', "Guarda el map", "Save the map", "Guarda el mapa",
  'save_table_btn', "Guarda la taula", "Save the table", "Guarda la tabla",
  "csv", "Text (csv)", "Text (csv)", "Texto (csv)",
  "xlsx", "MS Excel (xlsx)", "MS Excel (xlsx)", "MS Excel (xlsx)",
  "table_output_options_input", "Selecciona el format", "Choose the output format", "Selecciona el formato",
  # help module
  "glossary_var_input", "Selecciona el indicador a descriure", "Choose the indicator to describe", "Selecciona el indicador a describir",
  "link_to_tutorials_text", "Per obtenir més informació, aneu al tutorial de l'aplicació aquí", "For more info, please go to the application tutorial here", "Para obtener más información, vaya al tutorial de la aplicación aquí.",
  "var_description_title", "Descripció:", "Description:", "Descripción:",
  "var_units_title", "Unitats:", "Units:", "Unidades:",
  "var_servei_title", "Servei:", "Service:", "Servicio:",
  "dismiss", "Tancar", "Dismiss", "Cerrar",
  # info module
  "plot_id_info_plot_title", "Parcel·la seleccionada comparada amb les altres parcel·les al mapa", "Clicked plot compared to other plots in map", "Parcela seleccionada comparada con las otras parcelas en el mapa",
  "admin_region_info_plot_title", "Comarca seleccionada comparada amb les altres comarques al mapa", "Clicked region compared to other regions in map", "Comarca seleccionada comparada con las otras comarcas en el mapa",
  "admin_municipality_info_plot_title", "Municipi seleccionat comparat amb els altres municipis al mapa", "Clicked municipality compared to other municipalities in map", "Municipio seleccionada comparada con los otros municipios en el mapa",
  "poly_id_info_plot_title", "Polìgon seleccionat comparat amb les altres polìgons al mapa", "Clicked polygon compared to other polygons in map", "Polígono seleccionada comparada con los otros polígonos en el mapa",
  "admin_natura_network_2000_info_plot_title", "Àrea seleccionada comparada amb les altres àreas al mapa", "Clicked natural area compared to other natural areas in map", "Área seleccionada comparada con las otras áreas en el mapa",
  "admin_special_protection_natural_area_info_plot_title", "Àrea seleccionada comparada amb les altres àreas al mapa", "Clicked natural area compared to other natural areas in map", "Área seleccionada comparada con las otras áreas en el mapa",
  "admin_natural_interest_area_info_plot_title", "Àrea seleccionada comparada amb les altres àreas al mapa", "Clicked natural area compared to other natural areas in map", "Área seleccionada comparada con las otras áreas en el mapa",
  # progress bar
  "progress_message", "Obtenció de dades...", "Obtaining data...", "Obteniendo datos...",
  "progress_detail_initial", "Escalant les dades", "Data scaling", "Escalando los datos",
  # map
  "stats_unavailable_title", "No hi ha dades", "No data available", "No hay datos disponibles",
  "stats_unavailable", "El polígon actual conté menys de 3 parcel·les, no es calcularan estadístiques", "The current polygon contains less than 3 plots, no statistics will be calculated", "El polígono actual contiene menos de 3 gráficos, no se calcularán estadísticas",
  # visual_aids
  "diff_of_diffs", "Aquest indicador és una diferència de diferències, atès que el valor per a l'IFN3 és la diferència entre el període IFN2 - IFN3, i el valor per al IFN4 és la diferència per al període IFN3 - IFN4. D'aquesta manera, el valor presentat aquí és l'increment en la taxa, no una taxa per se", "This indicator is a difference of differences, as the value for the NFI3 is the difference for the period NFI2 - NFI3, and the value for NFI4 is the difference for the period NFI3 - NFI4. This way the value presented here is the increment on the rate, not a rate per se.", "Este indicador es una diferencia de diferencias, dado que el valor para el IFN3 es la diferencia entre el período IFN2 - IFN3, y el valor para el IFN4 es la diferencia para el período IFN3 - IFN4. De esta manera, el valor presentado aquí es el incremento en la tasa, no una tasa per se"
)

## deboscat per species ####
deboscat_species_all_affectations <-
  deboscat_table %>%
  tibble::as_tibble() %>%
  dplyr::group_by(species_id, year, county_name) %>%
  dplyr::summarise(
    number_of_episodes_all = dplyr::n(),
    total_episodes_area_all = round(sum(episode_area), 2),
    total_trees_area_all = round(sum(episode_area*(cover_perc/100)), 2),
    affected_area_all = round(sum(episode_area*(cover_perc/100)*(affected_trees_perc/100)), 2),
    decolorated_area_all = round(sum(episode_area*(cover_perc/100)*(decoloration_perc/100)), 2),
    defoliated_area_all = round(sum(episode_area*(cover_perc/100)*(defoliation_perc/100)), 2),
    dead_area_all = round(sum(episode_area*(cover_perc/100)*(mortality_perc/100)), 2)
  )

deboscat_species_new_affectations <-
  deboscat_table %>%
  tibble::as_tibble() %>%
  dplyr::filter(new_episode) %>%
  dplyr::group_by(species_id, year, county_name) %>%
  dplyr::summarise(
    number_of_episodes_new = dplyr::n(),
    total_episodes_area_new = round(sum(episode_area), 2),
    total_trees_area_new = round(sum(episode_area*(cover_perc/100)), 2),
    affected_area_new = round(sum(episode_area*(cover_perc/100)*(affected_trees_perc/100)), 2),
    decolorated_area_new = round(sum(episode_area*(cover_perc/100)*(decoloration_perc/100)), 2),
    defoliated_area_new = round(sum(episode_area*(cover_perc/100)*(defoliation_perc/100)), 2),
    dead_area_new = round(sum(episode_area*(cover_perc/100)*(mortality_perc/100)), 2)
  )

deboscat_species_old_affectations <-
  deboscat_table %>%
  tibble::as_tibble() %>%
  dplyr::filter(!new_episode) %>%
  dplyr::group_by(species_id, year, county_name) %>%
  dplyr::summarise(
    number_of_episodes_old = dplyr::n(),
    total_episodes_area_old = round(sum(episode_area), 2),
    total_trees_area_old = round(sum(episode_area*(cover_perc/100)), 2),
    affected_area_old = round(sum(episode_area*(cover_perc/100)*(affected_trees_perc/100)), 2),
    decolorated_area_old = round(sum(episode_area*(cover_perc/100)*(decoloration_perc/100)), 2),
    defoliated_area_old = round(sum(episode_area*(cover_perc/100)*(defoliation_perc/100)), 2),
    dead_area_old = round(sum(episode_area*(cover_perc/100)*(mortality_perc/100)), 2)
  )

deboscat_species_year_county_affectation_table <-
  deboscat_species_all_affectations %>%
  dplyr::full_join(deboscat_species_new_affectations) %>%
  dplyr::full_join(deboscat_species_old_affectations)

deboscat_species_year_affectation_table <-
  deboscat_species_year_county_affectation_table %>%
  dplyr::group_by(species_id, year) %>%
  dplyr::summarise(dplyr::across(where(is.numeric), sum, na.rm = TRUE)) %>%
  dplyr::ungroup()

## deboscat per counties ####

deboscat_counties_year_all_affectations <-
  deboscat_table %>%
  tibble::as_tibble() %>%
  dplyr::group_by(episode_id, year, county_name) %>%
  dplyr::summarise(
    episode_area = unique(episode_area),
    affected_trees_perc = sum(cover_perc*affected_trees_perc)/sum(cover_perc),
    decoloration_perc = sum(cover_perc*decoloration_perc)/sum(cover_perc),
    defoliation_perc = sum(cover_perc*defoliation_perc)/sum(cover_perc),
    mortality_perc = sum(cover_perc*mortality_perc)/sum(cover_perc),
    cover_perc = if_else(sum(cover_perc) > 100, 100, sum(cover_perc))
  ) %>%
  dplyr::group_by(year, county_name) %>%
  dplyr::summarise(
    number_of_episodes_all = dplyr::n(),
    total_episodes_area_all = round(sum(episode_area), 2),
    total_trees_area_all = round(sum(episode_area*(cover_perc/100)), 2),
    affected_area_all = round(sum(episode_area*(cover_perc/100)*(affected_trees_perc/100)), 2),
    decolorated_area_all = round(sum(episode_area*(cover_perc/100)*(decoloration_perc/100)), 2),
    defoliated_area_all = round(sum(episode_area*(cover_perc/100)*(defoliation_perc/100)), 2),
    dead_area_all = round(sum(episode_area*(cover_perc/100)*(mortality_perc/100)), 2)
  )

deboscat_counties_year_new_affectations <-
  deboscat_table %>%
  tibble::as_tibble() %>%
  dplyr::filter(new_episode) %>%
  dplyr::group_by(episode_id, year, county_name) %>%
  dplyr::summarise(
    episode_area = unique(episode_area),
    affected_trees_perc = sum(cover_perc*affected_trees_perc)/sum(cover_perc),
    decoloration_perc = sum(cover_perc*decoloration_perc)/sum(cover_perc),
    defoliation_perc = sum(cover_perc*defoliation_perc)/sum(cover_perc),
    mortality_perc = sum(cover_perc*mortality_perc)/sum(cover_perc),
    cover_perc = if_else(sum(cover_perc) > 100, 100, sum(cover_perc))
  ) %>%
  dplyr::group_by(year, county_name) %>%
  dplyr::summarise(
    number_of_episodes_new = dplyr::n(),
    total_episodes_area_new = round(sum(episode_area), 2),
    total_trees_area_new = round(sum(episode_area*(cover_perc/100)), 2),
    affected_area_new = round(sum(episode_area*(cover_perc/100)*(affected_trees_perc/100)), 2),
    decolorated_area_new = round(sum(episode_area*(cover_perc/100)*(decoloration_perc/100)), 2),
    defoliated_area_new = round(sum(episode_area*(cover_perc/100)*(defoliation_perc/100)), 2),
    dead_area_new = round(sum(episode_area*(cover_perc/100)*(mortality_perc/100)), 2)
  )

deboscat_counties_year_old_affectations <-
  deboscat_table %>%
  tibble::as_tibble() %>%
  dplyr::filter(!new_episode) %>%
  dplyr::group_by(episode_id, year, county_name) %>%
  dplyr::summarise(
    episode_area = unique(episode_area),
    affected_trees_perc = sum(cover_perc*affected_trees_perc)/sum(cover_perc),
    decoloration_perc = sum(cover_perc*decoloration_perc)/sum(cover_perc),
    defoliation_perc = sum(cover_perc*defoliation_perc)/sum(cover_perc),
    mortality_perc = sum(cover_perc*mortality_perc)/sum(cover_perc),
    cover_perc = if_else(sum(cover_perc) > 100, 100, sum(cover_perc))
  ) %>%
  dplyr::group_by(year, county_name) %>%
  dplyr::summarise(
    number_of_episodes_old = dplyr::n(),
    total_episodes_area_old = round(sum(episode_area), 2),
    total_trees_area_old = round(sum(episode_area*(cover_perc/100)), 2),
    affected_area_old = round(sum(episode_area*(cover_perc/100)*(affected_trees_perc/100)), 2),
    decolorated_area_old = round(sum(episode_area*(cover_perc/100)*(decoloration_perc/100)), 2),
    defoliated_area_old = round(sum(episode_area*(cover_perc/100)*(defoliation_perc/100)), 2),
    dead_area_old = round(sum(episode_area*(cover_perc/100)*(mortality_perc/100)), 2)
  )


deboscat_counties_year_affectation_table <-
  deboscat_counties_year_all_affectations %>%
  dplyr::full_join(deboscat_counties_year_new_affectations) %>%
  dplyr::full_join(deboscat_counties_year_old_affectations)

# add counties to counties tables
# counties
counties_sf <-
  sf::read_sf('../../01_nfi_app/nfiApp/data-raw/shapefiles/bm5mv20sh0tpc1_20180101_0.shp') %>%
  rmapshaper::ms_simplify(0.01, keep_shapes = TRUE) %>%
  dplyr::select(county_name = NOMCOMAR)

deboscat_counties_year_affectation_table <- deboscat_counties_year_affectation_table %>%
  dplyr::left_join(counties_sf) %>%
  sf::st_as_sf() %>%
  sf::st_transform(crs = 4326)

deboscat_species_year_county_affectation_table <- deboscat_species_year_county_affectation_table %>%
  dplyr::left_join(counties_sf) %>%
  sf::st_as_sf() %>%
  sf::st_transform(crs = 4326)

## use_data call ####
usethis::use_data(
  deboscat_table, deboscat_species_year_county_affectation_table,
  deboscat_species_year_affectation_table, deboscat_counties_year_affectation_table,
  app_translations, overwrite = TRUE, internal = TRUE
)

