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
  "h4_epiexp_episode_sel", "Selecciona l'episodi a explorar", "Select the episode to explore", "Selecciona el episodio a explorar",
  "episode_explorer_year_sel", "Anys que els episodis està actiu", "Years the episodes are active", "Años que los episodios está activo",
  "episode_explorer_county_sel", "Comarques on es localitzan les episodis", "Counties the episodes are located", "Comarcas donde se localizan los episodios",
  "episode_explorer_species_sel", "Espècies que ha de tenir les episodis", "Species the episodes must have", "Especies que debe tener los episodios",
  "plot", "Informe gràfic", "Visual report", "Informe gráfico",
  "affectation_general_info", "Informació general", "General info", "Información general",
  "affectation_plot", "Afectació per espècies", "Species affectation", "Afectación por especies",
  "spatial_plot", "Àrea episodi", "Episode area", "Área episodio",
  "species_number", "Nombre de espècies", "Species number", "Número de especies",
  "episode_starts", "Any de començament", "Start year", "Año de comienzo",
  "last_year_recorded", "Ùltim any registrat", "Last year recorded", "Último año registrado",

  # year explorer
  'h4_yearexp_episodes_data_sel', "Selecciona les dades a visualitzar", "Select the data to display", "Selecciona los datos a visualizar",
  "year_explorer_year_sel", "Any:", "Year:", "Año:",
  "year_explorer_new_episodes_sel", "Tipus d'episodi:", "Episode type:", "Tipo de episodio:",
  "all_episodes", "Tots els episodis", "All episodes", "Todos los episodios",
  "new_episodes", "Episodis nous", "New episodes", "Episodios nuevos",
  "old_episodes", "Episodis antics", "Old episodes", "Episodios antiguos",
  "year_explorer_species_breakdown_on", "Desactivar desglossat per espècies", "Disable broken down by species", "Desactivar desglosado por especies",
  "year_explorer_species_breakdown_off", "Activar desglossat per espècies", "Enable broken down by species", "Activar desglosado por especies",
  "species_breakdown_warning", "El desglossat per espècies es calcula de manera diferent a l'agrupat per comarques. Veure especificacions tècniques.", "The breakdown by species is calculated differently from that grouped by county. See technical specifications.", "El desglosado por especies se calcula de manera diferente al agrupado por comarcas. Ver especificaciones técnicas.",
  "h4_yearexp_episodes_viz_sel", "Visualització", "Visualization", "Visualización",
  "year_explorer_var_sel", "Variable:", "Variable:", "Variable:",
  "number_of_episodes", "Nombre de episodis", "Number of episodes", "Número de episodios",
  "total_episodes_area", "Àrea total dels episodis [ha]", "Total area of the episodes [ha]", "Área total de los episodios [ha]",
  "total_trees_area", "Àrea total de coberta dels arbres [ha]", "Total tree cover area [ha]", "Área total de cubierta de los árboles [ha]",
  "affected_area", "Àrea total afectada [ha]", "Total area affected [ha]", "Àrea total afectada [ha]",
  "decolorated_area", "Àrea total amb decoloració [ha]", "Total area decolorated", "Área total con decoloración [ha]",
  "defoliated_area", "Àrea total amb defoliació [ha]", "Total area defoliated", "Área total con defoliación [ha]",
  "dead_area", "Àrea total amb moratalitat [ha]", "Total area dead", "Área total con mortalidad [ha]",
  "year_explorer_species_sel", "Espècie", "Species", "Especie:",
  "show_species_info", "Espècies info", "Species info", "Especies info",
  "map", "Mapa", "Map", "Mapa",
  "table", "Taula", "Table", "Tabla",

  # tabs translations
  "episode_report_tab_translation", "Explorador d'episodis", "Episode explorer", "Explorador de episodios",
  "year_report_tab_translation", "Explorador de comarques", "Counties explorer", "Explorador de comarcas",
  "tech_specs_translation", "Especificacions tècniques", "Technical specifications", "Especificaciones técnicas",



  # data version choices
  # use file selection
  # "user_file_sel_label", "Selecciona l'arxiu a carregar", "Select the file to upload", "Selecciona el archivo a cargar",
  # "user_file_sel_buttonLabel", "Inspecciona...", "Browse...", "Inspecciona...",
  # "user_file_sel_placeholder", "Cap fitxer seleccionat", "No file selected", "Ningún archivo seleccionado",
  # "file_text", 'El fitxer pot ser un shapefile (comprimit en un fitxer zip) o un fitxer GeoPackage (.gpkg). Han de tenir un camp anomenat "poly_id" amb els identificadors dels polígons continguts.', 'File can be a shapefile (compressed in a zip file) or GeoPackage file (.gpkg). They must have a field called "poly_id" with the identifiers of the contained polygons.', 'El archivo puede ser un shapefile (comprimido en un archivo zip) o un archivo GeoPackage (.gpkg). Deben tener un campo llamado "poly_id" con los identificadores de los polígonos contenidos.',

  # shinyWidgets
  "deselect-all-text", "Cap", "None selected...", "Ninguno",
  "select-all-text", "Tots", "All selected...", "Todos",
  "count-selected-text-value", "{0} valors seleccionats (de {1})", "{0} values selected (of {1})", "{0} valores seleccionados (de {1})",

  # save
  "save", "Guardar", "Save", "Guardar",
  ".csv", "Text (csv)", "Text (csv)", "Texto (csv)",
  ".xlsx", "MS Excel (xlsx)", "MS Excel (xlsx)", "MS Excel (xlsx)",
  ".gpkg", "GeoPackage (gpkg)", "GeoPackage (gpkg)", "GeoPackage (gpkg)"
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

