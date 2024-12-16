# ==========================================================================================================================================
# Script Name: Automated Variable Extraction Function
# Authors: Hephzibah Adeniji, Grace Legris
# Date: 12/11/24
# Purpose: Extracts EVI, NDVI, rainfall, distance to water bodies, relative humidity, temperature, housing quality, PFPR (Plasmodium
#          Falciparum Parasite Rate), night time light, flood, NDWI (Normalized Difference Water Index), NDMI (Normalized Difference Moisture Index), 
#          elevation, surface soil wetness
#
#          Also recodes ward names in ITN datasets to match ward names in the state-level shapefiles
# ==========================================================================================================================================

# script with directories, functions, etc
source("/Users/grace/Desktop/UMP/NMEP_classification_my_fork/load_path.R", echo = T)

## -----------------------------------------------------------------------------------------------------------------------------------------
### Variable File Paths
## -----------------------------------------------------------------------------------------------------------------------------------------

paths <- list(
  evi_path = file.path(RastersDir, "Updated_Covariates", "2023-24_EVI_MOD13A1"),
  ndvi_path = file.path(RastersDir, "Updated_Covariates", "2023-24_NDVI_MOD13A1"),
  rainfall_path = file.path(RastersDir, "monthly rainfall 2023-24"),
  h2o_distance_path = file.path(RastersDir, "distance_to_water_bodies", "distance_to_water.tif"),
  elevation_path = file.path(RastersDir, "Elevation", "ELE.tif"), 
  rh_2023 = file.path(RastersDir, "temp_humidity_2023_grib", "relative_humidity_2023.grib"),
  rh_2024 = file.path(RastersDir, "temp_humidity_2023_grib", "relative_humidity_2024.grib"),
  temp_2023 = file.path(RastersDir, "temp_humidity_2023_grib", "temperature_2023.grib"),
  temp_2024 = file.path(RastersDir, "temp_humidity_2023_grib", "temperature_2024.grib"),
  housing_quality_path = file.path(RastersDir, "housing", 
                                   "2019_Nature_Africa_Housing_2015_NGA.tiff"),
  ndwi_path = file.path(RastersDir, "global_surface_water", 
                        "Nigeria_NDWI_2023.tif"),
  ndmi_path = file.path(RastersDir, "global_surface_water", 
                        "NDMI_Nigeria_2023.tif"),
  pfpr_path = file.path(PfDir, "pf_parasite_rate", 
                        "202406_Global_Pf_Parasite_Rate_NGA_2022.tiff"),
  lights_path = file.path(RastersDir, "night_time_light_2023"),
  surface_soil_wetness_path = file.path(RastersDir, "surface_soil_wetness"),
  flood_path = file.path(RastersDir, "flooding_2023"),
  #surface_h20_path = file.path(RastersDir, "global_surface_water"),
  settlement_block = file.path(RastersDir, "nigeria_settlement_classification", "blocks_V1.1",
                               "Nigeria_Blocks_V1.shp"),
  output_dir = file.path(OutputsDir)
)

## -----------------------------------------------------------------------------------------------------------------------------------------
### Extraction
## -----------------------------------------------------------------------------------------------------------------------------------------

extract_variables <- function(name, State, shapefile, paths) {
  
  # start timer
  tic(paste("Starting extract_variables for", name))
  
  tryCatch({
    # Load shapefile
    message("Loading shapefile for ", name)
    wards <- tryCatch({
      st_make_valid(st_read(shapefile))
    }, error = function(e) stop("Failed to load shapefile: ", e$message))
    
    empty_geo <- st_is_empty(wards)
    wards <- wards[!empty_geo,]
    wards_sp <- as(wards, "Spatial")

    # Extract EVI
    message("Extracting EVI for ", name)
    evi_files <- list.files(paths$evi_path, pattern = ".tif", full.names = TRUE)
    if (length(evi_files) == 0) stop("No EVI files found.")
    evi_data <- lapply(evi_files, raster)
    evi_extracted <- purrr::map(evi_data, ~raster::extract(., wards, fun = mean, df = TRUE)) %>%
      purrr::reduce(~merge(.x, .y, by = "ID"))
    evi_extracted$avgEVI <- rowMeans(evi_extracted[, -1], na.rm = TRUE)
    wards$mean_EVI <- evi_extracted$avgEVI

    # Extract NDVI
    message("Extracting NDVI for ", name)
    ndvi_files <- list.files(paths$ndvi_path, pattern = ".tif", full.names = TRUE)
    if (length(ndvi_files) == 0) stop("No NDVI files found.")
    ndvi_data <- lapply(ndvi_files, raster)
    ndvi_extracted <- purrr::map(ndvi_data, ~raster::extract(., wards, fun = mean, df = TRUE)) %>%
      purrr::reduce(~merge(.x, .y, by = "ID"))
    ndvi_extracted$avgNDVI <- rowMeans(ndvi_extracted[, -1], na.rm = TRUE)
    wards$mean_NDVI <- ndvi_extracted$avgNDVI

    # Extract rainfall
    message("Extracting Rainfall for ", name)
    rainfall_files <- list.files(paths$rainfall_path, pattern = ".tif", full.names = TRUE)
    if (length(rainfall_files) == 0) stop("No rainfall files found.")
    rainfall_data <- lapply(rainfall_files, raster)
    rainfall_extracted <- purrr::map(rainfall_data, ~raster::extract(., wards, fun = mean, df = TRUE)) %>%
      purrr::reduce(~merge(.x, .y, by = "ID"))
    rainfall_extracted$avgRainfall <- rowMeans(rainfall_extracted[, -1], na.rm = TRUE)
    wards$mean_rainfall <- rainfall_extracted$avgRainfall

    # Extract distance to water bodies
    message("Extracting Distance to Water Bodies for ", name)
    h2o_distance_raster <- tryCatch({
      raster(paths$h2o_distance_path)
    }, error = function(e) stop("Failed to load water distance raster: ", e$message))
    distance_extracted <- raster::extract(h2o_distance_raster, wards, fun = mean, df = TRUE)
    wards$distance_to_water <- distance_extracted$distance_to_water

    #Extract relative humidity
    message("Extracting Relative Humidity for ", name)
    rh_files <- list(rh_23 <- brick(paths$rh_2023),
                     rh_24 <- brick(paths$rh_2024))
    names(rh_files) <- c("2023", "2024")

    for (i in 1:length(rh_files)) {
      num_layers <- nlayers(rh_files[[i]])
      layer_names <- paste0("RH_", seq_len(num_layers))
      names(rh_files[[i]]) <- layer_names
    }

    ward_list_RH <- list()
    for (i in 1:length(rh_files)) {
      ward_RH_data <- extract(rh_files[[i]], wards_sp, fun = mean, df = TRUE)
      df_RH <- as.data.frame(ward_RH_data)
      df_RH$Year <- names(rh_files)[i]
      ward_list_RH[[i]] <- df_RH
    }

    ward_rh2 <- bind_rows(ward_list_RH)
    rh_ward <- ward_rh2 %>%
      group_by(ID) %>%
      summarize(RH_mean = mean(c_across(starts_with("RH_")), na.rm = TRUE))
    wards$RH_mean <- rh_ward$RH_mean
    
    
    # Extract temperature
    message("Extracting Temperature for ", name)
    temp_files <- list(temp_23 <- brick(paths$temp_2023),
                    temp_24 <- brick(paths$temp_2024))
    names(temp_files) <- c("2023", "2024")
    
    for (i in 1:length(temp_files)) {
      num_layers <- nlayers(temp_files[[i]])
      layer_names <- paste0("temp_", seq_len(num_layers))
      names(temp_files[[i]]) <- layer_names
    }
    
    ward_list_temp <- list()
    for (i in 1:length(temp_files)) {
      ward_temp_data <- extract(temp_files[[i]], wards_sp, fun = mean, df = TRUE)
      df_temp <- as.data.frame(ward_temp_data)
     df_temp$Year <- names(temp_files)[i]
      ward_list_temp[[i]] <- df_temp
    }
    
    ward_temp2 <- bind_rows(ward_list_temp)
    temp_ward <- ward_temp2 %>%
      group_by(ID) %>%
      summarize(temp_mean = mean(c_across(starts_with("temp_")), na.rm = TRUE))
    wards$temp_mean <- temp_ward$temp_mean
    
    
    # Extract housing quality
    message("Extracting Housing Quality for ", name)
    housing_quality_raster <- tryCatch({
      raster(paths$housing_quality_path)
    }, error = function(e) stop("Failed to load housing quality raster: ", e$message))
    housing_extracted <- raster::extract(housing_quality_raster, wards, fun = mean, df = TRUE)
    wards$housing_quality <- housing_extracted$X2019_Nature_Africa_Housing_2015_NGA
    
    # Extract PfPR
    message("Extracting PfPR for ", name)
    pfpr_raster <- tryCatch({
      raster(paths$pfpr_path)
    }, error = function(e) stop("Failed to load PfPR raster: ", e$message))
    pfpr_extracted <- raster::extract(pfpr_raster, wards, fun = mean, df = TRUE)
    wards$pfpr <- pfpr_extracted$X202406_Global_Pf_Parasite_Rate_NGA_2022_1
    
    # Extract night-time light
    message("Extracting Night-Time Light for ", name)
    light_files <- list.files(paths$lights_path, pattern = ".tif", full.names = TRUE)
    if (length(light_files) == 0) stop("No night-time light files found.")
    night_light_data <- lapply(light_files, raster)
    night_light_extracted <- purrr::map(night_light_data, ~raster::extract(., wards, fun = mean, df = TRUE)) %>%
      purrr::reduce(~merge(.x, .y, by = "ID"))
    night_light_extracted$avgRAD <- rowMeans(night_light_extracted[, -1], na.rm = TRUE)
    wards$avgRAD <- night_light_extracted$avgRAD
    
    # Extract flood
    message("Extracting Floods for ", name)
    flood_2023_files <- list.files(paths$flood_path, pattern = ".tif", full.names = TRUE)
    if (length(flood_2023_files) == 0) stop("No Flood files found.")
    flood_2023_data <- lapply(flood_2023_files, raster)
    #flood_2023_extracted <- purrr::map(flood_2023_data, ~raster::extract(., wards, fun = mean, df = TRUE)) %>%
      #purrr::reduce(~merge(.x, .y, by = "ID"))
    
    # Combine into a stack if there are multiple rasters
    if (length(flood_2023_data) > 1) {
      flood_stack <- raster::stack(flood_2023_data)
      flood_2023_extracted <- raster::extract(flood_stack, wards, fun = mean, df = TRUE)
      
      # Calculate mean across layers
      flood_2023_extracted$flood_presence <- rowMeans(flood_2023_extracted[, -1], na.rm = TRUE)
    } else {
      # Single raster case
      flood_2023_extracted <- raster::extract(flood_2023_data[[1]], wards, fun = mean, df = TRUE)
      flood_2023_extracted$flood_presence <- flood_2023_extracted[, 2] # Second column is the extracted value
    }
    wards$flood <- flood_2023_extracted$flood_presence

    # Extract Normalized Difference Water Index
    message("Extracting NDWI for ", name)
    ndwi_raster <- tryCatch({
      raster(paths$ndwi_path)
    }, error = function(e) stop("Failed to load NDWI raster: ", e$message))
    ndwi_extracted <- raster::extract(ndwi_raster, wards, fun = mean, df = TRUE)
    wards$NDWI <- ndwi_extracted$NDWI

    # Extract Normalized Difference Moisture Index
    message("Extracting NDMI for ", name)
    ndmi_raster <- tryCatch({
      raster(paths$ndmi_path)
    }, error = function(e) stop("Failed to load NDMI raster: ", e$message))
    ndmi_extracted <- raster::extract(ndmi_raster, wards, fun = mean, df = TRUE)
    wards$NDMI <- ndmi_extracted$NDMI

    # Extract settlement type


    # Extract Global surface water


    # Extract elevation
    message("Extracting Elevation for ", name)
    elevation_raster <- tryCatch({
      raster(paths$elevation_path)
    }, error = function(e) stop("Failed to load Elevation raster: ", e$message))
    elevation_extracted <- raster::extract(elevation_raster, wards, fun = mean, df = TRUE)
    wards$elevation <- elevation_extracted$ELE

    # Extract Surface Soil wetness
    message("Extracting Surface Soil Wetness for ", name)
    soil_h2o_files <- list.files(paths$surface_soil_wetness_path, pattern = ".tif", full.names = TRUE)
    if (length(soil_h2o_files) == 0) stop("No Surface Soil Wetness files found.")
    soil_h2o_data <- lapply(soil_h2o_files, raster)
    soil_h2o_extracted <- purrr::map(soil_h2o_data, ~raster::extract(., wards, fun = mean, df = TRUE)) %>%
      purrr::reduce(~merge(.x, .y, by = "ID"))
    soil_h2o_extracted$avg_soilwetness <- rowMeans(soil_h2o_extracted[, -1], na.rm = TRUE)
    wards$mean_soil_wetness <- soil_h2o_extracted$avg_soilwetness
    
    # Save
    message("Saving Extracted Variables for ", name)
    output_variables <- st_drop_geometry(wards)
    output_file <- file.path(paths$output_dir, "five_extractions", paste0(name, "_wards_variables.csv"))
    write.csv(output_variables, output_file, row.names = FALSE)
    
    message("Extraction completed successfully for ", name)
    return(output_variables)
    
    # end timer and display total elapsed time
    toc(log = TRUE)
    
  }, error = function(e) {
    toc(log = TRUE)
    message("An error occurred while processing ", name, ": ", e$message)
    NULL
  })
}

# Yet to include: surface water, settlement_type


## =========================================================================================================================================
### Run Extraction Function for 5 States in Nigeria: Kano, Kaduna, Katsina, Niger, Delta (NMEP Report)
## =========================================================================================================================================

# Extract variables for all Kano State Wards
kano_state_variables <- extract_variables(
  name = "Kano",
  State = "Kano",
  shapefile = file.path(ShpfilesDir, "all_reprioritization_nmep_states", "STATES", "Kano", "Kano_State.shp"),
  paths = paths
)

# Extract variables for all Kaduna State Wards
kaduna_state_variables <- extract_variables(
  name = "Kaduna",
  State = "Kaduna",
  shapefile = file.path(ShpfilesDir, "all_reprioritization_nmep_states", "STATES", "Kaduna", "Kaduna_State.shp"),
  paths = paths
)

# Extract variables for all Katsina State Wards
katsina_state_variables <- extract_variables(
  name = "Katsina",
  State = "Katsina",
  shapefile = file.path(ShpfilesDir, "all_reprioritization_nmep_states", "STATES", "Katsina", "Katsina_State.shp"),
  paths = paths
)

# Extract variables for all Niger State Wards
niger_state_variables <- extract_variables(
  name = "Niger",
  State = "Niger",
  shapefile = file.path(ShpfilesDir, "all_reprioritization_nmep_states", "STATES", "Niger", "Niger_State.shp"),
  paths = paths
)

# Extract variables for all Delta State Wards
delta_state_variables <- extract_variables(
  name = "Delta",
  State = "Delta",
  shapefile = file.path(ShpfilesDir, "all_reprioritization_nmep_states", "Delta", "Delta_Wards.shp"),
  paths = paths
)

# Extract variables for all Yobe State Wards
yobe_state_variables <- extract_variables(
  name = "Yobe",
  State = "Yobe",
  shapefile = file.path(ShpfilesDir, "all_reprioritization_nmep_states", "Yobe", "Yobe_State.shp"),
  paths = paths
)

# Extract variables for all Taraba State Wards
taraba_state_variables <- extract_variables(
  name = "Taraba",
  State = "Taraba",
  shapefile = file.path(ShpfilesDir, "all_reprioritization_nmep_states", "Taraba", "Taraba_State.shp"),
  paths = paths
)

## -----------------------------------------------------------------------------------------------------------------------------------------
### Find Number of Wards in Each State
## -----------------------------------------------------------------------------------------------------------------------------------------

# read in datasets
kano <- read_csv(file.path(OutputsDir, "five_extractions/Kano_wards_variables.csv"))
kaduna <- read_csv(file.path(OutputsDir, "five_extractions/Kaduna_wards_variables.csv"))
katsina <- read_csv(file.path(OutputsDir, "five_extractions/Katsina_wards_variables.csv"))
niger <- read_csv(file.path(OutputsDir, "five_extractions/Niger_wards_variables.csv"))
delta <- read_csv(file.path(OutputsDir, "five_extractions/Delta_wards_variables.csv"))
yobe <- read_csv(file.path(OutputsDir, "five_extractions/Yobe_wards_variables.csv"))
taraba <- read_csv(file.path(OutputsDir, "five_extractions/Taraba_wards_variables.csv"))

# make ward name var consistent across dfs
kaduna <- kaduna %>% rename(WardName = ward_name)
niger <- niger %>% rename(WardName = ward_name)

# create a list of dataframes
df_list <- list(kano, kaduna, katsina, niger, delta, yobe, taraba)

# create a vector of state names
state_names <- c("Kano", "Kaduna", "Katsina", "Niger", "Delta", "Yobe", "Taraba")

# create the table
ward_counts <- data.frame(
  State = state_names,
  Number_Wards = sapply(df_list, function(df) n_distinct(df$WardName))
)

write.csv(ward_counts, file.path(OutputsDir, "five_extractions/number_wards_per_state.csv"))

## -----------------------------------------------------------------------------------------------------------------------------------------
### Merge TPR Data with Extractions
## -----------------------------------------------------------------------------------------------------------------------------------------

# read in datasets again (they are an updated copy with the settlement type data)
kano <- read_csv(file.path(OutputsDir, "five_extractions/Kano_wards_variables.csv"))
kaduna <- read_csv(file.path(OutputsDir, "five_extractions/Kaduna_wards_variables.csv"))
katsina <- read_csv(file.path(OutputsDir, "five_extractions/Katsina_wards_variables.csv"))
niger <- read_csv(file.path(OutputsDir, "five_extractions/Niger_wards_variables.csv"))
delta <- read_csv(file.path(OutputsDir, "five_extractions/Delta_wards_variables.csv"))
yobe <- read_csv(file.path(OutputsDir, "five_extractions/Yobe_wards_variables.csv"))
taraba <- read_csv(file.path(OutputsDir, "five_extractions/Taraba_wards_variables.csv"))

# read in tpr data
TprDir <- file.path(DriveDir, "data/nigeria/nigeria_hmis/TPR data for selected NMEP prioritized states")
kano_tpr <- read_csv(file.path(TprDir, "kanotpr3.csv"))
kaduna_tpr <- read_csv(file.path(TprDir, "kadunatpr.csv"))
katsina_tpr <- read_csv(file.path(TprDir, "katsinatpr.csv"))
niger_tpr <- read_csv(file.path(TprDir, "nigertpr.csv"))
delta_tpr <- read_csv(file.path(TprDir, "Deltatpr.csv"))
yobe_tpr <- read_csv(file.path(TprDir, "yobetpr.csv"))
taraba_tpr <- read_csv(file.path(TprDir, "Tarabatpr.csv"))

# select only ward name and tpr columns in each df
kano_tpr <- kano_tpr %>% dplyr::select(WardName, u5_tpr_rdt)
kaduna_tpr <- kaduna_tpr %>% dplyr::select(WardName, u5_tpr_rdt)
katsina_tpr <- katsina_tpr %>% dplyr::select(WardName, u5_tpr_rdt)
niger_tpr <- niger_tpr %>% dplyr::select(WardName, u5_tpr_rdt)
delta_tpr <- delta_tpr %>% dplyr::select(WardName, u5_tpr_rdt)
yobe_tpr <- yobe_tpr %>% dplyr::select(WardName, u5_tpr_rdt)
taraba_tpr <- taraba_tpr %>% dplyr::select(WardName, u5_tpr_rdt)

# merge dfs
kano_final <- merge(kano, kano_tpr, by = "WardName", all = TRUE)
kaduna_final <- merge(kaduna, kaduna_tpr, by = "WardName", all = TRUE)
katsina_final <- merge(katsina, katsina_tpr, by = "WardName", all = TRUE)
niger_final <- merge(niger, niger_tpr, by = "WardName", all = TRUE)
delta_final <- merge(delta, delta_tpr, by = "WardName", all = TRUE)
yobe_final <- merge(yobe, yobe_tpr, by = "WardName", all = TRUE)
taraba_final <- merge(taraba, taraba_tpr, by = "WardName", all = TRUE)

# save dfs
write.csv(kano_final, file.path(OutputsDir, "Final Extractions/kano.csv"))
write.csv(kaduna_final, file.path(OutputsDir, "Final Extractions/kaduna.csv"))
write.csv(katsina_final, file.path(OutputsDir, "Final Extractions/katsina.csv"))
write.csv(niger_final, file.path(OutputsDir, "Final Extractions/niger.csv"))
write.csv(delta_final, file.path(OutputsDir, "Final Extractions/delta.csv"))
write.csv(yobe_final, file.path(OutputsDir, "Final Extractions/yobe.csv"))
write.csv(taraba_final, file.path(OutputsDir, "Final Extractions/taraba.csv"))

## -----------------------------------------------------------------------------------------------------------------------------------------
### NIGER: Check that ward names match with those in shapefiles
## -----------------------------------------------------------------------------------------------------------------------------------------

# read in shapefiles
niger.shp <- sf::st_read(file.path("/Users/grace/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/nigeria_shapefiles/shapefiles/ShinyApp_shapefiles/all_reprioritization_nmep_states/STATES/Niger/Niger_State.shp"))

# read in itn data
niger_itn <- readxl::read_xlsx(
  "/Users/grace/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/ITN_distribution/household_member_ward_summaries_Itn_distribution/ITN_distribution_total_ward_niger_2022.xlsx"
)

# convert shapefiles to dfs
niger_shp_df <- as.data.frame(niger.shp)

# extract unique ward names
ward_names_niger_shp <- unique(niger_shp_df$WardName)
ward_names_niger_itn <- unique(niger_itn$Ward)

# Find values in Ward that are not in WardName
not_in_itn <- setdiff(ward_names_niger_shp, ward_names_niger_itn)
not_in_shp <- setdiff(ward_names_niger_itn, ward_names_niger_shp)

# Print unmatched values
cat("Ward names in shapefile but not in ITN data:\n")
print(not_in_itn)

cat("\nWard names in ITN data but not in shapefile:\n")
print(not_in_shp)

## -----------------------------------------------------------------------------------------------------------------------------------------
### NIGER: recode ITN ward names to match those in shapefile
## -----------------------------------------------------------------------------------------------------------------------------------------

# Create a named vector for recoding ward names in niger_itn
ward_recode <- c(
  # Identify and map similar names from ITN to shapefile names
  "Busuru" = "Bunsuru",
  "Masaba A" = "Masaba A/Masaga I",
  "Masaga B" = "Masaga B/Masaga  II",
  "Umaru Majlgi A" = "Umaru Majagi A/Umaru Majigi  I",
  "Umaru Majlgi B" = "Umaru Majagi B/Umaru Majigi  II",
  "Dokodza" = "Dokoza",
  "Cheniyan" = "Chenian",
  "Chanchaga" = "Chachanga",
  "T/Wada North" = "T/Wada N",
  "T/Wada South" = "T/Wada S",
  "Gazhe" = "Gazhe 1",
  "Fahzi" = "Fazhi",
  "Nuwankota" = "Nuwan Kuta",
  "Edozhigi" = "Edozigi",
  "Sommazhiko" = "Sojmajiko",
  "Bonu" = "Bono",
  "Badeggi" = "Badegi",
  "Ebbo/Gbachinku" = "Ebbo",
  "Muye/Egba" = "Muye",
  "Kusotacin" = "Kuso Tachin",
  "Mantabefyan" = "Manbe Tafyan",
  "Dokko" = "Doko",
  "Dasun" = "Dassun",
  "Kucibusu" = "Kuchi Bussu",
  "Anaba" = "Anaba/Ibelu West",
  "Tungan Jika" = "Tungajika/Auna South",
  "Kura" = "Kura/Auna East",
  "Yangalu" = "Yangalu/Ibelu East",
  "Salka" = "Salka/Auna East Central",
  "Magamadaji" = "Magamadaji/Ibelu North",
  "Manigi" = "Manigi/Dapangi/Makera",
  "Sahonrami" = "Sahonrami/Saho-Rami",
  "Labohzi" = "Labozhi",
  "Muregi" = "Moregi",
  "Rabba Ndayoko" = "Raba Ndayako",
  "Dan Dauda" = "Dandaudu",
  "Sarkin Pawa" = "Sarki Power",
  "Nikuchi Tunga Mallam" = "Tungamallam/Nikuchi",
  "Kusheriki North" = "Kusherki North",
  "Kusheriki South" = "Kusherki South",
  "Ciki Gari" = "Cikin Gari",
  "Kundu (Gunna South)" = "Gunna South",
  "Yekila(Gunna)" = "Gunna Central",
  "Kawna (Tegina West)" = "Tegina West",
  "Madaka (Kakuri)" = "Kakuri",
  "Sabon Gari Ushe" = "Sabon Gari Wuse",
  "Danrangi" = "Danragi",
  "Kwaki" = "Kwaki/Chikuba",
  "Kuregbe" = "Kurebe/Kushaka",
  "Gusoro/Zumba" = "Gussoro/Zumba",
  "Pina" = "Pina/Kolu",
  "Ecwa/Gwada" = "Egwa/Gwada",
  "Iku 1" = "Iku 1/Iku South I",
  "Iku 2" = "Iku 2/Iku South II",
  "Gwarjiko" = "Gwarijiko",
  "Lokogoma" = "Lokogwoma"
)

# Recode Ward names in niger_itn to match shapefile names
niger_itn$Ward <- recode(niger_itn$Ward, !!!ward_recode)

# Validate the updated dataset
unmatched_after_recode <- setdiff(niger_itn$Ward, niger_shp_df$WardName)

# Print any remaining unmatched wards
print(unmatched_after_recode)

## -----------------------------------------------------------------------------------------------------------------------------------------
### KATSINA: Check that ward names match with those in shapefiles
## -----------------------------------------------------------------------------------------------------------------------------------------

# read in shapefiles
katsina.shp <- sf::st_read(file.path("/Users/grace/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/nigeria_shapefiles/shapefiles/ShinyApp_shapefiles/all_reprioritization_nmep_states/STATES/Katsina/Katsina_State.shp"))

# read in itn data
katsina_itn <- readxl::read_xlsx(
  "/Users/grace/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/ITN_distribution/household_member_ward_summaries_Itn_distribution/ITN_distribution_total_ward_katsina_2022.xlsx"
)
katsina_itn <- read.csv("/Users/grace/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/ITN_distribution/household_member_ward_summaries_Itn_distribution/katsina_itn_recoded.csv")

# convert shapefiles to dfs
katsina_shp_df <- as.data.frame(katsina.shp)

# extract unique ward names
ward_names_katsina_shp <- unique(katsina_shp_df$WardName)
ward_names_katsina_itn <- unique(katsina_itn$Ward)

# Find values in Ward that are not in WardName
not_in_itn <- setdiff(ward_names_katsina_shp, ward_names_katsina_itn)
not_in_shp <- setdiff(ward_names_katsina_itn, ward_names_katsina_shp)

# Print unmatched values
cat("Ward names in shapefile but not in ITN data:\n")
print(not_in_itn)

cat("\nWard names in ITN data but not in shapefile:\n")
print(not_in_shp)


## -----------------------------------------------------------------------------------------------------------------------------------------
### KATSINA: recode ITN ward names to match those in shapefile
## -----------------------------------------------------------------------------------------------------------------------------------------

# Create a named vector for recoding ward names in niger_itn
ward_recode <- c(
  # Identify and map similar names from ITN to shapefile names
  "Ang Musa" = "Anguwan Musa",
  "Ba'Awa" = "Ba'awa",
  "Babba Mutum" = "Babban Mutum",
  "Bagagadi" = "Bugagadi",
  "Barde/Kkw" = "Barde Kwantakwaran",
  "Borin Dawa" = "Borindawa",
  "Dan Alhaji" = "Dan Alhaji Yangayya",
  "Dan Kum" = "Dankum",
  "Danali" = "Dan Ali",
  "Dan'Aunai" = "Dan Aunai",
  "Danjanku" = "Danjanku-Karachi",
  "Danmarabu" = "Dan Murabu",
  "Danmusa A" = "Dan Musa A",
  "Danmusa B" = "Dan Musa B",
  "Dantakari" = "Dantankari",
  "Darini/ Magaji Abu" = "Darini Magaji Abu",
  "Daunaka/Bakin Kwari" = "Daunaka",
  "Dawan Musa" = "Dawa Musa",
  "Dudunni" = "Duddunni",
  "Dungun Muazu" = "Dugu Muazu",
  "Dutsin Kura" = "Dutsen Kura",
  "Dutsinma A" = "Dutsin Ma A",
  "Dutsinma B" = "Dutsin Ma B",
  "Duwan/Makau" = "Duwan Makau",
  "Fakuwa/Kafin Dangi" = "Fakuwa Kafin Dangi",
  "Gafia" = "Gafiya",
  "Gora Dansaka" = "Gora Dan Saka",
  "Kahutu A" = "Kahuta A",
  "Kahutu B" = "Kahuta B",
  "Kandawa /Jobe" = "Kandawa/Jobe",
  "Karadua" = "Karaduwa",
  "Katare" = "Ketare",
  "Kawarin Malama" = "Mai Gora",
  "Kudi 1" = "Kudu 1",
  "Kudi 2" = "Kudu 2",
  "Kudi 3" = "Kudu 3",
  "Kunduru/Gyaza" = "Kunduru Gyaza",
  "Kurami/Yankwani" = "Kurami Yankwani",
  "Magama/Masabo/Kurabau" = "Magama Masabo Kurabau",
  "Maiadua A" = "Mai'adua A",
  "Maiadua B" = "Mai'adua B",
  "Maiadua C" = "Mai'adua C",
  "Maigora" = "Mai Gora",
  "Mairuwa" = "Mairua",
  "Maje/Karare" = "Maje Karare",
  "Muduri" = "Mudiri",
  "Na Alma" = "Na'alma",
  "Ruma" = "Rumah",
  "Sabongari" = "Sabon Gari",
  "Sarikin Yara B" = "Sarkin Yara B",
  "Tabanni" = "Tabanni-Yarraddau",
  "Tafashiya/ Nassarawa" = "Tafashiya Nasarawa",
  "Tama/Daye" = "Tama",
  "Tsa/Magam" = "Tsa Magam",
  "Tsagem / Takusheyi" = "Tsagem Takusheyi",
  "Uban Dawaki A" = "Ubandawaki A",
  "Uban Dawaki B" = "Ubandawaki B",
  "Yanmama" = "Yarmama",
  "Yargayya" = "Yangayya",
  "Yau Yau/ Mallamawa" = "Yau-Yau Malamawa",
  "Yaya/Bidore" = "Yaya"
)

# Recode Ward names in niger_itn to match shapefile names
katsina_itn$Ward <- recode(katsina_itn$Ward, !!!ward_recode)

# Validate the updated dataset
unmatched_after_recode <- setdiff(katsina_itn$Ward, katsina_shp_df$WardName)

# Print any remaining unmatched wards
print(unmatched_after_recode)



## -----------------------------------------------------------------------------------------------------------------------------------------
### Save updated ITN data
## -----------------------------------------------------------------------------------------------------------------------------------------
write.csv(niger_itn, file.path(DataDir, "nigeria/ITN_distribution/household_member_ward_summaries_Itn_distribution/niger_itn_recoded.csv"))
write.csv(katsina_itn, file.path(DataDir, "nigeria/ITN_distribution/household_member_ward_summaries_Itn_distribution/katsina_itn_recoded.csv"))

