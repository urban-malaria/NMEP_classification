###Automated variable extraction function

#Extracts EVI, NDVI, Rainfall, Distance to water, relative humidity,
# temperature, housing quality, pfpr (Plasmodium falciparum parasite rate), night time light, 
#surface water, settlement type, elevation


#Variable File Paths

paths <- list(
  evi_path = file.path(RastersDir, "Updated_Covariates", "2023-24_EVI_MOD13A1"),
  ndvi_path = file.path(RastersDir, "Updated_Covariates", "2023-24_NDVI_MOD13A1"),
  rainfall_path = file.path(RastersDir, "monthly rainfall 2023-24"),
  h2o_distance_path = file.path(RastersDir, "distance_to_water_bodies", "distance_to_water.tif"),
  elevation_path <- file.path(RastersDir, "Elevation", "ELE.tif"), 
  rh_2023 = file.path(RastersDir, "temp_humidity_2023_grib", "relative_humidity_2023.grib"),
  rh_2024 = file.path(RastersDir, "temp_humidity_2023_grib", "relative_humidity_2024.grib"),
  temp_2023 = file.path(RastersDir, "temp_humidity_2023_grib", "temperature_2023.grib"),
  temp_2024 = file.path(RastersDir, "temp_humidity_2023_grib", "temperature_2024.grib"),
  housing_quality_path = file.path(RastersDir, "housing", 
                                   "2019_Nature_Africa_Housing_2015_NGA.tiff"),
  pfpr_path = file.path(PfDir, "pf_parasite_rate", 
                        "202406_Global_Pf_Parasite_Rate_NGA_2022.tiff"),
  lights_path = file.path(RastersDir, "night_time_light_2023"),
  surface_h20_path = file.path(RastersDir, "global_surface_water"),
  settlement_block = file.path(RastersDir, "nigeria_settlement_classification", "blocks_V1.1",
            "Nigeria_Blocks_V1.shp"),
  output_dir = file.path(OutputsDir)
)

extract_variables <- function(name, State, shapefile, paths) {
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
    
    
    #Extract temperature
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
    
    #Extract settlement type
    
    
    #Extract Global surface water
  
    
    #Extract elevation
    
    # Save
    message("Saving Extracted Variables for ", name)
    output_variables <- st_drop_geometry(wards)
    output_file <- file.path(paths$output_dir, paste0(name, "_wards_variables.csv"))
    write.csv(output_variables, output_file, row.names = FALSE)
    
    message("Extraction completed successfully for ", name)
    return(output_variables)
    
  }, error = function(e) {
    message("An error occurred while processing ", name, ": ", e$message)
    NULL
  })
}

# Yet to include: surface water, elevation, settlement_type






