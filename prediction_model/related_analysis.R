#extract rainfall, EVI, NDVI, distance to water bodies, elevation, relative humidity, temperature,

#source("~/NMEP_classification/load_path.R", echo = T)

library(sf);
library(raster);
library(terra);
library(haven)



Drive <- file.path(gsub("[//]", "/", Drive))
DriveDir <- file.path(Drive, "Urban Malaria Proj Dropbox", "urban_malaria")
RastersDir <- file.path(DriveDir, "data/nigeria/Raster_files")








empty_geometries <- st_is_empty(delta_shp)
delta_shp <- delta_shp[!empty_geometries, ]

##use replace all 'delta_shp' in the code with the shapefile variable you want to extract for

######extract from existing rainfall rasters
rainfall_rasters <-file.path(RastersDir, "monthly rainfall 2023-24")
rainfall <- list.files(file.path(rainfall_rasters), 
                       pattern = ".tif", full.names = TRUE)

rainfall_data <- lapply(seq_along(rainfall), 
                        function(x) raster::raster(rainfall[[x]]))

delta_rainfall <- rainfall_data %>%
  purrr::map(~raster::extract(.x, delta_shp, fun = mean, df = TRUE)) %>% 
  purrr::reduce(function(x, y) merge(x, y, by = "ID"))

#calculates meanrainfall and add value back to shape file for ease
delta_rainfall$avgRAIN <- rowMeans(delta_rainfall[ , -1])
delta_shp$mean_rainfall <- delta_rainfall$avgRAIN 


#EVI
evi_rasters <-file.path(RastersDir, "Updated_Covariates", "2023-24_EVI_MOD13A1")

evi <- list.files(file.path(evi_rasters), 
                  pattern = ".tif", full.names = TRUE)

evi_data <- lapply(seq_along(evi), 
                   function(x) raster::raster(evi[[x]]))

delta_evi <- evi_data %>%
  purrr::map(~raster::extract(., delta_shp, fun = mean, df = TRUE)) %>% 
  purrr::reduce(function(x, y) merge(x, y, by = "ID"))

delta_evi$avgEVI <- rowMeans(delta_evi[ , -1], na.rm = TRUE)
delta_shp$mean_EVI <- delta_evi$avgEVI

##NDVI
ndvi_rasters <-file.path(RastersDir, "Updated_Covariates", "2023-24_NDVI_MOD13A1")

ndvi <- list.files(file.path(ndvi_rasters), 
                   pattern = ".tif", full.names = TRUE)

ndvi_data <- lapply(seq_along(ndvi), 
                    function(x) raster::raster(ndvi[[x]]))

delta_ndvi <- ndvi_data %>%
  purrr::map(~raster::extract(., delta_shp, fun = mean, df = TRUE)) %>% 
  purrr::reduce(function(x, y) merge(x, y, by = "ID"))

delta_ndvi$avg_ndvi <- rowMeans(delta_ndvi[ , -1], na.rm = TRUE)
delta_shp$mean_NDVI <- delta_ndvi$avg_ndvi

#saveRDS(delta_shp, "delta.RDS") #save as R object if you want to continue later on
#delta_shp <- readRDS(file.path("~/delta.RDS"))

#modify the block of code for EVI/ NDVI for night time light rasters

##Distance to water bodies
delta_shp_sp <- as(delta_shp, "Spatial")

h2o_distance <- raster(file.path(RastersDir, "distance_to_water_bodies", "distance_to_water.tif"))

delta_distance <- extract(h2o_distance, delta_shp_sp, fun = mean, df = TRUE)
delta_shp$distance_to_water <- delta_distance$distance_to_water

##Elevation
delta_shp_sp <- as(delta_shp, "Spatial")

elevation <- raster(file.path(RastersDir, "Elevation", "ELE.tif"))

delta_ele <- extract(elevation, delta_shp_sp, fun = mean, df = TRUE)
delta_shp$elevation <- delta_ele$ELE


#relative humidity

list_RH <- list(rh_2023 <- brick(file.path(RastersDir, "relative_humidity_2023.grib")),
                rh_2024 <- brick(file.path(RastersDir, "relative_humidity_2024.grib"))
)

names(list_RH) <- c("2023", "2024")

for (i in 1:length(list_RH)) {
  num_layers <- nlayers(list_RH[[i]])
  layer_names <- paste0("RH_", seq_len(num_layers))
  names(list_RH[[i]]) <- layer_names
}


nlayers(list_RH[[2]])
plot(list_RH[[2]], 7)


delta_list_RH <- list() 

for (i in 1:length(list_RH)) {
  delta_RH_data <- extract(list_RH[[i]], delta_shp_sp, fun = mean, df = TRUE)
  df_RH <- as.data.frame(delta_RH_data)
  df_RH$Year <- names(list_RH)[i]
  delta_list_RH[[i]] <- df_RH
}

delta_rh2 <- bind_rows(delta_list_RH)

rh_delta <- delta_rh2 %>%
  group_by(ID) %>%
  summarize(RH_mean = mean(c_across(starts_with("RH_")), na.rm = TRUE))

delta_shp$RH_mean <- rh_delta$RH_mean

#temperature
list_temp <- list(temp_2023 <- brick(file.path(RastersDir, "temperature_2023.grib")),
                  temp_2024 <- brick(file.path(RastersDir, "temperature_2024.grib"))
)

names(list_temp) <- c("2023", "2024")

for (i in 1:length(list_temp)) {
  num_layers <- nlayers(list_temp[[i]])
  layer_names <- paste0("temp_", seq_len(num_layers))
  names(list_temp[[i]]) <- layer_names
}


nlayers(list_temp[[2]])
plot(list_temp[[2]], 7)

#extract for temperature for delta

delta_list_temp <- list() 

for (i in 1:length(list_temp)) {
  delta_temp_data <- extract(list_temp[[i]], delta_shp_sp, fun = mean, df = TRUE)
  df_temp <- as.data.frame(delta_temp_data)
  df_temp$Year <- names(list_temp)[i]
  delta_list_temp[[i]] <- df_temp
}

delta_temp2 <- bind_rows(delta_list_temp)

temp_delta <- delta_temp2 %>%
  group_by(ID) %>%
  summarize(temp_mean = mean(c_across(starts_with("temp_")), na.rm = TRUE))

delta_shp$temp_mean <- temp_delta$temp_mean


###save
delta_variables <- delta_shp %>%
  dplyr::select(WardName, Urban,
                mean_rainfall, mean_EVI, mean_NDVI, distance_to_water, elevation
                RH_mean, temp_mean) %>%  #add VIIRS
  st_drop_geometry() 

write.csv(delta_variables, file.path(x, "delta_variables.csv"))


