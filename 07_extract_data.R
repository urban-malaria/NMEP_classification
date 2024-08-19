##Extract envrionmental variables for Warri and Asaba

library(raster)
library(sf)
library(terra)
library(stringr)
library(ggplot2)
library(dplyr)
library(purrr)
library(haven)
library(stars)


#Load directories
Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", gsub("OneDrive", "", Sys.getenv("HOME")))))
Drive <- file.path(gsub("[//]", "/", Drive))
DriveDir <- file.path(Drive, "Urban Malaria Proj Dropbox", "urban_malaria")
RasterDir <- file.path(DriveDir, "data", "nigeria", "Raster_files")


## Load shapefiles
ShpDir <- file.path(DriveDir, "data", "nigeria", "shapefiles", "ShinyApp_shapefiles")

warri_shp <- st_read(file.path(ShpDir, "Warri", "Warri.shp"))
asaba_shp <- st_read(file.path(ShpDir, "Asaba", "Asaba.shp"))


# Rainfall
rainfall_rasters <-file.path(RasterDir, "monthly rainfall 2023-24")


#Extract rainfall for warri
rainfall <- list.files(file.path(rainfall_rasters), 
                       pattern = ".tif", full.names = TRUE)

rainfall_data <- lapply(seq_along(rainfall), 
                       function(x) raster::raster(rainfall[[x]]))

warri_rainfall <- rainfall_data %>%
  purrr::map(~raster::extract(., warri_shp, fun = mean, df = TRUE)) %>% 
  purrr::reduce(function(x, y) merge(x, y, by = "ID"))

#check <- warri_rainfall[[1]]

warri_rainfall$avgRAIN <- rowMeans(warri_rainfall[ , -1])
warri_shp$mean_rainfall <- warri_rainfall$avgRAIN


# Asaba
asaba_rainfall <- rainfall_data %>% 
  purrr::map(~raster::extract(., asaba_shp, fun = mean, df = TRUE)) %>% 
  purrr::reduce(function(x,y) merge(x,y, by = "ID"))

asaba_rainfall$avgRAIN <- rowMeans(asaba_rainfall[ , -1])
asaba_shp$mean_rainfall <- asaba_rainfall$avgRAIN


##EVI

evi_rasters <-file.path(RasterDir, "Updated_Covariates", "2023-24_EVI_MOD13A1")

evi <- list.files(file.path(evi_rasters), 
                       pattern = ".tif", full.names = TRUE)

evi_data <- lapply(seq_along(evi), 
                        function(x) raster::raster(evi[[x]]))

warri_evi <- evi_data %>%
  purrr::map(~raster::extract(., warri_shp, fun = mean, df = TRUE)) %>% 
  purrr::reduce(function(x, y) merge(x, y, by = "ID"))

warri_evi$avgEVI <- rowMeans(warri_evi[ , -1], na.rm = TRUE)
warri_shp$mean_EVI <- warri_evi$avgEVI


asaba_evi <- evi_data %>%
  purrr::map(~raster::extract(., asaba_shp, fun = mean, df = TRUE)) %>% 
  purrr::reduce(function(x, y) merge(x, y, by = "ID"))

asaba_evi$avgEVI <- rowMeans(asaba_evi[ , -1], na.rm = TRUE)
asaba_shp$mean_EVI <- asaba_evi$avgEVI


##NDVI

ndvi_rasters <-file.path(RasterDir, "Updated_Covariates", "2023-24_NDVI_MOD13A1")

ndvi <- list.files(file.path(ndvi_rasters), 
                  pattern = ".tif", full.names = TRUE)

ndvi_data <- lapply(seq_along(ndvi), 
                   function(x) raster::raster(ndvi[[x]]))

warri_ndvi <- ndvi_data %>%
  purrr::map(~raster::extract(., warri_shp, fun = mean, df = TRUE)) %>% 
  purrr::reduce(function(x, y) merge(x, y, by = "ID"))

warri_ndvi$avg_ndvi <- rowMeans(warri_ndvi[ , -1], na.rm = TRUE)
warri_shp$mean_NDVI <- warri_ndvi$avg_ndvi


asaba_ndvi <- ndvi_data %>%
  purrr::map(~raster::extract(., asaba_shp, fun = mean, df = TRUE)) %>% 
  purrr::reduce(function(x, y) merge(x, y, by = "ID"))

asaba_ndvi$avg_ndvi <- rowMeans(asaba_ndvi[ , -1], na.rm = TRUE)
asaba_shp$mean_NDVI <- asaba_ndvi$avg_ndvi


### Relative humidity

relative_humidity <- raster(file.path(RasterDir, "relative_humidity_2024.grib"))

warri_rh <- extract(relative_humidity, warri_shp, fun = mean, df = TRUE)
colnames(warri_rh)[2] <- "relative_humidity"
warri_shp$relative_humidity <- warri_rh$relative_humidity


asaba_rh <- extract(relative_humidity, asaba_shp, fun = mean, df = TRUE)
colnames(asaba_rh)[2] <- "relative_humidity"
asaba_shp$relative_humidity <- asaba_rh$relative_humidity

#### temperature
temperature <- raster(file.path(RasterDir, "temperature_2024.grib"))

warri_temp <- extract(temperature, warri_shp, fun = mean, df = TRUE)
colnames(warri_temp)[2] <- "temperature"
warri_shp$temp <- warri_temp$temperature


asaba_temp <- extract(temperature, asaba_shp, fun = mean, df = TRUE)
colnames(asaba_temp)[2] <- "temperature"
asaba_shp$temp <- asaba_temp$temperature


### distance to h2o bodies

h2o_distance <- raster(file.path(RasterDir, "distance_to_water_bodies", "distance_to_water.tif"))

warri_distance <- extract(h2o_distance, warri_shp, fun = mean, df = TRUE)
warri_shp$distance_to_water <- warri_distance$distance_to_water

asaba_distance <- extract(h2o_distance, asaba_shp, fun = mean, df = TRUE)
asaba_shp$distance_to_water <- asaba_distance$distance_to_water


##

warri_variables <- warri_shp %>%
  select(WardName, Urban,
         mean_rainfall, mean_EVI, mean_NDVI,
         relative_humidity, temp, distance_to_water)

saveRDS(warri_variables, "warri_var.RDS")

write.csv(warri_variables, file.path(ShpDir, "warri_variables.csv"))


asaba_variables <- asaba_shp %>%
  select(WardName, Urban,
         mean_rainfall, mean_EVI, mean_NDVI,
         relative_humidity, temp, distance_to_water)

saveRDS(asaba_variables, "asaba_var.RDS")

write.csv(asaba_variables, file.path(ShpDir, "asaba_variables.csv"))

  

