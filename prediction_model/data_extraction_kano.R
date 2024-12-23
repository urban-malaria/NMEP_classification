rm(list = ls())


library(sf);
library(raster);
library(terra);
library(haven)
library(dplyr)
library(sf)


Drive <- file.path("C:/Users/laure/Urban Malaria Proj Dropbox/urban_malaria/")
RastersDir <- file.path(Drive, "data/nigeria/Raster_files")



Kano_shpefile <- st_read("C:/Users/laure/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_shapefile/gridded_shapefile//gridded/Kano/Kano.shp")

empty_geometries <- st_is_empty(Kano_shpefile)
Kano_shpefile <- Kano_shpefile[!empty_geometries, ]



Kano_shpefile_cd <- st_centroid(st_make_valid(Kano_shpefile))




###### all rasters 

rainfall_rasters <-file.path(RastersDir, "monthly rainfall 2023-24") #Take long
evi_rasters <-file.path(RastersDir, "Updated_Covariates", "2023-24_EVI_MOD13A1")


ndvi_rasters <-file.path(RastersDir, "Updated_Covariates", "2023-24_NDVI_MOD13A1")






# main code 
rainfall <- list.files(file.path(rainfall_rasters), 
                       pattern = ".tif", full.names = TRUE)

rainfall_data <- lapply(seq_along(rainfall), 
                        function(x) raster::raster(rainfall[[x]]))


evi <- list.files(file.path(evi_rasters), 
                  pattern = ".tif", full.names = TRUE)


evi_data <- lapply(seq_along(evi), 
                   function(x) raster::raster(evi[[x]]))


ndvi <- list.files(file.path(ndvi_rasters), 
                  pattern = ".tif", full.names = TRUE)

ndvi_data <- lapply(seq_along(ndvi), 
                   function(x) raster::raster(ndvi[[x]]))





Kano_rainfall <- rainfall_data %>%
  purrr::map(~raster::extract(.x, Kano_shpefile_cd, fun = mean, df = TRUE)) %>% 
  purrr::reduce(function(x, y) merge(x, y, by = "ID"))



#calculates meanrainfall and add value back to shape file for ease
Kano_rainfall$avgRAIN <- rowMeans(Kano_rainfall[ , -1])

Kano_rainfall$avgRAIN_lagged <- rowMeans(Kano_rainfall[ , c(5,6,7 )])

Kano_shpefile_cd$mean_rainfall <- Kano_rainfall$avgRAIN 
Kano_shpefile_cd$mean_rainfall <- Kano_rainfall$avgRAIN_lagged # one month averaged over (5,6,7)


#EVI

evi <- list.files(file.path(evi_rasters), 
                  pattern = ".tif", full.names = TRUE)

evi_data <- lapply(seq_along(evi), 
                   function(x) raster::raster(evi[[x]]))

Kano_evi <- evi_data %>%
  purrr::map(~raster::extract(., Kano_shpefile_cd, fun = mean, df = TRUE)) %>% 
  purrr::reduce(function(x, y) merge(x, y, by = "ID"))

Kano_evi$avgEVI <- rowMeans(Kano_evi[ , -1], na.rm = TRUE)
Kano_shpefile_cd$mean_EVI <- Kano_evi$avgEVI





##NDVI

Kano_ndvi <- ndvi_data %>%
  purrr::map(~raster::extract(., Kano_shpefile_cd, fun = mean, df = TRUE)) %>% 
  purrr::reduce(function(x, y) merge(x, y, by = "ID"))

Kano_ndvi$avg_ndvi <- rowMeans(Kano_ndvi[ , -1], na.rm = TRUE)
Kano_shpefile_cd$mean_NDVI <- Kano_ndvi$avg_ndvi

#saveRDS(Kano_shpefile, "Kano.RDS") #save as R object if you want to continue later on
#Kano_shpefile <- readRDS(file.path("~/Kano.RDS"))

#modify the block of code for EVI/ NDVI for night time light rasters

##Distance to water bodies

Kano_shpefile_sp <- as(Kano_shpefile_cd, "Spatial")

h2o_distance <- raster::raster(file.path(RastersDir, "distance_to_water_bodies", "distance_to_water.tif"))

Kano_distance <- raster::extract(h2o_distance, buffer = 100, 
                                  Kano_shpefile_sp, fun = mean, df = TRUE)

Kano_shpefile_cd$distance_to_water_100m <- Kano_distance$distance_to_water

##Elevation
# Kano_shpefile_sp <- as(Kano_shpefile_cd, "Spatial")

elevation <- raster::raster(file.path(RastersDir, "Elevation", "ELE.tif"))

Kano_ele <- raster::extract(elevation, buffer = 100, Kano_shpefile_sp, fun = mean, df = TRUE)

Kano_shpefile_cd$elevation_100m <- Kano_ele$ELE


# write.csv(Kano_shpefile_cd, "C:/Users/laure/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/analysis_docs/kano_environmental_data_predictions.csv")


#relative humidity

list_RH <- list(rh_2023 <- brick(file.path(RastersDir, "temp_humidity_2023_grib","relative_humidity_2023.grib")),
                rh_2024 <- brick(file.path(RastersDir, "temp_humidity_2023_grib","relative_humidity_2024.grib"))
)

names(list_RH) <- c("2023", "2024")

for (i in 1:length(list_RH)) {
  num_layers <- nlayers(list_RH[[i]])
  layer_names <- paste0("RH_", seq_len(num_layers))
  names(list_RH[[i]]) <- layer_names
}


nlayers(list_RH[[2]])


Kano_list_RH <- list() 

for (i in 1:length(list_RH)) {
  Kano_RH_data <- raster::extract(list_RH[[i]], Kano_shpefile_sp, fun = mean, df = TRUE)
  df_RH <- as.data.frame(Kano_RH_data)
  df_RH$Year <- names(list_RH)[i]
  Kano_list_RH[[i]] <- df_RH
}

Kano_rh2 <- bind_rows(Kano_list_RH)

rh_Kano <- Kano_rh2 %>%
  group_by(ID) %>%
  summarize(RH_mean = mean(c_across(starts_with("RH_")), na.rm = TRUE))

Kano_shpefile_cd$RH_mean <- rh_Kano$RH_mean

#temperature
list_temp <- list(temp_2023 <- brick(file.path(RastersDir, "temp_humidity_2023_grib", "temperature_2023.grib")),
                  temp_2024 <- brick(file.path(RastersDir, "temp_humidity_2023_grib","temperature_2024.grib"))
)

names(list_temp) <- c("2023", "2024")

for (i in 1:length(list_temp)) {
  num_layers <- nlayers(list_temp[[i]])
  layer_names <- paste0("temp_", seq_len(num_layers))
  names(list_temp[[i]]) <- layer_names
}


nlayers(list_temp[[2]])


#extract for temperature for Kano

Kano_list_temp <- list() 

for (i in 1:length(list_temp)) {
  Kano_temp_data <- raster::extract(list_temp[[i]], Kano_shpefile_sp, fun = mean, df = TRUE)
  df_temp <- as.data.frame(Kano_temp_data)
  df_temp$Year <- names(list_temp)[i]
  Kano_list_temp[[i]] <- df_temp
}

Kano_temp2 <- bind_rows(Kano_list_temp)

temp_Kano <- Kano_temp2 %>%
  group_by(ID) %>%
  summarize(temp_mean = mean(c_across(starts_with("temp_")), na.rm = TRUE))

Kano_shpefile_cd$temp_mean <- temp_Kano$temp_mean

  

# NLT

ntl_rasters <- file.path(RastersDir, "night_time_light_2023")
ntl_files <- list.files(file.path(ntl_rasters), 
                        pattern = ".tif", 
                        full.names = TRUE)

ntl_data <- lapply(seq_along(ntl_files), 
                   function(x) raster::raster(ntl_files[[x]]))

points_ntl <- ntl_data %>%
  purrr::map(~raster::extract(.x, Kano_shpefile_cd, fun = mean, df = TRUE)) %>% 
  purrr::reduce(function(x, y) merge(x, y, by = "ID"))

Kano_shpefile_cd$mean_NTL <- rowMeans(points_ntl[, -1], na.rm = TRUE)



# 10. Extract NDWI 2023
NDWI_2023 <- brick(file.path(RastersDir, "global_surface_water", "Nigeria_NDWI_2023.tif"))
layer_names <- paste0("ndwi_", seq_len(nlayers(NDWI_2023)))
names(NDWI_2023) <- layer_names


points_ndwi_data <- extract(NDWI_2023, Kano_shpefile_cd, df = TRUE)

Kano_shpefile_cd$ndwi_mean <- points_ndwi_data[,2]



# 11. Extract NDMI 2023
NDMI_2023 <- brick(file.path(RastersDir, "global_surface_water", "NDMI_Nigeria_2023.tif"))
layer_names <- paste0("ndmi_", seq_len(nlayers(NDMI_2023)))
names(NDMI_2023) <- layer_names


points_ndmi_data <- extract(NDMI_2023, Kano_shpefile_cd, df = TRUE)

Kano_shpefile_cd$ndmi_mean <- points_ndmi_data[,2]


Kano_shpefile_cd$longitude = st_coordinates(Kano_shpefile_cd)[ ,1]
Kano_shpefile_cd$latitude =   st_coordinates(Kano_shpefile_cd)[ ,2]


Kano_shpefile_cd <- Kano_shpefile_cd %>% 
  st_drop_geometry() 

# write.csv(Kano_variables, file.path(x, "Kano_variables.csv"))

write.csv(Kano_shpefile_cd, "C:/Users/laure/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/analysis_docs/kano_environmental_data_predictions.csv")




Kano_shpefile_cd <- Kano_shpefile_cd %>% 
  st_drop_geometry() 

# write.csv(Kano_variables, file.path(x, "Kano_variables.csv"))

write.csv(Kano_shpefile_cd, "C:/Users/laure/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/analysis_docs/kano_environmental_data_predictions.csv")


