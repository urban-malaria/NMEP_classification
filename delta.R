###### DELTA STATE VARIABLES EXTRACTION 

#extract rainfall, EVI, NDVI, distance to water bodies, relative humidity, temperature,
# pf parasite rate, #housing quality

source("~/NMEP_classification/load_path.R", echo = T)

delta_shp <- st_read(file.path(ShpfilesDir, "Delta", "Delta_Wards.shp"))
delta_shp <- st_make_valid(st_read(file.path(ShpfilesDir, "Delta", "Delta_Wards.shp")))

empty_geometries <- st_is_empty(delta_shp)
delta_shp <- delta_shp[!empty_geometries, ]

######
rainfall_rasters <-file.path(RastersDir, "monthly rainfall 2023-24")
rainfall <- list.files(file.path(rainfall_rasters), 
                       pattern = ".tif", full.names = TRUE)

rainfall_data <- lapply(seq_along(rainfall), 
                        function(x) raster::raster(rainfall[[x]]))

delta_rainfall <- rainfall_data %>%
  purrr::map(~raster::extract(.x, delta_shp, fun = mean, df = TRUE)) %>%
  purrr::reduce(function(x, y) merge(x, y, by = "ID"))

delta_rainfall$avgRAIN <- rowMeans(delta_rainfall[ , -1])
delta_shp$mean_rainfall <- delta_rainfall$avgRAIN


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

#
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

saveRDS(delta_shp, "delta.RDS")
delta_shp <- readRDS(file.path("~/delta.RDS"))

#
delta_shp_sp <- as(delta_shp, "Spatial")

h2o_distance <- raster(file.path(RastersDir, "distance_to_water_bodies", "distance_to_water.tif"))

delta_distance <- extract(h2o_distance, delta_shp_sp, fun = mean, df = TRUE)
delta_shp$distance_to_water <- delta_distance$distance_to_water



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



#pf parasite rate


pfpr_2022 <- raster(file.path(PfDir, "pf_parasite_rate",
                              "202406_Global_Pf_Parasite_Rate_NGA_2022.tiff"))

delta_pfpr<- extract(pfpr_2022, delta_shp_sp, fun = mean, df = TRUE)
delta_shp$pfpr <- delta_pfpr$X202406_Global_Pf_Parasite_Rate_NGA_2022_1



#housing quality

housing_quality <- raster(file.path(RastersDir, "housing",
                                    "2019_Nature_Africa_Housing_2015_NGA.tiff"))

delta_housing<- extract(housing_quality, delta_shp_sp, fun = mean, df = TRUE)
delta_shp$housing_quality <- delta_housing$X2019_Nature_Africa_Housing_2015_NGA


## write to csv.

delta_variables <- delta_shp %>%
  dplyr::select(WardName, Urban,
         mean_rainfall, mean_EVI, mean_NDVI, distance_to_water,
         RH_mean, temp_mean, pfpr, housing_quality) %>% 
  st_drop_geometry()


write.csv(delta_variables, file.path(ShpfilesDir, "delta_variables.csv"))


##
delta_wards <- delta_shp %>% 
  st_drop_geometry() %>% 
  dplyr::select(StateCode, WardCode, WardName, LGACode, Urban)

write.csv(delta_wards, file.path(OutputsDir, "delta_wards.csv"))


##include urban classification

delta_urban <- st_read(file.path(ShpfilesDir, "Delta_urban_percentage.geojson"))

delta_urban <- delta_urban %>% 
  st_drop_geometry() %>% 
  dplyr::select(-id, -Timestamp) %>% 
  mutate(urban_gee = if_else(urbanPercentage >= 30, 0, 1))
  

delta_wards <- read.csv(file.path(OutputsDir, "delta_wards.csv"))

delta_wards <- delta_wards %>%
  left_join(delta_urban %>%
              dplyr::select(WardCode, WardName, totalArea, urbanArea, urbanPercentage,
                            urban_gee, ), by = c("WardCode", "WardName"))

write.csv(delta_wards, file.path(OutputsDir, "delta_wards.csv"))

##UNDER 5 TPR 
 ##Included on another script

delta_wards <- read.csv(file.path(OutputsDir, "delta_wards.csv"))


delta_variables <- read.csv(file.path(ShpfilesDir, "delta_variables.csv"))

delta_variables <- delta_variables %>% 
  left_join(delta_wards %>% 
              dplyr::select(WardName, totalArea, urbanArea, urbanPercentage, urban_gee,
                            u5_tpr), by = c("WardName"))

write.csv(delta_variables, file.path(ShpfilesDir, "delta_variables.csv"))


########################PLOTS##############################################

delta_variables <- read.csv(file.path(ShpfilesDir, "delta_variables.csv"))



delta_variables <- delta_variables %>% 
  mutate(Class = ifelse(urban_gee == 0, "Urban",
                        ifelse(urban_gee == 1, "Rural", NA)))

p1 <- ggplot(delta_variables, aes(x = u5_tpr)) +
  geom_histogram(binwidth = 0.05, alpha = 0.7) + 
  facet_wrap(~Class) + 
  theme_manuscript() +
  labs(title = "U5 TPR in Delta Wards",
       x = "u5 TPR",
       y = "Frequency")

p2 <- ggplot(delta_variables, aes(x = u5_tpr, fill = Class)) +
  geom_density(alpha = 0.5) + 
  theme_manuscript() +
  labs(title = "Density Plot of U5 TPR, Delta Wards",
       x = "U5 TPR",
       y = "Density",
       fill = "Ward Classification")

grid.arrange(p1, p2)


na_tpr_urban_count <- delta_variables %>% 
  filter(is.na(u5_tpr) & urban_gee == 0) %>% 
  summarise(count = n())