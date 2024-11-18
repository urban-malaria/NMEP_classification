##Extract environmental variables for Warri and Asaba

rm(list = ls())

source("~/NMEP_classification/load_path.R", echo = T)

## Load shapefiles
warri_shp <- st_read(file.path(ShpfilesDir, "Warri", "Warri.shp"))
asaba_shp <- st_read(file.path(ShpfilesDir, "Asaba", "Asaba.shp"))


# Rainfall
rainfall_rasters <-file.path(RastersDir, "monthly rainfall 2023-24")

rainfall <- list.files(file.path(rainfall_rasters), 
                       pattern = ".tif", full.names = TRUE)

rainfall_data <- lapply(seq_along(rainfall), 
                       function(x) raster::raster(rainfall[[x]]))


#Extract rainfall for warri

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

evi_rasters <-file.path(RastersDir, "Updated_Covariates", "2023-24_EVI_MOD13A1")

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

ndvi_rasters <-file.path(RastersDir, "Updated_Covariates", "2023-24_NDVI_MOD13A1")

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

relative_humidity <- raster(file.path(RastersDir, "relative_humidity_2024.grib"))

warri_rh <- extract(relative_humidity, warri_shp, fun = mean, df = TRUE)
colnames(warri_rh)[2] <- "relative_humidity"
warri_shp$relative_humidity <- warri_rh$relative_humidity


asaba_rh <- extract(relative_humidity, asaba_shp, fun = mean, df = TRUE)
colnames(asaba_rh)[2] <- "relative_humidity"
asaba_shp$relative_humidity <- asaba_rh$relative_humidity


#### temperature
temperature <- raster(file.path(RastersDir, "temperature_2024.grib"))

warri_temp <- extract(temperature, warri_shp, fun = mean, df = TRUE)
colnames(warri_temp)[2] <- "temperature"
warri_shp$temp <- warri_temp$temperature


asaba_temp <- extract(temperature, asaba_shp, fun = mean, df = TRUE)
colnames(asaba_temp)[2] <- "temperature"
asaba_shp$temp <- asaba_temp$temperature


### distance to h2o bodies

h2o_distance <- raster(file.path(RastersDir, "distance_to_water_bodies", "distance_to_water.tif"))

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

write.csv(warri_variables, file.path(ShpfilesDir, "warri_variables.csv"))


asaba_variables <- asaba_shp %>%
  select(WardName, Urban,
         mean_rainfall, mean_EVI, mean_NDVI,
         relative_humidity, temp, distance_to_water)

saveRDS(asaba_variables, "asaba_var.RDS")

write.csv(asaba_variables, file.path(ShpfilesDir, "asaba_variables.csv"))


#read summarized variables
warri_variables <- readRDS(file.path("~/warri_var.RDS"))
asaba_variables <- readRDS(file.path("~/asaba_var.RDS"))


### Relative humidity 2

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

#extract for relative humidity for warri

warri_list_RH <- list() 

for (i in 1:length(list_RH)) {
  warri_RH_data <- extract(list_RH[[i]], warri_shp, fun = mean, df = TRUE)
  df_RH <- as.data.frame(warri_RH_data)
  df_RH$Year <- names(list_RH)[i]
  warri_list_RH[[i]] <- df_RH
}

warri_rh2 <- bind_rows(warri_list_RH)

rh_warri <- warri_rh2 %>%
  group_by(ID) %>%
  summarize(RH_mean = mean(c_across(starts_with("RH_")), na.rm = TRUE))

warri_variables$RH_mean <- rh_warri$RH_mean


#extract for relative humidity for asaba

asaba_list_RH <- list()

for (i in 1:length(list_RH)) {
  asaba_RH_data <- extract(list_RH[[i]], asaba_shp, fun = mean, df = TRUE)
  df_RH <- as.data.frame(asaba_RH_data)
  df_RH$Year <- names(list_RH)[i]
  asaba_list_RH[[i]] <- df_RH
}

asaba_rh2 <- bind_rows(asaba_list_RH)

rh_asaba <- asaba_rh2 %>%
  group_by(ID) %>%
  summarize(RH_mean = mean(c_across(starts_with("RH_")), na.rm = TRUE))

asaba_variables$RH_mean <- rh_asaba$RH_mean


#Temperature 2

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

#extract for temperature for warri

warri_list_temp <- list() 

for (i in 1:length(list_temp)) {
  warri_temp_data <- extract(list_temp[[i]], warri_shp, fun = mean, df = TRUE)
  df_temp <- as.data.frame(warri_temp_data)
  df_temp$Year <- names(list_temp)[i]
  warri_list_temp[[i]] <- df_temp
}

warri_temp2 <- bind_rows(warri_list_temp)

temp_warri <- warri_temp2 %>%
  group_by(ID) %>%
  summarize(temp_mean = mean(c_across(starts_with("temp_")), na.rm = TRUE))

warri_variables$temp_mean <- temp_warri$temp_mean


#extract for temperature for asaba

asaba_list_temp <- list()

for (i in 1:length(list_temp)) {
  asaba_temp_data <- extract(list_temp[[i]], asaba_shp, fun = mean, df = TRUE)
  df_temp <- as.data.frame(asaba_temp_data)
  df_temp$Year <- names(list_temp)[i]
  asaba_list_temp[[i]] <- df_temp
}

asaba_temp2 <- bind_rows(asaba_list_temp)

temp_asaba <- asaba_temp2 %>%
  group_by(ID) %>%
  summarize(temp_mean = mean(c_across(starts_with("temp_")), na.rm = TRUE))

asaba_variables$temp_mean <- temp_asaba$temp_mean



### Plot variables

#WARRI
warri_evi <-
  ggplot()+
  geom_sf(data = warri_variables, aes(geometry = geometry, fill = mean_EVI))+
  scale_fill_continuous(low = "lightyellow", high = "darkgreen", na.value = "grey")+
  #geom_text_repel(data = umuagu_plot %>% 
   #                 filter(FID == "0"), aes(geometry = geometry, label = FID),
    #              stat = "sf_coordinates", size = 3, min.segment.length = 0) +
  labs(title = "2023- 2024 Mean EVI",
       x = "",
       y = "",
       fill = "EVI" )+
  map_theme()

warri_ndvi <-
  ggplot()+
  geom_sf(data = warri_variables, aes(geometry = geometry, fill = mean_NDVI))+
  scale_fill_continuous(low = "lightyellow", high = "darkgreen", na.value = "grey")+
  labs(title = "2023- 2024 Mean NDVI",
       x = "",
       y = "",
       fill = "NDVI" )+
  map_theme()

warri_rain <-
  ggplot()+
  geom_sf(data = warri_variables, aes(geometry = geometry, fill = mean_rainfall))+
  scale_fill_continuous(low = "lightblue", high = "darkblue", na.value = "grey")+
  labs(title = "2023- 2024 Mean Rainfall",
       x = "",
       y = "",
       fill = "Rainfall (mm)" )+
  map_theme()

warri_humidity <-
  ggplot()+
  geom_sf(data = warri_variables, aes(geometry = geometry, fill = RH_mean))+
  scale_fill_continuous(low = "lightyellow", high = "brown", na.value = "grey")+
  labs(title = "2023- 2024 Relative Humidity",
       x = "",
       y = "",
       fill = "Relative Humidity %" )+
  map_theme()


warri_h2o <-
  ggplot()+
  geom_sf(data = warri_variables, aes(geometry = geometry, fill = distance_to_water))+
  scale_fill_continuous(low = "lightyellow", high = "purple", na.value = "grey")+
  labs(title = "Distance to Water bodies",
       x = "",
       y = "",
       fill = "" )+
  map_theme()

warri_temp <-
  ggplot()+
  geom_sf(data = warri_variables, aes(geometry = geometry, fill = temp_mean))+
  scale_fill_continuous(low = "lightyellow", high = "maroon", na.value = "grey")+
  labs(title = "Mean Temperature",
       x = "",
       y = "",
       fill = "Temperature °C" )+
  map_theme()

grid.arrange(warri_evi, warri_ndvi, warri_rain, warri_humidity, warri_h2o, warri_temp, ncol = 2)


#ASABA

asaba_evi <-
  ggplot()+
  geom_sf(data = asaba_variables, aes(geometry = geometry, fill = mean_EVI))+
  scale_fill_continuous(low = "lightyellow", high = "darkgreen", na.value = "grey")+
  #geom_text_repel(data = umuagu_plot %>% 
  #                 filter(FID == "0"), aes(geometry = geometry, label = FID),
  #              stat = "sf_coordinates", size = 3, min.segment.length = 0) +
  labs(title = "2023- 2024 Mean EVI",
       x = "",
       y = "",
       fill = "EVI" )+
  map_theme()

asaba_ndvi <-
  ggplot()+
  geom_sf(data = asaba_variables, aes(geometry = geometry, fill = mean_NDVI))+
  scale_fill_continuous(low = "lightyellow", high = "darkgreen", na.value = "grey")+
  labs(title = "2023- 2024 Mean NDVI",
       x = "",
       y = "",
       fill = "NDVI" )+
  map_theme()

asaba_rain <-
  ggplot()+
  geom_sf(data = asaba_variables, aes(geometry = geometry, fill = mean_rainfall))+
  scale_fill_continuous(low = "lightblue", high = "darkblue", na.value = "grey")+
  labs(title = "2023- 2024 Mean Rainfall",
       x = "",
       y = "",
       fill = "Rainfall (mm)" )+
  map_theme()

asaba_humidity <-
  ggplot()+
  geom_sf(data = asaba_variables, aes(geometry = geometry, fill = RH_mean))+
  scale_fill_continuous(low = "lightyellow", high = "brown", na.value = "grey")+
  labs(title = "2023- 2024 Relative Humidity",
       x = "",
       y = "",
       fill = "Relative Humidity %" )+
  map_theme()


asaba_h2o <-
  ggplot()+
  geom_sf(data = asaba_variables, aes(geometry = geometry, fill = distance_to_water))+
  scale_fill_continuous(low = "lightyellow", high = "purple", na.value = "grey")+
  labs(title = "Distance to Water bodies",
       x = "",
       y = "",
       fill = "" )+
  map_theme()


asaba_temp <-
  ggplot()+
  geom_sf(data = asaba_variables, aes(geometry = geometry, fill = temp_mean))+
  scale_fill_continuous(low = "lightyellow", high = "maroon", na.value = "grey")+
  labs(title = "Mean Temperature",
       x = "",
       y = "",
       fill = "Temperature °C" )+
  map_theme()

grid.arrange(asaba_evi, asaba_ndvi, asaba_rain, asaba_humidity, asaba_h2o, asaba_temp, ncol = 3)



warri_variables2 <- warri_variables %>% 
  st_drop_geometry()
write.csv(warri_variables2, file.path(ShpfilesDir, "warri_variables.csv"))


asaba_variables2 <- asaba_variables %>% 
  st_drop_geometry()
write.csv(asaba_variables2, file.path(ShpfilesDir, "asaba_variables.csv"))


# Extract pf parasite rate

pfpr_2022 <- raster(file.path(PfDir, "pf_parasite_rate",
                          "202406_Global_Pf_Parasite_Rate_NGA_2022.tiff"))

warri_pfpr<- extract(pfpr_2022, warri_shp, fun = mean, df = TRUE)
warri_variables$pfpr <- warri_pfpr$X202406_Global_Pf_Parasite_Rate_NGA_2022_1


asaba_pfpr<- extract(pfpr_2022, asaba_shp, fun = mean, df = TRUE)
asaba_variables$pfpr <- asaba_pfpr$X202406_Global_Pf_Parasite_Rate_NGA_2022_1

#Include TPR values

warri_tpr <- read.csv(file.path(TPRDir, "warri TPR.csv"))
warri_variables <- left_join(
  warri_variables, 
  warri_tpr %>% select(WardName, tpr_u5, total_tpr), 
  by = "WardName"
)

asaba_tpr <- read.csv(file.path(TPRDir, "Asaba TPR.csv"))
asaba_variables <- left_join(
  asaba_variables,
  asaba_tpr %>% select(WardName, tpr_u5, total_tpr),
  by = "WardName"
)


#housing quality
housing_quality <- raster(file.path(RastersDir, "housing",
                              "2019_Nature_Africa_Housing_2015_NGA.tiff"))

warri_housing<- extract(housing_quality, warri_shp, fun = mean, df = TRUE)
warri_variables$housing_quality <- warri_housing$X2019_Nature_Africa_Housing_2015_NGA

asaba_housing<- extract(housing_quality, asaba_shp, fun = mean, df = TRUE)
asaba_variables$housing_quality <- asaba_housing$X2019_Nature_Africa_Housing_2015_NGA


################################################################################
##################Settlement Type Analysis######################################
################################################################################

#settlment_type <- raster(file.path(RastersDir, "nigeria_settlements_Worldpop",
                                   "NGA_settlement_sample.tif"))
#warri_settlement<- extract(settlment_type, warri_shp, fun = mean, df = TRUE)


settlement_blocks <- st_read(file.path(RastersDir, "nigeria_settlement_classification", "blocks_V1.1",
                                       "Nigeria_Blocks_V1.shp")) %>% 
  filter(state == 'Delta', landuse =='Residential')


st_crs(warri_shp)

st_crs(settlement_blocks)

warri_shp <- st_transform(warri_shp, crs = st_crs(settlement_blocks))

warri_settlement <- st_join(warri_shp, settlement_blocks, 
                            join = sf::st_overlaps)

ko_bar_dat <- warri_settlement
sf::st_geometry(ko_bar_dat) <- NULL

ko_bar_overall = ko_bar_dat %>% 
  dplyr::select(type) %>%  
  group_by(type) %>%  
  summarise(number = n())

settlement_type_warri = warri_settlement %>% 
  dplyr::select(WardName, settle_type = type) %>% 
  group_by(WardName, settle_type) %>% 
  summarise(number = n())

ggplot()+
  geom_sf(data = warri_shp)+
  geom_sf(data = settlement_type_warri, aes(geometry = geometry, fill = number))+
  scale_fill_continuous(low = "lightyellow", high = "brown", na.value = "grey")+
  facet_wrap(~settle_type)+
  labs(title = "Settlement Types in Warri")+
  map_theme()

sf::st_geometry(settlement_type_warri) <- NULL 

warri_settlement_types <- settlement_type_warri %>% 
  mutate(number = ifelse(is.na(number), 0,  number)) %>% 
  pivot_wider(names_from = settle_type, values_from = number)

plotting <- warri_shp %>% 
  inner_join(settlement_type_warri) %>% 
  group_by(WardName) %>% 
  mutate(total_settlement = sum(number),
         proportion_settlement_type = number / total_settlement) %>% 
  ungroup() %>% 
  mutate(class_stmnt_number = cut(number, c(0,2,4,6,8,10,15,20,30,50, 78), include.lowest = T),
         class_stmnt_proportion = cut(proportion_settlement_type, seq(0,1,0.2)))

plotting_v2 = plotting %>% 
  mutate(grp = ifelse(settle_type == "A" | settle_type == "B" | settle_type == "M", "Poor", "Good")) %>%
  group_by(WardName, grp, total_settlement) %>% 
  summarise(grp_number = sum(number)) %>% 
  mutate(proportion_settlement_type_grp = grp_number / total_settlement,
         grp_proportion = cut(proportion_settlement_type_grp, seq(0, 1, 0.2), include.lowest = T))

plotting_00 <- plotting_v2 %>% 
  filter(grp == "Poor") %>% 
  select(WardName, settlement_type = proportion_settlement_type_grp) %>% 
  st_drop_geometry()

palettes <- list((RColorBrewer::brewer.pal(10, "RdYlBu")))


warri_variables <- left_join(warri_variables, plotting_00, by = "WardName")

ggplot()+
  geom_sf(data = warri_variables, aes(geometry = geometry, fill = settlement_type))+
  scale_fill_continuous(low = "lightyellow", high = "brown", na.value = "grey")+
  labs(title = "Settlement Type in Warri",
       caption = "Proportion of settlements that are classified as 'Poor', Ugborodo has no data")+
  map_theme()


#Asaba

asaba_settlement <- st_join(asaba_shp, settlement_blocks, 
                            join = sf::st_overlaps)

settlement_type_asaba = asaba_settlement %>% 
  dplyr::select(WardName, settle_type = type) %>% 
  group_by(WardName, settle_type) %>% 
  summarise(number = n())

ggplot()+
  geom_sf(data = asaba_shp)+
  geom_sf(data = settlement_type_asaba, aes(geometry = geometry, fill = number))+
  scale_fill_continuous(low = "lightyellow", high = "brown", na.value = "grey")+
  facet_wrap(~settle_type)+
  labs(title = "Settlement Types in Asaba",
       caption = "Types A, B and M are classified as 'Poor' settlements")+
  map_theme()


sf::st_geometry(settlement_type_asaba) <- NULL 

asaba_settlement_types <- settlement_type_asaba %>% 
  mutate(number = ifelse(is.na(number), 0,  number)) %>% 
  pivot_wider(names_from = settle_type, values_from = number)

plotting_asaba <- asaba_shp %>% 
  inner_join(settlement_type_asaba) %>% 
  group_by(WardName) %>% 
  mutate(total_settlement = sum(number),
         proportion_settlement_type = number / total_settlement) %>% 
  ungroup() %>% 
  mutate(class_stmnt_number = cut(number, c(0,2,4,6,8,10,15,20,30,50, 78), include.lowest = T),
         class_stmnt_proportion = cut(proportion_settlement_type, seq(0,1,0.2)))

plotting_v2_asaba = plotting_asaba %>% 
  mutate(grp = ifelse(settle_type == "A" | settle_type == "B" | settle_type == "M", "Poor", "Good")) %>%
  group_by(WardName, grp, total_settlement) %>% 
  summarise(grp_number = sum(number)) %>% 
  mutate(proportion_settlement_type_grp = grp_number / total_settlement,
         grp_proportion = cut(proportion_settlement_type_grp, seq(0, 1, 0.2), include.lowest = T))

plotting_00_asaba <- plotting_v2_asaba %>% 
  filter(grp == "Poor") %>% 
  select(WardName, settlement_type = proportion_settlement_type_grp) %>% 
  st_drop_geometry()


asaba_variables <- left_join(asaba_variables, plotting_00_asaba, by = "WardName")

ggplot()+
  geom_sf(data = asaba_variables, aes(geometry = geometry, fill = settlement_type))+
  scale_fill_continuous(low = "lightyellow", high = "brown", na.value = "grey")+
  labs(title = "Settlement Type in Asaba",
       caption = "Proportion of settlements that are classified as 'Poor'")+
  map_theme()





