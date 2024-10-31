## building heights

getwd()
source("~/NMEP_classification/load_path.R", echo = T)


install.packages("exactextractr")
library(exactextractr)


#load settlement blocks
settlement_blocks <- st_read(file.path(RastersDir, "nigeria_settlement_classification", "blocks_V1.1",
                                       "Nigeria_Blocks_V1.shp")) %>% 
  filter(state == 'Delta', landuse =='Residential')

settlement_blocks <- st_transform(settlement_blocks, crs = 4326)
st_is_valid(settlement_blocks)
settlement_blocks <- st_make_valid(settlement_blocks)


#### ASABA
#load Asaba shape file and building footprints

asaba_shp <- st_read(file.path(ShpfilesDir, "Asaba", "Asaba.shp"))
asaba_shp <- st_transform(asaba_shp, crs = 4326)

asaba_geojson <- st_read(file.path(MSGlobalDir, "MS_footprints_Asaba.geojson")) %>% 
  st_as_sf()

asaba_residential_buildings <- st_join(asaba_geojson, settlement_blocks, join = st_within) %>% 
  filter(landuse == "Residential") %>% 
  filter(st_geometry_type(.) == "POLYGON")

ggplot()+
  geom_sf(data = asaba_residential_buildings, aes(geometry = geometry))


#load building height raster
asaba_height <- raster(file.path(RastersDir, "GoogleBuildings2_5/Asaba/Asaba_height.tif"))

#extract mean building height in Asaba wards
asaba_mean_building_height <- raster::extract(asaba_height, asaba_shp, fun = mean, df = T)

#Extract individual building heights
asaba_building_height_mean <- exactextractr::exact_extract(asaba_height, asaba_residential_buildings)
asaba_building_height_mean[[30000]]

#calculate mean
mean_heights <- lapply(asaba_building_height_mean, function(df) {
  mean(df$value, na.rm = TRUE)  
})

mean_heights <- data.frame(mean_height = unlist(mean_heights))

asaba_residential_buildings$building_height <- mean_heights$mean_height
 print(max(asaba_residential_buildings$building_height, na.rm = TRUE))

hist(asaba_residential_buildings$building_height)

asaba_height_summary <- asaba_residential_buildings %>%
  dplyr::select(id, FID, WardName, building_height) %>% 
  st_drop_geometry()

write.csv(asaba_height_summary, file.path(RastersDir, "GoogleBuildings2_5/Asaba/asaba_buildings.csv"))


#### WARRI

#load Warri shape file and building footprints

warri_shp <- st_read(file.path(ShpfilesDir, "Warri", "Warri.shp"))
warri_shp <- st_transform(warri_shp, crs = 4326)

warri_geojson <- st_read(file.path(MSGlobalDir, "warri_footprints.geojson")) %>% 
  st_as_sf()

warri_building_wards <- st_join(warri_geojson, warri_shp, join = st_within)

warri_residential_buildings <- st_join(warri_building_wards, settlement_blocks, join = st_within) %>% 
  filter(landuse == "Residential") %>% 
  filter(st_geometry_type(.) == "POLYGON")

ggplot()+
  geom_sf(data = warri_residential_buildings, aes(geometry = geometry))


#Extract from raster
warri_height <- raster(file.path(RastersDir, "GoogleBuildings2_5/Warri/Warri_height.tif"))

warri_mean_building_height <- raster::extract(warri_height, warri_shp, fun = mean, df = T)

warri_building_height_mean <- exactextractr::exact_extract(warri_height, warri_residential_buildings)

warri_mean_heights <- lapply(warri_building_height_mean, function(df) {
  mean(df$value, na.rm = TRUE)  
})

warri_mean_heights <- data.frame(mean_height = unlist(warri_mean_heights))

warri_residential_buildings$building_height <- warri_mean_heights$mean_height

hist(warri_residential_buildings$building_height)

warri_height_summary <- warri_residential_buildings %>%
  dplyr::select(id, FID, WardName, building_height) %>% 
  st_drop_geometry()

write.csv(warri_height_summary, file.path(RastersDir, "GoogleBuildings2_5/Warri/warri_buildings.csv"))



