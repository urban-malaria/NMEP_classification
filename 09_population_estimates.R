### Population Estimate Analysis

getwd()
source("~/NMEP_classification/load_path.R", echo = T)


warri_shp <- st_read(file.path(ShpfilesDir, "Warri", "Warri.shp"))
warri_shp <- st_transform(warri_shp, crs = 4326)

asaba_shp <- st_read(file.path(ShpfilesDir, "Asaba", "Asaba.shp"))
asaba_shp <- st_transform(asaba_shp, crs = 4326)

# population <- raster(file.path(RastersDir, "NGA_population_v2_1_gridded", 
#                                "NGA_population_v2_1_gridded.tif"))
# 
# 
# warri_pop <- raster::extract(population, warri_shp, fun = mean, df = TRUE)
# 
# asaba_pop <- raster::extract(population, asaba_shp, fun = mean, df = TRUE)
# 
# 
# # ibadan_shp <- st_read(file.path(ShpfilesDir, "Ibadan", "Ibadan.shp"))
# # ibadan_shp <- st_transform(ibadan_shp, crs = 4326)
# # ibadan_pop <- raster::extract(population, ibadan_shp, fun = mean, df = TRUE)
# 

population1 <- raster(file.path(RastersDir, "NGA_population_v2_0_gridded",
                                "NGA_population_v2_0_gridded.tif"))

warri_pop1 <- raster::extract(population1, warri_shp, fun = mean, df = TRUE)

warri_pop2 <- raster::extract(population1, warri_shp, fun = sum, na.rm = F, df = T)


asaba_pop1 <- raster::extract(population1, asaba_shp, fun = mean, df = TRUE)

asaba_pop2 <- raster::extract(population1, asaba_shp, fun = sum, na.rm = F, df = T)



### Attempt using settlement classification data

settlement_blocks <- st_read(file.path(RastersDir, "nigeria_settlement_classification", "blocks_V1.1",
                                       "Nigeria_Blocks_V1.shp")) %>% 
  filter(state == 'Delta', landuse =='Residential')

settlement_blocks <- st_transform(settlement_blocks, crs = 4326)
st_is_valid(settlement_blocks)
settlement_blocks <- st_make_valid(settlement_blocks)

ggplot()+
  geom_sf(data = settlement_blocks, aes(geometry = geometry))

##extract settlement count for residential areas in Warri

# warri_settlement <- st_join(warri_shp, settlement_blocks,
#                              join = sf::st_overlaps)
# ggplot()+
# geom_sf(data = warri_settlement, aes(geometry = geometry))


warri_footprints <- read.csv(file.path(FootprintsDir, "Warri.csv"))
warri_footprints <- st_as_sf(warri_footprints, wkt = "geometry")
st_crs(warri_footprints) <- 4326

ggplot()+
  geom_sf(data = warri_footprints, aes(geometry = geometry))
  

# warri_settlement_stats <- calculate_footstats(warri_footprints, zone = warri_settlement,
#                                            what = "settled",
#                                            how = list("binary","count"),
#                                            controlZone = list(zoneName = "WardName", method = "centroid"),
#                                            verbose = F)
# 
# 
# warri_stats <- calculate_footstats(warri_footprints, zone = warri_shp,
#                                        what = "settled",
#                                        how = list("binary","count"),
#                                        controlZone = list(zoneName = "WardName", method = "centroid"),
#                                        verbose = F)
# 
# warri_count <- left_join(warri_settlement_stats, warri_stats, by = "WardName")
# warri_count <- warri_count %>%
#   rename("Resd_areas" = settled_count.x,
#          "All" = settled_count.y)
# 
# warri_count_long <- pivot_longer(warri_count, 
#                           cols = c(Resd_areas, All),
#                           names_to = "Source", 
#                           values_to = "settled_count")
# 
# ggplot(warri_count_long, aes(x = WardName, y = settled_count, fill = Source)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   labs(title = " Number of Buildings in Residential Areas vs All Areas in Warri 1",
#        x = "Ward Name", y = "Count", fill = "Area") +
#   map_theme() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))



### again

warri_settlement2 <- st_intersection(settlement_blocks, warri_shp)

ggplot()+
  geom_sf(data = warri_settlement2, aes(geometry = geometry))


all_warri_stats <- calculate_footstats(warri_footprints, zone = warri_shp,
                                       what = "settled",
                                       how = list("binary","count"),
                                       controlZone = list(zoneName = "WardName", method = "centroid"),
                                       verbose = F)


warri_settlement_stats2 <- calculate_footstats(warri_footprints, zone = warri_settlement2,
                                               what = "settled",
                                               how = list("binary","count"),
                                               controlZone = list(zoneName = "WardName", method = "centroid"),
                                               verbose = F)

warri_count2 <- left_join(warri_settlement_stats2, all_warri_stats, by = "WardName")
warri_count2 <- warri_count2 %>%
  rename("Resd_areas" = settled_count.x,
         "All" = settled_count.y)

warri_count_long2 <- pivot_longer(warri_count2, 
                                 cols = c(Resd_areas, All),
                                 names_to = "Source", 
                                 values_to = "settled_count")

ggplot(warri_count_long2, aes(x = WardName, y = settled_count, fill = Source)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = " Number of Buildings in Residential Areas vs All Areas in Warri2",
       x = "Ward Name", y = "Count", fill = "Area") +
  map_theme() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


missing_rows <- anti_join(warri_shp, warri_count2, by = "WardName")
print(missing_rows)


##

ggplot()+
  geom_sf(data = warri_footprints, aes(geometry = geometry))+
  labs(title = "Building footprints in all of Warri")+
  map_theme()


residential_footprints <- st_intersection(warri_footprints, warri_settlement2)
ggplot()+
  geom_sf(data = residential_footprints, aes(geometry = geometry))+
  labs(title = "Building footprint polygons in residential areas in Warri")














# geom_text_repel(data = warri_count_long %>% 
#                   dplyr::filter(Source == "All"), aes(label = settled_count), 
#                 position = position_dodge(width = 0.9), 
#                 vjust = -0.5,   
#                 size = 4) + 

