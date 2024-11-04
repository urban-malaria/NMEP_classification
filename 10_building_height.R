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

###############################################################################
##########POPULATION ESTIMATES WITH BUILDING HEIGHTS ##########################
###############################################################################

#### ASABA###
asaba_heights <- read.csv(file.path(RastersDir, "GoogleBuildings2_5/Asaba/asaba_buildings.csv"))

#convert height to likely household number per building
asaba_storey_count <- asaba_heights %>%
  filter(building_height >= 1.8) %>%  #remove uninhabitable buildings less than one storey (8015)
  filter(building_height <= 14) %>%  #with average storey height of 3.5m remove buildings > 16m (3) or > 14m/ 4 storeys (7)
  mutate(resident_households = case_when(
    building_height >= 1.8 & building_height < 3.5 ~ 1, 
    building_height >= 3.5 & building_height < 7.0 ~ 1.5,
    building_height >= 7.0 & building_height < 10.5 ~ 2,
    building_height >= 10.5 & building_height < 14.0 ~ 2.5,
  )) #assumes household sizes in different storey buildings

asaba_household_counts <- asaba_storey_count %>%
  group_by(WardName) %>%
  summarise(total_households = sum(resident_households, na.rm = TRUE)) %>% 
  mutate(total_households = ceiling(total_households)) #round up

#estimates in different scenarios

#define urban wards
urban_asaba <- c("Umuagu", "Umuonaje", "Umuezei", "Okwe") 
part_urban_asaba <- c("West End", "Akuebolu", "Ugbomanta")

#define low risk wards in different scenarios
low_risk_asaba1 <- c("Ugbomanta", "Umuagu", "West End", "Umuezei")
low_risk_asaba2 <- c("Ugbomanta", "Umuagu", "West End", "Umuezei", "Akuebolu")


plot_asaba_all <- asaba_household_counts %>% 
  mutate(WardType = case_when(
    WardName %in% urban_asaba ~ "Urban", WardName %in% part_urban_asaba ~ "Part-Urban",
    TRUE ~ "Rural")) %>% 
  mutate(pop_estimate5 = total_households * 5,
         pop_estimate4 = total_households * 4,
         pop_estimate3 = total_households * 3 ) %>% #different estimates per household
  mutate(pop_estimateuneven = case_when(
    WardType == "Urban" ~ total_households * 3,  
    WardType == "Rural" ~ total_households * 5,
    WardType == "Part-Urban" ~ total_households * 4)) %>% #uneven population
  mutate(WardGroup = ifelse(WardName %in% low_risk_asaba1, WardName, "Rest of Asaba")) %>% #change low_risk_asaba1 to low_risk_asaba2 for different scenarios
  group_by(WardGroup) %>%
  summarize(pop_estimate5 = sum(pop_estimate5),
            pop_estimate3 = sum(pop_estimate3),
            pop_estimate4 = sum(pop_estimate4),
            pop_estimate_uneven = sum(pop_estimateuneven)) %>%
  pivot_longer(cols = starts_with("pop_estimate"), 
               names_to = "EstimateType", 
               values_to = "Population") %>%
  group_by(EstimateType) %>% 
  mutate(percentage = (Population / sum(Population)) * 100) %>% 
  mutate(label = paste0(Population, " - ", round(percentage, 2), "%"))


p1 <- ggplot(plot_asaba_all, aes(x = EstimateType, y = Population, fill = WardGroup))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values = c("Rest of Asaba" = "coral", "Umuagu" = "pink", "Umuezei" = "lightyellow",
                               "Ugbomanta" = "lightblue", "West End" = "lightgreen"))+
  labs(title = "Population Estimates in Asaba with Building Height (Scenario 1)",
       x = "Household sizes",
       y = "Population",
       fill = "Ward Group")+
  geom_text(aes(label = label), position = position_stack(vjust = 0.5))+
  theme_manuscript()

p2 <- ggplot(plot_asaba_all, aes(x = EstimateType, y = Population, fill = WardGroup))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values = c("Rest of Asaba" = "coral", "Umuagu" = "pink", "Umuezei" = "lightyellow",
                               "Ugbomanta" = "lightblue", "West End" = "lightgreen", "Akuebolu" = "purple"))+
  labs(title = "Population Estimates in Asaba with Building Height (Scenario 2)",
       x = "Household sizes",
       y = "Population",
       fill = "Ward Group")+
  geom_text(aes(label = label), position = position_stack(vjust = 0.5))+
  theme_manuscript()

grid.arrange(p1, p2)

###WARRI

warri_heights <- read.csv(file.path(RastersDir, "GoogleBuildings2_5/Warri/warri_buildings.csv"))


#convert height to likely household number per building
warri_storey_count <- warri_heights %>%
  filter(building_height >= 1.8) %>%  #remove uninhabitable buildings less than one storey (8015)
  filter(building_height <= 14) %>%  #with average storey height of 3.5m remove buildings > 16m (3) or > 14m/ 4 storeys (7)
  mutate(resident_households = case_when(
    building_height >= 1.8 & building_height < 3.5 ~ 1, 
    building_height >= 3.5 & building_height < 7.0 ~ 1.5,
    building_height >= 7.0 & building_height < 10.5 ~ 2,
    building_height >= 10.5 & building_height < 14.0 ~ 2.5,
  )) #assumes household sizes in different storey buildings

warri_household_counts <- warri_storey_count %>%
  group_by(WardName) %>%
  summarise(total_households = sum(resident_households, na.rm = TRUE)) %>% 
  mutate(total_households = ceiling(total_households)) #round up

#estimates in different scenarios

#define urban wards
urban_warri <- c("Avenue", "Bowen", "Esisi", "Ekurede", "Igbudu", "Okere") #Urban Wards based on GEE urban extent
part_urban_warri <- c("Pessu", "Ubeji")

#define low risk wards in different scenarios
low_risk_warri1 <- c("Esisi", "Avenue", "Ubeji")
low_risk_warri2 <- c("Esisi", "Avenue", "Ekurede", "Igbudu", "Ubeji")
low_risk_warri3 <- c("Avenue", "Esisi",  "Ekurede", "Ubeji", "Okere")



plot_warri_all <- warri_household_counts %>% 
  mutate(WardType = case_when(
    WardName %in% urban_warri ~ "Urban", WardName %in% part_urban_warri ~ "Part-Urban",
    TRUE ~ "Rural")) %>% 
  mutate(pop_estimate5 = total_households * 5,
         pop_estimate4 = total_households * 4,
         pop_estimate3 = total_households * 3 ) %>% #different estimates per household
  mutate(pop_estimateuneven = case_when(
    WardType == "Urban" ~ total_households * 3,  
    WardType == "Rural" ~ total_households * 5,
    WardType == "Part-Urban" ~ total_households * 4)) %>% #uneven population
  mutate(WardGroup = ifelse(WardName %in% low_risk_warri3, WardName, "Rest of Warri")) %>% #change low_risk_warri1 to low_risk_warri2/3 for different scenarios
  group_by(WardGroup) %>%
  summarize(pop_estimate5 = sum(pop_estimate5),
            pop_estimate3 = sum(pop_estimate3),
            pop_estimate4 = sum(pop_estimate4),
            pop_estimate_uneven = sum(pop_estimateuneven)) %>%
  pivot_longer(cols = starts_with("pop_estimate"), 
               names_to = "EstimateType", 
               values_to = "Population") %>%
  group_by(EstimateType) %>% 
  mutate(percentage = (Population / sum(Population)) * 100) %>% 
  mutate(label = paste0(Population, " - ", round(percentage, 2), "%"))

options(scipen = 999)

p3 <- 
  ggplot(plot_warri_all, aes(x = EstimateType, y = Population, fill = WardGroup))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values = c("Rest of Warri" = "coral", "Esisi" = "pink", "Avenue" = "lightyellow",
                               "Ubeji" = "lightblue"))+
  labs(title = "Population Estimates in Warri with Building Height (Scenario 1)",
       x = "Household sizes",
       y = "Population",
       fill = "Ward Group")+
  geom_text(aes(label = label), position = position_stack(vjust = 0.5))+
  theme_manuscript()


p4 <-
  ggplot(plot_warri_all, aes(x = EstimateType, y = Population, fill = WardGroup))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values = c("Rest of Warri" = "coral", "Esisi" = "pink", "Avenue" = "lightyellow",
                               "Ubeji" = "lightblue", "Ekurede" = "purple", "Igbudu" = "lightgreen")) +
  labs(title = "Population Estimates in Warri with Building Height (Scenario 2)",
       x = "Household sizes",
       y = "Population",
       fill = "Ward Group")+
  geom_text(aes(label = label), position = position_stack(vjust = 0.5))+
  theme_manuscript()

p5 <-
  ggplot(plot_warri_all, aes(x = EstimateType, y = Population, fill = WardGroup))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values = c("Rest of Warri" = "coral", "Esisi" = "pink", "Avenue" = "lightyellow",
                               "Ubeji" = "lightblue", "Ekurede" = "purple", "Okere" = "maroon")) +
  labs(title = "Population Estimates in Warri with Building Height (Scenario 3)",
       x = "Household sizes",
       y = "Population",
       fill = "Ward Group")+
  geom_text(aes(label = label), position = position_stack(vjust = 0.5))+
  theme_manuscript()

grid.arrange(p3, p4)
  