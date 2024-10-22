## population estimates

getwd()
source("~/NMEP_classification/load_path.R", echo = T)

#Residential areas in Delta State
settlement_blocks <- st_read(file.path(RastersDir, "nigeria_settlement_classification", "blocks_V1.1",
                                       "Nigeria_Blocks_V1.shp")) %>% 
  filter(state == 'Delta', landuse =='Residential')

settlement_blocks <- st_transform(settlement_blocks, crs = 4326)
st_is_valid(settlement_blocks)
settlement_blocks <- st_make_valid(settlement_blocks)

ggplot()+
  geom_sf(data = settlement_blocks, aes(geometry = geometry))


#### ASABA
asaba_shp <- st_read(file.path(ShpfilesDir, "Asaba", "Asaba.shp"))
asaba_shp <- st_transform(asaba_shp, crs = 4326)


asaba_geojson <- st_read(file.path(MSGlobalDir, "MS_footprints_Asaba.geojson"))

ggplot()+
  geom_sf(data = asaba_geojson, aes(geometry = geometry))+
  labs(title = "All Buildings in Asaba")+
  map_theme()

asaba_joined_data <- st_join(asaba_geojson, asaba_shp, join = st_within)

asaba_count_per_ward <- asaba_joined_data %>%
  st_drop_geometry() %>% 
  group_by(WardName) %>%  
  summarise(all_buildings = n_distinct(id))

ggplot(asaba_count_per_ward, aes(x = WardName, y = all_buildings)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = " Number of Buildings in Asaba",
       x = "Ward Name", y = "Count") +
  theme_manuscript() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


asaba_residential_areas <- st_join(asaba_joined_data, settlement_blocks, join = st_within) %>% 
  filter(landuse == "Residential")

asaba_count_per_ward_resd <- asaba_residential_areas %>%
  st_drop_geometry() %>%
  group_by(WardName) %>% 
  summarise(residential_buildings = n_distinct(id))


# plot building counts
asaba_buildings <- left_join(asaba_count_per_ward, asaba_count_per_ward_resd, by = "WardName")

asaba_buildings_long <- tidyr::pivot_longer(asaba_buildings, 
                                            cols = c(all_buildings, residential_buildings),
                                            names_to = "BuildingType", 
                                            values_to = "Count")

ggplot(asaba_buildings_long, aes(x = WardName, y = Count, fill = BuildingType)) +
  geom_bar(stat = "identity", position = "dodge") + 
  theme_manuscript() +
  labs(title = "Asaba Buildings",
       x = "Ward Name",
       y = "Number of Buildings",
       fill = "Building Type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

#population estimate scenarios

asaba_buildings <- asaba_buildings %>%
  mutate(pop_estimate = residential_buildings * 5) #estimated 5 persons per household

total_pop_asaba <- sum(asaba_buildings$pop_estimate)


#Scenario 1: Ugbomanta, Umuagu, West End, Umuezei

low_risk_asaba1 <- c("Ugbomanta", "Umuagu", "West End", "Umuezei")

asaba_grouped <- asaba_buildings %>%
  mutate(WardGroup = ifelse(WardName %in% low_risk_asaba1, WardName, "Rest of Asaba")) %>%
  group_by(WardGroup) %>%
  summarize(pop_estimate = sum(pop_estimate)) %>% 
  mutate(percentage = (pop_estimate / total_pop_asaba) * 100)

ggplot(asaba_grouped, aes(x = "", y = percentage, fill = WardGroup)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  labs(title = "Population Estimate in Asaba - Scenario 1", 
       x = NULL, 
       y = NULL, 
       fill = "Ward Group") +
  geom_text(aes(label = paste0(round(percentage, 2), "%")), 
            position = position_stack(vjust = 0.5)) +
  theme_manuscript() 


#Scenario 2: Ugbomanta, West End, Akuebolu, Umuezei, Umuagu

low_risk_asaba2 <- c("Ugbomanta", "Umuagu", "West End", "Umuezei", "Akuebolu")

asaba_grouped2 <- asaba_buildings %>%
  mutate(WardGroup = ifelse(WardName %in% low_risk_asaba2, WardName, "Rest of Asaba")) %>%
  group_by(WardGroup) %>%
  summarize(pop_estimate = sum(pop_estimate)) %>% 
  mutate(percentage = (pop_estimate / total_pop_asaba) * 100)

ggplot(asaba_grouped2, aes(x = "", y = percentage, fill = WardGroup)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  labs(title = "Population Estimate in Asaba - Scenario 2", 
       x = NULL, 
       y = NULL, 
       fill = "Ward Group") +
  geom_text(aes(label = paste0(round(percentage, 2), "%")), 
            position = position_stack(vjust = 0.5)) +
  theme_manuscript() 

##more Asaba scenarios: multiple population estimates
asaba_sc1 <- asaba_buildings %>% 
  mutate(pop_estimate5 = residential_buildings * 5,
         pop_estimate4 = residential_buildings * 4,
         pop_estimate3 = residential_buildings * 3 ) %>% #different estimates per household
  mutate(WardGroup = ifelse(WardName %in% low_risk_asaba1, WardName, "Rest of Asaba")) %>%
  group_by(WardGroup) %>%
  summarize(pop_estimate5 = sum(pop_estimate5),
            pop_estimate3 = sum(pop_estimate3),
            pop_estimate4 = sum(pop_estimate4)) %>%
  pivot_longer(cols = starts_with("pop_estimate"), 
               names_to = "EstimateType", 
               values_to = "Population") %>%
  group_by(EstimateType) %>% 
  mutate(percentage = (Population/ sum(Population)) *100)


ggplot(asaba_sc1, aes(x = "", y = percentage, fill = WardGroup)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  labs(title = "Population Estimate in Asaba Scenario 1", 
       x = NULL, 
       y = NULL, 
       fill = "Ward Group") +
  geom_text(aes(label = paste0(round(percentage, 2), "%")), 
            position = position_stack(vjust = 0.5)) +
  geom_text(data = asaba_sc1 %>% 
              filter(WardGroup == "Rest of Asaba"),
            aes(label = paste0("(",Population,")")), color = "blue")+
  facet_wrap(~EstimateType) +
  theme_manuscript()

##uneven population sizes
urban_asaba <- c("Umuagu", "Umuonaje", "Umuezei", "Okwe") 
part_urban_asaba <- c("West End", "Akuebolu", "Ugbomanta")

asaba_buildings <- asaba_buildings %>%
  mutate(WardType = case_when(
    WardName %in% urban_asaba ~ "Urban", WardName %in% part_urban_asaba ~ "Part-Urban",
    TRUE ~ "Rural"))

asaba_sc01 <- asaba_buildings %>%
  mutate(pop_estimate = case_when(
    WardType == "Urban" ~ residential_buildings * 3,  
    WardType == "Rural" ~ residential_buildings * 5,
    WardType == "Part-Urban" ~ residential_buildings * 4)) %>% #uneven population
  mutate(WardGroup = ifelse(WardName %in% low_risk_asaba1, WardName, "Rest of Asaba")) %>%
  group_by(WardGroup) %>%
  summarize(pop_estimate = sum(pop_estimate)) %>%
  mutate(percentage = (pop_estimate / sum(pop_estimate)) * 100)


ggplot(asaba_sc01, aes(x = "", y = percentage, fill = WardGroup)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  labs(title = "Population Estimate in Asaba Scenario 1 with Uneven Population Sizes", 
       x = NULL, 
       y = NULL, 
       fill = "Ward Group",
       caption = "Estimated household sizes: Urban = 3, Rural = 5, Part-Urban = 4 based on IGBP classification of urban extent") +
  geom_text(aes(label = paste0(round(percentage, 2), "%")), 
            position = position_stack(vjust = 0.5)) +
  geom_text(data = asaba_sc01 %>% filter(WardGroup == "Rest of Asaba"),
            aes(label = paste0("(", pop_estimate, ")")), color = "blue") +
  theme_manuscript()

#scenario 2
asaba_sc2 <- asaba_buildings %>% 
  mutate(pop_estimate5 = residential_buildings * 5,
         pop_estimate4 = residential_buildings * 4,
         pop_estimate3 = residential_buildings * 3 ) %>% 
  mutate(WardGroup = ifelse(WardName %in% low_risk_asaba2, WardName, "Rest of Asaba")) %>%
  group_by(WardGroup) %>%
  summarize(pop_estimate5 = sum(pop_estimate5),
            pop_estimate3 = sum(pop_estimate3),
            pop_estimate4 = sum(pop_estimate4)) %>%
  pivot_longer(cols = starts_with("pop_estimate"), 
               names_to = "EstimateType", 
               values_to = "Population") %>%
  group_by(EstimateType) %>% 
  mutate(percentage = (Population/ sum(Population)) *100)


ggplot(asaba_sc2, aes(x = "", y = percentage, fill = WardGroup)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  labs(title = "Population Estimate in Asaba Scenario 2", 
       x = NULL, 
       y = NULL, 
       fill = "Ward Group") +
  geom_text(aes(label = paste0(round(percentage, 2), "%")), 
            position = position_stack(vjust = 0.5)) +
  geom_text(data = asaba_sc1 %>% 
              filter(WardGroup == "Rest of Asaba"),
            aes(label = paste0("(",Population,")")), color = "blue")+
  facet_wrap(~EstimateType) +
  theme_manuscript()

#uneven
asaba_sc02 <- asaba_buildings %>%
  mutate(pop_estimate = case_when(
    WardType == "Urban" ~ residential_buildings * 3,  
    WardType == "Rural" ~ residential_buildings * 5,
    WardType == "Part-Urban" ~ residential_buildings * 4)) %>% #uneven population
  mutate(WardGroup = ifelse(WardName %in% low_risk_asaba2, WardName, "Rest of Asaba")) %>%
  group_by(WardGroup) %>%
  summarize(pop_estimate = sum(pop_estimate)) %>%
  mutate(percentage = (pop_estimate / sum(pop_estimate)) * 100)


ggplot(asaba_sc02, aes(x = "", y = percentage, fill = WardGroup)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  labs(title = "Population Estimate in Asaba Scenario 2 with Uneven Population Sizes", 
       x = NULL, 
       y = NULL, 
       fill = "Ward Group",
       caption = "Estimated household sizes: Urban = 3, Rural = 5, Part-Urban = 4 based on IGBP classification of urban extent") +
  geom_text(aes(label = paste0(round(percentage, 2), "%")), 
            position = position_stack(vjust = 0.5)) +
  geom_text(data = asaba_sc01 %>% filter(WardGroup == "Rest of Asaba"),
            aes(label = paste0("(", pop_estimate, ")")), color = "blue") +
  theme_manuscript()



## WARRI
warri_shp <- st_read(file.path(ShpfilesDir, "Warri", "Warri.shp"))
warri_shp <- st_transform(warri_shp, crs = 4326)


warri_geojson <- st_read(file.path(MSGlobalDir, "warri_footprints.geojson"))
ggplot()+
  geom_sf(data = warri_geojson, aes(geometry = geometry))


warri_joined_data <- st_join(warri_geojson, warri_shp, join = st_within)

warri_count_per_ward <- warri_joined_data %>%
  st_drop_geometry() %>% 
  group_by(WardName) %>%  
  summarise(all_buildings = n_distinct(id))

ggplot(warri_count_per_ward, aes(x = WardName, y = all_buildings)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = " Number of Buildings in Warri",
       x = "Ward Name", y = "Count") +
  theme_manuscript() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


warri_residential_areas <- st_join(warri_joined_data, settlement_blocks, join = st_within) %>% 
  filter(landuse == "Residential")

warri_count_per_ward_resd <- warri_residential_areas %>%
  st_drop_geometry() %>%
  group_by(WardName) %>% 
  summarise(residential_buildings = n_distinct(id))


# plot building counts
warri_buildings <- left_join(warri_count_per_ward, warri_count_per_ward_resd, by = "WardName")

warri_buildings_long <- tidyr::pivot_longer(warri_buildings, 
                                            cols = c(all_buildings, residential_buildings),
                                            names_to = "BuildingType", 
                                            values_to = "Count")

ggplot(warri_buildings_long, aes(x = WardName, y = Count, fill = BuildingType)) +
  geom_bar(stat = "identity", position = "dodge") + 
  theme_manuscript() +
  labs(title = "Warri Buildings",
       x = "Ward Name",
       y = "Number of Buildings",
       fill = "Building Type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  


##population estimates
#Warri Scenario 1: Esisi, Avenue, Ubeji (based on all composite scores)

low_risk_warri1 <- c("Esisi", "Avenue", "Ubeji")

warri_sc1 <- warri_buildings %>% 
  mutate(pop_estimate5 = residential_buildings * 5,
         pop_estimate4 = residential_buildings * 4,
         pop_estimate3 = residential_buildings * 3 ) %>% #different estimates per household
  mutate(WardGroup = ifelse(WardName %in% low_risk_warri1, WardName, "Rest of Warri")) %>%
  group_by(WardGroup) %>%
  summarize(pop_estimate5 = sum(pop_estimate5),
            pop_estimate3 = sum(pop_estimate3),
            pop_estimate4 = sum(pop_estimate4)) %>%
  pivot_longer(cols = starts_with("pop_estimate"), 
               names_to = "EstimateType", 
               values_to = "Population") %>%
  group_by(EstimateType) %>% 
  mutate(percentage = (Population/ sum(Population)) *100)


ggplot(warri_sc1, aes(x = "", y = percentage, fill = WardGroup)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  labs(title = "Population Estimate in Warri Scenario 1", 
       x = NULL, 
       y = NULL, 
       fill = "Ward Group",
       caption = "Esisi(1), Avenue(2), Ubeji(3) are ranked to be deprioritized based on all composite scores. Rural wards Abigborodo and Mandangho excluded.
       Number in blue represents number of people in the rest of the city, based on estimated household size: 3, 4 or 5") +
  geom_text(aes(label = paste0(round(percentage, 2), "%")), 
            position = position_stack(vjust = 0.5)) +
  geom_text(data = warri_sc1 %>% 
              filter(WardGroup == "Rest of Warri"),
            aes(label = paste0("(",Population,")")), color = "blue")+
  facet_wrap(~EstimateType) +
  theme_manuscript()


#uneven population size
urban_warri <- c("Avenue", "Bowen", "Esisi", "Ekurede", "Igbudu", "Okere") #Urban Wards based on GEE urban extent
part_urban_warri <- c("Pessu", "Ubeji")
warri_buildings <- warri_buildings %>%
  mutate(WardType = case_when(
    WardName %in% urban_warri ~ "Urban", WardName %in% part_urban_warri ~ "Part-Urban",
    TRUE ~ "Rural"))

warri_sc01 <- warri_buildings %>%
  mutate(pop_estimate = case_when(
    WardType == "Urban" ~ residential_buildings * 3,  
    WardType == "Rural" ~ residential_buildings * 5,
    WardType == "Part-Urban" ~ residential_buildings * 4)) %>% #uneven population
  mutate(WardGroup = ifelse(WardName %in% low_risk_warri1, WardName, "Rest of Warri")) %>%
  group_by(WardGroup) %>%
  summarize(pop_estimate = sum(pop_estimate)) %>%
  mutate(percentage = (pop_estimate / sum(pop_estimate)) * 100)


ggplot(warri_sc01, aes(x = "", y = percentage, fill = WardGroup)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  labs(title = "Population Estimate in Warri Scenario 1 with Uneven Population Sizes", 
       x = NULL, 
       y = NULL, 
       fill = "Ward Group",
       caption = "Estimated household sizes: Urban = 3, Rural = 5, Part-Urban = 4 based on IGBP classification of urban extent") +
  geom_text(aes(label = paste0(round(percentage, 2), "%")), 
            position = position_stack(vjust = 0.5)) +
  geom_text(data = warri_sc01 %>% filter(WardGroup == "Rest of Warri"),
            aes(label = paste0("(", pop_estimate, ")")), color = "blue") +
  theme_manuscript()


#Warri Scenario 2: Esisi, Avenue, Ekurede, Igbudu, Ubeji

low_risk_warri2 <- c("Esisi", "Avenue", "Ekurede", "Igbudu", "Ubeji")

warri_sc2 <- warri_buildings %>% 
  mutate(pop_estimate5 = residential_buildings * 5,
         pop_estimate4 = residential_buildings * 4,
         pop_estimate3 = residential_buildings * 3 ) %>% #different estimates per household
  mutate(WardGroup = ifelse(WardName %in% low_risk_warri2, WardName, "Rest of Warri")) %>%
  group_by(WardGroup) %>%
  summarize(pop_estimate5 = sum(pop_estimate5),
            pop_estimate3 = sum(pop_estimate3),
            pop_estimate4 = sum(pop_estimate4)) %>%
  pivot_longer(cols = starts_with("pop_estimate"), 
               names_to = "EstimateType", 
               values_to = "Population") %>%
  group_by(EstimateType) %>% 
  mutate(percentage = (Population/ sum(Population)) *100)


ggplot(warri_sc2, aes(x = "", y = percentage, fill = WardGroup)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  labs(title = "Population Estimate in Warri Scenario 2", 
       x = NULL, 
       y = NULL, 
       fill = "Ward Group",
       caption = "Esisi(1), Avenue(2), Ekurede(3), Igbudu(4), Ubeji(5) are ranked to be deprioritized based on settlement type and u5 TPR.
       Number in blue represents number of people in the rest of the city, based on estimated household size: 3, 4 or 5") +
  geom_text(aes(label = paste0(round(percentage, 2), "%")), 
            position = position_stack(vjust = 0.5)) +
  geom_text(data = warri_sc2 %>% 
              filter(WardGroup == "Rest of Warri"),
            aes(label = paste0("(",Population,")")), color = "blue")+
  facet_wrap(~EstimateType) +
  theme_manuscript()

#uneven
warri_sc02 <- warri_buildings %>%
  mutate(pop_estimate = case_when(
    WardType == "Urban" ~ residential_buildings * 3,  
    WardType == "Rural" ~ residential_buildings * 5,
    WardType == "Part-Urban" ~ residential_buildings * 4)) %>% #uneven population
  mutate(WardGroup = ifelse(WardName %in% low_risk_warri2, WardName, "Rest of Warri")) %>%
  group_by(WardGroup) %>%
  summarize(pop_estimate = sum(pop_estimate)) %>%
  mutate(percentage = (pop_estimate / sum(pop_estimate)) * 100)


ggplot(warri_sc02, aes(x = "", y = percentage, fill = WardGroup)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  labs(title = "Population Estimate in Warri Scenario 2 with Uneven Population Sizes", 
       x = NULL, 
       y = NULL, 
       fill = "Ward Group",
       caption = "Estimated household sizes: Urban = 3, Rural = 5, Part-Urban = 4 based on IGBP classification of urban extent") +
  geom_text(aes(label = paste0(round(percentage, 2), "%")), 
            position = position_stack(vjust = 0.5)) +
  geom_text(data = warri_sc02 %>% filter(WardGroup == "Rest of Warri"),
            aes(label = paste0("(", pop_estimate, ")")), color = "blue") +
  theme_manuscript()

#Warri Scenario 3: Avenue, Esisi,  Ekurede, Ubeji, Okere

low_risk_warri3 <- c("Avenue", "Esisi",  "Ekurede", "Ubeji", "Okere")

warri_sc3 <- warri_buildings %>% 
  mutate(pop_estimate5 = residential_buildings * 5,
         pop_estimate4 = residential_buildings * 4,
         pop_estimate3 = residential_buildings * 3 ) %>% #different estimates per household
  mutate(WardGroup = ifelse(WardName %in% low_risk_warri3, WardName, "Rest of Warri")) %>%
  group_by(WardGroup) %>%
  summarize(pop_estimate5 = sum(pop_estimate5),
            pop_estimate3 = sum(pop_estimate3),
            pop_estimate4 = sum(pop_estimate4)) %>%
  pivot_longer(cols = starts_with("pop_estimate"), 
               names_to = "EstimateType", 
               values_to = "Population") %>%
  group_by(EstimateType) %>% 
  mutate(percentage = (Population/ sum(Population)) *100)


ggplot(warri_sc3, aes(x = "", y = percentage, fill = WardGroup)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  labs(title = "Population Estimate in Warri Scenario 3", 
       x = NULL, 
       y = NULL, 
       fill = "Ward Group",
       caption = "Avenue(1), Esisi(2),  Ekurede(3), Ubeji(4) and Okere (5) are ranked to be deprioritized based on distance to water bodies and settlement type.
       Number in blue represents number of people in the rest of the city, based on estimated household size: 3, 4 or 5") +
  geom_text(aes(label = paste0(round(percentage, 2), "%")), 
            position = position_stack(vjust = 0.5)) +
  geom_text(data = warri_sc3 %>% 
              filter(WardGroup == "Rest of Warri"),
            aes(label = paste0("(",Population,")")), color = "blue")+
  facet_wrap(~EstimateType) +
  theme_manuscript()

#uneven
warri_sc03 <- warri_buildings %>%
  mutate(pop_estimate = case_when(
    WardType == "Urban" ~ residential_buildings * 3,  
    WardType == "Rural" ~ residential_buildings * 5,
    WardType == "Part-Urban" ~ residential_buildings * 4)) %>% 
  mutate(WardGroup = ifelse(WardName %in% low_risk_warri3, WardName, "Rest of Warri")) %>%
  group_by(WardGroup) %>%
  summarize(pop_estimate = sum(pop_estimate)) %>%
  mutate(percentage = (pop_estimate / sum(pop_estimate)) * 100)


ggplot(warri_sc03, aes(x = "", y = percentage, fill = WardGroup)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  labs(title = "Population Estimate in Warri Scenario 3 with Uneven Population Sizes", 
       x = NULL, 
       y = NULL, 
       fill = "Ward Group",
       caption = "Estimated household sizes: Urban = 3, Rural = 5, Part-Urban = 4 based on IGBP classification of urban extent") +
  geom_text(aes(label = paste0(round(percentage, 2), "%")), 
            position = position_stack(vjust = 0.5)) +
  geom_text(data = warri_sc03 %>% filter(WardGroup == "Rest of Warri"),
            aes(label = paste0("(", pop_estimate, ")")), color = "blue") +
  theme_manuscript()



### Summarized plots

#Asaba

asaba_all <- asaba_buildings %>% 
  mutate(pop_estimate5 = residential_buildings * 5,
         pop_estimate4 = residential_buildings * 4,
         pop_estimate3 = residential_buildings * 3 ) %>% 
  mutate(pop_estimateuneven = case_when(
    WardType == "Urban" ~ residential_buildings * 3,  
    WardType == "Rural" ~ residential_buildings * 5,
    WardType == "Part-Urban" ~ residential_buildings * 4)) %>%
  mutate(WardGroup = ifelse(WardName %in% low_risk_asaba2, WardName, "Rest of Asaba")) %>% #change low_risk_asaba1 to low_risk_asaba2 for different scenarios
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

p1<- ggplot(asaba_all, aes(x = EstimateType, y = Population, fill = WardGroup)) +
  geom_bar(stat = "identity", position = "stack") +   
  scale_fill_manual(values = c("Rest of Asaba" = "coral", "Umuagu" = "pink", "Umuezei" = "lightyellow",
                               "Ugbomanta" = "lightblue", "West End" = "lightgreen")) +
  labs(title = "Population Estimate in Asaba (Scenario 1)",
       x = "Household sizes",
       y = "Population",
       fill = "Ward Group", 
       caption = "pop_estimate3 = 3 persons per household, 
       pop_estimate4 = 4 persons per household, 
       pop_estimate5 = 5 persons per household, 
       pop_estimate_uneven = mixed household sizes") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5))+
  theme_manuscript() 

p2 <-  ggplot(asaba_all, aes(x = EstimateType, y = Population, fill = WardGroup)) +
  geom_bar(stat = "identity", position = "stack") +   
  scale_fill_manual(values = c("Rest of Asaba" = "coral", "Umuagu" = "pink", "Umuezei" = "lightyellow",
                               "Ugbomanta" = "lightblue", "West End" = "lightgreen", "Akuebolu" = "purple")) +
  labs(title = "Population Estimate in Asaba (Scenario 2)",
       x = "Household sizes",
       y = "Population",
       fill = "Ward Group", 
       caption = "pop_estimate3 = 3 persons per household, 
       pop_estimate4 = 4 persons per household, 
       pop_estimate5 = 5 persons per household, 
       pop_estimate_uneven = mixed household sizes") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5))+
  theme_manuscript() 

grid.arrange(p1, p2)

##WARRI 
warri_all <- warri_buildings %>% 
  mutate(pop_estimate5 = residential_buildings * 5,
         pop_estimate4 = residential_buildings * 4,
         pop_estimate3 = residential_buildings * 3 ) %>% 
  mutate(pop_estimateuneven = case_when(
    WardType == "Urban" ~ residential_buildings * 3,  
    WardType == "Rural" ~ residential_buildings * 5,
    WardType == "Part-Urban" ~ residential_buildings * 4)) %>%
  mutate(WardGroup = ifelse(WardName %in% low_risk_warri3, WardName, "Rest of Warri")) %>% #change low_risk_warri1 to low_risk_warri2 for different scenarios
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

p3<- ggplot(warri_all, aes(x = EstimateType, y = Population, fill = WardGroup)) +
  geom_bar(stat = "identity", position = "stack") +   
  scale_fill_manual(values = c("Rest of Warri" = "coral", "Esisi" = "pink", "Avenue" = "lightyellow",
                               "Ubeji" = "lightblue")) +
  labs(title = "Population Estimate in Warri (Scenario 1)",
       x = "Household sizes",
       y = "Population",
       fill = "Ward Group", 
       caption = "pop_estimate3 = 3 persons per household, 
       pop_estimate4 = 4 persons per household, 
       pop_estimate5 = 5 persons per household, 
       pop_estimate_uneven = mixed household sizes") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5))+
  theme_manuscript() 

p4 <-  ggplot(warri_all, aes(x = EstimateType, y = Population, fill = WardGroup)) +
  geom_bar(stat = "identity", position = "stack") +   
  scale_fill_manual(values = c("Rest of Warri" = "coral", "Esisi" = "pink", "Avenue" = "lightyellow",
                               "Ubeji" = "lightblue", "Ekurede" = "purple", "Igbudu" = "lightgreen")) +
  labs(title = "Population Estimate in Warri (Scenario 2)",
       x = "Household sizes",
       y = "Population",
       fill = "Ward Group") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5))+
  theme_manuscript() 

p5 <-  ggplot(warri_all, aes(x = EstimateType, y = Population, fill = WardGroup)) +
  geom_bar(stat = "identity", position = "stack") +   
  scale_fill_manual(values = c("Rest of Warri" = "coral", "Esisi" = "pink", "Avenue" = "lightyellow",
                               "Ubeji" = "lightblue", "Ekurede" = "purple", "Okere" = "lightgreen")) +
  labs(title = "Population Estimate in Warri (Scenario 3)",
       x = "Household sizes",
       y = "Population",
       fill = "Ward Group") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5))+
  theme_manuscript() 

grid.arrange(p3, p4)

##compare with ITN household sizes

ITNDir <- file.path(DataDir, "nigeria/ITN_distribution/Extracts from ITN distribution database")

asaba_itn_data <- read.csv(file.path(ITNDir, "Delta_Asaba_itn.csv"))

asaba_itn_clean <- asaba_itn_data %>% 
  rename(household_number = iqNumberOfFamilyMembers) %>% 
  mutate(geometry_clean = as.numeric(gsub("[^0-9.]", "", geometry)),
         geometry2_clean = as.numeric(gsub("[^0-9.]", "", geometry2))) %>% 
  dplyr::select(household_number, geometry_clean, geometry2_clean )

asaba_itn <- st_as_sf(asaba_itn_clean, coords = c("geometry_clean", "geometry2_clean"),
                      crs = 4326)

ggplot()+
  geom_sf(data = asaba_shp, aes(geometry = geometry))+
  geom_sf(data = asaba_itn, aes(geometry = geometry))

asaba_wards_itn <- st_join(asaba_itn, asaba_shp)

asaba_wards_itn <- asaba_wards_itn %>% 
  st_drop_geometry() %>% 
  group_by(WardName) %>% 
  summarise(pop_itn = sum(household_number, na.rm = T))

#plot scenarios with ITN data

plot_itn_asaba <- asaba_wards_itn %>% 
  mutate(WardGroup = ifelse(WardName %in% low_risk_asaba1, WardName, "Rest of Asaba")) %>% #change low_risk_asaba1 to low_risk_asaba2 for different scenarios
  group_by(WardGroup) %>%
  summarize(x = sum(pop_itn)) %>%
  pivot_longer(cols = c(x), 
               names_to = "EstimateType", 
               values_to = "Population") %>%
  mutate(percentage = (Population / sum(Population)) * 100) %>% 
  mutate(label = paste0(Population, " - ", round(percentage, 2), "%"))

options(scipen = 999)

y1<- ggplot(plot_itn_asaba, aes(x = EstimateType, y = Population, fill = WardGroup)) +
  geom_bar(stat = "identity", position = "stack") +   
  scale_fill_manual(values = c("Rest of Asaba" = "coral", "Umuagu" = "pink", "Umuezei" = "lightyellow",
                               "Ugbomanta" = "lightblue", "West End" = "lightgreen")) +
  labs(title = "Asaba Scenario 1 Using ITN Data",
       x = "Household sizes",
       y = "Population",
       fill = "Ward Group") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5))+
  theme_manuscript() 

y2 <- ggplot(plot_itn_asaba, aes(x = EstimateType, y = Population, fill = WardGroup)) +
  geom_bar(stat = "identity", position = "stack") +   
  scale_fill_manual(values = c("Rest of Asaba" = "coral", "Umuagu" = "pink", "Umuezei" = "lightyellow",
                               "Ugbomanta" = "lightblue", "West End" = "lightgreen", "Akuebolu" = "purple")) +
  labs(title = "Asaba Scenario 2 Using ITN Data",
       x = "Household sizes",
       y = "Population",
       fill = "Ward Group") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5))+
  theme_manuscript() 

grid.arrange(y1, y2)


#Warri
warri_itn_data <- read.csv(file.path(ITNDir, "Delta_Warri_itn.csv"))

warri_itn_clean <- warri_itn_data %>% 
  rename(household_number = iqNumberOfFamilyMembers) %>% 
  mutate(geometry_clean = as.numeric(gsub("[^0-9.]", "", geometry)),
         geometry2_clean = as.numeric(gsub("[^0-9.]", "", geometry2))) %>% 
  dplyr::select(household_number, geometry_clean, geometry2_clean )

warri_itn <- st_as_sf(warri_itn_clean, coords = c("geometry_clean", "geometry2_clean"),
                      crs = 4326)

warri_wards_itn <- st_join(warri_itn, warri_shp)
warri_wards_itn <- warri_wards_itn %>% 
  st_drop_geometry() %>% 
  group_by(WardName) %>% 
  summarise(pop_itn = sum(household_number, na.rm = T))

plot_itn_warri <- warri_wards_itn %>% 
  mutate(WardGroup = ifelse(WardName %in% low_risk_warri3, WardName, "Rest of Warri")) %>% #change low_risk_warri1 to low_risk_warri2 or 3 for different scenarios
  group_by(WardGroup) %>%
  summarize(x = sum(pop_itn)) %>%
  pivot_longer(cols = c(x), 
               names_to = "EstimateType", 
               values_to = "Population") %>%
  mutate(percentage = (Population / sum(Population)) * 100) %>% 
  mutate(label = paste0(Population, " - ", round(percentage, 2), "%"))

y3<- ggplot(plot_itn_warri, aes(x = EstimateType, y = Population, fill = WardGroup)) +
  geom_bar(stat = "identity", position = "stack") +   
  scale_fill_manual(values = c("Rest of Warri" = "coral", "Esisi" = "pink", "Avenue" = "lightyellow",
                               "Ubeji" = "lightblue")) +
  labs(title = "Scenario 1 - Warri ITN",
       x = "Household sizes",
       y = "Population",
       fill = "Ward Group") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5))+
  theme_manuscript() 

y4 <- ggplot(plot_itn_warri, aes(x = EstimateType, y = Population, fill = WardGroup)) +
  geom_bar(stat = "identity", position = "stack") +   
  scale_fill_manual(values = c("Rest of Warri" = "coral", "Esisi" = "pink", "Avenue" = "lightyellow",
                               "Ubeji" = "lightblue", "Ekurede" = "purple", "Igbudu" = "lightgreen")) +
  labs(title = "Scenario 2 - Warri ITN",
       x = "Household sizes",
       y = "Population",
       fill = "Ward Group") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5))+
  theme_manuscript() 

y5 <- ggplot(plot_itn_warri, aes(x = EstimateType, y = Population, fill = WardGroup)) +
  geom_bar(stat = "identity", position = "stack") +   
  scale_fill_manual(values = c("Rest of Warri" = "coral", "Esisi" = "pink", "Avenue" = "lightyellow",
                               "Ubeji" = "lightblue", "Ekurede" = "purple", "Okere" = "lightgreen")) +
  labs(title = "Scenario 3 - Warri ITN",
       x = "Household sizes",
       y = "Population",
       fill = "Ward Group") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5))+
  theme_manuscript() 

grid.arrange(y3, y4, y5)

