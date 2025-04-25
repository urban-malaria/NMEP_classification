# ==========================================================================================================================================
# Script Name: Reprioritization
# Purpose: Creates reprioritization plots.
# ==========================================================================================================================================

source("load_path.R")


################## DELTA STATE ###########################################
## -----------------------------------------------------------------------------------------------------------------------------------------
### DELTA
## -----------------------------------------------------------------------------------------------------------------------------------------

#kano_urban <- st_read(file.path(DataDir, "nigeria/urban_extent", "Kano_urban_percentage.geojson")) 

delta_shp <- st_read(file.path(StateShpDir,"Delta", "Delta_State.shp"))

delta_variables <- read.csv(file.path(OutputsDir, "Final Extractions", "delta_plus.csv")) %>% 
  dplyr::select(X, WardName, WardCode, urbanPercentage) %>% 
  mutate(
    classification_20 = ifelse(urbanPercentage > 20, "Urban", "Rural"),
    classification_30 = ifelse(urbanPercentage > 30, "Urban", "Rural"),
    classification_50 = ifelse(urbanPercentage > 50, "Urban", "Rural"),
    classification_75 = ifelse(urbanPercentage > 75, "Urban", "Rural")
  )


#delta_ranks <- read.csv(file.path(OutputsDir, "Delta_rankings2.csv")) %>%
delta_ranks <- read.csv(file.path(OutputsDir, "rankings", "Delta_rankings.csv")) %>% 
  dplyr::select(WardCode, WardName, ranks) #Ward, Rank
delta_ranks$WardName <- str_trim(delta_ranks$WardName)
delta_ranks$ranks <- str_trim(delta_ranks$ranks)


delta_itn_data <- readxl::read_excel(
  file.path(ITNDir, "pbi_distribution_Delta.xlsx"), 
  sheet = 3)

delta_itn_clean <- delta_itn_data %>% 
  rename(population = N_FamilyMembers,
         Ward = AdminLevel3) %>% 
  dplyr::select(population, Ward) %>% 
  group_by(Ward) %>% 
  summarise(Population = sum(population, na.rm =T)) %>% 
  mutate(Ward = case_when(
    Ward == "Aballa/ Inyi/ Onuaboh" ~	"Aballa/Inyi/Onuaboh",
    Ward == "Abbi 1" ~	"Abbi 8",
    Ward == "Abbi 2" ~	"Abbi 9",
    Ward == "Aboh/ Akarai" ~	"Aboh/Akarai",
    Ward == "Abraka" ~	"Abraka I",
    Ward == "Achalla/Ezukwu/Ogboli" ~	"",
    Ward == "Afor/ Obikwele" ~	"Afor/Obikwele",
    Ward == "Agbarho I" ~	"Agbarho 1",
    Ward == "Agbarho II" ~	"Agbarho 2",
    Ward == "Agbor Obi" ~	"Agbor-Obi 1/Agbor Town II",
    Ward == "Agbor Obi Ward 2" ~	"",
    Ward == "Agbor-Nta" ~	"Agbor-Nta/Agbor Town I",
    Ward == "Agidiasi" ~	"Agiadiasi",
    Ward == "Ajudabo" ~	"Ajudaibo",
    Ward == "Akugbene 3" ~	"Akugbene III",
    Ward == "Akwuebulu" ~	"Akuebolu",
    Ward == "Alihagwu" ~	"Alihagwu/Ihiuiyase I",
    Ward == "Arigborodo" ~	"Abigborodo",
    Ward == "Ashaka/ Ushie" ~	"Ashaka/Ushie",
    Ward == "Boji Boji 1" ~	"Boji-Boji Owa 1/Owa III",
    Ward == "Boji Boji Agbor 1" ~	"Boji-Boji I/ Agbor 7",
    Ward == "Boji Boji Agbor 2" ~	"Boji-Boji II/ Agbor 8",
    Ward == "Boji Boji Owa 2" ~	"Boji-Boji Owa 2/Owa IV",
    Ward == "Egbo" ~	"Egbo/Agbon  VII",
    Ward == "Egini" ~	"Egini/Ovwian II",
    Ward == "Egodor" ~	"",
    Ward == "Ejeme/ Egbudu" ~	"Ejeme/Egbudu",
    Ward == "Ekametagbene/Kalafio" ~	"",
    Ward == "Ekpan I" ~	"Ekpan 9",
    Ward == "Ekpan II" ~	"Ekpan 10",
    Ward == "Eku" ~	"Eku/Agbon  VI",
    Ward == "Enerhen 1" ~	"Enerhen 3",
    Ward == "Enerhen I" ~	"",
    Ward == "Enerhen II" ~	"Enerhen 4",
    Ward == "Ewulu/ Isheagu" ~	"Ewulu/Isheagu",
    Ward == "Ibedeni/ Azagba" ~	"Ibedeni/Azagba",
    Ward == "Iberede/Onu/Iyede-Ame" ~	"Ibrede/Onu/Iyede-Ame",
    Ward == "Idumuje-Umor" ~	"Idumuje-Unor",
    Ward == "Igun" ~	"Igun/Agbon  V",
    Ward == "Irri 1" ~	"Irri 10",
    Ward == "Irri 2" ~	"Irri 11",
    Ward == "Isiokolo" ~	"Isiokolo/Agbon  VIII",
    Ward == "Kokori" ~	"Kokori/Agbon  III",
    Ward == "Mandangho" ~	"Madangho",
    Ward == "Ogbe - Obiaruku" ~	"Ogbeobiaruku",
    Ward == "Ogbe-Udu" ~	"Ogbe Udu",
    Ward == "Ogbudugbudu" ~	"Okbudugbudu",
    Ward == "Ogharefe 1" ~	"Ogharefe 1/Oghara I",
    Ward == "Ogharefe 2" ~	"Ogharefe 2/Oghara II",
    Ward == "Ogharefe 3" ~	"Ogharefe 3/Oghara III",
    Ward == "Oghareki 1" ~	"",
    Ward == "Oghareki 2" ~	"",
    Ward == "Ogor" ~	"",
    Ward == "Ogume 1" ~	"Ogume 6",
    Ward == "Ogume 2" ~	"Ogume 7",
    Ward == "Oko-Ogbele" ~	"Oko Ogbele",
    Ward == "Okpanam/ Ugbolu" ~	"Okpanam/Ugbolu",
    Ward == "Okpara" ~	"Okpara/Agbon  I",
    Ward == "Okuzu" ~	"Okuzu/Obiaruku  II",
    Ward == "Olomu 3" ~	"Olomu 3-Effurun-Otor",
    Ward == "Orhaorpo" ~	"Orhaorpo/Agbon  IV",
    Ward == "Oria" ~	"Oria/Abraka  III",
    Ward == "Orogun I" ~	"Orogun 1",
    Ward == "Orogun II" ~	"Orogun 2-Erhobaro",
    Ward == "Otor-Udu" ~	"Otor Udu/Udu I",
    Ward == "Ovu" ~	"Ovu/Agbon  II",
    Ward == "Owa- Alizomor" ~	"Owa-Alizomor/Owa  VI",
    Ward == "Owa Oyibu" ~	"Owa-Oyibu",
    Ward == "Owa-Alero" ~	"Owa-Alero/Owa II",
    Ward == "Owanta" ~	"Owanta/Owa  V",
    Ward == "Owhe Ward 1" ~	"Owhe 1",
    Ward == "Owhe Ward 2" ~	"Owhe 2",
    Ward == "Owhe Ward 3" ~	"Owhe 3",
    Ward == "Owhrode" ~	"Owhrode/Udu II",
    Ward == "Oyede Ward" ~	"Oyede",
    Ward == "Oyoko" ~	"Oyoko/Abavo I",
    Ward == "Ozanogogo" ~	"Ozanogogo/Ihuozomor (Ozanogogo Alisimie)",
    Ward == "Ozoro Ward 1" ~	"Ozoro 1",
    Ward == "Ozoro Ward 2" ~	"Ozoro 2",
    Ward == "Ozoro Ward 3" ~	"Ozoro 3",
    Ward == "Udomi-Azuowa" ~	"Udomi-Azuowa/Abavo II",
    Ward == "Urhuovie" ~	"Urhuovie/Abraka  II",
    TRUE ~ Ward))


combined_wards <- left_join(delta_variables, delta_ranks, by = c("WardCode", "WardName"))

combined_wards2 <- left_join(combined_wards, delta_itn_clean, by = c("WardName" = "Ward")) 

prioritized_delta1 <- prioritize_wards(data = combined_wards2, 
                                      population_col = "Population", 
                                      rank_col = "ranks", 
                                      class_col = "classification_20", 
                                      ward_col = "WardName", 
                                      target_percentage = 30) 
write.csv(prioritized_delta1, file.path(OutputsDir, "NMEP Presentation Reprioritization Tables", "Delta", "delta_scenario1.csv"))



prioritized_delta2 <- prioritize_wards(data = combined_wards2, 
                                        population_col = "Population", 
                                        rank_col = "ranks", 
                                        class_col = "classification_30", 
                                        ward_col = "WardName", 
                                        target_percentage = 30)
write.csv(prioritized_delta2, file.path(OutputsDir, "NMEP Presentation Reprioritization Tables", "Delta", "delta_scenario2.csv"))




prioritized_delta3 <- prioritize_wards(data = combined_wards2, 
                                        population_col = "Population", 
                                        rank_col = "ranks", 
                                        class_col = "classification_50", 
                                        ward_col = "WardName", 
                                        target_percentage = 30)
write.csv(prioritized_delta3, file.path(OutputsDir, "NMEP Presentation Reprioritization Tables", "Delta", "delta_scenario3.csv"))



prioritized_delta4 <- prioritize_wards(data = combined_wards2, 
                                        population_col = "Population", 
                                        rank_col = "ranks", 
                                        class_col = "classification_75", 
                                        ward_col = "WardName", 
                                        target_percentage = 30)
write.csv(prioritized_delta4, file.path(OutputsDir, "NMEP Presentation Reprioritization Tables", "Delta", "delta_scenario4.csv"))


combined_plot <-  delta_shp %>% 
  left_join(combined_wards2, by = "WardName")
combined_plot$ranks <- as.numeric(as.character(combined_plot$ranks))


p <- ggplot()+
  geom_sf(data = combined_plot, aes(geometry = geometry, fill = ranks))+
  scale_fill_gradient(low = "lightyellow", high = "orangered") +
  labs(title = "Risk Map in Delta State") +
  map_theme()


FigDir <- file.path(OutputsDir, "NMEP Presentation Risk Maps/Delta")
ggsave(paste0(FigDir,"/", Sys.Date(),"_Delta_risk_map.pdf"), p, width = 13, height = 14)

### PLOT DELTA SCENARIOS

######################## 20%
combined_plot2 <- delta_shp %>% 
  left_join(prioritized_delta1, by = c("WardName" = "SelectedWards")) %>% 
  mutate(status = ifelse(is.na(WardPopulation), "Not Reprioritized", "Reprioritized"))


p1<- ggplot(data = combined_plot2) +
  geom_sf(aes(geometry = geometry, fill = status)) +
  scale_fill_manual(values = c("Not Reprioritized" = "red", "Reprioritized" = "green"),
                    name = "Status") +
  labs(title = "Reprioritization Scenario 1",
       caption = "Suggested wards are low the lowest-ranking wards under these conditions:
       1. Using composite scores from EVI, u5_tpr, distance to water bodies, settlement type, and flood intensity
       2. Have at least 20% urban area")+
  map_theme()


############## 30%
combined_plot2 <- delta_shp %>% 
  left_join(prioritized_delta2, by = c("WardName" = "SelectedWards")) %>% 
  mutate(status = ifelse(is.na(WardPopulation), "Not Reprioritized", "Reprioritized"))


p2<- ggplot(data = combined_plot2) +
  geom_sf(aes(geometry = geometry, fill = status)) +
  scale_fill_manual(values = c("Not Reprioritized" = "red", "Reprioritized" = "green"),
                    name = "Status") +
  labs(title = "Reprioritization Scenario 2",
       caption = "Suggested wards are low the lowest-ranking wards under these conditions:
       1. Using composite scores from EVI, u5_tpr, distance to water bodies, settlement type, and flood intensity
       2. Have at least 30% urban area")+
  map_theme()

############## 50%
combined_plot2 <- delta_shp %>% 
  left_join(prioritized_delta3, by = c("WardName" = "SelectedWards")) %>% 
  mutate(status = ifelse(is.na(WardPopulation), "Not Reprioritized", "Reprioritized"))


p3<- ggplot(data = combined_plot2) +
  geom_sf(aes(geometry = geometry, fill = status)) +
  scale_fill_manual(values = c("Not Reprioritized" = "red", "Reprioritized" = "green"),
                    name = "Status") +
  labs(title = "Reprioritization Scenario 3",
       caption = "Suggested wards are low the lowest-ranking wards under these conditions:
       1. Using composite scores from EVI, u5_tpr, distance to water bodies, settlement type, and flood intensity
       2. Have at least 50% urban area")+
  map_theme()

############## 75%
combined_plot2 <- delta_shp %>% 
  left_join(prioritized_delta4, by = c("WardName" = "SelectedWards")) %>% 
  mutate(status = ifelse(is.na(WardPopulation), "Not Reprioritized", "Reprioritized"))


p4<- ggplot(data = combined_plot2) +
  geom_sf(aes(geometry = geometry, fill = status)) +
  scale_fill_manual(values = c("Not Reprioritized" = "red", "Reprioritized" = "green"),
                    name = "Status") +
  labs(title = "Reprioritization Scenario 4",
       caption = "Suggested wards are low the lowest-ranking wards under these conditions:
       1. Using composite scores from EVI, u5_tpr, distance to water bodies, settlement type, and flood intensity
       2. Have at least 75% urban area")+
  map_theme()


p_all <- p1 + p2 + p3 + p4
ggsave(paste0(FigDir,"/", Sys.Date(),"_Delta_reprioritzation_maps.pdf"), p_all, width = 13, height = 14)



#########################################################
combined_plot2 <- delta_shp %>% 
  left_join(prioritized_delta1, by = c("WardName" = "SelectedWards")) %>% 
  mutate(status = ifelse(is.na(WardPopulation), "Not Reprioritized", "Reprioritized"))


ggplot(data = combined_plot2) +
  geom_sf(aes(geometry = geometry, fill = status)) +
  scale_fill_manual(values = c("Not Reprioritized" = "red", "Reprioritized" = "green"),
                    name = "Status") +
  labs(title = "Reprioritization in Delta State (Scenario 1)",
       caption = "Suggested wards are low the lowest-ranking wards under these conditions:
       1. Using composite scores from EVI, u5_tpr, rainfall, urbanPercentage, distance to water bodies and housing quality
       2. Have at least 20% urban area")+
  map_theme()


#NIGER

niger_shp <- st_read(file.path(StateShpDir,"Kano", "Kano_State.shp"))

niger_variables <- read.csv(file.path(OutputsDir, "Final Extractions", "niger_plus.csv")) %>% 
  dplyr::select(X, WardName, WardCode, urbanPercentage) %>% 
  mutate(
    classification_20 = ifelse(urbanPercentage > 20, "Urban", "Rural"),
    classification_30 = ifelse(urbanPercentage > 30, "Urban", "Rural"),
    classification_50 = ifelse(urbanPercentage > 50, "Urban", "Rural"),
    classification_75 = ifelse(urbanPercentage > 75, "Urban", "Rural")
  )

niger_itn_data <- read.csv(file.path(ITNDir2, "niger_itn_recoded.csv")) %>% 
  rename(population = `Sum.of.N_FamilyMembers`)


niger_ranks <- read.csv(file.path(OutputsDir, "rankings", "Kano_rankings.csv")) %>% 
  dplyr::select(WardName, WardCode, median_rank)

# niger_ranks$WardName <- str_trim(niger_ranks$WardName)
# niger_ranks$rank <- str_trim(niger_ranks$rank)


combined_wards_ni <- left_join(niger_variables, niger_itn_data, by = c("WardName" = "Ward")) 

combined_wards2_ni <- left_join(combined_wards_ni, niger_ranks, by = "WardCode")

# na_count <- sum(is.na(combined_wards2_ni$population))
# print(na_count)

prioritized_niger <- prioritize_wards(data = combined_wards2_ni, 
                                       population_col = "Population", 
                                       rank_col = "Rank", 
                                       class_col = "classification_20", 
                                       ward_col = "WardName", 
                                       target_percentage = 30) 
write.csv(prioritized_niger, file.path(OutputsDir, "NMEP Presentation Reprioritization Tables", "niger_scenario1.csv"))


combined_plot_ni <-  niger_shp %>% 
  left_join(combined_wards2_ni, by = "WardName")
combined_plot$Rank <- as.numeric(as.character(combined_plot$Rank))

ggplot()+
  geom_sf(data = combined_plot_ni, aes(geometry = geometry, fill = Rank))+
  scale_fill_gradient(na.value = "grey") +
  labs(title = "Risk Map in Kano State") +
  map_theme()


combined_plot2_ni <- niger_shp %>% 
  left_join(prioritized_niger, by = c("WardName" = "SelectedWards")) %>% 
  mutate(status = ifelse(is.na(WardPopulation), "Not Reprioritized", "Reprioritized"))


ggplot(data = combined_plot2_ni) +
  geom_sf(aes(geometry = geometry, fill = status)) +
  scale_fill_manual(values = c("Not Reprioritized" = "red", "Reprioritized" = "green"),
                    name = "Status") +
  labs(title = "Reprioritization in Kano State (Scenario 1)",
       caption = "Suggested wards are low the lowest-ranking wards under these conditions:
       1. Using composite scores from EVI, u5_tpr, rainfall, urbanPercentage, distance to water bodies and housing quality
       2. Have at least 20% urban area")+
  map_theme()



##KANO

kano_shp <- st_read(file.path(StateShpDir,"Kano", "Kano_State.shp"))

kano_variables <- read.csv(file.path(OutputsDir, "Final Extractions", "kano_plus.csv")) %>% 
  dplyr::select(X, WardName, WardCode, urbanPercentage) %>% 
  mutate(
    classification_20 = ifelse(urbanPercentage > 20, "Urban", "Rural"),
    classification_30 = ifelse(urbanPercentage > 30, "Urban", "Rural"),
    classification_50 = ifelse(urbanPercentage > 50, "Urban", "Rural"),
    classification_75 = ifelse(urbanPercentage > 75, "Urban", "Rural")
  )

kano_ranks <- read.csv(file.path(OutputsDir, "rankings", "Kano_rankings.csv")) %>% 
  dplyr::select(WardName, WardCode, median_rank)


kano_itn_data <- readxl::read_excel(
  file.path(ITNDir2, "ITN_distribution_total_ward_kano_2022.xlsx"), sheet = 1) %>% 
  rename(population = `Sum of N_FamilyMembers`)


###manipulate ITN

ITN_coord_file <-file.path(DataDir, "nigeria/ITN_distribution")
ITN_coord <- read_csv(file.path(ITN_coord_file, "pbi_distribution_Kano.csv"))

summary_fam <- ITN_coord %>%  group_by(AdminLevel3) %>%  
  rename(population =N_FamilyMembers) %>% 
  dplyr::select(population, AdminLevel3) %>% 
  summarise(Population = sum(population, na.rm =T))

ITN_coord2 <- sf::st_as_sf(ITN_coord, coords=c('AvgLongitude', 'AvgLatitude'), crs=4326) %>% 
  group_by(AdminLevel3) %>% 
  summarise(geometry = st_union(geometry)) %>%
  st_centroid()

kano_itn_clean <- left_join(ITN_coord2, summary_fam)

##
combined_wards_kn <- left_join(kano_variables, kano_ranks, by = c("WardCode" = "WardCode"))
check_kn <- left_join(kano_shp, combined_wards_kn,  by = c("WardCode" = "WardCode"))
check2 <- st_join(check_kn, kano_itn_clean) %>% 
  distinct(WardCode, .keep_all = T)

na_count <- sum(is.na(check2$Population))
print(na_count)

check3 <- sf::st_drop_geometry(check2)


combined_wards_kn <- left_join(kano_variables, kano_itn_data, by = c("WardName" = "Ward")) 
 #fix multiple names

na_count <- sum(is.na(combined_wards_kn$population))
print(na_count)

combined_wards2_kn <- left_join(combined_wards_kn, kano_ranks, by = "WardCode") %>% 
  distinct(WardCode, .keep_all = T)


###
prioritized_kano <- prioritize_wards(data = combined_wards2_kn, 
                                      population_col = "Population", 
                                      rank_col = "Rank", 
                                      class_col = "classification_20", 
                                      ward_col = "WardName", 
                                      target_percentage = 30) 
write.csv(prioritized_kano, file.path(OutputsDir, "NMEP Presentation Reprioritization Tables", "kano_scenario1.csv"))


combined_plot_kn <-  kano_shp %>% 
  left_join(combined_wards2_kn, by = "WardName")
combined_plot$Rank <- as.numeric(as.character(combined_plot$Rank))

ggplot()+
  geom_sf(data = combined_plot_kn, aes(geometry = geometry, fill = Rank))+
  scale_fill_gradient(na.value = "grey") +
  labs(title = "Risk Map in Kano State") +
  map_theme()


combined_plot2_kn <- kano_shp %>% 
  left_join(prioritized_kano, by = c("WardName" = "SelectedWards")) %>% 
  mutate(status = ifelse(is.na(WardPopulation), "Not Reprioritized", "Reprioritized"))


ggplot(data = combined_plot2_kn) +
  geom_sf(aes(geometry = geometry, fill = status)) +
  scale_fill_manual(values = c("Not Reprioritized" = "red", "Reprioritized" = "green"),
                    name = "Status") +
  labs(title = "Reprioritization in Kano State (Scenario 1)",
       caption = "Suggested wards are low the lowest-ranking wards under these conditions:
       1. Using composite scores from EVI, u5_tpr, rainfall, urbanPercentage, distance to water bodies and housing quality
       2. Have at least 20% urban area")+
  map_theme()


## -----------------------------------------------------------------------------------------------------------------------------------------
### KATSINA
## -----------------------------------------------------------------------------------------------------------------------------------------

# set file paths
StateShpDir <- file.path(ShpfilesDir, "all_reprioritization_nmep_states/STATES")
ITNDir <- file.path(DataDir, "nigeria/ITN_distribution")

# load shapefile
katsina_shp <- st_read(file.path(StateShpDir, "Katsina", "Katsina_State.shp"))

# load and clean variables
katsina_variables <- read.csv(file.path(OutputsDir, "Final Extractions", "katsina_plus.csv")) %>% 
  distinct(WardCode, .keep_all = TRUE) %>%
  dplyr::select(X, WardName.x, urbanPercentage, WardCode) %>% 
  mutate(
    classification_20 = ifelse(urbanPercentage > 20, "Urban", "Rural"),
    classification_30 = ifelse(urbanPercentage > 30, "Urban", "Rural"),
    classification_50 = ifelse(urbanPercentage > 50, "Urban", "Rural"),
    classification_75 = ifelse(urbanPercentage > 75, "Urban", "Rural")
  )

# read in rankings
katsina_ranks <- read.csv(file.path(OutputsDir, "rankings", "Katsina_rankings.csv")) %>%
  dplyr::mutate(WardName = str_trim(WardName), ranks = str_trim(ranks))

# read in ITN data
katsina_itn_data <- readxl::read_excel(file.path(ITNDir, "pbi_distribution_Katsina.xlsx"))

# clean ITN data
katsina_itn_clean <- katsina_itn_data %>% 
  rename(population = N_FamilyMembers,
         Ward = AdminLevel3) %>% 
  dplyr::select(population, Ward) %>% 
  group_by(Ward) %>% 
  summarise(Population = sum(population, na.rm =T))
  

# combine datasets
combined_wards <- left_join(katsina_variables, katsina_ranks, by = "WardCode")
combined_wards2 <- left_join(combined_wards, katsina_itn_clean, by = c("WardName.x" = "Ward")) 

# run reprioritization function - CLASSIFICATION 20 (from pop_estimate_function.R) 
prioritized_katsina_20 <- prioritize_wards(data = combined_wards2, 
                                         population_col = "Population", 
                                         rank_col = "ranks", 
                                         class_col = "classification_20", 
                                         ward_col = "WardName", 
                                         target_percentage = 30) 

# run reprioritization function - CLASSIFICATION 30
prioritized_katsina_30 <- prioritize_wards(data = combined_wards2, 
                                         population_col = "Population", 
                                         rank_col = "ranks", 
                                         class_col = "classification_30", 
                                         ward_col = "WardName", 
                                         target_percentage = 30) 

# run reprioritization function - CLASSIFICATION 50
prioritized_katsina_50 <- prioritize_wards(data = combined_wards2, 
                                         population_col = "Population", 
                                         rank_col = "ranks", 
                                         class_col = "classification_50", 
                                         ward_col = "WardName", 
                                         target_percentage = 30) 

# run reprioritization function - CLASSIFICATION 75
prioritized_katsina_75 <- prioritize_wards(data = combined_wards2, 
                                         population_col = "Population", 
                                         rank_col = "ranks", 
                                         class_col = "classification_75", 
                                         ward_col = "WardName", 
                                         target_percentage = 30) 

# save reprioritization tables
write.csv(prioritized_katsina_20, file.path(OutputsDir, "NMEP Presentation Reprioritization Tables", "Katsina/", "katsina_scenario1_20.csv"))
write.csv(prioritized_katsina_30, file.path(OutputsDir, "NMEP Presentation Reprioritization Tables", "Katsina/", "katsina_scenario2_30.csv"))
write.csv(prioritized_katsina_50, file.path(OutputsDir, "NMEP Presentation Reprioritization Tables", "Katsina/", "katsina_scenario3_50.csv"))
write.csv(prioritized_katsina_75, file.path(OutputsDir, "NMEP Presentation Reprioritization Tables", "Katsina/", "katsina_scenario4_75.csv"))

# convert shapefile wardcode var to integer to match type of combined_wards wardcode var
katsina_shp$WardCode <- as.numeric(katsina_shp$WardCode)

# final join with shapefile for mapping
combined_plot <- katsina_shp %>%
  left_join(combined_wards2, by = "WardCode")

# make ranks var numeric for plotting
combined_plot$ranks <- as.numeric(combined_plot$ranks)

# create risk map
katsina_risk_map <- ggplot()+
  geom_sf(data = combined_plot, aes(geometry = geometry, fill = ranks))+
  scale_fill_gradient(na.value = "grey") +
  labs(title = "Risk Map in Katsina State")
katsina_risk_map

# SCENARIO 1 (20): make df with data from reprioritization function
combined_plot_1_20 <- katsina_shp %>% 
  left_join(prioritized_katsina_20, by = c("WardName" = "SelectedWards")) %>% 
  mutate(status = ifelse(is.na(WardPopulation), "Not Reprioritized", "Reprioritized"))

# SCENARIO 1 (20): create reprioritization map 
katsina_reprioritization_map_20 <- ggplot(data = combined_plot_1_20) +
  geom_sf(aes(geometry = geometry, fill = status)) +
  scale_fill_manual(values = c("Not Reprioritized" = "red", "Reprioritized" = "green"),
                    name = "Status") +
  labs(title = "Reprioritization Scenario 1",
       caption = "Suggested wards are low the lowest-ranking wards under these conditions:
       1. Using composite scores from EVI, u5_tpr, rainfall, urbanPercentage, distance to water bodies and housing quality
       2. Have at least 20% urban area")+
  map_theme()
katsina_reprioritization_map_20

# SCENARIO 2 (30): make df with data from reprioritization function
combined_plot_2_30 <- katsina_shp %>% 
  left_join(prioritized_katsina_30, by = c("WardName" = "SelectedWards")) %>% 
  mutate(status = ifelse(is.na(WardPopulation), "Not Reprioritized", "Reprioritized"))

# SCENARIO 2 (30): create reprioritization map
katsina_reprioritization_map_30 <- ggplot(data = combined_plot_2_30) +
  geom_sf(aes(geometry = geometry, fill = status)) +
  scale_fill_manual(values = c("Not Reprioritized" = "red", "Reprioritized" = "green"),
                    name = "Status") +
  labs(title = "Reprioritization Scenario 2",
       caption = "Suggested wards are low the lowest-ranking wards under these conditions:
       1. Using composite scores from EVI, u5_tpr, rainfall, urbanPercentage, distance to water bodies and housing quality
       2. Have at least 30% urban area")+
  map_theme()
katsina_reprioritization_map_30

# SCENARIO 3 (50): make df with data from reprioritization function
combined_plot_3_50 <- katsina_shp %>% 
  left_join(prioritized_katsina_50, by = c("WardName" = "SelectedWards")) %>% 
  mutate(status = ifelse(is.na(WardPopulation), "Not Reprioritized", "Reprioritized"))

# SCENARIO 3 (50): create reprioritization map
katsina_reprioritization_map_50 <- ggplot(data = combined_plot_3_50) +
  geom_sf(aes(geometry = geometry, fill = status)) +
  scale_fill_manual(values = c("Not Reprioritized" = "red", "Reprioritized" = "green"),
                    name = "Status") +
  labs(title = "Reprioritization Scenario 3",
       caption = "Suggested wards are low the lowest-ranking wards under these conditions:
       1. Using composite scores from EVI, u5_tpr, rainfall, urbanPercentage, distance to water bodies and housing quality
       2. Have at least 50% urban area")+
  map_theme()
katsina_reprioritization_map_50

# SCENARIO 4 (75): make df with data from reprioritization function
combined_plot_4_75 <- katsina_shp %>% 
  left_join(prioritized_katsina_75, by = c("WardName" = "SelectedWards")) %>% 
  mutate(status = ifelse(is.na(WardPopulation), "Not Reprioritized", "Reprioritized"))

# create reprioritization map - SCENARIO 4 (75)
katsina_reprioritization_map_75 <- ggplot(data = combined_plot_4_75) +
  geom_sf(aes(geometry = geometry, fill = status)) +
  scale_fill_manual(values = c("Not Reprioritized" = "red", "Reprioritized" = "green"),
                    name = "Status") +
  labs(title = "Reprioritization Scenario 4",
       caption = "Suggested wards are low the lowest-ranking wards under these conditions:
       1. Using composite scores from EVI, u5_tpr, rainfall, urbanPercentage, distance to water bodies and housing quality
       2. Have at least 75% urban area")+
  map_theme()
katsina_reprioritization_map_75

# combine plots
p_all <- katsina_reprioritization_map_20 + katsina_reprioritization_map_30 + katsina_reprioritization_map_50 + katsina_reprioritization_map_75
p_all

# save plots as PDFs

ggsave(filename = paste0(OutputsDir, "/NMEP Presentation Risk Maps/", "Katsina/", 'katsina_risk.pdf'), plot = katsina_risk_map, width = 6, height = 4)
ggsave(filename = paste0(OutputsDir, "/NMEP Presentation Risk Maps/", "Katsina/", 'katsina_rep_s1_20.pdf'), plot = katsina_reprioritization_map_20, width = 12, height = 8)
ggsave(filename = paste0(OutputsDir, "/NMEP Presentation Risk Maps/", "Katsina/", 'katsina_rep_s2_30.pdf'), plot = katsina_reprioritization_map_30, width = 12, height = 8)
ggsave(filename = paste0(OutputsDir, "/NMEP Presentation Risk Maps/", "Katsina/", 'katsina_rep_s3_50.pdf'), plot = katsina_reprioritization_map_50, width = 12, height = 8)
ggsave(filename = paste0(OutputsDir, "/NMEP Presentation Risk Maps/", "Katsina/", 'katsina_rep_s4_75.pdf'), plot = katsina_reprioritization_map_75, width = 12, height = 8)
ggsave(filename = paste0(OutputsDir, "/NMEP Presentation Risk Maps/", "Katsina/", 'katsina_all.pdf'), plot = p_all, width = 18, height = 12)