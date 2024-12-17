### PRIORITZED AREAS

#source("~/NMEP_classification/load_path.R")

library(patchwork)

#Yobe

# StateShpDir <- file.path(ShpfilesDir, "all_reprioritization_nmep_states/STATES")
# 
  Taraba_shp <- st_read(file.path(StateShpDir,"Taraba", "Taraba_State.shp"))
# 
# pathLGA <- "C:/Users/ozodi/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/nigeria_shapefiles/Nigeria LGAs shapefile (260216)"
# 
# Taraba_lGAshp <- st_read(file.path(pathLGA, "NGA_LGAs.shp"))
# sf_use_s2(FALSE)
# 
# lga_ward <- st_intersection( Taraba_shp, Taraba_lGAshp)
# 
# 
# Taraba_variables <- read.csv(file.path(OutputsDir, "Final Extractions", "Taraba_plus.csv")) %>% 
#   distinct(WardCode, .keep_all = TRUE) %>%
#   dplyr::select(X, WardName, urbanPercentage, WardCode) %>% 
#   mutate(
#     classification_20 = ifelse(urbanPercentage > 20, "Urban", "Rural"),
#     classification_30 = ifelse(urbanPercentage > 30, "Urban", "Rural"),
#     classification_50 = ifelse(urbanPercentage > 50, "Urban", "Rural"),
#     classification_75 = ifelse(urbanPercentage > 75, "Urban", "Rural")
#   ) 
# 
# 
# Taraba_ranks <- read.csv(file.path(OutputsDir, "rankings", "Taraba_rankings.csv")) 
# Taraba_ranks$WardName <- str_trim(Taraba_ranks$WardName)
# Taraba_ranks$ranks <- str_trim(Taraba_ranks$ranks)
# 
# 
# ITNDir <- file.path(DataDir, "nigeria/ITN_distribution/household_member_ward_summaries_Itn_distribution")
# Taraba_itn_data <- readxl::read_excel(
#   file.path(ITNDir, "ITN_distribution_total_ward_taraba_2022.xlsx"))
# 
# ITN_coord_file <-"C:/Users/ozodi/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/ITN_distribution"
# ITN_coord <- read_csv(
#   file.path(ITN_coord_file, "taraba_itn_2022.csv"))
# 
# summary_fam <- ITN_coord %>%  group_by(AdminLevel3) %>%  
#   rename(population =N_FamilyMembers) %>% 
#   dplyr::select(population, AdminLevel3) %>% 
#   summarise(Population = sum(population, na.rm =T)) 
# 
# 
# ITN_coord2 <- sf::st_as_sf(ITN_coord, coords=c('AvgLongitude', 'AvgLatitude'), crs=4326) %>% 
#   group_by(AdminLevel3) %>% 
#   summarise(geometry = st_union(geometry)) %>%
#  st_centroid()
# 
# Tarba_itn_clean <- left_join(ITN_coord2, summary_fam)
# 
# 
# 
# 
# combined_wards <- left_join(Taraba_variables, Taraba_ranks, by = c("WardCode" = "WardCode"))
# 
# check <- left_join(Taraba_shp, combined_wards,  by = c("WardCode" = "WardCode"))
# 
# check2 <- st_join(check, Tarba_itn_clean)
# 
# check3 <- sf::st_drop_geometry(check2)
# 
# write.csv(check3, "taraba_pop_all.csv")

combined_wards2<- read.csv("taraba_pop_all.csv")


#combined_wards2 <- left_join(combined_wards, Taraba_itn_clean, by = c("WardName" = "Ward")) 

glimpse(combined_wards2)
prioritized_taraba1 <- prioritize_wards(data = combined_wards2, 
                                       population_col = "Population", 
                                       rank_col = "ranks", 
                                       class_col = "classification_20", 
                                       ward_col = "WardName", 
                                       target_percentage = 30) 

prioritized_taraba2 <- prioritize_wards(data = combined_wards2, 
                                       population_col = "Population", 
                                       rank_col = "ranks", 
                                       class_col = "classification_30", 
                                       ward_col = "WardName", 
                                       target_percentage = 30) 



prioritized_taraba3 <- prioritize_wards(data = combined_wards2, 
                                        population_col = "Population", 
                                        rank_col = "ranks", 
                                        class_col = "classification_50", 
                                        ward_col = "WardName", 
                                        target_percentage = 30) 


prioritized_taraba4 <- prioritize_wards(data = combined_wards2, 
                                        population_col = "Population", 
                                        rank_col = "ranks", 
                                        class_col = "classification_75", 
                                        ward_col = "WardName", 
                                        target_percentage = 30) 


combined_plot <-  Taraba_shp %>% 
  left_join(combined_wards2, by = "WardCode")
combined_plot$Rank <- as.numeric(as.character(combined_plot$ranks))


p<- ggplot()+
  geom_sf(data = combined_plot, aes(geometry = geometry, fill = ranks))+
  scale_fill_gradient(na.value = "grey") +
  labs(title = "Risk Map in Taraba State") +
  map_theme()

FigDir <- "C:/Users/ozodi/Urban Malaria Proj Dropbox/urban_malaria/projects/urban_microstratification/Shiny App/NMEP Presentation Risk Maps/Taraba/deprioritization_scenarios"

ggsave(paste0(FigDir,"/", Sys.Date(),"_Taraba_risk_map.pdf"), p, width = 13, height = 14)

######################## 20%
combined_plot2 <- Taraba_shp %>% 
  left_join(prioritized_taraba1, by = c("WardName" = "SelectedWards")) %>% 
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
combined_plot2 <- Taraba_shp %>% 
  left_join(prioritized_taraba2, by = c("WardName" = "SelectedWards")) %>% 
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
combined_plot2 <- Taraba_shp %>% 
  left_join(prioritized_taraba3, by = c("WardName" = "SelectedWards")) %>% 
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
combined_plot2 <- Taraba_shp %>% 
  left_join(prioritized_taraba4, by = c("WardName" = "SelectedWards")) %>% 
  mutate(status = ifelse(is.na(WardPopulation), "Not Reprioritized", "Reprioritized"))


p4<- ggplot(data = combined_plot2) +
  geom_sf(aes(geometry = geometry, fill = status)) +
  scale_fill_manual(values = c("Not Reprioritized" = "red", "Reprioritized" = "green"),
                    name = "Status") +
  labs(title = "Reprioritization Scenario 4",
       caption = "Suggested wards are low the lowest-ranking wards under these conditions:
       1. Using composite scores from EVI, u5_tpr, distance to water bodies, settlement type, and flood intensity
       2. Have at least 70% urban area")+
  map_theme()

p_all <- p1 + p2 + p3 + p4

FigDir <- "C:/Users/ozodi/Urban Malaria Proj Dropbox/urban_malaria/projects/urban_microstratification/Shiny App/Plots/deprioritization_scenarios"

ggsave(paste0(FigDir,"/", Sys.Date(),"_Taraba_reprioritization_scenarios.pdf"), p_all, width = 13, height = 14)
