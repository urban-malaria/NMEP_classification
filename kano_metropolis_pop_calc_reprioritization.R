### PRIORITZED AREAS

source("~/NMEP_classification/load_path.R")


#Kano

StateShpDir <- file.path(ShpfilesDir, "all_reprioritization_nmep_states/STATES")

Kano_shp <- sf::st_read(file.path(StateShpDir ,"Kano", "Kano_State.shp"))



Kano_shpefile <- sf::st_read("C:/Users/ozodi/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/nigeria_shapefiles/shapefiles/ShinyApp_shapefiles/Kano_Ibadan/Kano-Ibadan.shp") %>% 
  dplyr::filter(StateCode == "KN")

Kano_LGA <- st_read("C:/Users/ozodi/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan/kano_ibadan_shape_files/Kano_metro_sixLGA_shapes/Kano_metro_sixLGA_shapes.shp")

Kano_variables <- inner_join(Kano_shpefile, read.csv(file.path(OutputsDir, "Final Extractions", "Kano_plus.csv")), 
                             by = c("WardCode" = "WardCode", "WardName" = "WardName")) %>% 
  st_drop_geometry()%>%
  dplyr::select(X, WardName, urbanPercentage, WardCode) %>% 
  mutate(
    classification_20 = ifelse(urbanPercentage > 20, "Urban", "Rural"),
    classification_30 = ifelse(urbanPercentage > 30, "Urban", "Rural"),
    classification_50 = ifelse(urbanPercentage > 50, "Urban", "Rural"),
    classification_75 = ifelse(urbanPercentage > 75, "Urban", "Rural")
  )  %>%
  mutate(num = 1:n())

# class50 <- Kano_variables %>%  filter(classification_50== "Urban") 
# class75 <- Kano_variables %>%  filter(classification_75== "Urban") 

Kano_ranks <- read.csv(file.path(OutputsDir, "rankings", "kano_metropolis_rankings.csv")) 

# Ward_predict_data
# 
# read.csv("C:/Users/laure/Downloads/ward_level_prevalence_20241219.csv") %>% 
# inner_join(read.csv("C:/Users/laure/Downloads/ward_level_risk_assessment_updated.csv"), 
#            by = c("wardname" = "wardname")) %>%
# mutate(ranks02 = rank(Mean_Probability, 
#                     ties.method = "first"))

Kano_ranks$WardName <- stringr::str_trim(Kano_ranks$wardname)
# Kano_ranks$ranks <- stringr::str_trim(Kano_ranks$ranks)

Kano_ranks <- Kano_ranks %>% 
  mutate(num = 1:n())


# ITNDir <- file.path(DataDir, "Kano/ITN_distribution")

Kano_itn_data <- readxl::read_excel(
  "C:/Users/ozodi/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/ITN_distribution/pbi_distribution_Kano.xlsx", 
  sheet = 2) 




Kano_itn_clean <- Kano_itn_data %>% 
  rename(population = N_FamilyMembers,
         Ward = AdminLevel3) %>% 
  dplyr::select(population, Ward) %>% 
  group_by(Ward) %>% 
  summarise(Population = sum(population, na.rm =T)) %>% 
  ungroup() %>% 
  mutate(num = 1:n())



combined_wards <- left_join(Kano_variables, Kano_ranks , by = c("WardName" = "WardName"))

combined_wards2 <- left_join(combined_wards, Kano_itn_clean, by = c("WardName" = "Ward")) 



colums <- c("classification_20", "classification_30", "classification_50", "classification_75")


prioritized_kano2 <- prioritize_wards(data = combined_wards2, 
                                         population_col = "Population", 
                                         rank_col = "ranks", 
                                         class_col = "classification_50", 
                                         ward_col = "WardName", 
                                         target_percentage = 30) 


prioritized_kano3 <- prioritize_wards(data = combined_wards2, 
                                      population_col = "Population", 
                                      rank_col = "ranks", 
                                      class_col = "classification_75", 
                                      ward_col = "WardName", 
                                      target_percentage = 30) 

#make datasets for plotting 50 and 75% urban 

#50
ward_50 <- read.csv(file.path(OutputsDir, "NMEP Presentation Reprioritization Tables", 
                              "Kano_metropolis", paste0("kano_scenario_3", ".csv"))) %>%  left_join(Kano_ranks, by = c("SelectedWards" = "WardName"))
priorite_50 <- Kano_shp  %>%  left_join(ward_50, by = c("WardCode")) %>%  filter(!is.na(WardPopulation)) %>% 
  mutate(new_rank = rank(ranks))


p_50<- ggplot(data = Kano_LGA) +
  geom_sf(aes(fill =LGAName)) +
  geom_sf(data = priorite_50, aes(geometry = geometry), fill = "green") +
  # scale_fill_manual(values = c("Not Reprioritized" = "red", "Reprioritized" = "green"),
  #                   name = "Status") +
  geom_text_repel(data = Kano_LGA,
                  aes(label =  LGAName, geometry = geometry),color ='black',
                  stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1, max.overlaps = Inf)+
  labs(title = "Reprioritization Scenario 3",
       caption = "conditions: 1. composite scores from EVI, u5_tpr, distance to water bodies, settlement type, and flood intensity, 
       2. Have at least 50% urban area")+
  map_theme() +
  xlab("")+
  ylab("")

p3_ward = ggplot()+
  geom_sf(data = priorite_50, aes(geometry = geometry), fill = "green")+
  geom_text_repel(data = priorite_50,
                  aes(label =  paste0(new_rank,". ", WardName, " (", round(WardPopulation*1.1, 0), ")"), geometry = geometry),color ='black',
                  stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1, max.overlaps = Inf)+
  map_theme() +
  xlab("")+
  ylab("")

p50_all = p_50 + p3_ward
FigDir <- "C:/Users/ozodi/Urban Malaria Proj Dropbox/urban_malaria/projects/urban_microstratification/Shiny App/NMEP Presentation Risk Maps"
ggsave(paste0(FigDir,"/", "urban_50_75", "/", Sys.Date(),"_Kano_reprioritization_scenarios_50.pdf"), p50_all, width = 13, height = 14)


ward_75 <- read.csv(file.path(OutputsDir, "NMEP Presentation Reprioritization Tables", 
                              "Kano_metropolis", paste0("kano_scenario_4", ".csv"))) %>%  left_join(Kano_ranks, by = c("SelectedWards" = "WardName"))
priorite_75 <- Kano_shp  %>%  left_join(ward_75, by = c("WardCode")) %>%  filter(!is.na(WardPopulation)) %>% 
  mutate(new_rank = rank(ranks))


p_75<- ggplot(data = Kano_LGA) +
  geom_sf(aes(fill =LGAName)) +
  geom_sf(data = priorite_75, aes(geometry = geometry), fill = "green") +
  # scale_fill_manual(values = c("Not Reprioritized" = "red", "Reprioritized" = "green"),
  #                   name = "Status") +
  geom_text_repel(data = Kano_LGA,
                  aes(label =  LGAName, geometry = geometry),color ='black',
                  stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1, max.overlaps = Inf)+
  labs(title = "Reprioritization Scenario 4",
       caption = "conditions: 1. composite scores from EVI, u5_tpr, distance to water bodies, settlement type, and flood intensity, 
       2. Have at least 75% urban area")+
  map_theme() +
  xlab("")+
  ylab("")

p4_ward = ggplot()+
  geom_sf(data = priorite_75, aes(geometry = geometry), fill = "green")+
  geom_text_repel(data = priorite_75,
                  aes(label =  paste0(new_rank,". ", WardName, " (", round(WardPopulation*1.1, 0), ")"), geometry = geometry),color ='black',
                  stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1, max.overlaps = Inf)+
  map_theme() +
  xlab("")+
  ylab("")

p75_all = p_75 + p4_ward
FigDir <- "C:/Users/ozodi/Urban Malaria Proj Dropbox/urban_malaria/projects/urban_microstratification/Shiny App/NMEP Presentation Risk Maps"
ggsave(paste0(FigDir,"/", "urban_50_75", "/", Sys.Date(),"_Kano_reprioritization_scenarios_75.pdf"), p75_all, width = 13, height = 14)

#####################################
plots <- list()
prioritized_Kano <- list()

for (ii in seq_along(colums)){
  
  
  prioritized_Kano[[ii]] <- prioritize_wards(data = combined_wards2, 
                                             population_col = "Population", 
                                             rank_col = "ranks", 
                                             class_col = colums[ii], 
                                             ward_col = "WardName", 
                                             target_percentage = 30) 
  
  write.csv(prioritized_Kano[[ii]], file.path(OutputsDir, "NMEP Presentation Reprioritization Tables",
                                              "Kano_metropolis", paste0("kano_scenario_", ii, ".csv")))
  # 
  # 
  combined_plot2 <- Kano_shpefile %>% 
    left_join(prioritized_Kano[[ii]], by = c("WardName" = "SelectedWards")) %>% 
    mutate(status = ifelse(is.na(WardPopulation), "Not Reprioritized", "Reprioritized"))
  
  
  plots[[ii]] <- ggplot(data = combined_plot2) +
    geom_sf(aes(geometry = geometry, fill = status)) +
    scale_fill_manual(values = c("Not Reprioritized" = "red", "Reprioritized" = "green"),
                      name = "Status") +
    labs(title = "Metropolis")+
    map_theme()
  
  
  
}


p_all <- plot_grid(plots[[1]], plots[[2]], plots[[3]], plots[[4]], 
                   ncol = 2,   # Arrange in 2 columns
                   align = "hv",  # Align horizontally and vertically
                   labels = "AUTO") 

FigDir <- "C:/Users/laure/Urban Malaria Proj Dropbox/urban_malaria/projects/urban_microstratification/Shiny App/NMEP Presentation Risk Maps"

ggsave(paste0(FigDir,"/", Sys.Date(),"_Kano_reprioritization_scenarios.pdf"), p_all, width = 13, height = 14)



colums <- c("classification_20", "classification_30", "classification_50", "classification_75")


plots <- list()
prioritized_Kano <- list()

for (ii in seq_along(colums)){
  
  
  prioritized_Kano[[ii]] <- prioritize_wards(data = combined_wards2, 
                                             population_col = "Population", 
                                             rank_col = "ranks", 
                                             class_col = colums[ii], 
                                             ward_col = "WardName.x", 
                                             target_percentage = 30) 
  
  
  write.csv(prioritized_Kano[[ii]], file.path(OutputsDir, "NMEP Presentation Reprioritization Tables", 
                                              "Kano", paste0("kano_scenario_", ii, ".csv")))
  
  combined_plot2 <- Kano_shp %>% 
    left_join(prioritized_Kano[[ii]], by = c("WardName" = "SelectedWards", "WardCode" = "WardCode")) %>% 
    mutate(status = ifelse(is.na(WardPopulation), "Not Reprioritized", "Reprioritized"))
  
  
  plots[[ii]] <- ggplot(data = combined_plot2) +
    geom_sf(aes(geometry = geometry, fill = status)) +
    scale_fill_manual(values = c("Not Reprioritized" = "red", "Reprioritized" = "green"),
                      name = "Status") +
    labs(title = "Reprioritization Scenario 1",
         caption = "Suggested wards are low the lowest-ranking wards under these conditions:
       1. Using composite scores from EVI, u5_tpr, distance to water bodies, settlement type, and flood intensity
       2. Have at least 20% urban area")+
    map_theme()
  
  
  
}