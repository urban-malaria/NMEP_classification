source("load_path.R", echo=F)
source("pop_estimate_function.R", echo=F)

pathLGA <-  file.path(DriveDir, "/data/nigeria/nigeria_shapefiles/Nigeria LGAs shapefile (260216)")


StateShpDir <- file.path(ShpfilesDir, "all_reprioritization_nmep_states/STATES")

Kaduna_shp <- sf::st_read(file.path(StateShpDir ,"kaduna", "Kaduna_State.shp"))


lGAshp <- sf::st_read(file.path(pathLGA, "NGA_LGAs.shp"))
sf::sf_use_s2(FALSE)

lga_ward <- sf::st_intersection( Kaduna_shp, lGAshp)


Kaduna_variables <- read.csv(file.path(OutputsDir, "Final Extractions", "Kaduna_plus.csv")) %>% 
  distinct(WardCode, .keep_all = TRUE) %>%
  dplyr::select(X, WardName.x, urbanPercentage, WardCode) %>% 
  mutate(
    classification_20 = ifelse(urbanPercentage > 20, "Urban", "Rural"),
    classification_30 = ifelse(urbanPercentage > 30, "Urban", "Rural"),
    classification_50 = ifelse(urbanPercentage > 50, "Urban", "Rural"),
    classification_75 = ifelse(urbanPercentage > 75, "Urban", "Rural")
  )  %>% 
  mutate(num = 1:n())


Kaduna_ranks <- read.csv(file.path(OutputsDir, "rankings", "Kaduna_rankings.csv")) 
Kaduna_ranks$WardName <- stringr::str_trim(Kaduna_ranks$WardName)

Kaduna_ranks <- Kaduna_ranks %>% 
  mutate(num = 1:n())


ITNDir <- file.path(DataDir, "nigeria/ITN_distribution")
Kaduna_itn_data <- readxl::read_excel(
  file.path(ITNDir, "pbi_distribution_Kaduna.xlsx"), 
  sheet = 2) 




Kaduna_itn_clean <- Kaduna_itn_data %>% 
  rename(population = N_FamilyMembers,
         Ward = AdminLevel3) %>% 
  dplyr::select(population, Ward) %>% 
  group_by(Ward) %>% 
  summarise(Population = sum(population, na.rm =T)) %>% 
  ungroup() %>% 
  mutate(num = 1:n())



combined_wards <- left_join(Kaduna_variables, Kaduna_ranks , by = c("WardCode" = "WardCode"))

combined_wards2 <- left_join(combined_wards, Kaduna_itn_clean, by = c("WardName.x" = "Ward")) 


colums <- c("classification_20", "classification_30", "classification_50", "classification_75")

# ii = 3
plots <- list()
prioritized_kaduna <- list()

for (ii in seq_along(colums)){
  
  
  prioritized_kaduna[[ii]] <- prioritize_wards(data = combined_wards2, 
                                               population_col = "Population", 
                                               rank_col = "ranks", 
                                               class_col = colums[ii], 
                                               ward_col = "WardName", 
                                               target_percentage = 30) 
  
  
  write.csv(prioritized_kaduna[[ii]], file.path(OutputsDir, "NMEP Presentation Reprioritization Tables", 
                                              "Kaduna", paste0("kaduna_scenario_", ii, ".csv")))
  
  combined_plot2 <- Kaduna_shp %>% 
    left_join(prioritized_kaduna[[ii]], by = c("WardName" = "SelectedWards")) %>% 
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


#make datasets for plotting 50 and 75% urban 

#50
ward_50 <- read.csv(file.path(OutputsDir, "NMEP Presentation Reprioritization Tables", 
                              "Kaduna", paste0("kaduna_scenario_3", ".csv"))) %>%  left_join(Kaduna_ranks, by = c("WardCode"))
priorite_50 <- Kaduna_shp  %>%  left_join(ward_50, by = c("WardCode")) %>%  filter(!is.na(WardPopulation)) %>% 
  mutate(new_rank = rank(ranks))


p_50<- ggplot(data = filter(lGAshp, State == "Kaduna")) +
  geom_sf(fill = "red") +
  geom_sf(data = priorite_50, aes(geometry = geometry), fill = "green") +
  # scale_fill_manual(values = c("Not Reprioritized" = "red", "Reprioritized" = "green"),
  #                   name = "Status") +
  geom_text_repel(data = filter(lGAshp, State == "Kaduna"),
                  aes(label =  LGA, geometry = geometry),color ='black',
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
                  aes(label =  paste0(new_rank,". ", WardName.x, " (", round(WardPopulation*1.1, 0), ")"), geometry = geometry),color ='black',
                  stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1, max.overlaps = Inf)+
  map_theme() +
  xlab("")+
  ylab("")

p50_all = p_50 + p3_ward

#75

ward_75 <- read.csv(file.path(OutputsDir, "NMEP Presentation Reprioritization Tables", 
                              "Kaduna", paste0("kaduna_scenario_4", ".csv"))) %>%  left_join(Kaduna_ranks, by = c("WardCode"))
priorite_75 <- Kaduna_shp  %>%  left_join(ward_75, by = c("WardCode")) %>%  filter(!is.na(WardPopulation)) %>% 
  mutate(new_rank = rank(ranks))


p_75<- ggplot(data = filter(lGAshp, State == "Kaduna")) +
  geom_sf(fill = "red") +
  geom_sf(data = priorite_75, aes(geometry = geometry), fill = "green") +
  # scale_fill_manual(values = c("Not Reprioritized" = "red", "Reprioritized" = "green"),
  #                   name = "Status") +
  geom_text_repel(data = filter(lGAshp, State == "Kaduna"),
                  aes(label =  LGA, geometry = geometry),color ='black',
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
                  aes(label =  paste0(new_rank,". ", WardName.x, " (", round(WardPopulation*1.1, 0), ")"), geometry = geometry),color ='black',
                  stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1, max.overlaps = Inf)+
  map_theme() +
  xlab("")+
  ylab("")

p75_all = p_75 + p4_ward

p_high_urban = p50_all/p75_all

FigDir <- "C:/Users/ozodi/Urban Malaria Proj Dropbox/urban_malaria/projects/urban_microstratification/Shiny App/NMEP Presentation Risk Maps"
ggsave(paste0(FigDir,"/", "urban_50_75", "/", Sys.Date(),"_Kaduna_reprioritization_scenarios.pdf"), p_high_urban, width = 13, height = 14)

