### PRIORITZED AREAS

source("~/NMEP_classification/load_path.R")


#Yobe

StateShpDir <- file.path(ShpfilesDir, "all_reprioritization_nmep_states/STATES")

Kano_shp <- sf::st_read(file.path(StateShpDir ,"Kano", "Kano_State.shp"))



Kano_shpefile <- sf::st_read("C:/Users/laure/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/nigeria_shapefiles/shapefiles/ShinyApp_shapefiles/Kano_Ibadan/Kano-Ibadan.shp") %>% 
  dplyr::filter(StateCode == "KN")


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


Kano_ranks <- ward_map

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
  "C:/Users/laure/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/ITN_distribution/pbi_distribution_Kano.xlsx", 
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


plots <- list()
prioritized_Kano <- list()

for (ii in seq_along(colums)){
  
  
  prioritized_Kano[[ii]] <- prioritize_wards(data = combined_wards2, 
                                              population_col = "Population", 
                                              rank_col = "ranks", 
                                              class_col = colums[ii], 
                                              ward_col = "WardName", 
                                              target_percentage = 30) 
  
  # write.csv(prioritized_Kano[[ii]], file.path(OutputsDir, "NMEP Presentation Reprioritization Tables",
  #                                             "Kano_metropolis", paste0("kano_scenario_", ii, ".csv")))
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



