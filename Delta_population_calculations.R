### PRIORITZED AREAS
source("load_path.R", echo=F)
source("pop_estimate_function.R", echo=F)

pathLGA <-  file.path(DriveDir, "/data/nigeria/nigeria_shapefiles/Nigeria LGAs shapefile (260216)")


StateShpDir <- file.path(ShpfilesDir, "all_reprioritization_nmep_states/STATES")
Delta_shp <- sf::st_read(file.path(StateShpDir ,"Delta", "Delta_State.shp"))


Delta_lGAshp <- sf::st_read(file.path(pathLGA, "NGA_LGAs.shp"))
sf::sf_use_s2(FALSE)

lga_ward <- sf::st_intersection( Delta_shp, Delta_lGAshp)


Delta_variables <- read.csv(file.path(OutputsDir, "Final Extractions", "Delta_plus.csv")) %>% 
  distinct(WardCode, .keep_all = TRUE) %>%
  dplyr::select(X, WardName, urbanPercentage, WardCode) %>% 
  mutate(
    classification_20 = ifelse(urbanPercentage > 20, "Urban", "Rural"),
    classification_30 = ifelse(urbanPercentage > 30, "Urban", "Rural"),
    classification_50 = ifelse(urbanPercentage > 50, "Urban", "Rural"),
    classification_75 = ifelse(urbanPercentage > 75, "Urban", "Rural")
  )  %>% 
  mutate(num = 1:n())


Delta_ranks <- read.csv(file.path(OutputsDir, "rankings", "Delta_rankings.csv")) 
Delta_ranks$WardName <- stringr::str_trim(Delta_ranks$WardName)
# Delta_ranks$ranks <- stringr::str_trim(Delta_ranks$ranks)

Delta_ranks <- Delta_ranks %>% 
  mutate(num = 1:n())


ITNDir <- file.path(DataDir, "nigeria/ITN_distribution")
Delta_itn_data <- readxl::read_excel(
  file.path(ITNDir, "pbi_distribution_Delta.xlsx"), 
  sheet = 3) 




Delta_itn_clean <- Delta_itn_data %>% 
  rename(population = N_FamilyMembers,
         Ward = AdminLevel3) %>% 
  dplyr::select(population, Ward) %>% 
  group_by(Ward) %>% 
  summarise(Population = sum(population, na.rm =T)) %>% 
  ungroup() %>% 
  mutate(num = 1:n())



combined_wards <- left_join(Delta_variables, Delta_ranks , by = c("WardCode" = "WardCode"))

combined_wards2 <- left_join(combined_wards, Delta_itn_clean, by = c("WardName.x" = "Ward")) 



colums <- c("classification_20", "classification_30", "classification_50", "classification_75")


plots <- list()
prioritized_Delta <- list()

for (ii in seq_along(colums)){
  
  
  prioritized_Delta[[ii]] <- prioritize_wards(data = combined_wards2, 
                                             population_col = "Population", 
                                             rank_col = "ranks", 
                                             class_col = colums[ii], 
                                             ward_col = "WardName.x", 
                                             target_percentage = 30) 
  
  write.csv(prioritized_Delta[[ii]], file.path(OutputsDir, "NMEP Presentation Reprioritization Tables", 
                                              "Delta", paste0("Delta_scenario_", ii, ".csv")))
  
  combined_plot2 <- Delta_shp %>% 
    left_join(prioritized_Delta[[ii]], by = c("WardName" = "SelectedWards")) %>% 
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


# p_all <- plot_grid(plots[[1]], plots[[2]], plots[[3]], plots[[4]], 
#                    ncol = 2,   # Arrange in 2 columns
#                    align = "hv",  # Align horizontally and vertically
#                    labels = "AUTO") 
# 
# FigDir <- file.path(DriveDir,"/projects/urban_microstratification/Shiny App/Plots/deprioritization_scenarios")
# 
# ggsave(paste0(FigDir,"/", Sys.Date(),"_Delta_reprioritization_scenarios.pdf"), p_all, width = 13, height = 14)
