source("load_path.R", echo=F)
source("pop_estimate_function.R", echo=F)

pathLGA <-  file.path(DriveDir, "/data/nigeria/nigeria_shapefiles/Nigeria LGAs shapefile (260216)")

StateShpDir <- file.path(ShpfilesDir, "all_reprioritization_nmep_states/STATES")
Kaduna_shp <- sf::st_read(file.path(StateShpDir ,"kaduna", "Kaduna_State.shp"))


kaduna_lGAshp <- sf::st_read(file.path(pathLGA, "NGA_LGAs.shp"))
sf::sf_use_s2(FALSE)

lga_ward <- sf::st_intersection( Kaduna_shp, kaduna_lGAshp)


Kaduna_named_shp<- st_read(file.path(pathLGA, "NGA_LGAs.shp")) 

Kaduna_shp <- st_transform(Kaduna_shp, st_crs(Kaduna_named_shp))



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

# Kaduna
Kaduna_itn_new <- readxl::read_excel( file.path(ITNDir,"cleaned/pbi_distribution_Kaduna_clean.xlsx"))

Kaduna_itn_old <- readxl::read_excel( file.path(ITNDir, "household_member_ward_summaries_Itn_distribution/ITN_distribution_total_ward_kaduna_2022_recoded.xlsx") ) %>%
  rename(Population = `Sum of N_FamilyMembers`) %>%
  mutate(num = 1:n())


comparison <- list(Kaduna_itn_new, Kaduna_itn_old)


# combineddata_new <- full_join(Kaduna_shp, Kaduna_itn_new, by = c("WardName" = "Ward")) 
# 
# 
# combineddata_old <-  full_join(Kaduna_shp, Kaduna_itn_old, by = c("WardName" = "Ward")) %>% 
#   st_drop_geometry() %>% 
#   filter(is.na(WardCode))

#write.csv(combineddata, "combineddata.csv")

# data_missing_old <- combineddata %>% 
#   filter(is.na(Population.y))
# 
# 
# data_missing_new <- combineddata %>% 
#   filter(is.na(Population.x))


combined_wards <- left_join(Kaduna_variables, Kaduna_ranks , by = c("WardCode" = "WardCode"))


combined_wards2 <- lapply(seq_along(comparison), function (x) 
  left_join(combined_wards, comparison[[x]], by = c("WardName.x" = "Ward")) %>% 
    distinct(WardName.x, urbanPercentage, WardCode, .keep_all = T)) 


colums <- c("classification_50", "classification_75")

ii = 4

prioritized_kaduna <- list()

for (ii in seq_along(colums)){
  
  
  prioritized_kaduna[[ii]] <- lapply(seq_along(combined_wards2), function (x) prioritize_wards(data = combined_wards2[[x]], 
                                               population_col = "Population", 
                                               rank_col = "ranks", 
                                               class_col = colums[ii], 
                                               ward_col = "WardName", 
                                               target_percentage = 30) )
  
}
  
  
  




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


# p_all <- plot_grid(plots[[1]], plots[[2]], plots[[3]], plots[[4]], 
#                    ncol = 2,   # Arrange in 2 columns
#                    align = "hv",  # Align horizontally and vertically
#                    labels = "AUTO") 
# 
# FigDir <- "C:/Users/laure/Urban Malaria Proj Dropbox/urban_malaria/projects/urban_microstratification/Shiny App/Plots/deprioritization_scenarios"
# 
# ggsave(paste0(FigDir,"/", Sys.Date(),"_kaduna_reprioritization_scenarios.pdf"), p_all, width = 13, height = 14)
