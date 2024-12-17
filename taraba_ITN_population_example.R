### PRIORITZED AREAS

#source("~/NMEP_classification/load_path.R")


#Yobe

StateShpDir <- file.path(ShpfilesDir, "all_reprioritization_nmep_states/STATES")

Taraba_shp <- st_read(file.path(StateShpDir,"Taraba", "Taraba_State.shp"))

pathLGA <- "C:/Users/ozodi/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/nigeria_shapefiles/Nigeria LGAs shapefile (260216)"

Taraba_lGAshp <- st_read(file.path(pathLGA, "NGA_LGAs.shp"))
sf_use_s2(FALSE)

lga_ward <- st_intersection( Taraba_shp, Taraba_lGAshp)


Taraba_variables <- read.csv(file.path(OutputsDir, "Final Extractions", "Taraba_plus.csv")) %>% 
  distinct(WardCode, .keep_all = TRUE) %>%
  dplyr::select(X, WardName, urbanPercentage, WardCode) %>% 
  mutate(
    classification_20 = ifelse(urbanPercentage > 20, "Urban", "Rural"),
    classification_30 = ifelse(urbanPercentage > 30, "Urban", "Rural"),
    classification_50 = ifelse(urbanPercentage > 50, "Urban", "Rural"),
    classification_75 = ifelse(urbanPercentage > 75, "Urban", "Rural")
  ) 


Taraba_ranks <- read.csv(file.path(OutputsDir, "rankings", "Taraba_rankings.csv")) 
Taraba_ranks$WardName <- str_trim(Taraba_ranks$WardName)
Taraba_ranks$ranks <- str_trim(Taraba_ranks$ranks)


ITNDir <- file.path(DataDir, "nigeria/ITN_distribution/household_member_ward_summaries_Itn_distribution")
Taraba_itn_data <- readxl::read_excel(
  file.path(ITNDir, "ITN_distribution_total_ward_taraba_2022.xlsx"))

ITN_coord_file <-"C:/Users/ozodi/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/ITN_distribution"
ITN_coord <- read_csv(
  file.path(ITN_coord_file, "taraba_itn_2022.csv"))

summary_fam <- ITN_coord %>%  group_by(AdminLevel3) %>%  
  rename(population =N_FamilyMembers) %>% 
  dplyr::select(population, AdminLevel3) %>% 
  summarise(Population = sum(population, na.rm =T)) 


ITN_coord2 <- sf::st_as_sf(ITN_coord, coords=c('AvgLongitude', 'AvgLatitude'), crs=4326) %>% 
  group_by(AdminLevel3) %>% 
  summarise(geometry = st_union(geometry)) %>%
 st_centroid()

Tarba_itn_clean <- left_join(ITN_coord2, summary_fam)




combined_wards <- left_join(Taraba_variables, Taraba_ranks, by = c("WardCode" = "WardCode"))

check <- left_join(Taraba_shp, combined_wards,  by = c("WardCode" = "WardCode"))

check2 <- st_join(check, Tarba_itn_clean)

check3 <- sf::st_drop_geometry(check2)

write.csv(check3, "taraba_pop_all.csv")

combined_wards2<- read.csv("taraba_pop_all.csv")
data<- read.csv("taraba_pop_all.csv")
data$Population <- data$population_col
data$Population <- data$population_col

#combined_wards2 <- left_join(combined_wards, Taraba_itn_clean, by = c("WardName" = "Ward")) 

glimpse(combined_wards2)
prioritized_delta1 <- prioritize_wards(data = combined_wards2, 
                                       population_col = "Population", 
                                       rank_col = "ranks", 
                                       class_col = "classification_20", 
                                       ward_col = "WardName", 
                                       target_percentage = 30) 

prioritized_delta2 <- prioritize_wards(data = combined_wards2, 
                                       population_col = "Population", 
                                       rank_col = "Rank", 
                                       class_col = "classification_30", 
                                       ward_col = "WardName", 
                                       target_percentage = 30) 


combined_plot <-  delta_shp %>% 
  left_join(combined_wards2, by = "WardName")
combined_plot$Rank <- as.numeric(as.character(combined_plot$Rank))


ggplot()+
  geom_sf(data = combined_plot, aes(geometry = geometry, fill = Rank))+
  scale_fill_gradient(na.value = "grey") +
  labs(title = "Risk Map in Delta State") +
  map_theme()


combined_plot2 <- delta_shp %>% 
  left_join(prioritized_delta1, by = c("WardName" = "SelectedWards")) %>% 
  mutate(status = ifelse(is.na(WardPopulation), "Not Reprioritized", "Reprioritized"))


ggplot(data = combined_plot2) +
  geom_sf(aes(geometry = geometry, fill = status)) +
  scale_fill_manual(values = c("Not Reprioritized" = "red", "Reprioritized" = "green"),
                    name = "Status") +
  labs(title = "Reprioritization Scenario 1",
       caption = "Suggested wards are low the lowest-ranking wards under these conditions:
       1. Using composite scores from EVI, u5_tpr, rainfall, urbanPercentage, distance to water bodies and housing quality
       2. Have at least 20% urban area")+
  map_theme()




