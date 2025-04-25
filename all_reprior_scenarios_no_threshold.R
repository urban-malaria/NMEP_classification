# ==========================================================================================================================================
# Script Name: Compiled code to run reprioritization for Niger, Kano State, Katsina, Kaduna, Taraba, and Yobe (Delta was removed from the manuscript)
# this code runs it without the 30% reprioritization threshold (for comparison purposes) and only the 50% and 75% scenarios
# Author: Grace Legris, Research Data Analyst, gracebea@gmail.com
# ==========================================================================================================================================

source("load_path.R")
source("pop_estimate_function.R", echo = FALSE)

# load spatial data for niger
niger_shp <- st_read(file.path(StateShpDir, "Niger", "Niger_State.shp"))

# load and process niger variables
niger_variables <- read.csv(file.path(OutputsDir, "Final Extractions", "niger_plus.csv")) %>% 
  dplyr::select(X, WardName, WardCode, urbanPercentage) %>% 
  mutate(
    classification_20 = ifelse(urbanPercentage > 20, "Urban", "Rural"),
    classification_30 = ifelse(urbanPercentage > 30, "Urban", "Rural"),
    classification_50 = ifelse(urbanPercentage > 50, "Urban", "Rural"),
    classification_75 = ifelse(urbanPercentage > 75, "Urban", "Rural")
  )

niger_itn_clean <- read_excel(file.path("/Users/grace/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/ITN_distribution/household_member_ward_summaries_Itn_distribution/ITN_distribution_total_ward_niger_2022.xlsx"))

niger_itn_clean <- niger_itn_clean %>%
  rename(population = `Sum of N_FamilyMembers`) %>%
  dplyr::select(population, Ward) %>%
  group_by(Ward) %>%
  summarise(Population = sum(population, na.rm = T)) %>%
  ungroup()
  
# read in ranks data for niger
niger_ranks <- read.csv(file.path(OutputsDir, "rankings", "Niger_rankings.csv")) %>% 
  dplyr::select(WardName, WardCode, median_rank, ranks)

# merge datasets for niger
combined_wards2_ni <- niger_variables %>% 
  left_join(niger_itn_clean, by = c("WardName" = "Ward")) %>% 
  left_join(niger_ranks, by = "WardCode")

# prioritize wards for niger
prioritized_niger_3 <- prioritize_wards(
  data = combined_wards2_ni, 
  population_col = "Population", 
  rank_col = "ranks", 
  class_col = "classification_50", 
  ward_col = "WardName.x",
  target_percentage = 20
)
prioritized_niger_4 <- prioritize_wards(
  data = combined_wards2_ni, 
  population_col = "Population", 
  rank_col = "ranks", 
  class_col = "classification_75", 
  ward_col = "WardName.x",
  target_percentage = 30
)

# save prioritized wards
# write.csv(prioritized_niger_3, file.path(OutputsDir, "NMEP Presentation Reprioritization Tables", "Niger", "niger_scenario3_no_threshold.csv"))
# write.csv(prioritized_niger_4, file.path(OutputsDir, "NMEP Presentation Reprioritization Tables", "Niger", "niger_scenario4_no_threshold.csv"))

## =========================================================================================================================================
### KANO
## =========================================================================================================================================

# load spatial data for kano
kano_shp <- st_read(file.path(StateShpDir, "Kano", "Kano_State.shp"))

# load and process kano variables
kano_variables <- read.csv(file.path(OutputsDir, "Final Extractions", "kano_plus.csv")) %>% 
  dplyr::select(X, WardName, WardCode, urbanPercentage) %>% 
  mutate(
    classification_20 = ifelse(urbanPercentage > 20, "Urban", "Rural"),
    classification_30 = ifelse(urbanPercentage > 30, "Urban", "Rural"),
    classification_50 = ifelse(urbanPercentage > 50, "Urban", "Rural"),
    classification_75 = ifelse(urbanPercentage > 75, "Urban", "Rural")
  )

# load rankings for kano
kano_ranks <- read.csv(file.path(OutputsDir, "rankings", "Kano_rankings.csv"))

# load ITN data for kano
kano_itn_clean <- read_excel(file.path("/Users/grace/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/ITN_distribution/household_member_ward_summaries_Itn_distribution/ITN_distribution_total_ward_kano_2022.xlsx"))

kano_itn_clean <- kano_itn_clean %>%
  rename(population = `Sum of N_FamilyMembers`) %>%
  dplyr::select(population, Ward) %>%
  group_by(Ward) %>%
  summarise(Population = sum(population, na.rm = T)) %>%
  ungroup()

# merge datasets for kano
combined_wards2_kn <- kano_variables %>% 
  left_join(kano_itn_clean, by = c("WardName" = "Ward")) %>% 
  left_join(kano_ranks, by = "WardCode") %>% 
  distinct(WardCode, .keep_all = T)

# prioritize wards for kano
prioritized_kano_3 <- prioritize_wards(
  data = combined_wards2_kn, 
  population_col = "Population", 
  rank_col = "ranks", 
  class_col = "classification_50", 
  ward_col = "WardName.x",
  target_percentage = 30
)
prioritized_kano_4 <- prioritize_wards(
  data = combined_wards2_kn, 
  population_col = "Population", 
  rank_col = "ranks", 
  class_col = "classification_75", 
  ward_col = "WardName.x",
  target_percentage = 30
)

# save prioritized wards
# write.csv(prioritized_kano_3, file.path(OutputsDir, "NMEP Presentation Reprioritization Tables", "Kano", "kano_scenario3_no_threshold.csv"))
# write.csv(prioritized_kano_4, file.path(OutputsDir, "NMEP Presentation Reprioritization Tables", "Kano", "kano_scenario4_no_threshold.csv"))

## =========================================================================================================================================
### KATSINA
## =========================================================================================================================================

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

# load ITN data for Katsina
katsina_itn_clean <- read_excel(file.path("/Users/grace/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/ITN_distribution/household_member_ward_summaries_Itn_distribution/ITN_distribution_total_ward_katsina_2022.xlsx"))

katsina_itn_clean <- katsina_itn_clean %>%
  rename(population = `Sum of N_FamilyMembers`) %>%
  dplyr::select(population, Ward) %>%
  group_by(Ward) %>%
  summarise(Population = sum(population, na.rm = T)) %>%
  ungroup()


# combine datasets
combined_wards <- left_join(katsina_variables, katsina_ranks, by = "WardCode")
combined_wards2 <- left_join(combined_wards, katsina_itn_clean, by = c("WardName.x" = "Ward")) 

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
# write.csv(prioritized_katsina_50, file.path(OutputsDir, "NMEP Presentation Reprioritization Tables", "Katsina/", "katsina_scenario_3_no_threshold.csv"))
# write.csv(prioritized_katsina_75, file.path(OutputsDir, "NMEP Presentation Reprioritization Tables", "Katsina/", "katsina_scenario_4_no_threshold.csv"))

## =========================================================================================================================================
### KADUNA
## =========================================================================================================================================

# load shapefile
kaduna_shp <- st_read(file.path(StateShpDir, "Kaduna", "Kaduna_State.shp"))

# load and clean variables
kaduna_variables <- read.csv(file.path(OutputsDir, "Final Extractions", "kaduna_plus.csv")) %>% 
  distinct(WardCode, .keep_all = TRUE) %>%
  dplyr::select(X, WardName.x, urbanPercentage, WardCode) %>% 
  mutate(
    classification_20 = ifelse(urbanPercentage > 20, "Urban", "Rural"),
    classification_30 = ifelse(urbanPercentage > 30, "Urban", "Rural"),
    classification_50 = ifelse(urbanPercentage > 50, "Urban", "Rural"),
    classification_75 = ifelse(urbanPercentage > 75, "Urban", "Rural")
  )

# read in rankings
kaduna_ranks <- read.csv(file.path(OutputsDir, "rankings", "Kaduna_rankings.csv")) %>%
  dplyr::mutate(WardName = str_trim(WardName), ranks = str_trim(ranks))

# load ITN data for Katsina
kaduna_itn_clean <- read_excel(file.path("/Users/grace/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/ITN_distribution/household_member_ward_summaries_Itn_distribution/ITN_distribution_total_ward_kaduna_2022.xlsx"))

kaduna_itn_clean <- kaduna_itn_clean %>%
  rename(population = `Sum of N_FamilyMembers`) %>%
  dplyr::select(population, Ward) %>%
  group_by(Ward) %>%
  summarise(Population = sum(population, na.rm = T)) %>%
  ungroup()

# combine datasets
combined_wards <- left_join(kaduna_variables, kaduna_ranks, by = "WardCode")
combined_wards2 <- left_join(combined_wards, kaduna_itn_clean, by = c("WardName.x" = "Ward")) 

# run reprioritization function - CLASSIFICATION 50
prioritized_kaduna_50 <- prioritize_wards(data = combined_wards2, 
                                           population_col = "Population", 
                                           rank_col = "ranks", 
                                           class_col = "classification_50", 
                                           ward_col = "WardName",
                                           target_percentage = 30)

# run reprioritization function - CLASSIFICATION 75
prioritized_kaduna_75 <- prioritize_wards(data = combined_wards2, 
                                           population_col = "Population", 
                                           rank_col = "ranks", 
                                           class_col = "classification_75", 
                                           ward_col = "WardName",
                                           target_percentage = 30)

# save reprioritization tables
# write.csv(prioritized_katsina_50, file.path(OutputsDir, "NMEP Presentation Reprioritization Tables", "Katsina/", "katsina_scenario_3_no_threshold.csv"))
# write.csv(prioritized_katsina_75, file.path(OutputsDir, "NMEP Presentation Reprioritization Tables", "Katsina/", "katsina_scenario_4_no_threshold.csv"))

## =========================================================================================================================================
### TARABA
## =========================================================================================================================================

# load shapefile
taraba_shp <- st_read(file.path(StateShpDir, "Taraba", "Taraba_State.shp"))

# load and clean variables
taraba_variables <- read.csv(file.path(OutputsDir, "Final Extractions", "taraba_plus.csv")) %>%
  distinct(WardCode, .keep_all = TRUE) %>%
  dplyr::select(X, WardName, urbanPercentage, WardCode) %>%
  mutate(
    classification_20 = ifelse(urbanPercentage > 20, "Urban", "Rural"),
    classification_30 = ifelse(urbanPercentage > 30, "Urban", "Rural"),
    classification_50 = ifelse(urbanPercentage > 50, "Urban", "Rural"),
    classification_75 = ifelse(urbanPercentage > 75, "Urban", "Rural")
  )

# read in rankings
taraba_ranks <- read.csv(file.path(OutputsDir, "rankings", "Taraba_rankings.csv")) %>%
  dplyr::mutate(WardName = str_trim(WardName), ranks = str_trim(ranks))

# load ITN data for Taraba
taraba_itn_clean <- read_excel(file.path("/Users/grace/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/ITN_distribution/household_member_ward_summaries_Itn_distribution/ITN_distribution_total_ward_taraba_2022.xlsx"))

taraba_itn_clean <- taraba_itn_clean %>%
  rename(population = `Sum of N_FamilyMembers`) %>%
  dplyr::select(population, Ward) %>%
  group_by(Ward) %>%
  summarise(Population = sum(population, na.rm = T)) %>%
  ungroup()

# combine datasets
combined_wards <- left_join(taraba_variables, taraba_ranks, by = "WardCode")
combined_wards2 <- left_join(combined_wards, taraba_itn_clean, by = c("WardName.x" = "Ward"))

# run reprioritization function - CLASSIFICATION 50
prioritized_taraba_50 <- prioritize_wards(data = combined_wards2,
                                          population_col = "Population",
                                          rank_col = "ranks",
                                          class_col = "classification_50",
                                          ward_col = "WardName.x",
                                          target_percentage = 30)

# run reprioritization function - CLASSIFICATION 75
prioritized_taraba_75 <- prioritize_wards(data = combined_wards2,
                                          population_col = "Population",
                                          rank_col = "ranks",
                                          class_col = "classification_75",
                                          ward_col = "WardName.x",
                                          target_percentage = 30)

# save reprioritization tables
# write.csv(prioritized_taraba_50, file.path(OutputsDir, "NMEP Presentation Reprioritization Tables", "Taraba/", "taraba_scenario_3_no_threshold.csv"))
# write.csv(prioritized_taraba_75, file.path(OutputsDir, "NMEP Presentation Reprioritization Tables", "Taraba/", "taraba_scenario_4_no_threshold.csv"))


## =========================================================================================================================================
### YOBE
## =========================================================================================================================================

# read shapefiles for Yobe state and LGAs
yobe_shp <- sf::st_read(file.path(StateShpDir, "Yobe", "Yobe_State.shp"))
#yobe_lga_shp <- sf::st_read(file.path(pathLGA, "NGA_LGAs.shp"))
sf::sf_use_s2(FALSE)

# perform spatial intersection to link wards to LGAs
#lga_ward <- sf::st_intersection(yobe_shp, yobe_lga_shp)

# read and process Yobe variables
yobe_variables <- read.csv(file.path(OutputsDir, "Final Extractions", "Yobe_plus.csv")) %>% 
  distinct(WardCode, .keep_all = TRUE) %>%
  dplyr::select(X, WardName, urbanPercentage, WardCode) %>% 
  mutate(
    classification_20 = ifelse(urbanPercentage > 20, "Urban", "Rural"),
    classification_30 = ifelse(urbanPercentage > 30, "Urban", "Rural"),
    classification_50 = ifelse(urbanPercentage > 50, "Urban", "Rural"),
    classification_75 = ifelse(urbanPercentage > 75, "Urban", "Rural")
  ) %>% 
  mutate(num = row_number())

# read and process Yobe rankings
yobe_ranks <- read.csv(file.path(OutputsDir, "rankings", "Yobe_rankings.csv")) 
yobe_ranks$WardName <- stringr::str_trim(yobe_ranks$WardName)

yobe_ranks <- yobe_ranks %>% 
  mutate(num = row_number())

# load ITN data for Yobe
yobe_itn_clean <- read_excel(file.path("/Users/grace/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/ITN_distribution/household_member_ward_summaries_Itn_distribution/ITN_distribution_total_ward_yobe_2022_recoded.xlsx"))

yobe_itn_clean <- yobe_itn_clean %>%
  rename(population = `Sum of N_FamilyMembers`) %>%
  dplyr::select(population, Ward) %>%
  group_by(Ward) %>%
  summarise(Population = sum(population, na.rm = T)) %>%
  ungroup()

# merge datasets
combined_wards <- left_join(yobe_variables, yobe_ranks, by = "WardCode")
combined_wards2 <- left_join(combined_wards, yobe_itn_clean, by = c("WardName.x" = "Ward"))

# run reprioritization function - CLASSIFICATION 50
prioritized_yobe_50 <- prioritize_wards(data = combined_wards2,
                                          population_col = "Population",
                                          rank_col = "ranks",
                                          class_col = "classification_50",
                                          ward_col = "WardName.x",
                                        target_percentage = 30)

# run reprioritization function - CLASSIFICATION 75
prioritized_yobe_75 <- prioritize_wards(data = combined_wards2,
                                          population_col = "Population",
                                          rank_col = "ranks",
                                          class_col = "classification_75",
                                          ward_col = "WardName.x",
                                        target_percentage = 30)

# save reprioritization tables
# write.csv(prioritized_yobe_50, file.path(OutputsDir, "NMEP Presentation Reprioritization Tables", "Yobe/", "yobe_scenario_3_no_threshold.csv"))
# write.csv(prioritized_yobe_75, file.path(OutputsDir, "NMEP Presentation Reprioritization Tables", "Yobe/", "yobe_scenario_4_no_threshold.csv"))
