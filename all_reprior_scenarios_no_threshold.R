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

# load and clean itn data for niger
niger_itn_data <- readxl::read_excel(
  file.path(ITNDir, "pbi_distribution_Niger.xlsx"), 
  sheet = 1) 

niger_itn_clean <- niger_itn_data %>%
  rename(population = `Sum of N_Nets`,
         Ward = `Row Labels`) %>%
  dplyr::select(population, Ward) %>%
  group_by(Ward) %>%
  summarise(Population = sum(population, na.rm = T)) %>%
  ungroup() %>%
  mutate(Ward = case_when(
    Ward == "Adunu" ~ "Adono",
    Ward == "Akare" ~ "Akari",
    Ward == "Anaba" ~ "Anaba/Ibelu West",
    Ward == "Badeggi" ~ "Badegi",
    Ward == "Bagama B" ~ "Bagama b",
    Ward == "Bonu" ~ "Bono",
    Ward == "Chanchaga" ~ "Chachanga",
    Ward == "Cheniyan" ~ "Chenian",
    Ward == "Ciki Gari" ~ "Cikin Gari",
    Ward == "Danrangi" ~ "Danragi",
    Ward == "Dokko" ~ "Doko",
    Ward == "Dokodza" ~ "Dokoza",
    Ward == "Ebbo/Gbachinku" ~ "Ebbo",
    Ward == "Ecwa/Gwada" ~ "Egwa/Gwada",
    Ward == "Edozhigi" ~ "Edozigi",
    Ward == "Fahzi" ~ "Fazhi",
    Ward == "Gazhe" ~ "Gazhe 1",
    Ward == "Gupa/Abugi" ~ "Gupa",
    Ward == "Gwarjiko" ~ "Gwarijiko",
    Ward == "Kafin Koro" ~ "Kafinkoro",
    Ward == "Kuchi Bussu" ~ "Kucibusu",
    Ward == "Kura" ~ "Kura/Auna East",
    Ward == "Kurebe/Kushaka" ~ "Kuregbe",
    Ward == "Kuso Tachin" ~ "Kusotacin",
    Ward == "Labohzi" ~ "Labozhi",
    Ward == "Lokogoma" ~ "Lokogwoma",
    Ward == "Magamadaji" ~ "Magamadaji/Ibelu North",
    Ward == "Mantabefyan" ~ "Manbe Tafyan",
    Ward == "Moregi" ~ "Muregi",
    Ward == "Muye" ~ "Muye/Egba",
    TRUE ~ Ward  # Keep original value if no match
  )) %>%
  mutate(num = 1:n())


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
  ward_col = "WardName.x"
)
prioritized_niger_4 <- prioritize_wards(
  data = combined_wards2_ni, 
  population_col = "Population", 
  rank_col = "ranks", 
  class_col = "classification_75", 
  ward_col = "WardName.x"
)

# save prioritized wards
write.csv(prioritized_niger_3, file.path(OutputsDir, "NMEP Presentation Reprioritization Tables", "Niger", "niger_scenario3_no_threshold.csv"))
write.csv(prioritized_niger_4, file.path(OutputsDir, "NMEP Presentation Reprioritization Tables", "Niger", "niger_scenario4_no_threshold.csv"))

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

# load and clean itn data for kano
kano_itn_data <- readxl::read_excel(
  file.path(ITNDir, "pbi_distribution_Kano.xlsx"), 
  sheet = 2) 

kano_itn_clean <- kano_itn_data %>% 
  rename(population = N_FamilyMembers,
         Ward = AdminLevel3) %>% 
  dplyr::select(population, Ward) %>% 
  group_by(Ward) %>% 
  summarise(Population = sum(population, na.rm =T)) %>% 
  ungroup() %>% 
  mutate(num = 1:n())

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
  ward_col = "WardName.x"
)
prioritized_kano_4 <- prioritize_wards(
  data = combined_wards2_kn, 
  population_col = "Population", 
  rank_col = "ranks", 
  class_col = "classification_75", 
  ward_col = "WardName.x"
)

# save prioritized wards
write.csv(prioritized_kano_3, file.path(OutputsDir, "NMEP Presentation Reprioritization Tables", "Kano", "kano_scenario3_no_threshold.csv"))
write.csv(prioritized_kano_4, file.path(OutputsDir, "NMEP Presentation Reprioritization Tables", "Kano", "kano_scenario4_no_threshold.csv"))

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

# run reprioritization function - CLASSIFICATION 50
prioritized_katsina_50 <- prioritize_wards(data = combined_wards2, 
                                           population_col = "Population", 
                                           rank_col = "ranks", 
                                           class_col = "classification_50", 
                                           ward_col = "WardName")

# run reprioritization function - CLASSIFICATION 75
prioritized_katsina_75 <- prioritize_wards(data = combined_wards2, 
                                           population_col = "Population", 
                                           rank_col = "ranks", 
                                           class_col = "classification_75", 
                                           ward_col = "WardName")

# save reprioritization tables
write.csv(prioritized_katsina_50, file.path(OutputsDir, "NMEP Presentation Reprioritization Tables", "Katsina/", "katsina_scenario_3_no_threshold.csv"))
write.csv(prioritized_katsina_75, file.path(OutputsDir, "NMEP Presentation Reprioritization Tables", "Katsina/", "katsina_scenario_4_no_threshold.csv"))

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

# read in ITN data
taraba_itn_data <- readxl::read_excel(file.path(ITNDir, "pbi_distribution_Taraba.xlsx"))

# clean ITN data
taraba_itn_clean <- taraba_itn_data %>%
  rename(Ward = `Row Labels`, num_nets = `Sum of N_Nets`) %>%
  dplyr::select(Ward, num_nets) %>%
  group_by(Ward) %>%
  summarise(num_nets = sum(num_nets, na.rm = TRUE)) %>%
  mutate(Ward = case_when(
    Ward == "Dampar I" ~ "Dampar 1",
    Ward == "Dampar II" ~ "Dampar 2",
    Ward == "Dampar III" ~ "Dampar 3",
    Ward == "Lissam I" ~ "Lissam 1",
    Ward == "Lissam Ii" ~ "Lissam 2",
    Ward == "Nwonyo I" ~ "Nwonyo 1",
    Ward == "Nwonyo II" ~ "Nwonyo 2",
    Ward == "Rimi Uku I" ~ "Rimi Uku 1",
    Ward == "Rimi Uku II" ~ "Rimi Uku 2",
    Ward == "Sarkin Kudu I" ~ "Sarkin Kudu 1",
    Ward == "Sarkin Kudu II" ~ "Sarkin Kudu 2",
    Ward == "Sarkin Kudu III" ~ "Sarkin Kudu 3",
    Ward == "Kussum Badakoshi" ~ "Kussum/Badakoshi",
    Ward == "Panti-Sawa" ~ "Pantisawa",
    Ward == "Zangonkombi" ~ "Zangon Kombi",
    Ward == "Kassala-Sembe" ~ "Kachala-Sembe",
    Ward == "Kpambo Purfi" ~ "Kpambo Puri",
    Ward == "Kotsensi" ~ "Kosensi",
    Ward == "Mbamaga" ~ "Mbamnga",
    Ward == "Gangdole" ~ "Gang Dole",
    Ward == "Ganglari" ~ "Gang Lari",
    Ward == "Gangmata" ~ "Gang Mata", 
    Ward == "Gangtiba" ~ "Gang Tiba",
    Ward == "Jenkaigama" ~ "Jen Kaigama",
    Ward == "Wurojam" ~ "Wuro Jam",
    TRUE ~ Ward
  )) %>%
  filter(Ward != "(blank)" & 
           Ward != "Grand Total" & 
           !is.na(Ward) & 
           Ward != "")

# combine datasets
combined_wards <- left_join(taraba_variables, taraba_ranks, by = "WardCode")
combined_wards2 <- left_join(combined_wards, taraba_itn_clean, by = c("WardName.x" = "Ward"))

# run reprioritization function - CLASSIFICATION 50
prioritized_taraba_50 <- prioritize_wards(data = combined_wards2,
                                          population_col = "num_nets",
                                          rank_col = "ranks",
                                          class_col = "classification_50",
                                          ward_col = "WardName.x")

# run reprioritization function - CLASSIFICATION 75
prioritized_taraba_75 <- prioritize_wards(data = combined_wards2,
                                          population_col = "num_nets",
                                          rank_col = "ranks",
                                          class_col = "classification_75",
                                          ward_col = "WardName.x")

# save reprioritization tables
write.csv(prioritized_taraba_50, file.path(OutputsDir, "NMEP Presentation Reprioritization Tables", "Taraba/", "taraba_scenario_3_no_threshold.csv"))
write.csv(prioritized_taraba_75, file.path(OutputsDir, "NMEP Presentation Reprioritization Tables", "Taraba/", "taraba_scenario_4_no_threshold.csv"))


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

# load ITN distribution data
itn_dir <- file.path(DataDir, "nigeria/ITN_distribution")
yobe_itn_data <- readxl::read_excel(
  file.path(itn_dir, "pbi_distribution_Yobe.xlsx"), 
  sheet = 1
)

# clean ITN data
yobe_itn_clean <- yobe_itn_data %>% 
  rename(population = N_FamilyMembers,
         Ward = AdminLevel3) %>% 
  dplyr::select(population, Ward) %>% 
  group_by(Ward) %>% 
  summarise(Population = sum(population, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(num = row_number())

# merge datasets
combined_wards <- left_join(yobe_variables, yobe_ranks, by = "WardCode")
combined_wards2 <- left_join(combined_wards, yobe_itn_clean, by = c("WardName.x" = "Ward"))

# run reprioritization function - CLASSIFICATION 50
prioritized_yobe_50 <- prioritize_wards(data = combined_wards2,
                                          population_col = "Population",
                                          rank_col = "ranks",
                                          class_col = "classification_50",
                                          ward_col = "WardName.x")

# run reprioritization function - CLASSIFICATION 75
prioritized_yobe_75 <- prioritize_wards(data = combined_wards2,
                                          population_col = "Population",
                                          rank_col = "ranks",
                                          class_col = "classification_75",
                                          ward_col = "WardName.x")

# save reprioritization tables
write.csv(prioritized_yobe_50, file.path(OutputsDir, "NMEP Presentation Reprioritization Tables", "Yobe/", "yobe_scenario_3_no_threshold.csv"))
write.csv(prioritized_yobe_75, file.path(OutputsDir, "NMEP Presentation Reprioritization Tables", "Yobe/", "yobe_scenario_4_no_threshold.csv"))
