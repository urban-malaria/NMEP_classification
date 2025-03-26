# ==========================================================================================================================================
# Script Name: Composite Score Variable Maps
# Purpose: Creates maps showing the ward-level geographic distribution of variables included in the composite score in each state.
# Author: Grace Legris, Research Data Analyst, gracebea@gmail.com
# ==========================================================================================================================================

source("load_path.R")

# read in state shapefiles
kaduna_shp <- sf::st_read(file.path(StateShpDir ,"Kaduna", "Kaduna_State.shp"))
katsina_shp <- sf::st_read(file.path(StateShpDir ,"Katsina", "Katsina_State.shp"))
niger_shp <- sf::st_read(file.path(StateShpDir ,"Niger", "Niger_State.shp"))
taraba_shp <- sf::st_read(file.path(StateShpDir ,"Taraba", "Taraba_State.shp"))
yobe_shp <- sf::st_read(file.path(StateShpDir ,"Yobe", "Yobe_State.shp"))
kano_metro_shp <- sf::st_read(file.path(DataDir, "nigeria/nigeria_shapefiles/shapefiles/ShinyApp_shapefiles/Kano_Ibadan/Kano-Ibadan.shp")) %>% 
  dplyr::filter(StateCode == "KN")

# read in extracted data for each state
kaduna_data <- read.csv(file.path(OutputsDir, "Final Extractions/kaduna_plus.csv"))
katsina_data <- read.csv(file.path(OutputsDir, "Final Extractions/katsina_plus.csv"))
niger_data <- read.csv(file.path(OutputsDir, "Final Extractions/niger_plus.csv"))
taraba_data <- read.csv(file.path(OutputsDir, "Final Extractions/taraba_plus.csv"))
yobe_data <- read.csv(file.path(OutputsDir, "Final Extractions/yobe_plus.csv"))
kano_metro_data <- 