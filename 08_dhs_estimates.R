# ==========================================================================================================================================
## Script Name: DHS Analysis for Malaria Prevalence
## This script processes DHS data and shapefiles to analyze and visualize malaria prevalence in the Delta State, 
# focusing on Warri and Asaba. It includes steps for loading and processing data, calculating survey-weighted malaria prevalence, 
# and mapping point-level prevalence using spatial intersections with shapefiles. The final output includes visualizations 
# of malaria prevalence across the specified regions.
# ==========================================================================================================================================

library(raster)
library(sf)
library(terra)
library(stringr)
library(ggplot2)
library(dplyr)
library(purrr)
library(haven)
library(stars)
library(gridExtra)

library(tidyr)
library(srvyr)
library(sjlabelled)
library(RColorBrewer)
library(blscrapeR)
library(readr)
library(labelled)
library(tidyverse)
library(janitor)
library(mapsf)
library(readxl)
library(survey)
library(ggthemes)
library(ggrepel)



#Load directories
Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", gsub("OneDrive", "", Sys.getenv("HOME")))))
Drive <- file.path(gsub("[//]", "/", Drive))
DriveDir <- file.path(Drive, "Urban Malaria Proj Dropbox", "urban_malaria")
DHSDir <- file.path(DriveDir, "data", "dhs", "Downloads")
ShpDir <- file.path(DriveDir, "data", "nigeria", "shapefiles", "ShinyApp_shapefiles")


## Load shapefiles
warri_shp <- st_read(file.path(ShpDir, "Warri", "Warri.shp"))
asaba_shp <- st_read(file.path(ShpDir, "Asaba", "Asaba.shp"))


### read .dta files
read.files <- function(path, general_pattern, specific_pattern, fun) {
  files <- list.files(path = path , pattern = general_pattern, full.names = TRUE, recursive = TRUE)
  files<- files[(grep(specific_pattern, files))]
  sapply(files, fun, simplify = F)
}


# Read in PR and KR data files and rename state variable
pr_files <- read.files(DHSDir, "*NGPR.*\\.DTA", 'NGPR4CFL|NGPR53FL|NGPR61FL|NGPR6AFL|NGPR71FL|NGPR7AFL|NGPR81FL', read_dta)

kr_files <- read.files(DHSDir, "*NGKR.*\\.DTA", 'NGKR4BFL|NGKR53FL|NGKR61FL|NGKR6AFL|NGKR71FL|NGKR7AFL|NGKR81FL', read_dta)


pr_files[[5]]$state <- as_label(pr_files[[5]]$shstate)
pr_files[[6]]$state <- as_label(pr_files[[6]]$shstate)
pr_files[[7]]$state <- as_label(pr_files[[7]]$hv024)
pr_data <- bind_rows(pr_files)

pr_data_2021 <- pr_files[[7]]


lapply(pr_files, function(x) table(x$hml32))


## Load spatial points
sf21 = sf::st_read(file.path(DHSDir, "NG_2021_MIS_12052022_1735_141460/NGGE81FL/NGGE81FL.shp"))
sf18 = sf::st_read(file.path(DHSDir, "NG_2018_DHS_11072019_1720_86355/NGGE7BFL/NGGE7BFL.shp")) 
sf15 = sf::st_read(file.path(DHSDir, "NG_2015_MIS_06192019/NGGE71FL/NGGE71FL.shp")) 
sf_all = rbind(sf21, sf18, sf15) %>% rename(cluster = DHSCLUST)


malaria_prev <- pr_data %>%
  filter(hv042 == 1 & hv103 == 1 & hc1 %in% c(6:59) ) %>%
  filter(hml32 < 6) %>% 
  mutate(malaria = ifelse(hml32 == 1, 1, 0), wt = hv005/1000000, id  = hv021, strat=hv022) %>% 
  srvyr:: as_survey_design(., ids = id, strata = strat,nest = T, weights = wt) %>%
  group_by(state, cluster = hv001, year = hv007) %>% 
  summarize(prev = round(survey_mean(malaria),2),
             total_malaria = survey_total()) %>%
  mutate(class= cut(prev,  c(seq(0, 20, 5),30,50, 100), include.lowest = T)) %>% 
  filter(state == "delta") %>%
  inner_join(sf_all, by = c("year" = "DHSYEAR","cluster" = "cluster" )) %>%
  drop_na(prev)

ggplot()+
  geom_sf(data = malaria_prev, aes(geometry = geometry))

#point malaria prevalence

# Warri
malaria_prev2 <- sf::st_as_sf(malaria_prev, coords = c("LONGNUM", "LATNUM"))
sf::st_crs(malaria_prev2) <-  sf::st_crs(warri_shp)
warri_pr_data <- st_intersection(warri_shp, malaria_prev2)


#Asaba

malaria_prev3 <- sf::st_as_sf(malaria_prev, coords = c("LONGNUM", "LATNUM"))
sf::st_crs(malaria_prev3) <-  sf::st_crs(asaba_shp)
asaba_pr_data <- st_intersection(asaba_shp, malaria_prev3)

ggplot()+
  geom_sf(data = asaba_shp, aes(geometry = geometry))+
  geom_sf(data = asaba_pr_data, aes(geometry = geometry))+
  ggrepel::geom_text_repel(data = asaba_pr_data, aes(label = prev, geometry = geometry),
                           color = "black", stat = "sf_coordinates",
                           min.segment.length = 0, size = 4, force = 1)+
  labs(title = "Malaria point prevalence Asaba",
       x = "", y = "",
       caption = "Data only available for Umuagu")+
  map_theme()










