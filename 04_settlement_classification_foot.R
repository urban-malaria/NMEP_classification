# ==========================================================================================================================================
## Script Name: Classification of Wards with Building Footprints
## Purpose: This script classifies wards in Nigerian cities based on building footprints. It processes shapefiles and building footprint 
# data, calculates settlement statistics (e.g., counts and binary classifications), and visualizes results with ggplot2. 
# Outputs include ward-level settlement statistics saved as CSV files and visualizations of settlement distributions for selected cities.
# ==========================================================================================================================================

rm(list =ls())

# install foot package from github
#devtools::install_github("wgpg/foot", build_vignettes=TRUE)

#load packages including foot
library(sf)
library(dplyr)
library(ggplot2)
library(foot)

Drive <- gsub("Documents", "", Sys.getenv("HOME"))
DriveDir <- file.path(Drive, "Urban Malaria Proj Dropbox", "urban_malaria") 
CitiesshpDir <- file.path(DriveDir, "data/nigeria/shapefiles/ShinyApp_shapefiles")
FootprintsDir <- file.path(DriveDir, "data/nigeria/building_footprints")
GriddedWardsDir <- file.path(CitiesshpDir, "gridded")


#Warri
warri_shp <- st_read(file.path(CitiesshpDir, "Warri", "Warri.shp"))

combined_geometry <- st_union(warri_shp)
warri_wkt <- st_as_text(combined_geometry)
print(warri_wkt)

warri_polygon <- st_as_sf(combined_geometry)
ggplot()+
  geom_sf(data = warri_polygon)

ggplot

write(warri_wkt, file = "C:/Users/USER/Downloads/warripolygon.txt")


# Asaba

asaba_shp <- st_read(file.path(CitiesshpDir, "Asaba", "Asaba.shp"))

asaba_footprints <- read.csv(file.path(FootprintsDir, "Asaba.csv"))
asaba_footprints <- st_as_sf(asaba_footprints, wkt = "geometry")
st_crs(asaba_footprints) <- 4326
  
ggplot()+
  geom_sf(data = asaba_footprints)


asaba_buildings <- zonalIndex(asaba_footprints, asaba_shp, zoneField = "WardName",
                              method = "centroid", returnObject = TRUE)

asaba_stats <- calculate_footstats(asaba_buildings)


saveRDS(asaba_stats, file = file.path(FootprintsDir, "asaba_stats.RDS"))
asaba_stats <- readRDS(file = file.path(FootprintsDir, "asaba_stats.RDS"))


asaba_ward_stats <- calculate_footstats(asaba_footprints, zone = asaba_shp,
                                         what = "settled",
                                         how = list("binary","count"),
                                         controlZone = list(zoneName = "WardName", method = "centroid"),
                                         verbose = F)
write.csv(asaba_ward_stats, file.path(FootprintsDir, "asaba_wards_stats.csv"))

asaba_ward_stats$geometry <- asaba_shp$geometry

ggplot()+
  geom_sf(data = asaba_ward_stats, aes(geometry = geometry, fill = settled_count))+
  labs(title = "Number of settlements in Asaba",
       fill = "Settlement count",
       x = "",
       y = "", )+
  scale_fill_continuous()+
  map_theme()
  
ggplot()+
  geom_sf(data = asaba_ward_stats, aes(geometry = geometry, fill = settled_binary))+
  labs(title = "Settlement Classification",
       fill = "Binary classification",
       x = "",
       y = "", )+
  map_theme()
  
#other cities
cities <- c("Abeokuta", "Damaturu", "Dutse", "Gombe", "Ilorin", "Jalingo", "Kano", "Katsina")


abeokuta_shp <- st_read(file.path(CitiesshpDir, "Abeokuta", "Abeokuta.shp"))

abeokuta_footprints <- read.csv(file.path(FootprintsDir, "Abeokuta.csv"))
abeokuta_footprints <- st_as_sf(abeokuta_footprints, wkt = "geometry")
st_crs(abeokuta_footprints) <- 4326

ggplot()+
  geom_sf(data = abeokuta_footprints, aes(geometry = geometry))

abeokuta_ward_stats <- calculate_footstats(abeokuta_footprints, zone = abeokuta_shp,
                                        what = "settled",
                                        how = list("binary","count"),
                                        controlZone = list(zoneName = "WardName", method = "centroid"),
                                        verbose = F)

write.csv(abeokuta_ward_stats, file.path(FootprintsDir, "abeokuta_wards_stats.csv"))

abeokuta_ward_stats$geometry <- abeokuta_shp$geometry
  
ggplot()+
  geom_sf(data = abeokuta_ward_stats, aes(geometry = geometry, fill = settled_count))+
  labs(title = "Number of settlements in Abeokuta",
       fill = "Settlement count",
       x = "",
       y = "", )+
  scale_fill_continuous()+
  map_theme()
