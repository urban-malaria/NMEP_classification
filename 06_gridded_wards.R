#summarized morphology metrics for gridded wards

rm(list =ls())

library(sf)
library(dplyr)
library(ggplot2)
library(foot)

Drive <- gsub("Documents", "", Sys.getenv("HOME"))
DriveDir <- file.path(Drive, "Urban Malaria Proj Dropbox", "urban_malaria") 
CitiesshpDir <- file.path(DriveDir, "data/nigeria/shapefiles/ShinyApp_shapefiles")
GriddedWardsDir <- file.path(CitiesshpDir, "gridded")
FootprintsDir <- file.path(DriveDir, "data/nigeria/building_footprints")

#Asaba
#urban wards: Okwe, Umuezei, Umuagu, Umuonaje

#Umuagu
umuagu_shp <- st_read(file.path(GriddedWardsDir, "Umuagu", "Umuagu.shp"))
umuagu_shp[1, "FID"] <- "29"

ggplot()+
  geom_sf(data = umuagu_shp)

asaba_footprints <- read.csv(file.path(FootprintsDir, "Asaba.csv"))
asaba_footprints <- st_as_sf(asaba_footprints, wkt = "geometry")
st_crs(asaba_footprints) <- 4326


umuagu_stats <- calculate_footstats(asaba_footprints, zone = umuagu_shp,
                                    what =list(list("area"), list("perimeter"), list("compact"), list("angle"), list("shape"), list("settled")), 
                                    how =list(list("mean","sum"), list("mean"), list("mean"), list("mean"), list("mean"), list("binary", "count")),
                                       controlZone = list(zoneName = "FID", 
                                         method = "centroid"),
                                        verbose = F)

write.csv(umuagu_stats, file.path(FootprintsDir, "Summary Metrics", "umuagu_stats.csv"))

ggplot()+
  geom_sf(data = umuagu_shp)+
  geom_text_repel(data = umuagu)


#Umuonaje

umuonaje_shp <- st_read(file.path(GriddedWardsDir, "Umuonaje", "Umuonaje.shp"))

ggplot()+
  geom_sf(data = umuonaje_shp, aes(geometry = geometry))


umuonaje_stats <- calculate_footstats(asaba_footprints, zone = umuonaje_shp,
                                    what =list(list("area"), list("perimeter"), list("compact"), list("angle"), list("shape"), list("settled")), 
                                    how =list(list("mean","sum"), list("mean"), list("mean"), list("mean"), list("mean"), list("binary", "count")),
                                    controlZone = list(zoneName = "FID", 
                                                       method = "centroid"),
                                    verbose = F)

write.csv(umuonaje_stats, file.path(FootprintsDir, "Summary Metrics", "umuonaje_stats.csv"))


#Warri

#Ekurede
