#summarized morphology metrics for gridded wards

rm(list =ls())

library(sf)
library(dplyr)
library(ggplot2)
library(foot)
library(units)
library(ggrepel)
library(tidyr)
library(patchwork)
library(gridExtra)



Drive <- gsub("Documents", "", Sys.getenv("HOME"))
DriveDir <- file.path(Drive, "Urban Malaria Proj Dropbox", "urban_malaria") 
CitiesshpDir <- file.path(DriveDir, "data/nigeria/shapefiles/ShinyApp_shapefiles")
GriddedWardsDir <- file.path(CitiesshpDir, "gridded")
FootprintsDir <- file.path(DriveDir, "data/nigeria/building_footprints")

nga_footprints <- read.csv(file.path(FootprintsDir, "NGA.csv"))
nga_footprints <- st_as_sf(nga_footprints, wkt = "geometry")
st_crs(nga_footprints) <- 4326

saveRDS(nga_footprints, file.path(FootprintsDir, "NGA.RDS"))

#Asaba
#urban wards: Okwe, Umuezei, Umuagu, Umuonaje

#Umuagu
umuagu_shp <- st_read(file.path(GriddedWardsDir, "Umuagu", "Umuagu.shp"))
#umuagu_shp[1, "FID"] <- "29"

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

umuagu_plot <- left_join(umuagu_shp, umuagu_stats, by = "FID")


ggplot()+
  geom_sf(data = umuagu_plot, aes(geometry = geometry, fill = as.numeric(area_mean)))+
  scale_fill_continuous(low = "lightblue", high = "darkblue", na.value = "black")+
  geom_text_repel(data = umuagu_plot %>% 
                    filter(FID == "0"), aes(geometry = geometry, label = FID),
                  stat = "sf_coordinates", size = 3, min.segment.length = 0) +
  labs(title = "Umuagu Average Area",
       x = "",
       y = "",
       fill = "" )+
  map_theme()
  

# Individual plots
plot_area_mean <- ggplot()+
  geom_sf(data = umuagu_plot, aes(geometry = geometry, fill = as.numeric(area_mean)))+
  scale_fill_continuous(low = "lightblue", high = "darkblue", na.value = "black")+
  geom_text_repel(data = umuagu_plot %>% 
                    filter(FID == "0"), aes(geometry = geometry, label = FID),
                  stat = "sf_coordinates", size = 3, min.segment.length = 0) +
  labs(title = "Umuagu Average Area",
       x = "",
       y = "",
       fill = "Mean Area (m^2)" )+
  map_theme()

plot_compact_mean <- ggplot()+
  geom_sf(data = umuagu_plot, aes(geometry = geometry, fill = as.numeric(compact_mean)))+
  scale_fill_continuous(low = "lightblue", high = "darkblue", na.value = "black")+
  geom_text_repel(data = umuagu_plot %>% 
                    filter(FID == "0"), aes(geometry = geometry, label = FID),
                  stat = "sf_coordinates", size = 3, min.segment.length = 0) +
  labs(title = "Umuagu Compactness",
       x = "",
       y = "",
       fill = "Mean compactness" )+
  map_theme()

plot_shape_mean <- ggplot()+
  geom_sf(data = umuagu_plot, aes(geometry = geometry, fill = as.numeric(shape_mean)))+
  scale_fill_continuous(low = "lightblue", high = "darkblue", na.value = "black")+
  geom_text_repel(data = umuagu_plot %>% 
                    filter(FID == "0"), aes(geometry = geometry, label = FID),
                  stat = "sf_coordinates", size = 3, min.segment.length = 0) +
  labs(title = "Shape",
       x = "",
       y = "",
       fill = "" )+
  map_theme()


plot_settled_count <- ggplot()+
  geom_sf(data = umuagu_plot, aes(geometry = geometry, fill = settled_count))+
  scale_fill_continuous(low = "lightblue", high = "darkblue", na.value = "black")+
  geom_text_repel(data = umuagu_plot %>% 
                    filter(FID == "0"), aes(geometry = geometry, label = FID),
                  stat = "sf_coordinates", size = 3, min.segment.length = 0) +
  labs(title = "Building count",
       x = "",
       y = "",
       fill = "" )+
  map_theme()

grid.arrange(plot_area_mean, plot_compact_mean, plot_shape_mean, plot_settled_count, ncol = 2)



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
ekurede_shp <- st_read(file.path(GriddedWardsDir, "Ekurede", "Ekurede.shp"))

ggplot()+
  geom_sf(data = ekurede_shp, aes(geometry = geometry))


ekurede_stats <- calculate_footstats(NGA_footprints, zone = ekurede_shp,
                                      what =list(list("area"), list("perimeter"), list("compact"), list("angle"), list("shape"), list("settled")), 
                                      how =list(list("mean","sum"), list("mean"), list("mean"), list("mean"), list("mean"), list("binary", "count")),
                                      controlZone = list(zoneName = "FID", 
                                                         method = "centroid"),
                                      verbose = F)

write.csv(ekurede_stats, file.path(FootprintsDir, "Summary Metrics", "ekurede_stats.csv"))

