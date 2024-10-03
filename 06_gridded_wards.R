#summarized morphology metrics for gridded wards

rm(list =ls())

install.packages("mclust")

library(sf)
library(dplyr)
library(ggplot2)
library(foot)
library(units)
library(ggrepel)
library(tidyr)
library(patchwork)
library(gridExtra)
library(mclust)


Drive <- gsub("Documents", "", Sys.getenv("HOME"))
DriveDir <- file.path(Drive, "Urban Malaria Proj Dropbox", "urban_malaria") 
CitiesshpDir <- file.path(DriveDir, "data/nigeria/shapefiles/ShinyApp_shapefiles")
GriddedWardsDir <- file.path(CitiesshpDir, "gridded")
FootprintsDir <- file.path(DriveDir, "data/nigeria/building_footprints")
OpenDir <- file.path(FootprintsDir, "OpenBuildings")

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

open_asaba <- read.csv(file.path(FootprintsDir, "Asaba.csv"))
open_asaba <- st_as_sf(open_asaba, wkt = "geometry")
st_crs(open_asaba) <- 4326



umuagu_stats <- calculate_footstats(open_asaba, zone = umuagu_shp, 
                                    what =list(list("area"), list("perimeter"), list("compact"), list("angle"), list("shape"), list("settled"), list("nndist")), 
                                    how =list(list("mean","sum"), list("mean"), list("mean"), list("entropy", "mean"), list("mean"), list("binary", "count"), list("nnindex")),
                                       controlZone = list(zoneName = "FID", 
                                         method = "centroid"),
                                        verbose = F)

#write.csv(umuagu_stats, file.path(FootprintsDir, "Summary Metrics", "umuagu_stats.csv"))

umuagu_plot_open <- left_join(umuagu_shp, umuagu_stats, by = "FID")


ggplot()+
  geom_sf(data = umuagu_plot_open, aes(geometry = geometry, fill = as.numeric(area_mean)))+
  scale_fill_continuous(low = "lightblue", high = "darkblue", na.value = "black")+
  geom_text_repel(data = umuagu_plot_open %>% 
                    filter(FID == "0"), aes(geometry = geometry, label = FID),
                  stat = "sf_coordinates", size = 3, min.segment.length = 0) +
  labs(title = "Average Area of Buildings (m2)",
       x = "",
       y = "",
       fill = "" )+
  map_theme()
  

# Individual plots
plot_area_mean <- ggplot()+
  geom_sf(data = umuagu_plot_open, aes(geometry = geometry, fill = as.numeric(area_mean)))+
  scale_fill_continuous(low = "lightblue", high = "darkblue", na.value = "black")+
  geom_text_repel(data = umuagu_plot_open %>% 
                    filter(FID == "0"), aes(geometry = geometry, label = FID),
                  stat = "sf_coordinates", size = 3, min.segment.length = 0) +
  labs(title = "Umuagu Average Area",
       x = "",
       y = "",
       fill = "Mean Area (m^2)" )+
  map_theme()

plot_compact_mean <- ggplot()+
  geom_sf(data = umuagu_plot_open, aes(geometry = geometry, fill = as.numeric(compact_mean)))+
  scale_fill_continuous(low = "lightblue", high = "darkblue", na.value = "black")+
  geom_text_repel(data = umuagu_plot_open %>% 
                    filter(FID == "0"), aes(geometry = geometry, label = FID),
                  stat = "sf_coordinates", size = 3, min.segment.length = 0) +
  labs(title = "Umuagu Compactness",
       x = "",
       y = "",
       fill = "Mean compactness" )+
  map_theme()

plot_shape_mean <- ggplot()+
  geom_sf(data = umuagu_plot_open, aes(geometry = geometry, fill = as.numeric(shape_mean)))+
  scale_fill_continuous(low = "lightblue", high = "darkblue", na.value = "black")+
  geom_text_repel(data = umuagu_plot_open %>% 
                    filter(FID == "0"), aes(geometry = geometry, label = FID),
                  stat = "sf_coordinates", size = 3, min.segment.length = 0) +
  labs(title = "Shape",
       x = "",
       y = "",
       fill = "" )+
  map_theme()



plot_settled_count <- ggplot()+
  geom_sf(data = umuagu_plot_open, aes(geometry = geometry, fill = settled_count))+
  scale_fill_continuous(low = "lightblue", high = "darkblue", na.value = "black")+
  geom_text_repel(data = umuagu_plot_open %>% 
                    filter(FID == "0"), aes(geometry = geometry, label = FID),
                  stat = "sf_coordinates", size = 3, min.segment.length = 0) +
  labs(title = "Building count",
       x = "",
       y = "",
       fill = "" )+
  map_theme()


#nnidex
ggplot()+
  geom_sf(data = umuagu_plot_open, aes(geometry = geometry, fill = nndist_nnindex))+
  scale_fill_continuous(low = "lightblue", high = "darkblue", na.value = "black")+
  #geom_text_repel(data = umuagu_plot_open, aes(geometry = geometry, label = FID),
   #               stat = "sf_coordinates", size = 3, min.segment.length = 0) +
  labs(title = "Nearest Neighbour Index",
       x = "",
       y = "",
       fill = "" )+
  map_theme()

#angle entropy
ggplot()+
  geom_sf(data = umuagu_plot_open %>% 
            filter(FID != "5" | FID != "11"), aes(geometry = geometry, fill = angle_entropy))+
  scale_fill_continuous(low = "lightyellow", high = "maroon", na.value = "black")+
  geom_text_repel(data = umuagu_plot_open, aes(geometry = geometry, label = FID),
                  stat = "sf_coordinates", size = 3, min.segment.length = 0) +
  labs(title = "Angle Entropy",
       x = "",
       y = "",
       fill = "" )+
  map_theme()

ggplot()+
  geom_sf(data = umuagu_plot_open, aes(geometry = geometry, fill = angle_entropy))+
  scale_fill_continuous(low = "lightyellow", high = "maroon", na.value = "black")+
  #geom_text_repel(data = umuagu_plot_open, aes(geometry = geometry, label = FID),
   #               stat = "sf_coordinates", size = 3, min.segment.length = 0) +
  labs(title = "Angle Entropy",
       x = "",
       y = "",
       fill = "" )+
  map_theme()



plot(umuagu_plot_open$nndist_nnindex, umuagu_plot_open$angle_entropy,
     xlab = "NNI",
     ylab = "Entropy")

hist(umuagu_plot_open$nndist_nnindex)

grid.arrange(plot_area_mean, plot_compact_mean, plot_shape_mean, plot_settled_count, ncol = 2)



#Umuonaje

umuonaje_shp <- st_read(file.path(GriddedWardsDir, "Umuonaje", "Umuonaje.shp"))

ggplot()+
  geom_sf(data = umuonaje_shp, aes(geometry = geometry))


umuonaje_stats <- calculate_footstats(open_asaba, zone = umuonaje_shp,
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



##### Using Microsoft Building Footprints

ms_asaba <- read_sf(file.path(FootprintsDir, "MS_footprints_Asaba.geojson"))
ms_asaba <- st_as_sf(ms_asaba, wkt = "geometry")
st_crs(ms_asaba) <- 4326



ms1 <- 
  ggplot()+
  geom_sf(data = ms_asaba, aes(geometry = geometry))+
  labs(title = "Buildings using MS Footprints Data set",
       x= "",
       y = "",
       caption = "43 000 buildings")
  

gg1 <- ggplot()+
  geom_sf(data = open_asaba, aes(geometry = geometry))+
  labs(title = "Buildings using Google OpenBuildings",
       x= "",
       y = "",
       caption = "56 424 buildings")

grid.arrange(ms1, gg1,ncol = 2)


umuagu_stats2 <- calculate_footstats(ms_asaba, zone = umuagu_shp,
                                    what =list(list("area"), list("perimeter"), list("compact"), list("angle"), list("shape"), list("settled"), list("nndist")), 
                                    how =list(list("mean","sum"), list("mean"), list("mean"), list("entropy", "mean"), list("mean"), list("binary", "count"), list("nnindex")),
                                    controlZone = list(zoneName = "FID", 
                                                       method = "centroid"),
                                    verbose = F)
umuagu_plot_ms <- left_join(umuagu_stats2, umuagu_shp, by = "FID")

## compare footprints in umuagu

ms_umuagu <- st_intersection(umuagu_shp, ms_asaba)

open_umuagu <- st_intersection(umuagu_shp, open_asaba)

ggplot()+
  geom_sf(data = umuagu_shp, aes(geometry = geometry))+
  geom_sf(data = open_umuagu, aes(geometry = geometry))+
  labs(title = "Building footrpints in Umuagu, Open Buildings",
       x = "",
       y = "") +
  map_theme()



ggplot()+
  geom_sf(data = umuagu_shp, aes(geometry = geometry))+
  geom_text_repel(data = umuagu_plot_open
                 # %>% filter(FID == "5" |FID == "6" |FID == "7" |FID == "11" |FID == "12" |FID == "13" | FID == "19" ), 
                  ,aes(geometry = geometry, label = settled_count), #1 or 2
                  stat = "sf_coordinates", size = 3, min.segment.length = 0) +
  labs(title = "Number of buildings in Umuagu for Open  Buildings",
       x = "",
       y = "",
       fill = "" )+
  map_theme()

ggplot()+
  geom_sf(data = umuagu_plot_open, aes(geometry = geometry, fill = as.numeric(area_sum)))+
  scale_fill_continuous(low = "lightblue", high = "darkblue", na.value = "black")+
  #geom_text_repel(data = umuagu_plot_open %>% 
   #                 filter(FID == "0"), aes(geometry = geometry, label = FID),
                  #stat = "sf_coordinates", size = 3, min.segment.length = 0) +
  labs(title = "Total Area covered by buildings, Open Buildings",
       x = "",
       y = "",
       fill = "" )+
  map_theme()


#Microsoft

ggplot()+
  geom_sf(data = umuagu_shp, aes(geometry = geometry))+
  geom_text_repel(data =  
                  # %>% filter(FID == "5" |FID == "6" |FID == "7" |FID == "11" |FID == "12" |FID == "13" | FID == "19" ), 
                  ,aes(geometry = geometry, label = settled_count), #1 or 2
                  stat = "sf_coordinates", size = 3, min.segment.length = 0) +
  labs(title = "Number of buildings in Umuagu for Microsoft",
       x = "",
       y = "",
       fill = "" )+
  map_theme()




ggplot()+
  geom_sf(data = umuagu_shp, aes(geometry = geometry))+
  geom_text_repel(data = umuagu_stats_ms 
                  # %>% filter(FID == "5" |FID == "6" |FID == "7" |FID == "11" |FID == "12" |FID == "13" | FID == "19" ), 
                  ,aes(geometry = geometry, label = settled_count), #1 or 2
                  stat = "sf_coordinates", size = 3, min.segment.length = 0) +
  labs(title = "Number of buildings in Umuagu for Open  Buildings",
       x = "",
       y = "",
       fill = "" )+
  map_theme()

