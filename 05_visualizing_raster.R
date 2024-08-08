#Visualizing urban sprawl maps

library(raster)
library(sf)
library(dplyr)
library(ggplot2)
library(foot)

install.packages("rasterVis")
library(rasterVis)



Drive <- gsub("Documents", "", Sys.getenv("HOME"))
DriveDir <- file.path(Drive, "Urban Malaria Proj Dropbox", "urban_malaria") 
CitiesshpDir <- file.path(DriveDir, "data/nigeria/shapefiles/ShinyApp_shapefiles")
FootprintsDir <- file.path(DriveDir, "data/nigeria/building_footprints")


warri_landcover <- raster(file.path("C:/Users/USER/Downloads/Warri_LandCover_2021.tif"))

raster::plot(warri_landcover, main = "Land Cover in Warri")


# Create a level plot
levelplot(warri_landcover, main = "Enhanced Land Cover Plot in Warri")+
  raster::plot(warri_shp, add = TRUE, border = "red")

  



warri_shp <- st_read(file.path(CitiesshpDir, "Warri", "Warri.shp"))


# Custom function to calculate mode
compute_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


warri_values <- warri_landcover %>% 
  raster::extract(., warri_shp, fun = compute_mode)


warri_values <- raster::extract(warri_landcover, warri_shp, fun = mode, na.rm = na.rm)

