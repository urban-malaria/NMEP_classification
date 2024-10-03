###load path NMEP classification

rm(list=ls())

#directories
Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", gsub("OneDrive", "", Sys.getenv("HOME")))))
Drive <- file.path(gsub("[//]", "/", Drive))
DriveDir <- file.path(Drive, "Urban Malaria Proj Dropbox", "urban_malaria")

DataDir <- file.path(DriveDir, "data")

ShpfilesDir <- file.path(DriveDir, "data/nigeria/nigeria_shapefiles/shapefiles/ShinyApp_shapefiles")
RastersDir <- file.path(DriveDir, "data/nigeria/Raster_files")
GriddedWardsDir <- file.path(ShpfilesDir, "gridded")
FootprintsDir <- file.path(DataDir, "nigeria/building_footprints/OpenBuildings")


OutputsDir <- file.path(DriveDir, "projects/urban_microstratification/Shiny App")
plotsDir <- file.path(OutputsDir, "Plots")



#load packages

list_of_packages <- c("RColorBrewer", "readr", "haven", "data.table", "reshape",
                      "ggplot2", "labelled", "tidyverse", "janitor", "terra",
                      "readxl", "mapsf", "survey","srvyr", "plotly", "hdf5r",
                      "broom", "ggthemes", "ggrepel", "sjlabelled", "sf", 
                      "dplyr", "ggpubr", "viridis", "patchwork", 
                      "raster", "wordcloud", "ggwordcloud", "terra", "plotly",
                      "gridExtra", "grid", "openxlsx", "officer", "magrittr", "mclust",
                      "foot", "units", "tidyr", "foreach", "doParallel")

read_install_pacakges <- function(packages = list_of_packages
){
  new_packages <- packages[!(list_of_packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new_packages)
  return(sapply(list_of_packages, require, character.only = TRUE))
}

read_install_pacakges()

#custom functions

map_theme <- function(){
  theme(axis.text.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        rect = ggplot2::element_blank(),
        plot.background = ggplot2::element_rect(fill = "white", colour = NA), 
        plot.title = element_text(hjust = 0.5),
        legend.title=element_text(size=8, colour = 'black', hjust = 0.5), 
        legend.text =element_text(size = 8, colour = 'black'),
        legend.key.height = unit(0.65, "cm"))
}

theme_manuscript <- function(){
  theme_bw() + 
    theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(size = 12, color = "black"), 
          axis.text.y = element_text(size = 12, color = "black"),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size =12),
          legend.title=element_text(size=12, colour = 'black'),
          legend.text =element_text(size = 12, colour = 'black'),
          legend.key.height = unit(1, "cm"))
}





