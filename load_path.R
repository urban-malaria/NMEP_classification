###load path NMEP classification

rm(list=ls())

#directories
Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", gsub("OneDrive", "", Sys.getenv("HOME")))))
Drive <- file.path(gsub("[//]", "/", Drive))
DriveDir <- file.path(Drive, "Urban Malaria Proj Dropbox", "urban_malaria")

DataDir <- file.path(DriveDir, "data")

ShpfilesDir <- file.path(DriveDir, "data/nigeria/nigeria_shapefiles/shapefiles/ShinyApp_shapefiles")
ShpfilesDir2 <- file.path(DriveDir, "data/nigeria/nigeria_shapefiles/shapefiles/NMEP Net Distribution States Shapefiles for ADM3 - 13 States")
StateShpDir <- file.path(ShpfilesDir, "all_reprioritization_nmep_states/STATES")

ITNDir <- file.path(DataDir, "nigeria/ITN_distribution")
ITNDir2 <- file.path(DataDir, "nigeria/ITN_distribution/household_member_ward_summaries_Itn_distribution")


GriddedWardsDir <- file.path(ShpfilesDir, "gridded")
RastersDir <- file.path(DriveDir, "data/nigeria/Raster_files")
TPRDir <- file.path(DriveDir, "data", "nigeria", "TPR")
PfDir <- file.path(RastersDir, "Malaria Atlas")

FootprintsDir <- file.path(DataDir, "nigeria/building_footprints/OpenBuildings")
MSGlobalDir <- file.path(DataDir, "nigeria/building_footprints/MSGlobal")

OutputsDir <- file.path(DriveDir, "projects/urban_microstratification/Shiny App")
plotsDir <- file.path(OutputsDir, "Plots")

FieldDataDir <- file.path(DataDir, "nigeria/kano_ibadan_epi/new_field_data")


#load packages


list_of_packages <- c("RColorBrewer", "readr", "haven", "data.table", "reshape",
                      "ggplot2", "labelled", "tidyverse", "janitor", "terra",
                      "readxl", "mapsf", "survey","srvyr", "plotly", "hdf5r",
                      "broom", "ggthemes", "ggrepel", "sjlabelled", "sf", "ggpubr", "viridis", "patchwork", 
                      "raster", "wordcloud", "ggwordcloud", "terra", "plotly",
                      "gridExtra", "grid", "openxlsx", "officer", "magrittr", "mclust",
                      "foot", "units", "tidyr", "foreach", "doParallel", "future.apply", "dplyr",
                      "stringr", "purrr", "stars")

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


get_model_results <- function(data) {
  #survey design
  design <- svydesign(
    id = ~sn + ea,
    strata = ~Ward + settlement_type,
    weights = ~ind_weight,
    data = data,
    nest = TRUE
  )
  #adjusted
  adjusted_model <- svyglm(malaria_positive ~ net_own + net_use3, family = "binomial", design = design)
  adjusted_results <- broom::tidy(adjusted_model) %>%
    mutate(
      oddsratio = round(exp(estimate), 3),
      ci_low = round(exp(estimate - (1.96 * std.error)), 3),
      ci_high = round(exp(estimate + (1.96 * std.error)), 3),
      model = "adjusted"
    )
  #unadjusted: net_own only
  unadjusted_net_own <- svyglm(malaria_positive ~ net_own, family = "binomial", design = design)
  unadjusted_net_own_results <- broom::tidy(unadjusted_net_own) %>%
    mutate(
      oddsratio = round(exp(estimate), 3),
      ci_low = round(exp(estimate - (1.96 * std.error)), 3),
      ci_high = round(exp(estimate + (1.96 * std.error)), 3),
      model = "unadjusted"
    )
  # unadjusted: net_use only
  unadjusted_net_use <- svyglm(malaria_positive ~ net_use3, family = "binomial", design = design)
  unadjusted_net_use_results <- broom::tidy(unadjusted_net_use) %>%
    mutate(
      oddsratio = round(exp(estimate), 3),
      ci_low = round(exp(estimate - (1.96 * std.error)), 3),
      ci_high = round(exp(estimate + (1.96 * std.error)), 3),
      model = "unadjusted"
    )
  bind_rows(adjusted_results, unadjusted_net_own_results, unadjusted_net_use_results)
}

source("~/NMEP_classification/15_extraction_function.R", echo = T)
source("~/NMEP_classification/pop_estimate_function.R", echo = T)
