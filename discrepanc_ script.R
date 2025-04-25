library(sf)
library(readxl)
library(dplyr)
library(stringr)

source("load_path.R")


Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", gsub("OneDrive", "", Sys.getenv("HOME")))))
Drive <- file.path(gsub("[//]", "/", Drive))
DriveDir <- file.path(Drive, "Urban Malaria Proj Dropbox", "urban_malaria")

states <- c("Kaduna", "Katsina", "Niger", "Yobe", "Taraba")  


shapefile_dir <- file.path(DriveDir, "data/nigeria/nigeria_shapefiles/shapefiles/ShinyApp_shapefiles/all_reprioritization_nmep_states/STATES")
itn_dir <- file.path(DriveDir, "data/nigeria/ITN_distribution")

normalize <- function(x) str_to_lower(str_trim(x))


for (state in states) {
  
  cat("\n==================", toupper(state), "==================\n")
  

  shp_path <- file.path(shapefile_dir, state, paste0(state, "_State.shp"))
  
  if (!file.exists(shp_path)) {
    warning(paste("Shapefile not found for", state))
    next
  }
  
  shp <- sf::st_read(shp_path, quiet = TRUE)
  
  # Read New cleaned ITN file
 new_itn_path <- file.path(itn_dir, "cleaned", paste0("pbi_distribution_", tolower(state), "_clean.xlsx"))
  
  if (!file.exists(new_itn_path)) {
    warning(paste("New ITN file not found for", state))
    next
  }
  
  
  new_itn <- read_excel(new_itn_path)
  
  # Read Old ITN ward-level summary
  
  old_itn_path <- file.path(itn_dir, "household_member_ward_summaries_Itn_distribution/itns_validation",
                             paste0("ITN_distribution_total_ward_", tolower(state), "_2022.xlsx"))
  
  if (!file.exists(old_itn_path)) {
    warning(paste("old data, ITN file not found for", state))
    next
  }
  
  old_itn <- read_excel(old_itn_path)
  
  shp_wards <- shp$WardName
  new_wards <- new_itn$Ward
  old_wards <- old_itn$Ward
  
  # Comparison
  mismatch_new <- setdiff(shp_wards, new_wards)
  
  mismatch_old <- setdiff(shp_wards, old_wards)
  
  if (length(mismatch_new) > 0) {
    cat("\nWards in shapefile but not in new data:\n")
    print(mismatch_new)
  } else {
    cat("No mismatches found with new data data.\n")
  }
  
  
  if (length(mismatch_old) > 0) {
    cat("\nWards in shapefile but not in old dataset data:\n")
    print(mismatch_old)
  } else {
    cat("No mismatches found with old dataset data.\n")
  }
}




