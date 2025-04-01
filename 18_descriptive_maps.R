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
kano_shp <- sf::st_read(file.path(StateShpDir ,"Kano", "Kano_State.shp"))

# read in extracted data for each state
kaduna_data <- read.csv(file.path(OutputsDir, "Final Extractions/kaduna_plus.csv"))
katsina_data <- read.csv(file.path(OutputsDir, "Final Extractions/katsina_plus.csv"))
niger_data <- read.csv(file.path(OutputsDir, "Final Extractions/niger_plus.csv"))
taraba_data <- read.csv(file.path(OutputsDir, "Final Extractions/taraba_plus.csv"))
yobe_data <- read.csv(file.path(OutputsDir, "Final Extractions/yobe_plus.csv"))
kano_data <- read.csv(file.path(OutputsDir, "Final Extractions/kano_plus.csv"))

# list of variables to map
vars <- c("mean_EVI", "mean_NDVI", "mean_rainfall", "distance_to_water", "RH_mean", "temp_mean", 
          "housing_quality", "pfpr", "avgRAD", "flood", "NDWI", "NDMI", "elevation", 
          "mean_soil_wetness", "settlement_type", "u5_tpr_rdt", "urbanPercentage")

var_labels <- c(
  mean_EVI = "Mean EVI",
  mean_NDVI = "Mean NDVI",
  mean_rainfall = "Mean Rainfall (mm)",
  distance_to_water = "Distance to Water (km)",
  RH_mean = "Mean Relative Humidity (%)",
  temp_mean = "Mean Temperature (°C)",
  housing_quality = "Housing Quality Index",
  pfpr = "Plasmodium Falciparum Prevalence (%)",
  avgRAD = "Average Nighttime Lights (RAD)",
  flood = "Flood Risk",
  NDWI = "Normalized Difference Water Index",
  NDMI = "Normalized Difference Moisture Index",
  elevation = "Elevation (m)",
  mean_soil_wetness = "Mean Soil Wetness",
  settlement_type = "Settlement Type",
  u5_tpr_rdt = "Under-5 Malaria Test Positivity Rate (%)",
  urbanPercentage = "Urban Percentage (%)"
)


# Function to create and save maps
map_variable <- function(shapefile, data, state_name, output_dir, vars) {
  
  message("Making maps for ", state_name)
  
  data$WardCode <- as.character(data$WardCode)
  shapefile$WardCode <- as.character(data$WardCode)
  
  # make pfpr a percentage
  data$pfpr <- data$pfpr * 100
  
  # Merge shapefile with data
  merged_data <- shapefile %>%
    left_join(data, by = "WardCode") 
  
  # Create output directory if it doesn't exist
  state_output_dir <- file.path(output_dir, state_name)
  if (!dir.exists(state_output_dir)) dir.create(state_output_dir, recursive = TRUE)
  individual_output_dir <- file.path(state_output_dir, "individual plots")
  if (!dir.exists(individual_output_dir)) dir.create(individual_output_dir, recursive = TRUE)
  
  plot_list <- list()
  
  for (var in vars) {
    if (var %in% names(merged_data)) {
      
      # Generate map
      map_plot <- ggplot(merged_data) +
        geom_sf(aes(geometry = geometry, fill = !!sym(var))) +
        scale_fill_gradient(low = "#DFECF7", high = "#203569", na.value = "orange") +
        labs(title = paste(state_name, "-", var_labels[[var]]), fill = var_labels[[var]]) +
        map_theme() +
        theme(
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
        )
      
      # Save map
      ggsave(filename = file.path(individual_output_dir, paste0(var, ".png")), 
             plot = map_plot, width = 8, height = 6, dpi = 300)
      
      # Store plot in list
      plot_list[[var]] <- map_plot
    }
  }
  # Save all plots for the state in a single PDF
  pdf(file.path(state_output_dir, paste0(state_name, "_maps.pdf")), width = 8, height = 6)
  for (plot in plot_list) {
    print(plot)
  }
  dev.off()
  
  return(plot_list)
}

# Define state shapefiles and data
state_shapefiles <- list(
  "Kaduna" = kaduna_shp,
  "Katsina" = katsina_shp,
  "Niger" = niger_shp,
  "Taraba" = taraba_shp,
  "Yobe" = yobe_shp,
  "Kano" = kano_shp
)

state_data <- list(
  "Kaduna" = kaduna_data,
  "Katsina" = katsina_data,
  "Niger" = niger_data,
  "Taraba" = taraba_data,
  "Yobe" = yobe_data,
  "Kano" = kano_data
)

plotsDir <- file.path(plotsDir, "covariate maps")

# Loop through states and map each variable
for (state in names(state_shapefiles)) {
  map_variable(state_shapefiles[[state]], state_data[[state]], state, plotsDir, vars)
}
