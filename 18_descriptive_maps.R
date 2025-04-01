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
vars <- c("mean_EVI", "u5_tpr_rdt", "distance_to_water", "settlement_type", "flood")

var_labels <- c(
  mean_EVI = "Mean Enhanced Vegetation Index (EVI)",
  distance_to_water = "Distance to Water (km)",
  flood = "Flood Risk",
  settlement_type = "Proportion of Homes Classified as Poor Housing",
  u5_tpr_rdt = "Under-5 Malaria Test Positivity Rate (%)"
)

# define color scale for each variable
color_scales <- list(
  "mean_EVI" = scale_fill_gradient(low = "white", high = "#385e3c", na.value = "grey"),
  "distance_to_water" = scale_fill_gradient(low = "white", high = "#542c5d", na.value = "grey"),
  "flood" = scale_fill_gradient(low = "white", high = "#2e5984", na.value = "grey"),
  "settlement_type" = scale_fill_gradient(low = "white", high = "#cc5500", na.value = "grey"),
  "u5_tpr_rdt" = scale_fill_gradient(low = "white", high = "red", na.value = "grey")
)

# function to create and save maps
map_variable <- function(shapefile, data, state_name, output_dir, vars) {
  
  message("Making maps for ", state_name)
  
  data$WardCode <- as.character(data$WardCode)
  shapefile$WardCode <- as.character(shapefile$WardCode)
  
  # merge shapefile with data
  merged_data <- shapefile %>%
    left_join(data, by = "WardCode") 
  
  # list to store plots
  plot_list <- list()
  
  for (var in vars) {
    if (var %in% names(merged_data)) {
      
      fill_scale <- color_scales[[var]]
      
      # generate map
      map_plot <- ggplot(merged_data) +
        geom_sf(aes(geometry = geometry, fill = !!sym(var))) +
        fill_scale +
        labs(title = var_labels[[var]], fill = var_labels[[var]]) +
        map_theme() +
        theme(
          plot.title = element_text(size = 10, hjust = 0.5),
        )
      
      # store plot in list
      plot_list[[var]] <- map_plot
    }
  }
  
  # combine plots into a grid (single page)
  combined_plot <- wrap_plots(plot_list, ncol = 2) + 
    plot_annotation(title = paste(state_name, "Maps")) & 
    theme(plot.title = element_text(hjust = 0.5)) 
  
  # save the combined plot as a PDF (single page)
  ggsave(file.path(plotsDir, paste0(state_name, "_maps.pdf")), 
         plot = combined_plot, width = 8, height = 8, dpi = 300, device = cairo_pdf)
  
  return(combined_plot)
}

# define state shapefiles and data
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

# loop through states and map each variable
for (state in names(state_shapefiles)) {
  map_variable(state_shapefiles[[state]], state_data[[state]], state, plotsDir, vars)
}
