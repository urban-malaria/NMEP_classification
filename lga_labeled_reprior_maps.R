# ==========================================================================================================================================
# Script Name: Reprioritization
# Purpose: Function to create reprioritization plots for 50% and 75% urban scenarios for each Nigerian state included in the analysis. 
# LGA boundaries are outlined in color. Reprioritized wards are labeled on each map. 
# ==========================================================================================================================================

source("load_path.R")
library(ggrepel)
library(sf)
library(ggplot2)


create_state_reprioritization_maps <- function(state_name) {
  
  # read in state and LGA shapefiles
  state_shp <- sf::st_read(file.path(StateShpDir, state_name, paste0(state_name, "_State.shp")))
  
  # extract LGA boundaries from the state shapefile (to add orange boundaries on map)
  lga_boundaries <- state_shp %>%  
    group_by(LGACode) %>%  # group by LGA code to merge geometries for each LGA  
    summarize(geometry = sf::st_union(geometry)) %>%  # merge geometries to create LGA boundaries  
    ungroup() 
  
  # read in ward prioritization data for both scenarios
  ward_50 <- read.csv(file.path(OutputsDir, "NMEP Presentation Reprioritization Tables", 
                                paste0(state_name), paste0(state_name, "_scenario_3.csv")))
  ward_75 <- read.csv(file.path(OutputsDir, "NMEP Presentation Reprioritization Tables", 
                                paste0(state_name), paste0(state_name, "_scenario_4.csv")))
  
  # match with shapefile by ward code for Kaduna, Niger, Delta, and Yobe
  if (state_name %in% c("Kaduna", "Niger", "Delta", "Yobe")) {
    ward_50$WardCode <- as.character(ward_50$WardCode)
    ward_75$WardCode <- as.character(ward_75$WardCode)
    state_shp$WardCode <- as.character(state_shp$WardCode)
    
    prioritized_50 <- state_shp %>% 
      left_join(ward_50, by = c("WardCode")) %>% 
      mutate(status = ifelse(is.na(WardPopulation), "Not Reprioritized", "Reprioritized"))
    
    prioritized_75 <- state_shp %>% 
      left_join(ward_75, by = c("WardCode")) %>% 
      mutate(status = ifelse(is.na(WardPopulation), "Not Reprioritized", "Reprioritized"))
  }
  
  # match with shapefile by ward name for Katsina (no ward code variable)
  if(state_name == "Katsina") {
    ward_50$SelectedWards <- as.character(ward_50$SelectedWards)
    ward_75$SelectedWards <- as.character(ward_75$SelectedWards)
    state_shp$WardName <- as.character(state_shp$WardName)
    
    prioritized_50 <- state_shp %>% 
      left_join(ward_50, by = c("WardName" = "SelectedWards")) %>% 
      mutate(status = ifelse(is.na(WardPopulation), "Not Reprioritized", "Reprioritized"))
    
    prioritized_75 <- state_shp %>% 
      left_join(ward_75, by = c("WardName" = "SelectedWards")) %>% 
      mutate(status = ifelse(is.na(WardPopulation), "Not Reprioritized", "Reprioritized"))
  }
  
  # # check for duplicate WardName entries in state_shp
  # duplicate_wards <- state_shp %>%
  #   group_by(WardName) %>%
  #   summarise(count = n()) %>%
  #   filter(count > 1)
  # 
  # if (nrow(duplicate_wards) > 0) {
  #   warning("The following WardNames appear more than once in state_shp:")
  #   print(duplicate_wards)
  # }
  
  # # check for WardNames in ward_50 that do not exist in state_shp
  # missing_wards <- ward_50 %>%
  #   filter(!WardCode %in% state_shp$WardCode)
  # 
  # if (nrow(missing_wards) > 0) {
  #   warning("The following WardNames in ward_50 do not exist in state_shp:")
  #   print(missing_wards)
  # }
  
  labels_50 <- prioritized_50 %>%
    dplyr::filter(status == "Reprioritized") %>%
    st_centroid() %>%
    mutate(coords = st_coordinates(.))
  labels_75 <- prioritized_75 %>%
    dplyr::filter(status == "Reprioritized") %>%
    st_centroid() %>%
    mutate(coords = st_coordinates(.))
  
  # ------------------------------------------------------------------------------------------------------------------------------#
  # ADD LGA NAMES TO DATA
  lga_shp <- sf::st_read(file.path(DataDir, "nigeria/NMEP_nigeria_shapefiles/LGAs/NGA_LGAs.shp"))

  # check and match crs
  if (st_crs(state_shp) != st_crs(lga_shp)) {
    lga_shp <- st_transform(lga_shp, st_crs(state_shp))
  }

  # make geometries valid
  lga_shp <- st_make_valid(lga_shp)
  state_shp <- st_make_valid(state_shp)

  # spatial join to attach the state name to each lga
  lga_with_state <- st_join(lga_shp, state_shp, left = FALSE)  # left = FALSE keeps only LGAs that intersect a state

  # re-filter to only include state of interest, filter to only ward name and LGA name
  lga_wards_names <- lga_with_state %>%
    dplyr::filter(State == state_name) %>%
    dplyr::select(State, LGA, WardName)

  # Remove the geometry column and convert to a regular data frame
  lga_wards_names_df <- lga_wards_names %>%
    st_drop_geometry() %>%
    dplyr::select(State, LGA, WardName)

  # add LGA names to dfs of reprioritized wards
  labels_50 <- labels_50 %>%
    left_join(lga_wards_names_df, by = c("WardName"))
  labels_75 <- labels_75 %>%
    left_join(lga_wards_names_df, by = c("WardName"))
  # ------------------------------------------------------------------------------------------------------------------------------#
  
  lga_color <- "orange"
  
  plot_50 <- ggplot(data = prioritized_50) +
    geom_sf(aes(geometry = geometry, fill = status)) +
    geom_sf(data = lga_boundaries, aes(geometry = geometry), color = lga_color, fill = NA, linewidth = 0.6) +
    geom_text_repel(
      data = labels_50,  # Now contains only the reprioritized wards
      aes(x = coords[,1], y = coords[,2], label = paste0(WardName, " (", LGA, ")")),
      color = "black",
      size = 3,  # Adjust text size
      box.padding = 0.5,  # Space around labels
      segment.color = "black",  # Black line from label to ward
      segment.size = 0.5,
      max.overlaps = Inf,  # Ensure all labels appear
      force = 10  # Helps spread labels further
    ) +
    scale_fill_manual(
      values = c("Not Reprioritized" = "#F5F6F7", "Reprioritized" = "#22AAE2"),
      guide = guide_legend(title = NULL)
    ) +
    labs(title = "Reprioritization Scenario 1", subtitle = "at least 50% of ward is urban") +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.text = element_text(size = 16),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    ) +
    map_theme()

  plot_75 <- ggplot(data = prioritized_75) +
    geom_sf(aes(geometry = geometry, fill = status)) +
    geom_sf(data = lga_boundaries, aes(geometry = geometry), color = lga_color, fill = NA, linewidth = 0.6) +
    geom_text_repel(
      data = labels_75,  # Now contains only the reprioritized wards
      aes(x = coords[,1], y = coords[,2], label = paste0(WardName, " (", LGA, ")")),
      color = "black",
      size = 3,  # Adjust text size
      box.padding = 0.5,  # Space around labels
      segment.color = "black",  # Black line from label to ward
      segment.size = 0.5,
      max.overlaps = Inf,  # Ensure all labels appear
      force = 10  # Helps spread labels further
    ) +
    scale_fill_manual(
      values = c("Not Reprioritized" = "#F5F6F7", "Reprioritized" = "#22AAE2"),
      guide = guide_legend(title = NULL)
    ) +
    labs(title = "Reprioritization Scenario 2", subtitle = "at least 75% of ward is urban") +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.text = element_text(size = 16),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    ) +
    map_theme()

  # arrange plots
  final <- plot_50 / plot_75
  
  # save output plot
  output_file <- file.path(FigDir, "maps_with_lga", paste0(Sys.Date(), "_", state_name, "_reprioritization_scenarios_50_75.pdf"))
  ggsave(output_file, final, width = 8, height = 12)
}

create_state_reprioritization_maps("Niger")
create_state_reprioritization_maps("Delta")
create_state_reprioritization_maps("Katsina")
create_state_reprioritization_maps("Kaduna")
create_state_reprioritization_maps("Yobe")

# ==========================================================================================================================================
# Separate function for Taraba as no wards were reprioritized in the 75% scenario
# ==========================================================================================================================================
taraba_maps <- function(state_name) {
  
  # read in state and LGA shapefiles
  state_shp <- sf::st_read(file.path(StateShpDir, state_name, paste0(state_name, "_State.shp")))
  
  # extract LGA boundaries from the state shapefile (to add orange boundaries on map)
  lga_boundaries <- state_shp %>%  
    group_by(LGACode) %>%  # group by LGA code to merge geometries for each LGA  
    summarize(geometry = sf::st_union(geometry)) %>%  # merge geometries to create LGA boundaries  
    ungroup() 
  
  # read in ward prioritization data for both scenarios
  ward_50 <- read.csv(file.path(OutputsDir, "NMEP Presentation Reprioritization Tables", 
                                paste0(state_name), paste0(state_name, "_scenario_3.csv"))) %>%
    left_join(state_ranks, by = c("SelectedWards" = "WardName"))
  
  prioritized_50 <- state_shp %>% 
    left_join(ward_50, by = c("WardName" = "SelectedWards")) %>% 
    mutate(status = ifelse(is.na(WardPopulation), "Not Reprioritized", "Reprioritized"))
  
  labels_50 <- prioritized_50 %>%
    dplyr::filter(status == "Reprioritized") %>%
    st_centroid() %>%
    mutate(coords = st_coordinates(.))
  
  # ------------------------------------------------------------------------------------------------------------------------------#
  # ADD LGA NAMES TO DATA
  lga_shp <- sf::st_read(file.path(DataDir, "nigeria/NMEP_nigeria_shapefiles/LGAs/NGA_LGAs.shp"))
  
  # check and match crs
  st_crs(state_shp) == st_crs(lga_shp)
  lga_shp <- st_transform(lga_shp, st_crs(state_shp))
  
  # make geometries valid
  lga_shp <- st_make_valid(lga_shp)
  state_shp <- st_make_valid(state_shp)
  
  # spatial join to attach the state name to each lga
  lga_with_state <- st_join(lga_shp, state_shp, left = FALSE)  # left = FALSE keeps only LGAs that intersect a state
  
  # re-filter to only include state of interest, filter to only ward name and LGA name
  lga_wards_names <- lga_with_state %>%
    dplyr::filter(State == state_name) %>%
    dplyr::select(State, LGA, WardName)
  
  # Remove the geometry column and convert to a regular data frame
  lga_wards_names_df <- lga_wards_names %>%
    st_drop_geometry() %>%
    dplyr::select(State, LGA, WardName)
  
  # add LGA names to dfs of reprioritized wards
  labels_50 <- labels_50 %>%
    left_join(lga_wards_names_df, by = c("WardName"))
  # ------------------------------------------------------------------------------------------------------------------------------#
  
  lga_color <- "orange"
  
  plot_50 <- ggplot(data = prioritized_50) +
    geom_sf(aes(geometry = geometry, fill = status)) +
    geom_sf(data = lga_boundaries, aes(geometry = geometry), color = lga_color, fill = NA, linewidth = 0.6) +
    geom_text_repel(
      data = labels_50,  # Now contains only the reprioritized wards
      aes(x = coords[,1], y = coords[,2], label = paste0(WardName, " (", LGA, ")")),
      color = "black",
      size = 3,  # Adjust text size
      box.padding = 0.5,  # Space around labels
      segment.color = "black",  # Black line from label to ward
      segment.size = 0.5,
      max.overlaps = Inf,  # Ensure all labels appear
      force = 10  # Helps spread labels further
    ) +
    scale_fill_manual(
      values = c("Not Reprioritized" = "#F5F6F7", "Reprioritized" = "#22AAE2"),
      guide = guide_legend(title = NULL)
    ) +
    labs(title = "Reprioritization Scenario 1", subtitle = "at least 50% of ward is urban") +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.text = element_text(size = 16),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    ) +
    map_theme()
  
  # save output plot
  output_file <- file.path(FigDir, "maps_with_lga", paste0(Sys.Date(), "_", state_name, "_reprioritization_scenarios_50.pdf"))
  ggsave(output_file, plot_50, width = 8, height = 12)
}

create_state_reprioritization_maps("Taraba")
