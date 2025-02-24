### prioritized areas

# load necessary scripts
source("load_path.R")
source("pop_estimate_function.R", echo = FALSE)

# read in kano shapefiles
Kano_shp <- sf::st_read(file.path(StateShpDir, "Kano", "Kano_State.shp"))
Kano_shpefile <- sf::st_read(file.path(ShpfilesDir, "Kano_Ibadan/Kano-Ibadan.shp")) %>%  
  dplyr::filter(StateCode == "KN")
Kano_LGA <- st_read(file.path(DataDir, "nigeria/kano_ibadan/kano_ibadan_shape_files/Kano_metro_sixLGA_shapes/Kano_metro_sixLGA_shapes.shp"))

# read in kano ward-level data and merge with extracted data
Kano_variables <- inner_join(Kano_shpefile, read.csv(file.path(OutputsDir, "Final Extractions", "Kano_plus.csv")),  
                             by = c("WardCode" = "WardCode", "WardName" = "WardName")) %>%  
  st_drop_geometry() %>%
  dplyr::select(X, WardName, urbanPercentage, WardCode) %>%  
  mutate(
    classification_20 = ifelse(urbanPercentage > 20, "Urban", "Rural"),
    classification_30 = ifelse(urbanPercentage > 30, "Urban", "Rural"),
    classification_50 = ifelse(urbanPercentage > 50, "Urban", "Rural"),
    classification_75 = ifelse(urbanPercentage > 75, "Urban", "Rural")
  )  %>%
  mutate(num = 1:n())

# read in ward rankings data
Kano_ranks <- read.csv(file.path(OutputsDir, "rankings", "kano_metropolis_rankings.csv"))  
Kano_ranks$WardName <- stringr::str_trim(Kano_ranks$wardname)
Kano_ranks <- Kano_ranks %>%  
  mutate(num = 1:n())

# read in itn distribution data
Kano_itn_data <- read.csv(file.path(DataDir, "nigeria/ITN_distribution/pbi_distribution_Kano.csv"))  
Kano_itn_clean <- Kano_itn_data %>%  
  rename(population = N_FamilyMembers,
         Ward = AdminLevel3) %>%  
  dplyr::select(population, Ward) %>%  
  group_by(Ward) %>%  
  summarise(Population = sum(population, na.rm = TRUE)) %>%  
  ungroup() %>%  
  mutate(num = 1:n())

# combine data into one dataset
combined_wards <- left_join(Kano_variables, Kano_ranks, by = c("WardName" = "WardName"))
combined_wards2 <- left_join(combined_wards, Kano_itn_clean, by = c("WardName" = "Ward"))

# prioritize wards based on urban classification thresholds
prioritized_kano2 <- prioritize_wards(data = combined_wards2,  
                                      population_col = "Population",  
                                      rank_col = "ranks",  
                                      class_col = "classification_50",  
                                      ward_col = "WardName",  
                                      target_percentage = 30)  

prioritized_kano3 <- prioritize_wards(data = combined_wards2,  
                                      population_col = "Population",  
                                      rank_col = "ranks",  
                                      class_col = "classification_75",  
                                      ward_col = "WardName",  
                                      target_percentage = 30)  

# read in ward prioritization data
ward_50 <- read.csv(file.path(OutputsDir, "NMEP Presentation Reprioritization Tables",  
                              "Kano_metropolis", paste0("kano_scenario_3", ".csv"))) %>%  
  left_join(Kano_ranks, by = c("SelectedWards" = "WardName"))
priorite_50 <- Kano_shp %>%  
  left_join(ward_50, by = c("WardCode")) %>%  
  filter(!is.na(WardPopulation)) %>%  
  mutate(new_rank = rank(ranks))

ward_75 <- read.csv(file.path(OutputsDir, "NMEP Presentation Reprioritization Tables",  
                              "Kano_metropolis", paste0("kano_scenario_4", ".csv"))) %>%  
  left_join(Kano_ranks, by = c("SelectedWards" = "WardName"))
priorite_75 <- Kano_shp %>%  
  left_join(ward_75, by = c("WardCode")) %>%  
  filter(!is.na(WardPopulation)) %>%  
  mutate(new_rank = rank(ranks))

# separate maps for each lga at 75% urban classification
LGA <- c("Tarauni", "Gwale", "Dala", "Fagge", "Nassarawa", "Kano Municipal")
LGAcode <- c("20001", "20012", "20024", "20030", "20037", "20043")
plots <- list()

for (x in seq_along(LGA)) {
  plots[[x]] <- ggplot(data = filter(Kano_LGA, LGAName == sym(LGA[[x]]))) +
    geom_sf(aes(color = LGAName)) +
    geom_sf(data = filter(priorite_75, LGACode == sym(LGAcode[[x]])), aes(geometry = geometry), fill = "#2AACE3") +
    geom_text_repel(data = filter(priorite_75, LGACode == sym(LGAcode[[x]])),
                    aes(label = wardname, geometry = geometry), color = 'black',
                    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1, max.overlaps = Inf) +
    labs(title = LGA[[x]]) +
    map_theme() +
    xlab("") +
    ylab("") +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "none"
    )
}

# Determine fixed xlim and ylim across all LGAs (adjust as needed)
common_xlim <- c(min(st_coordinates(Kano_LGA)[,1]), max(st_coordinates(Kano_LGA)[,1]))
common_ylim <- c(min(st_coordinates(Kano_LGA)[,2]), max(st_coordinates(Kano_LGA)[,2]))

for (x in seq_along(LGA)) {
  plots[[x]] <- ggplot(data = filter(Kano_LGA, LGAName == sym(LGA[[x]]))) +
    geom_sf(aes(color = LGAName)) +
    geom_sf(data = filter(priorite_75, LGACode == sym(LGAcode[[x]])), aes(geometry = geometry), fill = "#2AACE3") +
    geom_text_repel(data = filter(priorite_75, LGACode == sym(LGAcode[[x]])),
                    aes(label = wardname, geometry = geometry), color = 'black',
                    stat = "sf_coordinates",
                    min.segment.length = 0,  # Ensures segments are always drawn
                    segment.size = 0.5,      # Makes segment lines more visible
                    segment.color = "black",
                    box.padding = 1,         # Pushes text further from points
                    point.padding = 0.5,     # Extra space between text and point
                    force = 10,               # Stronger force to spread labels
                    size = 3.5, 
                    max.overlaps = Inf) +
    labs(title = LGA[[x]]) +
    map_theme() +
    xlab("") +
    ylab("") +
    coord_sf(xlim = common_xlim, ylim = common_ylim, expand = FALSE) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "none"
    )
}

# arrange plots
p <- (plots[[1]] + plots[[2]]) / plots[[3]] + plots[[4]] / plots[[5]] + plots[[6]]

# save output plot
output_file <- file.path(FigDir, "urban_50_75", paste0(Sys.Date(), "_Kano_reprioritization_scenarios_75_seperate_LGA_2.pdf"))
ggsave(output_file, p, width = 16, height = 20)




############ FUNCTION TO DO THIS - DOESN'T WORK YET
generate_reprioritization_plots <- function(state_name, threshold) {
  # Load necessary scripts
  source("load_path.R")
  source("pop_estimate_function.R", echo = FALSE)
  
  # Define file paths
  state_shp_file <- file.path(StateShpDir, state_name, paste0(state_name, "_State.shp"))
  state_lga_shp_file <- file.path(DataDir, "nigeria", state_name, paste0(state_name, "_LGA_shapes.shp"))
  ward_data_file <- file.path(OutputsDir, "Final Extractions", paste0(state_name, "_plus.csv"))
  rankings_file <- file.path(OutputsDir, "rankings", paste0(tolower(state_name), "_metropolis_rankings.csv"))
  scenario_file <- file.path(OutputsDir, "NMEP Presentation Reprioritization Tables", state_name, 
                             paste0(tolower(state_name), "_scenario_", threshold, ".csv"))
  
  # Read in shapefiles
  state_shp <- sf::st_read(state_shp_file)
  state_lga <- sf::st_read(state_lga_shp_file)
  
  # Read and merge ward-level data
  ward_data <- inner_join(
    sf::st_read(state_lga_shp_file),
    read.csv(ward_data_file),
    by = c("WardCode" = "WardCode", "WardName" = "WardName")
  ) %>%
    st_drop_geometry() %>%
    mutate(classification = ifelse(urbanPercentage > threshold, "Urban", "Rural"))
  
  # Read in ward rankings and prioritization data
  rankings <- read.csv(rankings_file) %>% mutate(WardName = stringr::str_trim(wardname))
  scenario_data <- read.csv(scenario_file) %>% left_join(rankings, by = c("SelectedWards" = "WardName"))
  prioritized_wards <- state_shp %>% left_join(scenario_data, by = "WardCode") %>% filter(!is.na(WardPopulation))
  
  # Define LGAs for the state (lookup table should be pre-defined)
  lga_list <- get_lga_list(state_name)  # Function to retrieve LGA names and codes
  
  # Define common plot limits
  common_xlim <- c(min(st_coordinates(state_lga)[,1]), max(st_coordinates(state_lga)[,1]))
  common_ylim <- c(min(st_coordinates(state_lga)[,2]), max(st_coordinates(state_lga)[,2]))
  
  # Generate plots
  plots <- list()
  for (x in seq_along(lga_list$LGA)) {
    plots[[x]] <- ggplot(data = filter(state_lga, LGAName == lga_list$LGA[[x]])) +
      geom_sf(aes(color = LGAName)) +
      geom_sf(data = filter(prioritized_wards, LGACode == lga_list$LGACode[[x]]), aes(geometry = geometry), fill = "#2AACE3") +
      geom_text_repel(data = filter(prioritized_wards, LGACode == lga_list$LGACode[[x]]),
                      aes(label = wardname, geometry = geometry), color = 'black',
                      stat = "sf_coordinates", segment.size = 0.5, segment.color = "black",
                      box.padding = 1, point.padding = 0.5, force = 10, size = 3.5, max.overlaps = Inf) +
      labs(title = lga_list$LGA[[x]]) +
      map_theme() +
      xlab("") +
      ylab("") +
      coord_sf(xlim = common_xlim, ylim = common_ylim, expand = FALSE) +
      theme_minimal() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "white", color = NA),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            legend.position = "none")
  }
  
  # Arrange and save the plots
  p <- wrap_plots(plots, ncol = 2)
  output_file <- file.path(FigDir, "urban_reprioritization", paste0(Sys.Date(), "_", state_name, "_reprioritization_", threshold, ".pdf"))
  ggsave(output_file, p, width = 16, height = 20)
}

 