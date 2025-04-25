source("load_path.R", echo = FALSE)
source("pop_estimate_function.R", echo = FALSE)

Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", gsub("OneDrive", "", Sys.getenv("HOME")))))
Drive <- file.path(gsub("[//]", "/", Drive))
DriveDir <- file.path(Drive, "Urban Malaria Proj Dropbox", "urban_malaria")


shapefile_dir <-file.path(DriveDir, "data/nigeria/nigeria_shapefiles/shapefiles/ShinyApp_shapefiles/all_reprioritization_nmep_states/STATES")
itn_dir <- file.path(DriveDir, "nigeria/ITN_distribution")

OutputsDir <- file.path(DriveDir, "/projects/urban_microstratification/Shiny App")


sf::sf_use_s2(FALSE)

states_to_run <- c( "Katsina", "Niger", "Yobe", "Taraba")   


for (state_name in states_to_run) {
  
  state_name <- "Niger"
  
  message(paste("Processing", state_name, "..."))
  
  # === Format Paths and Filenames ===
  state_display_name <- tools::toTitleCase(state_name)
  
  pathLGA <- file.path(DriveDir, "data/nigeria/nigeria_shapefiles/Nigeria LGAs shapefile (260216)")

  
  state_shapefile_path <- file.path(shapefile_dir, state_name, paste0(state_display_name, "_State.shp"))
  state_shapefile <- sf::st_read(state_shapefile_path, quiet = TRUE)
  
  lga_shp <- sf::st_read(file.path(pathLGA, "NGA_LGAs.shp"), quiet = TRUE)
  state_shapefile <- st_transform(state_shapefile, st_crs(lga_shp))
  

  state_variables_file <- file.path(shapefile_dir,  paste0(state_display_name, "_plus.csv"))
  if (!file.exists(state_variables_file)) {
    warning(paste("Missing variable file for", state_name)); next
  }
  
  state_variables <- read.csv(state_variables_file) %>%
    distinct(WardCode, .keep_all = TRUE) %>%
    dplyr::select(WardName, urbanPercentage, WardCode) %>%
    mutate(
      classification_20 = ifelse(urbanPercentage > 20, "Urban", "Rural"),
      classification_30 = ifelse(urbanPercentage > 30, "Urban", "Rural"),
      classification_50 = ifelse(urbanPercentage > 50, "Urban", "Rural"),
      classification_75 = ifelse(urbanPercentage > 75, "Urban", "Rural"),
      num = 1:n()
    )
  
  

  state_ranks_file <- file.path(OutputsDir, "rankings", paste0(state_display_name, "_rankings.csv"))
  if (!file.exists(state_ranks_file)) {
    warning(paste("Missing rankings file for", state_name)); next
  }
  
  state_ranks <- read.csv(state_ranks_file) %>%
    mutate(WardName = stringr::str_trim(WardName),
           num = 1:n())
  
  

  itn_new_path <- file.path(ITNDir, "cleaned", paste0("pbi_distribution_", state_display_name, "_clean.xlsx"))
  itn_old_path <- file.path(ITNDir, "household_member_ward_summaries_Itn_distribution/itns_validation",
                            paste0("ITN_distribution_total_ward_", state_display_name, "_2022.xlsx"))
  
  if (!file.exists(itn_new_path) | !file.exists(itn_old_path)) {
    warning(paste("Missing ITN files for", state_name)); next
  }
  
  itn_new <- readxl::read_excel(itn_new_path)
  itn_old <- readxl::read_excel(itn_old_path) %>%
    rename(Population = `Sum of N_FamilyMembers`) %>%
    mutate(num = 1:n())
  
  comparison <- list(itn_new, itn_old)
  
  # === Combine Data ===
  combined_wards <- left_join(state_variables, state_ranks, by = "WardCode")
  combined_wards2 <- lapply(seq_along(comparison), function(x)
    left_join(combined_wards, comparison[[x]], by = c("WardName.x" = "Ward")) %>%
      distinct(WardName.x, urbanPercentage, WardCode, .keep_all = TRUE)
  )
  
  # === Prioritize and Plot ===
  colums <- c("classification_50", "classification_75")
  prioritized_list <- list()
  plots <- list()
  
  for (ii in seq_along(colums)) {
    
    prioritized_list[[ii]] <- lapply(seq_along(combined_wards2), function(x)
      prioritize_wards(
        data = combined_wards2[[x]],
        population_col = "Population",
        rank_col = "ranks",
        class_col = colums[ii],
        ward_col = "WardName.x",
        target_percentage = 30
      )
    )
    
  }
    
    # Output directory
    output_dir <- file.path(OutputsDir, "NMEP Presentation Reprioritization Tables", state_display_name)
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

    # Save CSV (first of the two comparisons)
    write.csv(prioritized_list[[ii]][[1]],
              file.path(output_dir, paste0(state_name, "_scenario_", ii, ".csv")),
              row.names = FALSE)

    # Generate Plot
    combined_plot <- state_shapefile %>%
      left_join(prioritized_list[[ii]][[1]], by = c("WardName" = "SelectedWards")) %>%
      mutate(status = ifelse(is.na(WardPopulation), "Not Reprioritized", "Reprioritized"))

    plots[[ii]] <- ggplot(data = combined_plot) +
      geom_sf(aes(fill = status)) +
      scale_fill_manual(values = c("Not Reprioritized" = "red", "Reprioritized" = "green"),
                        name = "Status") +
      labs(
        title = paste("Reprioritization Scenario", ii, "-", state_display_name),
        caption = paste0("Suggested wards based on:\n",
                         "- Composite ranks (EVI, u5_tpr, water access, settlement, flood risk)\n",
                         "- At least ", gsub("classification_", "", colums[ii]), "% urban area")
      ) +
      map_theme()
  }


  message(paste("Completed processing for", state_name))
  
