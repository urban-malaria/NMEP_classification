# ==========================================================================================================================================
# Script Name: Plots for Presentation
# Author: Grace Legris, Research Data Analyst
# Date: 12/12/24
# Purpose: Prepare Plots for 12/13/24 Presentation - WET SEASON DATA ONLY
# ==========================================================================================================================================

## =========================================================================================================================================
### Read in Data
## =========================================================================================================================================

rm(list = ls())

# script with directories, functions, etc
source("/Users/grace/Desktop/UMP/NMEP_classification_my_fork/load_path.R", echo = T)

wet <- read_dta(file.path(KanoFieldData, "kano_long_wetseason_household_members_with_ind_nets.dta"))

# data cleaning: WET
wet <- wet %>%
  dplyr::select(sn, hl1, hl4, hl5, hl6, agebin, #identifiers
                women_q223, # if woman is currently pregnant
                women_q702, # woman's malaria test result
                q302, #test result
                Wardn, #settle_type, 
                settlement1, enumeration_area, 
                ward_weight, ea_settlement_weight, overall_hh_weight, ind_weights_hh,  #weights
                nh101a, nh105, slept_under_net, #net ownership/ use
                bi12i, # interview visit 1 date
                longitude, latitude # (of household)
                #bi9
  ) %>%
  filter(!is.na(settlement1)) %>%  
  filter(!Wardn == "") %>%  
  filter(!is.na(q302)) %>%  # remove non-tested individuals, included this line after running surveyed population
  mutate(unique_id = paste(sn, hl1, sep = "_")) %>% 
  rename(ward = Wardn,
         settlement_type = settlement1,
         ea = enumeration_area, 
         ea_weight = ea_settlement_weight,
         hh_weight = overall_hh_weight,
         ind_weight = ind_weights_hh,
         net_ownership = nh101a,
         net_use = slept_under_net,
         sex = hl4,
         age = hl5,
         dob = hl6,
         pregnant = women_q223,
         visit1_date = bi12i,
         net_use_frequency = nh105
  ) %>% 
  mutate(net_use = ifelse(is.na(net_use), 0, net_use), # set all NA values of net_use to 0 (did not use)
         net_own = ifelse(net_ownership == 1, 1, 0),
         malaria_positive = ifelse(q302 == 1, 1, 0), # convert all to 0 and 1
         woman_malaria_positive = ifelse(women_q702 == 1, 1, 0)) %>% # same for woman's test result var
  # net_use2 is a cleaned version of net_use where: 
  # if the person does not own a net (net_own == 0), their net_use value is set to NA (since they couldn’t use a net they don’t own).
  # if they own a net (net_own == 1), their net_use value remains unchanged.
  mutate(net_use2 = case_when(
    net_own == 0 ~ NA_real_, net_own == 1 ~ net_use)) %>% 
  # net_use3 is further cleaning of net_use2, with more specific handling:
  # if the person does not own a net and reported using a net (net_use == 1), their value is set to NA (impossible situation).
  # if the person does not own a net and reported not using a net (net_use == 0), their value is set to 0 (logical case).
  # if the person owns a net (net_own == 1), their net_use value remains unchanged.
  mutate(net_use3 = case_when(
    net_own == 0 & net_use == 1 ~ NA_real_,
    net_own == 0 & net_use == 0 ~ 0,
    net_own == 1 ~ net_use)) %>% ##net use cleaned for model
  mutate(age_cat = case_when(
    age <= 5 ~ "under 5",
    age >= 6 & age <= 10 ~ "6–10",
    age >= 11 & age <= 17 ~ "11–17",
    age >= 18 & age <= 30 ~ "18–30",
    age > 30 ~ "31+"
  ))

# add "Wet" identifier variable and reorder variables
wet <- wet %>%
  mutate(season = "Wet") %>%
  dplyr::select(sn, hl1, sex, age, dob, age_cat, pregnant, ward, settlement_type, visit1_date, unique_id, malaria_positive,
                woman_malaria_positive, season, ea, ward_weight, ea_weight, hh_weight, ind_weight, net_use_frequency,
                net_use, net_own, net_use2, net_use3, longitude, latitude)

# APPLY SURVEY WEIGHTING
wet_weighted <- svydesign(
  id = ~sn + ea,  # specify sampling units
  strata = ~ward + settlement_type,  # stratify by ward and settlement type
  weights = ~hh_weight,  # apply household weights
  data = wet,  # input dataset
  nest = TRUE  # ensure proper nesting of survey levels
)

## =========================================================================================================================================
### PLOT 1: Bar Plot for TPR by Settlement Type
## =========================================================================================================================================

weight_adjusted_tpr <- wet %>%
  filter(settlement_type != "") %>% 
  group_by(settlement_type) %>% 
  summarise(positive = sum(malaria_positive), 
            total = n(),
            negative = total - positive,
            tpr = round(sum(malaria_positive * hh_weight, na.rm = T) / sum(hh_weight, na.rm = T) * 100, 3),
            compliment = 100 - tpr)


new_data <- weight_adjusted_tpr %>% 
  dplyr::select(settlement_type, positive, negative) %>% 
  reshape2::melt(id = c("settlement_type"))

names(new_data) <- c("settlement_type", "result", "value")

labels_new_data <- weight_adjusted_tpr %>% 
  dplyr::select(settlement_type, tpr, compliment) %>% 
  reshape2::melt(id = c("settlement_type")) %>% 
  mutate(variable = ifelse(variable == "tpr", "positive", "negative"))

names(labels_new_data) <- c("settlement_type", "result", "percentage")

plotting_data <- inner_join(new_data, labels_new_data) %>% 
  mutate(plot_position = cumsum(value) - (value)) %>% 
  mutate(result = case_when(
    result == "positive" ~ "Positive",
    result == "negative" ~ "Negative",
    TRUE ~ result
  ))

# combine informal settlement and slum
plotting_data_combined <- plotting_data %>%
  mutate(settlement_type = case_when(
    settlement_type %in% c("Informal", "Slum") ~ "Informal + Slum",  # Combine Informal and Slum
    TRUE ~ settlement_type  # Keep other settlement types unchanged
  )) %>%
  group_by(settlement_type, result) %>%
  summarise(
    value = sum(value),  # Sum values for combined rows
    plot_position = min(plot_position),  # Retain the earliest position for plotting
    .groups = "drop"
  )

# add percentages and create percentage label for plot
plotting_data_combined <- plotting_data_combined %>%
  group_by(settlement_type) %>%
  mutate(percentage = (value / sum(value)) * 100) %>%
  ungroup() %>%
  mutate(
    label = paste0(round(percentage, 1), "%")  # percentage labels
  )

tpr_settlement_bar <- ggplot(data = plotting_data_combined) +
  geom_bar(aes(x = settlement_type, y = value, fill = result), 
           stat = "identity", position = "stack") +
  geom_text(aes(x = settlement_type, y = value, label = label),  
            color = "white",
            size = 4, nudge_y = 10) +
  scale_fill_manual(values = c("Negative" = "#f2a5a1", "Positive" = "#c55c80")) +
  labs(title = "Malaria Test Results by Settlement Type",
       subtitle = "Wet Season Data Only",
       x = "Settlement Type",
       y = "Number of People Tested for Malaria",
       fill = "Malaria RDT Result") +
  theme_bw(base_size = 12, base_family = "") +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )
tpr_settlement_bar

# save as .pdf
ggsave(filename = paste0(NMEPOutputs, "/presentation/", Sys.Date(), '_weighted_tpr_settlement.pdf'), plot = tpr_settlement_bar, width = 6, height = 6)


## =========================================================================================================================================
### PLOT 2: Box Plot for TPR by Settlement Type
## =========================================================================================================================================

# recode all slums to be called informal 
wet_recoded <- wet %>%
  mutate(settlement_type = case_when(
    settlement_type == "Slum" ~ "Informal",
    TRUE ~ settlement_type
  ))

EA_weight_adjusted_tpr <- wet_recoded %>%
  group_by(settlement_type, ea, ward) %>%
  summarise(
    Positive = sum(malaria_positive),
    total = n(),
    Negative = total - Positive,
    tpr = round(sum(malaria_positive * hh_weight) / sum(hh_weight) * 100, 3),
    compliment = 100 - tpr,
    # Calculate the centroid of the household locations within each EA
    avg_latitude = mean(latitude, na.rm = TRUE),  # Take the average latitude for each EA
    avg_longitude = mean(longitude, na.rm = TRUE)  # Take the average longitude for each EA
  )

# save this as .csv
write.csv(EA_weight_adjusted_tpr, file.path(NMEPOutputs, "EA_tpr_data.csv"), row.names = F) 

# rename "informal" to "informal + slum"
EA_weight_adjusted_tpr <- EA_weight_adjusted_tpr %>%
  mutate(settlement_type = case_when(
    settlement_type == "Informal" ~ "Informal + Slum",
    TRUE ~ settlement_type
  ))

tpr_settlement_box <- ggplot(EA_weight_adjusted_tpr, aes(x = settlement_type, y = tpr)) +
  geom_boxplot(outlier.shape = NA, fill = "#F5F5F5", color = "black") +  # box plot without outliers
  geom_jitter(aes(color = settlement_type, size = total), width = 0.08, alpha = 0.6) +  # jitter for individual EAs
  scale_color_manual(values = c("#6baed6", "#74c476")) +
  labs(
    title = "Distribution of Malaria TPR by \nSettlement Type and Enumeration Area",
    subtitle = "Wet Season Data Only",
    x = "Settlement Type",
    y = "Test Positivity Rate (TPR) (%)",
    color = "Settlement Type",
    size = "Number Tested per EA"
  ) +
  theme_bw(base_size = 12, base_family = "") +  # Clean theme
  theme(
    legend.position = "right",  # Adjust legend position
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )
tpr_settlement_box

# save as .pdf
ggsave(filename = paste0(NMEPOutputs, "/presentation/", Sys.Date(), '_weighted_box.pdf'), plot = tpr_settlement_box, width = 6, height = 6)

 ## =========================================================================================================================================
### PLOT 3: Map of TPR by Ward with Overlaid EA Estimate
## =========================================================================================================================================

# read in Kano shapefile
kano_shapefile <- "/Users/grace/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/nigeria_shapefiles/shapefiles/ShinyApp_shapefiles/Kano/Kano.shp"
kano.shp <- st_read(kano_shapefile)

# rename vars
kano.shp <- kano.shp %>%
  rename(ward = "WardName")

# merge spatial data with TPR data
merged_data <- kano.shp %>%
  left_join(EA_weight_adjusted_tpr, by = "ward")

# Ensure 'merged_data' is an sf object (if it is not already)
merged_data_sf <- st_as_sf(merged_data, wkt = "geometry")

# First, ensure your merged_data_sf is in the correct spatial format (sf object)
merged_data_sf <- st_as_sf(merged_data_sf, coords = c("avg_longitude", "avg_latitude"), crs = 4326, remove = FALSE)

merged_data_sf <- merged_data_sf %>% 
  mutate(
    tpr_category = case_when(
      tpr <= 20 ~ "0-20",        # Category for TPR between 0 and 20
      tpr <= 40 ~ "21-40",       # Category for TPR between 21 and 40
      tpr <= 60 ~ "41-60",       # Category for TPR between 41 and 60
      tpr <= 80 ~ "61-80",       # Category for TPR between 61 and 80
      tpr <= 100 ~ "81-100",     # Category for TPR between 81 and 100
      TRUE ~ "NA"                # Handle any outlier values (if needed)
    )
  )

tpr_map <- ggplot(merged_data_sf) +  
  # Map of wards filled by categorical TPR
  geom_sf(aes(fill = tpr_category), color = "white", size = 0.2) +  
  
  # Overlay EA points based on the average latitude and longitude (using centroid coordinates)
  #geom_point(aes(x = avg_longitude, y = avg_latitude, color = tpr_category), size = 2, shape = 16, stroke = 0.2, alpha = 0.6) +  
  
  geom_point(aes(x = avg_longitude, y = avg_latitude, fill = tpr_category), 
             color = "black", size = 2, shape = 21, stroke = 0.5, alpha = 0.8) +
  
  # Categorical color scale for TPR categories
  scale_fill_manual(
    values = c(
      "0-20" = "#fff33b", 
      "21-40" = "#fdc70c", 
      "41-60" = "#f3903f", 
      "61-80" = "#ed683c", 
      "81-100" = "#e93e3a", 
      "NA" = "lightgrey"
    ), 
    na.value = "lightgrey"
  ) +  
  
  # scale_color_manual(
  #   values = c(
  #     "0-20" = "#fff33b", 
  #     "21-40" = "#fdc70c", 
  #     "41-60" = "#f3903f", 
  #     "61-80" = "#ed683c", 
  #     "81-100" = "#e93e3a", 
  #     "NA" = "lightgrey"
  #   ), 
  #   na.value = "lightgrey"
  # ) +  

  scale_fill_manual(
    values = c(
      "0-20" = "#fff33b", 
      "21-40" = "#fdc70c", 
      "41-60" = "#f3903f", 
      "61-80" = "#ed683c", 
      "81-100" = "#e93e3a", 
      "NA" = "lightgrey"
    ), 
    na.value = "lightgrey"
  ) +
  
  # Labels for the map
  labs(title = "Map of TPR by Ward with Overlaid EA Estimate", 
       subtitle = "Wet Season Data Only", 
       fill = "TPR Categories") +  
  
  # Minimal theme with adjustments
  theme_minimal() +  
  theme(
    legend.position = "bottom", 
    plot.title = element_text(hjust = 0.5), 
    plot.subtitle = element_text(hjust = 0.5), 
    axis.text = element_blank(),  # Remove axis labels
    axis.ticks = element_blank(),  # Remove axis ticks
    axis.title = element_blank()   # Remove axis titles
  )
tpr_map


# create map
tpr_map <- ggplot(merged_data) + 
  geom_sf(aes(fill = tpr), color = "white", size = 0.2) +
  scale_fill_viridis_c(option = "plasma", na.value = "lightgrey") +
  geom_point(aes(x = avg_longitude, y = avg_latitude), color = "black", size = 2, shape = 16, alpha = 0.6) +  
  labs(title = "Map of TPR by Ward with Overlaid EA Estimate", 
       subtitle = "Wet Season Data Only", 
       fill = "TPR (%)", 
       color = "EA Estimate") + 
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text = element_blank(),  # Remove axis labels
    axis.ticks = element_blank(),  # Remove axis ticks
    axis.title = element_blank()   # Remove axis titles
  )
tpr_map

# save as .pdf
ggsave(filename = paste0(NMEPOutputs, "/presentation/", Sys.Date(), '_map_tpr_wards_kano.pdf'), plot = tpr_map, width = 6, height = 6)
