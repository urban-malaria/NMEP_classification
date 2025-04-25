user <- Sys.getenv("USERNAME")
Datadir <- file.path("C:/Users/ebamgboye/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria")
ReqDir <- file.path(Datadir, "nigeria/nigeria_hmis", "HF Data 2019-2023")


##Read in HMIS data
hmis_2019 <- read_excel(file.path(DataDir, "nigeria/nigeria_hmis", "HF Data 2019-2023", "HF Data 2019-2023", "NMEP Malaria 2019 Data.xlsx"), sheet = 1)
hmis_2020 <- read_excel(file.path(DataDir, "nigeria/nigeria_hmis", "HF Data 2019-2023", "HF Data 2019-2023", "NMEP Malaria 2020 Data.xlsx"), sheet = 1)
hmis_2021 <- read_excel(file.path(DataDir, "nigeria/nigeria_hmis", "HF Data 2019-2023", "HF Data 2019-2023", "NMEP Malaria 2021 HF Data.xlsx"), sheet = 1)
hmis_2022 <- read.csv(file.path(DataDir, "nigeria/nigeria_hmis", "HF Data 2019-2023", "HF Data 2019-2023", "2022 HF Data", "2022 HF Data", "Niger 2022.csv"))
hmis_2023 <- read.csv(file.path(DataDir, "nigeria/nigeria_hmis", "HF Data 2019-2023", "HF Data 2019-2023", "2023 HF Data", "2023 HF Data", "Niger 2023 2.csv"))


#Extracting Primary health care facilities alone, niger and other relevant variables
ni_hmis_2019_p <- hmis_2019 %>%
  dplyr::filter(state == "ni Niger State") %>%
  dplyr::select(state, lga, ward, facilityname, opd, conf_u5) %>%
  dplyr::filter(str_detect(facilityname, "Primary Health Center"))%>%
  rename(HF_type = `facilityname`,
         outpatient = `opd`,
         rdt_u5 = `conf_u5`)

ni_hmis_2020_p <- hmis_2020 %>%
  dplyr::filter(state == "ni Niger State") %>%
  dplyr::select(state, lga, ward, level_of_care, opd, conf_u5) %>%
  dplyr::filter(level_of_care == "Primary") %>%
  rename(HF_type = `level_of_care`,
         outpatient = `opd`,
         rdt_u5 = `conf_u5`)

ni_hmis_2021_p <- hmis_2021 %>%
  dplyr::filter(State == "ni Niger State") %>%
  dplyr::select(State, LGA, Ward, Level, `Out-patient Attendance`, `Persons tested positive for malaria by RDT <5yrs`) %>%
  dplyr::filter(Level == "Primary Health Facility")%>%
  rename( state = State,
          lga = LGA,
          ward = Ward,
          HF_type = `Level`,
          outpatient = `Out-patient Attendance`,
          rdt_u5 = `Persons tested positive for malaria by RDT <5yrs`)

ni_hmis_2022_p <- hmis_2022 %>%
  dplyr::select(organisationunitname.1, orgunitlevel2, orgunitlevel3, orgunitlevel4, `Out.patient.Attendance`,
                `Persons.tested.positive.for.malaria.by.RDT..5yrs`) %>%
  dplyr::filter(organisationunitname.1 == "Primary Health Facility")%>%
  rename(state = orgunitlevel2,
         lga = orgunitlevel3,
         ward = orgunitlevel4,
         HF_type = `organisationunitname.1`,
         outpatient = `Out.patient.Attendance`,
         rdt_u5 = `Persons.tested.positive.for.malaria.by.RDT..5yrs`)


ni_hmis_2023_p <- hmis_2023 %>%
  dplyr::select(organisationunitname.1, orgunitlevel2, orgunitlevel3, orgunitlevel4, `Out.patient.Attendance`,
                `Persons.tested.positive.for.malaria.by.RDT..5yrs`) %>%
  dplyr::filter(organisationunitname.1 == "Primary Health Facility")%>%
  rename(state = orgunitlevel2,
         lga = orgunitlevel3,
         ward = orgunitlevel4,
         HF_type = `organisationunitname.1`,
         outpatient = `Out.patient.Attendance`,
         rdt_u5 = `Persons.tested.positive.for.malaria.by.RDT..5yrs`)

##Combine all datasets
merged_ni_hmis_2019_2023 <- rbind(ni_hmis_2019_p, ni_hmis_2020_p, ni_hmis_2021_p, ni_hmis_2022_p, ni_hmis_2023_p)

nigertpr_201923 <- merged_ni_hmis_2019_2023 %>%
  group_by(ward) %>% #HF and level added
  summarise(
    u5_tpr_rdt = sum(rdt_u5, na.rm = TRUE) / sum(outpatient, na.rm = TRUE) * 100,
  ) %>%
  ungroup()

# clean ward names in dfs to use them for merging
merged_ni_hmis_2019_2023$ward <- gsub("^ni\\s+|\\s+Ward$", "", merged_ni_hmis_2019_2023$ward)
nigertpr_201923$ward <- gsub("^ni\\s+|\\s+Ward$", "", nigertpr_201923$ward)
nigertpr_201923 <- nigertpr_201923 %>%  dplyr::filter(ward != "Unknown")

# add lga back
nigertpr_201923 <- nigertpr_201923 %>%
  left_join(merged_ni_hmis_2019_2023 %>%
              dplyr::select(ward, lga) %>%
              distinct(), by = c("ward" = "ward")) %>%
  rename(LGA = lga, WardName = ward) %>%
  dplyr::select(LGA, WardName, u5_tpr_rdt)

# clean up LGA names
nigertpr_201923$LGA <- gsub("^ni\\s+|\\s+Local Government Area$", "", nigertpr_201923$LGA)


# #Clean up names for ease of merging with shapefile
# nigertpr_201923 <- nigertpr_201923 %>%
#   mutate(ward = gsub("^ni ", "", ward),
#          ward = gsub("^ni ", "", ward), # Remove "ta " prefix
#          ward = gsub(" Ward$", "", ward),  # Remove " Ward" suffix
#   )

##Merge niger shapefile to tpr data
ni_wardshp <- st_read(file.path(DataDir,"nigeria/nigeria_shapefiles", "Nigeria Boundary Files_All", "Boundary_VaccWards_Export", "Boundary_VaccWards_Export.shp"))%>%
  dplyr::filter(StateCode == "NI")

# nigertpr_201923 <- nigertpr_201923 %>%
#   rename(WardName = ward)  # Replace 'WardName' with the actual column name in kano_u5_tpr_df

# Perform the merge
niger_tpr_data <- ni_wardshp %>%
  left_join(nigertpr_201923, by = "WardName")

niger_tpr_data <- st_set_crs(niger_tpr_data, 4326)

niger_tpr_data <- st_make_valid(niger_tpr_data)

##Fill up wards with nearest neighbours
w <- spdep::poly2nb(niger_tpr_data, queen = TRUE)
w_listw <- spdep::nb2listw(w)


# Compute the average test positivity rate from neighboring polygons

mean_neighbors <- weights(w_listw, niger_tpr_data$u5_tpr_rdt)

missing_indices <- which(is.na(niger_tpr_data$u5_tpr_rdt))

neighbors_list <- w_listw$neighbours

neighbors_list <- w_listw$neighbours

# Impute missing 'tpr_u5' values with the mean of neighboring values
for (index in seq_along(missing_indices)) {
  polygon <- missing_indices[index]
  neighbor_tprs <- neighbors_list[[polygon]]
  niger_tpr_data$u5_tpr_rdt[polygon] <- mean(niger_tpr_data$u5_tpr_rdt[neighbor_tprs], na.rm = TRUE)
}


ggplot(data = niger_tpr_data) +
  geom_sf(aes(fill = u5_tpr_rdt)) +  # Replace with the correct column for prevalence
  scale_fill_gradient(low = "hotpink", high = "gold1", na.value = "grey",
                      name = "Malaria u5TPR(Burden)") +
  # geom_text(
  #   aes(x = X, y = Y, label = round(u5_tpr3_rdt, 1)),  # Use the centroid coordinates
  #   vjust = 1.2, color = "black", size = 3.5
  # )
  # geom_sf(data = health_facilities_sf, aes(size = rate), shape = 21, fill = "blue", color = "white") +
  # scale_size_continuous(name = "Health Facility Rate") +
  labs(
    title = "Malaria TPR by Study Wards using HMIS 2019-2023(niger)",
    #   subtitle = "Health Facilities with High Rates Highlighted",
    caption = "u5TPR = Confirmed u5/Overall Gen. Attendance"
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    legend.position = "right",
    axis.text = element_blank(),      # Remove axis text
    axis.ticks = element_blank()      # Remove axis ticks
  )+
  theme_manuscript()

niger_tpr_data <- niger_tpr_data %>%
  dplyr::select(WardCode, WardName, LGA, u5_tpr_rdt) %>%
  st_drop_geometry()

write.csv(niger_tpr_data, file.path(LuDir, "nigertpr_updated.csv" ))
