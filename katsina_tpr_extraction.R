user <- Sys.getenv("USERNAME")
Datadir <- file.path("C:/Users/ebamgboye/Urban Malaria Proj Dropbox/urban_malaria/data")
ReqDir <- file.path(Datadir, "nigeria/nigeria_hmis", "HF Data 2019-2023")


##Read in HMIS data
hmis_2019 <- read_excel(file.path(DataDir, "nigeria/nigeria_hmis", "HF Data 2019-2023", "HF Data 2019-2023", "NMEP Malaria 2019 Data.xlsx"), sheet = 1)
hmis_2020 <- read_excel(file.path(DataDir, "nigeria/nigeria_hmis", "HF Data 2019-2023", "HF Data 2019-2023", "NMEP Malaria 2020 Data.xlsx"), sheet = 1)
hmis_2021 <- read_excel(file.path(DataDir, "nigeria/nigeria_hmis", "HF Data 2019-2023", "HF Data 2019-2023", "NMEP Malaria 2021 HF Data.xlsx"), sheet = 1)
hmis_2022 <- read.csv(file.path(DataDir, "nigeria/nigeria_hmis", "HF Data 2019-2023", "HF Data 2019-2023", "2022 HF Data", "2022 HF Data", "Katsina 2022 2.csv"))
hmis_2023 <- read.csv(file.path(DataDir, "nigeria/nigeria_hmis", "HF Data 2019-2023", "HF Data 2019-2023", "2023 HF Data", "2023 HF Data", "Katsina 2023 2.csv"))


#Extracting Primary health care facilities alone, katsina and other relevant variables
kt_hmis_2019_p <- hmis_2019 %>%
  dplyr::filter(state == "kt Katsina State") %>%
  dplyr::select(state, lga, ward, facilityname, opd, conf_u5) %>%
  dplyr::filter(str_detect(facilityname, "Primary Health Center"))%>%
  rename(HF_type = `facilityname`,
         outpatient = `opd`,
         rdt_u5 = `conf_u5`)

kt_hmis_2020_p <- hmis_2020 %>%
  dplyr::filter(state == "kt Katsina State") %>%
  dplyr::select(state, lga, ward, level_of_care, opd, conf_u5) %>%
  dplyr::filter(level_of_care == "Primary") %>%
  rename(HF_type = `level_of_care`,
         outpatient = `opd`,
         rdt_u5 = `conf_u5`)

kt_hmis_2021_p <- hmis_2021 %>%
  dplyr::filter(State == "kt Katsina State") %>%
  dplyr::select(State, LGA, Ward, Level, `Out-patient Attendance`, `Persons tested positive for malaria by RDT <5yrs`) %>%
  dplyr::filter(Level == "Primary Health Facility")%>%
  rename( state = State,
          lga = LGA,
          ward = Ward,
          HF_type = `Level`,
          outpatient = `Out-patient Attendance`,
          rdt_u5 = `Persons tested positive for malaria by RDT <5yrs`)

kt_hmis_2022_p <- hmis_2022 %>%
  dplyr::select(organisationunitname.1, orgunitlevel2, orgunitlevel3, orgunitlevel4, `Out.patient.Attendance`,
                `Persons.tested.positive.for.malaria.by.RDT..5yrs`) %>%
  dplyr::filter(organisationunitname.1 == "Primary Health Facility")%>%
  rename(state = orgunitlevel2,
         lga = orgunitlevel3,
         ward = orgunitlevel4,
         HF_type = `organisationunitname.1`,
         outpatient = `Out.patient.Attendance`,
         rdt_u5 = `Persons.tested.positive.for.malaria.by.RDT..5yrs`)


kt_hmis_2023_p <- hmis_2023 %>%
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
merged_kt_hmis_2019_2023 <- rbind(kt_hmis_2019_p, kt_hmis_2020_p, kt_hmis_2021_p, kt_hmis_2022_p, kt_hmis_2023_p)

# clean up ward/LGA names
merged_kt_hmis_2019_2023$ward <- gsub("^kt\\s+|\\s+Ward$", "", merged_kt_hmis_2019_2023$ward)
merged_kt_hmis_2019_2023$lga <- gsub("^kt\\s+|\\s+Local Government Area$", "", merged_kt_hmis_2019_2023$lga)
merged_kt_hmis_2019_2023 <- merged_kt_hmis_2019_2023 %>%
  mutate(ward = case_when(
    ward == "Zango" & lga == "Zango" ~ "Zango (Zango LGA)",
    ward == "Zango" & lga == "Kankara" ~ "Zango (Kankara LGA)",
    TRUE ~ ward
  ))

katsinatpr_201923 <- merged_kt_hmis_2019_2023 %>%
  group_by(ward) %>% #HF and level added
  summarise(
    u5_tpr_rdt = sum(rdt_u5, na.rm = TRUE) / sum(outpatient, na.rm = TRUE) * 100,
  ) %>%
  ungroup()

# clean ward names in dfs to use them for merging
katsinatpr_201923$ward <- gsub("^kt\\s+|\\s+Ward$", "", katsinatpr_201923$ward)
katsinatpr_201923 <- katsinatpr_201923 %>%  dplyr::filter(ward != "Unknown")

# add lga back
katsinatpr_201923 <- katsinatpr_201923 %>%
  left_join(merged_kt_hmis_2019_2023 %>%
              dplyr::select(ward, lga) %>%
              distinct(), by = c("ward" = "ward")) %>%
  rename(LGA = lga, WardName = ward) %>%
  dplyr::select(LGA, WardName, u5_tpr_rdt)

##Merge katsina shapefile to tpr data
kt_wardshp <- st_read(file.path(DataDir,"nigeria/nigeria_shapefiles", "Nigeria Boundary Files_All", "Boundary_VaccWards_Export", "Boundary_VaccWards_Export.shp"))%>%
  dplyr::filter(StateCode == "KT")

# add lga to ward name for duplicates
kt_wardshp <- kt_wardshp %>%
  mutate(WardName = case_when(
    WardName == "Sabon Gari" & WardCode == "41008" ~ "Sabon Gari (Daura LGA)",
    WardName == "Sabon Gari" & WardCode == "41410"  ~ "Sabon Gari (Funtua LGA)",
    WardName == "Sabon Gari" & WardCode == "43010" ~ "Sabon Gari (Rimi LGA)",
    WardName == "Baure" & WardCode == "40402" ~ "Baure (Baure LGA)",
    WardName == "Baure" & WardCode == "40501" ~ "Baure (Bindawa LGA)",
    WardName == "Gurbi" & WardCode == "41605" ~ "Gurbi (Jibia LGA)",
    WardName == "Gurbi" & WardCode == "41904" ~ "Gurbi (Kankara LGA)",
    WardName == "Kandawa" & WardCode == "40305" ~ "Kandawa (Batsari LGA)",
    WardName == "Kandawa" & WardCode == "41507" ~ "Kandawa (Ingawa LGA)",
    WardName == "Machika" & WardCode == "42607" ~ "Machika (Mani LGA)",
    WardName == "Machika" & WardCode == "43105" ~ "Machika (Sabuwa LGA)",
    WardName == "Makera" & WardCode == "41210" ~ "Makera (Dutsin-Ma LGA)",
    WardName == "Makera" & WardCode == "41407" ~ "Makera (Funtua LGA)",
    WardName == "Mazoji A" & WardCode == "41004" ~ "Mazoji A (Daura LGA)",
    WardName == "Mazoji A" & WardCode == "42807" ~ "Mazoji A (Matazu LGA)",
    WardName == "Mazoji B" & WardCode == "41005" ~ "Mazoji B (Daura LGA)",
    WardName == "Mazoji B" & WardCode == "42808" ~ "Mazoji B (Matazu LGA)",
    WardName == "Safana" & WardCode == "40609" ~ "Safana (Charanchi LGA)",
    WardName == "Safana" & WardCode == "43207" ~ "Safana (Safana LGA)",
    WardName == "Zango" & WardCode == "41911" ~ "Zango (Kankara LGA)",
    WardName == "Zango" & WardCode == "43410" ~ "Zango (Zango LGA)",
    TRUE ~ WardName
  ))

# add lga to ward name for duplicates
katsinatpr_201923 <- katsinatpr_201923 %>%
  mutate(WardName = case_when(
    WardName == "Sabon Gari" & LGA == "Daura" ~ "Sabon Gari (Daura LGA)",
    WardName == "Sabon Gari" & LGA == "Funtua"  ~ "Sabon Gari (Funtua LGA)",
    WardName == "Sabon Gari" & LGA == "Rimi" ~ "Sabon Gari (Rimi LGA)",    WardName == "Baure" & LGA == "Baure" ~ "Baure (Baure LGA)",
    WardName == "Baure" & LGA == "Bindawa" ~ "Baure (Bindawa LGA)",
    WardName == "Gurbi" & LGA == "Jibia" ~ "Gurbi (Jibia LGA)",
    WardName == "Gurbi" & LGA == "Kankara" ~ "Gurbi (Kankara LGA)",
    WardName == "Kandawa" & LGA == "Batsari" ~ "Kandawa (Batsari LGA)",
    WardName == "Kandawa" & LGA == "Ingawa" ~ "Kandawa (Ingawa LGA)",
    WardName == "Machika" & LGA == "Mani" ~ "Machika (Mani LGA)",
    WardName == "Machika" & LGA == "Sabuwa" ~ "Machika (Sabuwa LGA)",
    WardName == "Makera" & LGA == "Dutsin Ma" ~ "Makera (Dutsin Ma LGA)",
    WardName == "Makera" & LGA == "Funtua" ~ "Makera (Funtua LGA)",
    WardName == "Mazoji A" & LGA == "Daura" ~ "Mazoji A (Daura LGA)",
    WardName == "Mazoji A" & LGA == "Matazu" ~ "Mazoji A (Matazu LGA)",
    WardName == "Mazoji B" & LGA == "Daura" ~ "Mazoji B (Daura LGA)",
    WardName == "Mazoji B" & LGA == "Matazu" ~ "Mazoji B (Matazu LGA)",
    WardName == "Safana" & LGA == "Charanchi" ~ "Safana (Charanchi LGA)",
    WardName == "Safana" & LGA == "Safana" ~ "Safana (Safana LGA)",
    WardName == "Zango" & LGA == "Kankara" ~ "Zango (Kankara LGA)",
    WardName == "Zango" & LGA == "Zango" ~ "Zango (Zango LGA)",
    WardName == "Gurbi (Jibia)Ward" & LGA == "Jibia" ~ "Gurbi (Jibia LGA)",
    WardName == "Kandawa/Jobe" & LGA == "Ingawa" ~ "Kandawa (Ingawa LGA)",
    WardName == "Machika Ward - M" & LGA == "Mani" ~ "Machika (Mani LGA)",
    WardName == "Mazoji A -  Matazu" & LGA == "Matazu" ~ "Mazoji A (Matazu LGA)",
    WardName == "Mazoji B -  Daura" & LGA == "Daura" ~ "Mazoji B (Daura LGA)",
    WardName == "Mazoji B -  Matazu" & LGA == "Matazu" ~ "Mazoji B (Matazu LGA)",
    WardName == "Danja B (Danja)" ~ "Danja B",
    TRUE ~ WardName
  ))

# katsinatpr_201923 <- katsinatpr_201923 %>%
#   rename(WardName = ward)  # Replace 'WardName' with the actual column name in kano_u5_tpr_df

# Perform the merge
katsina_tpr_data <- kt_wardshp %>%
  left_join(katsinatpr_201923, by = "WardName")

katsina_tpr_data <- st_set_crs(katsina_tpr_data, 4326)

katsina_tpr_data <- st_make_valid(katsina_tpr_data)

##Fill up wards with nearest neighbours
w <- spdep::poly2nb(katsina_tpr_data, queen = TRUE)
w_listw <- spdep::nb2listw(w)


# Compute the average test positivity rate from neighboring polygons

mean_neighbors <- weights(w_listw, katsina_tpr_data$u5_tpr_rdt)

missing_indices <- which(is.na(katsina_tpr_data$u5_tpr_rdt))

neighbors_list <- w_listw$neighbours

neighbors_list <- w_listw$neighbours

# Impute missing 'tpr_u5' values with the mean of neighboring values
for (index in seq_along(missing_indices)) {
  polygon <- missing_indices[index]
  neighbor_tprs <- neighbors_list[[polygon]]
  katsina_tpr_data$u5_tpr_rdt[polygon] <- mean(katsina_tpr_data$u5_tpr_rdt[neighbor_tprs], na.rm = TRUE)
}


ggplot(data = katsina_tpr_data) +
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
    title = "Malaria TPR by Study Wards using HMIS 2019-2023(katsina)",
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

katsina_tpr_data <- katsina_tpr_data %>%
  dplyr::select(WardCode, WardName, LGA, u5_tpr_rdt) %>%
  st_drop_geometry()

LuDir = file.path("/Users/grace/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/nigeria_hmis/TPR data for selected NMEP prioritized states")
write.csv(katsina_tpr_data, file.path(LuDir, "katsinatpr_updated.csv" ))
