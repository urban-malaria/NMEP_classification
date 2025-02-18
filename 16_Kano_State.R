# ==========================================================================================================================================
# Script Name: Kano State
# Purpose: Extracts variables for different Nigerian states (Kano, Kaduna, Katsina, Niger, Delta). 
# It reads shapefiles for each state, extracts environmental data (NDWI and NDMI) for specific wards in Kano, and applies a custom 
# extraction function to gather variables for both metropolitan and regional areas across multiple states. 
# The extracted data is then stored for further analysis.
# ==========================================================================================================================================

user <- Sys.getenv("USER")
if ("grace" %in% user) {
  source("load_path.R", echo = TRUE)
} else {
  source("~/NMEP_classification/load_path.R", echo = T) # if get a new_packages error, run this a couple more times and it will work
}  

library(foot)
library(units)

#Extract variables for all Kano State Wards
kano_state_variables <- extract_variables(
  name = "Kano",
  State = "Kano",
  shapefile = file.path(ShpfilesDir, "all_reprioritization_nmep_states", "Kano State", "Kano_State.shp"),
  paths = paths
)


#Extract NDWI and NDMI for Kano 5 (test)
kano_5_shp <- st_read(file.path(ShpfilesDir, "KN_5_Wards", "Kano_5.shp"))

NDWI_raster <- raster(file.path(RastersDir, "global_surface_water", "Nigeria_NDWI_2023.tif"))
NDWI_extracted <- raster::extract(NDWI_raster, kano_5_shp, fun = mean, df = TRUE) ## average of -0.26

NDMI_raster <- raster(file.path(RastersDir, "global_surface_water", "NDMI_Nigeria_2023.tif"))
NDMI_extracted <- raster::extract(NDMI_raster, kano_5_shp, fun = mean, df = TRUE) #average of 0.

kano_5_shp$NDWI <- NDWI_extracted$NDWI

# Extract variables for Kano metropolitan household points

kano_household <- read.csv(file.path(FieldDataDir, "analysis_docs", 
                                     "Kano_cleaned_location.csv")) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

st_write(kano_household, file.path(ShpfilesDir, "Kano_household", "Kano_household.shp"))

ggplot()+
  geom_sf(data = kano_5_shp, aes(geometry = geometry))+
  geom_sf(data = kano_household, aes(geometry = geometry, color = ward))+
  labs(title = "Household points in Kano Metropolis",
       colour = "Ward")+
  map_theme()


kano_household_variables <- extract_variables2(
  name = "Kano_household",
  State = "Kano",
  shapefile = file.path(ShpfilesDir, "Kano_household", "Kano_household.shp"),
  paths = paths
)

## extract relative humidity

kano_household_sp <- as(kano_household, "Spatial")

list_RH <- list(rh_2023 <- brick(file.path(RastersDir, "temp_humidity_2023_grib", "relative_humidity_2023.grib")),
                rh_2024 <- brick(file.path(RastersDir, "temp_humidity_2023_grib", "relative_humidity_2024.grib"))
)

names(list_RH) <- c("2023", "2024")

for (i in 1:length(list_RH)) {
  num_layers <- nlayers(list_RH[[i]])
  layer_names <- paste0("RH_", seq_len(num_layers))
  names(list_RH[[i]]) <- layer_names
}


nlayers(list_RH[[2]])
plot(list_RH[[2]], 7)


kano_household_list_RH <- list() 

for (i in 1:length(list_RH)) {
  kano_household_RH_data <- extract(list_RH[[i]], kano_household_sp, fun = mean, df = TRUE)
  df_RH <- as.data.frame(kano_household_RH_data)
  df_RH$Year <- names(list_RH)[i]
  kano_household_list_RH[[i]] <- df_RH
}

kano_household_rh2 <- bind_rows(kano_household_list_RH)

rh_kano_household <- kano_household_rh2 %>%
  group_by(ID) %>%
  summarize(RH_mean = mean(c_across(starts_with("RH_")), na.rm = TRUE))

kano_household$RH_mean <- rh_kano_household$RH_mean


#Extract Building morphology metrics for both household points and prediction points

kano_fp <- read.csv(file.path(FootprintsDir, "Kano.csv"))
kano_fp <- st_as_sf(kano_fp, wkt = "geometry")
st_crs(kano_fp) <- 4326

kano_fp <- kano_fp %>%
  mutate(FID = row_number())


kano_prediction <- st_simplify(st_make_valid(st_read(file.path(DataDir, 
              "nigeria/kano_shapefile/gridded_shapefile/gridded", 
              "Kano_cd/Kano_cd.shp"))), dTolerance = 0.001) 
kano_prediction_points <- kano_prediction %>% 
  mutate(Type = "Prediction")


kano_household <-  st_simplify(st_make_valid(st_read(file.path(ShpfilesDir, "Kano_household", "Kano_household.shp"))),
                               dTolerance = 0.001)
kano_household_points <- kano_household %>% 
  unique() %>% 
  dplyr::select(sn, ward, geometry) %>% 
  rename(FID = sn) %>% 
  mutate(Type = "Household") 

combined_points <- rbind(kano_prediction_points, kano_household_points)

ggplot()+
  geom_sf(data = combined_points, aes(geometry = geometry))+
  map_theme()


st_write(combined_points, file.path(ShpfilesDir, "combined_kano_points",
                                   "combined_points.shp"))


# Extract only intersecting polygons
#combined_points_fp <- st_intersection(kano_fp, combined_points)
#combined_polygons <- combined_points[st_intersects(kano_fp, combined_points, sparse = FALSE)[, 1], ]


ggplot()+
  geom_sf(data = combined_points_fp, aes(geometry = geometry))+
  map_theme()


combined_points_stats <- calculate_footstats( combined_points_fp,
  zone = NULL,
  what = "all",
  how = "all",
  controlZone = list(zoneName = "FID", method = "centroid"),
  controlUnits = list(areaUnit = "m^2", perimUnit = "m", distUnit = "m"),
  controlDist = list(maxSearch = 100, method = "centroid", unit =
                       controlUnits$distUnit),
  filter = list(minArea = NULL, maxArea = NULL), verbose = TRUE)


kano_fp <- read.csv(file.path(FootprintsDir, "Kano.csv"))
kano_fp <- st_as_sf(kano_fp, wkt = "geometry")
st_crs(kano_fp) <- 4326

kano_fp <- kano_fp %>%
  mutate(FID = row_number())

kano_building_stats <- calculate_footstats( kano_fp,
                                              zone = NULL,
                                              what = "all",
                                              how = "all",
                                              controlZone = list(zoneName = "FID", method = "centroid"),
                                              #controlUnits = list(areaUnit = "m^2", perimUnit = "m", distUnit = "m"),
                                              #controlDist = list(maxSearch = 100, method = "centroid", unit =
                                               #                    controlUnits$distUnit),
                                              #filter = list(minArea = NULL, maxArea = NULL), 
                                            verbose = TRUE)

##########################
#extract additonal variables for kano_prediction points

kano_prediction_variables <- extract_variables2(
  name = "Kano_prediction_points",
  State = "Kano",
  shapefile = file.path(DataDir, 
                        "nigeria/kano_shapefile/gridded_shapefile/gridded", 
                        "Kano_cd/Kano_cd.shp"),
  paths = paths
)


## =========================================================================================================================================
### Run Extraction Function for Remaining States
## =========================================================================================================================================

# Extract variables for all Kaduna State Wards
kaduna_state_variables <- extract_variables(
  name = "Kaduna",
  State = "Kaduna",
  shapefile = file.path(ShpfilesDir, "all_reprioritization_nmep_states", "Kaduna", "kaduna_ward_default.shp"),
  paths = paths
)

# Extract variables for all Katsina State Wards
katsina_state_variables <- extract_variables(
  name = "Katsina",
  State = "Katsina",
  shapefile = file.path(ShpfilesDir, "all_reprioritization_nmep_states", "Katsina", "Katsina.shp"),
  paths = paths
)

# Extract variables for all Niger State Wards
niger_state_variables <- extract_variables(
  name = "Niger",
  State = "Niger",
  shapefile = file.path(ShpfilesDir, "all_reprioritization_nmep_states", "Niger", "Niger_ward_default.shp"),
  paths = paths
)

# Extract variables for all Delta State Wards
delta_state_variables <- extract_variables(
  name = "Delta",
  State = "Delta",
  shapefile = file.path(ShpfilesDir, "all_reprioritization_nmep_states", "Delta", "Delta_Wards.shp"),
  paths = paths
)
