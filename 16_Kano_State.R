## Kano State

user <- Sys.getenv("USER")
if ("grace" %in% user) {
  source("load_path.R", echo = TRUE)
} else {
  source("~/NMEP_classification/load_path.R", echo = T) # if get a new_packages error, run this a couple more times and it will work
}  

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

kano_grid_variables <- extract_variables(
  name = "Kano",
  State = "Kano",
  shapefile = file.path(ShpfilesDir, "all_reprioritization_nmep_states", "Kano State", "Kano_State.shp"),
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