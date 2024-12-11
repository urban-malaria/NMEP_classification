## Kano State

source("~/NMEP_classification/load_path.R", echo = T)

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
