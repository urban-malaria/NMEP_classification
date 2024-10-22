## building heights

getwd()
source("~/NMEP_classification/load_path.R", echo = T)

ibadan_shp <- st_read(file.path(ShpfilesDir, "Ibadan", "Ibadan.shp"))

ibadan_buildings <- raster(file.path(RastersDir, "GoogleBuildings2_5/Ibadan/Ibadan_high_res.tif"))
crs(ibadan_buildings) <- CRS("+init=epsg:4326")  
lets_see <- raster::extract(ibadan_buildings, ibadan_shp, fun = mean)


ibadan_buildings2 <- rast(file.path(RastersDir, "GoogleBuildings2_5/Ibadan/Ibadan_high_res.tif"))
crs(ibadan_buildings2) <- "EPSG:4326"  
lets_see2 <- raster::extract(ibadan_buildings2, ibadan_shp, fun = mean)


