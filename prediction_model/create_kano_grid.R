#Generate gridded shapefiles for wards of interest
getwd()

source("load_path.R", echo = T)

ShpfilesDir <- "C:/Users/laure/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_shapefile/gridded_shapefile"
Kano_shpefile <- st_read(file.path(Drive,"/data/nigeria/nigeria_shapefiles/shapefiles/ShinyApp_shapefiles/Kano_Ibadan/Kano-Ibadan.shp")) %>% 
  dplyr::filter(StateCode == "KN")

wards = cities = Kano_shpefile$WardName



all_shapefile <- lapply(seq_along(cities), function(x) {
  shp <- Kano_shpefile %>% filter(WardName == cities[x])
  st_crs(shp) <- 4326 
  return(shp)
})



allwards <- lapply(seq_along(all_shapefile), function(x) {
  city_name <- cities[x]
  all_shapefile[[x]] %>% filter(WardName %in% wards)
})


polygon_allwards <- lapply(seq_along(allwards), 
                           function(x)  split(allwards[[x]], allwards[[x]]$WardName)) 


# Convert to an sf object
flattened_polygon_list <- do.call(c, polygon_allwards)

polygon_sf <-lapply(seq_along(flattened_polygon_list), 
                    function(x) sf::st_as_sfc(flattened_polygon_list[[x]], crs = 4326))


#Divide polygons

subdivide_polygon <- function(polygon, rows, cols) {
  bounds <- st_bbox(polygon)
  x_range <- seq(bounds["xmin"], bounds["xmax"], length.out = cols + 1)
  y_range <- seq(bounds["ymin"], bounds["ymax"], length.out = rows + 1)
  
  sub_polygons <- list()
  
  for (i in 1:cols) {
    for (j in 1:rows) {
      sub_poly <- st_polygon(list(rbind(c(x_range[i], y_range[j]), 
                                        c(x_range[i + 1], y_range[j]), 
                                        c(x_range[i + 1], y_range[j + 1]), 
                                        c(x_range[i], y_range[j + 1]), 
                                        c(x_range[i], y_range[j]))))
      sub_polygons <- append(sub_polygons, list(sub_poly))
    }
  }
  
  st_sfc(sub_polygons, crs = st_crs(polygon))
}

#Loop to subdivide and save

for (index in seq_along(flattened_polygon_list)){ 
  
  polygon <- flattened_polygon_list[[index]]
  
  ward <- gsub(" ", "", polygon$WardName)
  
  # Define the number of rows and columns for subdivision
  rows <- 5
  cols <- 5
  
  # Subdivide the polygon
  subdivided_polygons <- subdivide_polygon(polygon_sf[[index]], rows, cols)
  
  newshape <- st_intersection(polygon_sf[[index]], subdivided_polygons)
  
  invalid_geometries <- st_is_valid(newshape, reason = TRUE)
  
  
  newshape <- st_make_valid(newshape)
  

  
  # Define the path to the directory
  
  
  dir_path <- file.path(ShpfilesDir, paste0("/gridded/", ward))
  
  # Check if the directory exists and create it if it doesn't
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, showWarnings = FALSE, recursive = TRUE)
    
    cat("Directory created:", dir_path, "\n")
    
  } else {
    
    cat("Directory already exists:", dir_path, "\n")
  }
  
  
  tryCatch(st_write(newshape, file.path(paste0(dir_path, "/", ward, ".shp"))), error = function(e) {
    cat("An error occurred:\n")
    print(e)})
  
}



#combine files 

shapefiles <- list.files(
  path = file.path(ShpfilesDir, "gridded"), 
  pattern = "\\.shp$",                      
  full.names = TRUE,                       
  recursive = TRUE                          
)

combined_shapefile <- do.call(rbind, lapply(seq_along(shapefiles),
                                           function(x) st_read(shapefiles[x]) %>% 
                                             mutate(ward = paste0(wards[x], "_",FID)))
                              )



st_write(combined_shapefile, file.path(paste0("C:/Users/laure/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_shapefile/gridded_shapefile//gridded/Kano", ".shp")))

ggplot2::ggplot() +
  geom_sf(data = st_as_sf(combined_shapefile), fill = "white", alpha = 0.3) +
  map_theme()


range(st_area(st_read(file.path(paste0("C:/Users/laure/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_shapefile/gridded_shapefile/gridded/Kano", ".shp")))))
