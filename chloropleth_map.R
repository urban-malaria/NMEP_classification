# Load required packages
library(ggplot2)
library(sf)
library(dplyr)


library(lwgeom)



# Read malaria predictions and shapefile
kano_prevalence <- read.csv("C:/Users/laure/Downloads/kano_malaria_predictions_recalibrated.csv")
Kano_shapefile <- sf::st_read("C:/Users/laure/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_shapefile/gridded_shapefile/gridded/Kano/Kano.shp")


# Make geometries valid and remove duplicate vertices
Kano_shapefile <- st_make_valid(Kano_shapefile) %>% 
  st_transform(crs = st_crs(Kano_shapefile))



spatial_data <- st_as_sf(kano_prevalence, coords = c("longitude", "latitude"), crs = 4326)
Kano_shapefile <- st_transform(Kano_shapefile, crs = 4326)

Kano_shapefile <- st_transform(Kano_shapefile, crs = st_crs(spatial_data))


kano_choropleth_data <- st_join(Kano_shapefile, spatial_data)

# Plot the choropleth map
ggplot(kano_choropleth_data) +
  geom_sf(aes(fill = malaria_probability)) +  
  scale_fill_gradient(low = "yellow", high = "red", name = "Malaria Probability") +  
  labs(
    title = "Malaria Probability in Kano",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_void()





Kano_shpefile <- sf::st_read("C:/Users/laure/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/nigeria_shapefiles/shapefiles/ShinyApp_shapefiles/Kano_Ibadan/Kano-Ibadan.shp") %>% 
  dplyr::filter(StateCode == "KN")



summarised_data <- kano_choropleth_data  %>% 
  st_drop_geometry() %>% 
  group_by(wardname) %>% 
  summarise(malaria_risk_probability01 = mean(malaria_probability, na.rm = T), 
            malaria_risk_probability00 = max(malaria_probability, na.rm = T), 
            malaria_risk_probability02= median(malaria_probability, na.rm = T)) %>%
  inner_join(Kano_shpefile, by = c("wardname" ="WardName" )) %>%
  mutate(class = cut(malaria_risk_probability02, c(seq(0, 0.6, 0.05)), include.lowest = T))


# kano_cd <- read.csv("C:/Users/laure/Downloads/EA_Centroid.csv")
# 
# field_data <- read.csv("C:/Users/laure/Downloads/EA_tpr_data.csv") %>%  
#   mutate(class = cut(tpr/100, c(seq(0, max(tpr), 0.05), 1), include.lowest = T)) %>% 
#   inner_join(kano_cd, by = c("ea" = "ea_cluster"))
# 



agric_palette <- c("#018571", "#80cdc1", "#f5f5f5", "#dfc27d", "#a6611a") 
interval_labels <- c("[0,0.025]", "(0.025,0.05]", "(0.05,0.075]", "(0.075,0.1]","(0.1,0.125]", "(0.125,0.15]","(0.15,0.175]", "(0.175,0.]")
  # c("[0,0.05]", "(0.05,0.1]", "(0.1,0.15]", "(0.15,0.2]", "(0.2,1]")




 ggplot() +
  
  # color subdivisions by proportion using the agric_palette
  geom_sf(data = summarised_data, aes(fill = class, geometry = geometry), color = "white") +
  
  # apply agric_palette to the fill scale
   scale_fill_manual(values = agric_palette, labels = interval_labels, na.value = "gray") +
  

  # cluster data points without jitter
  # geom_point(data = field_data, aes(x = centroid_lon, y =  centroid_lat, fill = class),
  #            size = 3, shape = 21, stroke = 0.2, show.legend = FALSE) +
  scale_color_gradientn(colors = agric_palette) +
  theme_void()
 
