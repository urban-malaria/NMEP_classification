rm(list=ls())


if (!requireNamespace("CARBayes", quietly = TRUE)) {
  install.packages("CARBayes")
}
if (!requireNamespace("spdep", quietly = TRUE)) {
  install.packages("spdep")
}
if (!requireNamespace("sf", quietly = TRUE)) {
  install.packages("sf")
}

library(CARBayes)
library(spdep)
library(sf)




Kano_data <- read.csv("C:/Users/laure/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/Kano data/combined_data.csv")

Kano_shpefile <- st_read("C:/Users/laure/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/nigeria_shapefiles/shapefiles/ShinyApp_shapefiles/Kano_Ibadan/Kano-Ibadan.shp") %>% 
  dplyr::filter(StateCode == "KN")

Kano_shp <- st_centroid(Kano_shpefile, crs = 4326)


Kano_shp$longitude <- st_coordinates(Kano_shp)[ ,1]
Kano_shp$latitude <- st_coordinates(Kano_shp)[, 2]

kanodata_coordinates <- dplyr::inner_join(Kano_data, Kano_shp) %>% 
  dplyr::select(longitude, latitude,
                settlement_type_poor,
                distance_water, 
                meanEVI, 
                #nets_per_capita,
                target_variable = tpr_u5_new)



spatial_data <- st_as_sf(kanodata_coordinates, coords = c("longitude", "latitude"), crs = 4326)



coords <- st_coordinates(spatial_data)
nb <- knn2nb(knearneigh(coords, k = 5))  
listw <- nb2listw(nb, style = "B")
W <- nb2mat(nb, style = "B", zero.policy = TRUE)


W <- (W + t(W)) / 2 

W <- as.matrix(W, "CsparseMatrix")



formula <- target_variable ~ settlement_type_poor + distance_water #+  nets_per_capita

# Fit a CAR Bayesian model

car_model <- S.CARleroux(
  formula = formula,
  data = spatial_data,
  W = W,
  family = "gaussian",  
  burnin = 1000,        
  n.sample = 10000,     
  thin = 10             
)


summary(car_model)


spatial_data$fitted_values <- car_model$fitted.values




new_data <- st_join(Kano_shpefile, spatial_data) 

new_data$Rank <- rank(new_data$fitted_values)



ggplot() +
  geom_sf(data = new_data, aes(fill = fitted_values))+
  # geom_sf(data  = spatial_data, aes(fill = fitted_values.y))+
  geom_sf_label(data = new_data, aes(label = Rank))+
  scale_fill_gradient(low = "#eed6f1", high = "#ba5dc8", na.value = NA) 
  
  # map_theme() + 
  # labs(title= "Wards in Illorin")+
  # xlab("")+
  # ylab("")
