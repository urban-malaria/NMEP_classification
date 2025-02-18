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
library(dplyr)

Kano_data_co <- read.csv("C:/Users/laure/Downloads/Kano_cleaned_location.csv") %>% 
  distinct()

Kano_data <- read.csv("C:/Users/laure/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/analysis_docs/kano_environmental_data.csv") %>% 
  group_by(sn) %>% 
  summarise(positive  = sum(ifelse(malaria_status == 1, 1, 0)), 
            negative = sum(ifelse(malaria_status == 2, 1, 0)),
            prevalence = positive/(positive + negative),
            mean_monthly_rainfall = mean(mean_monthly_rainfall, na.rm = T),
            mean_EVI= mean(mean_EVI, na.rm = T),            
            mean_NDVI= mean(mean_NDVI, na.rm = T),             
            mean_NTL= mean(mean_NTL, na.rm = T),           
            distance_to_water= mean(distance_to_water, na.rm = T),
            elevation= mean(elevation, na.rm = T),          
            RH_mean= mean(RH_mean, na.rm = T),
            temp_mean= mean(temp_mean, na.rm = T),
            ndwi_mean= mean(ndwi_mean, na.rm = T), 
            ndmi_mean= mean(ndmi_mean, na.rm = T)
            ) %>% 
  inner_join(Kano_data_co)




kanodata_coordinates <- Kano_data %>% 
  dplyr::select(longitude, latitude,
                mean_monthly_rainfall,
                mean_EVI,
               # mean_NDVI, 
                mean_NTL,
                distance_to_water, elevation, 
                RH_mean,  temp_mean, ndwi_mean, 
                ndmi_mean,
                target_variable = prevalence)



spatial_data <- st_as_sf(kanodata_coordinates, coords = c("longitude", "latitude"), crs = 4326)



coords <- st_coordinates(spatial_data)
nb <- knn2nb(knearneigh(coords, k = 5))  
listw <- nb2listw(nb, style = "B")
W <- nb2mat(nb, style = "B", zero.policy = TRUE)


W <- (W + t(W)) / 2 

W <- as.matrix(W, "CsparseMatrix")

formula <- target_variable ~ mean_monthly_rainfall + mean_EVI +  mean_NTL + distance_to_water + elevation + RH_mean + temp_mean + ndwi_mean + ndmi_mean


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
