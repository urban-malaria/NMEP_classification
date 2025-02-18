
library(xgboost)
library(sf)         
library(spdep)      
library(dplyr)


#######################
Kano_data_co <- read.csv("C:/Users/laure/Downloads/Kano_cleaned_location.csv") %>% 
  distinct()

Kano_data <- read.csv("C:/Users/laure/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/analysis_docs/kano_environmental_data.csv") %>% 
  group_by(sn) %>% 
  summarise(positive  = sum(ifelse(malaria_status == 1, 1, 0)), 
            negative = sum(ifelse(malaria_status == 2, 1, 0)),
            target_variable = positive/(positive + negative),
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
  inner_join(Kano_data_co) %>% 
  ungroup() 

# %>% 
#   select(-c(sn, longitude, latitude)) %>% 
#   distinct()


min_max_scale <- function(x) {
  if (!is.numeric(x)) {
    stop("Input must be numeric.")
  }
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

de_scaled <- as.data.frame(lapply(Kano_data[,-c(1:3, 11,12,15:19)], min_max_scale))  
  

Kano_data <- Kano_data[,c(4, 18,19)]%>% 
  cbind(de_scaled)


spatial_data <- st_as_sf(Kano_data, coords = c("longitude", "latitude"), crs = 4326)



coords <- st_coordinates(spatial_data)
nb <- knn2nb(knearneigh(coords, k = 5))  
listw <- nb2listw(nb, style = "B")
W <- nb2mat(nb, style = "B", zero.policy = TRUE)


W <- (W + t(W)) / 2 

W <- as.matrix(W, "CsparseMatrix")


formula <- target_variable ~ mean_monthly_rainfall + mean_EVI + mean_NDVI + mean_NTL + distance_to_water + elevation + RH_mean + temp_mean + ndwi_mean + ndmi_mean
 #+  nets_per_capita

#######################


coords <- st_coordinates(spatial_data)
nb <- knn2nb(knearneigh(coords, k = 5))  
listw <- nb2listw(nb, style = "W")


moran_result <- moran.test(spatial_data$target_variable, listw)

spatial_data$moran_i <- moran_result$estimate[1]


spatial_data$longitude = st_coordinates(spatial_data)[ ,1]
spatial_data$latitude =   st_coordinates(spatial_data)[ ,2]

features <- spatial_data %>%
  dplyr::select(-c(target_variable, target_variable.1))  %>% 
  st_drop_geometry()

target <- spatial_data$target_variable


dtrain <- xgb.DMatrix(data = as.matrix(features), label = target)


params <- list(
  objective = "reg:squarederror", 
  eval_metric = "rmse",
  eta = 0.1,
  max_depth = 6,
  gamma = 1,
  subsample = 0.8,
  colsample_bytree = 0.8
)


xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 10000,
  watchlist = list(train = dtrain),
  print_every_n = 10,
  early_stopping_rounds = 10
)



predictions <- predict(xgb_model, newdata = as.matrix(features))
correlation <- cor(predictions, target)

cat("Correlation between predictions and actual values:", correlation, "\n")


importance <- xgb.importance(model = xgb_model, feature_names = colnames(features))
xgb.plot.importance(importance)

# Save the model
xgb.save(xgb_model, "xgboost_spatial_model.model")



