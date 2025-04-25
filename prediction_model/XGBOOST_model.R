
library(xgboost)
library(sf)         
library(spdep)      
library(dplyr)


# data <- read.csv("C:/Users/laure/Downloads/spatial_malaria_data.csv")
# spatial_data <- st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326)
# 


#######################
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

#######################


coords <- st_coordinates(spatial_data)
nb <- knn2nb(knearneigh(coords, k = 5))  
listw <- nb2listw(nb, style = "W")


moran_result <- moran.test(spatial_data$target_variable, listw)

spatial_data$moran_i <- moran_result$estimate[1]


spatial_data$longitude = st_coordinates(spatial_data)[ ,1]
spatial_data$latitude =   st_coordinates(spatial_data)[ ,2]

features <- spatial_data %>%
  dplyr::select(-c(target_variable, longitude, latitude))  %>% 
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



