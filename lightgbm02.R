rm(list = ls())

# Load necessary libraries
library(lightgbm)
library(sf)
library(dplyr)
library(tidyr)
library(Matrix)
library(FNN)
library(ggplot2)

# set.seed(611)

# Load spatial data
spatial_data <- st_read("C:/Users/laure/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/analysis_docs/Kano_Predictions_New/kano_environmental_data.csv")
predict_data <- read.csv("C:/Users/laure/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/analysis_docs/kano_environmental_data_predictions.csv")


# Proportions for each set
train_prop <- 0.7
test_prop <- 0.15
val_prop <- 0.15


# all  spatial data model and predictions 
all_data <- spatial_data %>%
  group_by(sn) %>%
  summarise(
    positive = sum(ifelse(malaria_status == 1, 1, 0)),
    negative = sum(ifelse(malaria_status == 2, 1, 0)),
    target_variable = positive / (positive + negative),
    lon = mean(as.numeric(longitude), na.rm = TRUE),
    lat = mean(as.numeric(latitude), na.rm = TRUE),
    mean_monthly_rainfall = mean(as.numeric(mean_monthly_rainfall), na.rm = TRUE),
    mean_EVI = mean(as.numeric(mean_EVI), na.rm = TRUE),
    mean_NDVI = mean(as.numeric(mean_NDVI), na.rm = TRUE),
    mean_NTL = mean(as.numeric(mean_NTL), na.rm = TRUE),
    distance_to_water = mean(as.numeric(distance_to_water), na.rm = TRUE),
    elevation = mean(as.numeric(elevation), na.rm = TRUE),
    RH_mean = mean(as.numeric(RH_mean), na.rm = TRUE),
    temp_mean = mean(as.numeric(temp_mean), na.rm = TRUE),
    ndwi_mean = mean(as.numeric(ndwi_mean), na.rm = TRUE),
    ndmi_mean = mean(as.numeric(ndmi_mean), na.rm = TRUE),
    flooding2023 = mean(as.numeric(flooding2023), na.rm = TRUE),
    building_height = mean(as.numeric(building_height), na.rm = TRUE),
    total_hh_affected_by_flood_2024 = mean(as.numeric(total_hh_affected_by_flood_2024), na.rm = TRUE),
    settlement_type = mean(as.numeric(settlement_type.y), na.rm = TRUE),
    urbanPercentage = mean(as.numeric(urbanPercentage), na.rm = TRUE),
    u5_tpr3_rdt = mean(as.numeric(u5_tpr3_rdt), na.rm = TRUE),
    area_mean = mean(as.numeric(area_mean), na.rm = TRUE),
    perimeter_mean = mean(as.numeric(perimeter_mean), na.rm = TRUE),
    compact_mean = mean(as.numeric(compact_mean), na.rm = TRUE),
    angle_mean = mean(as.numeric(angle_mean), na.rm = TRUE),
    shape_mean = mean(as.numeric(shape_mean), na.rm = TRUE),
    settled_mean = mean(as.numeric(settled_mean), na.rm = TRUE)
  )

coords <- all_data[, c("lon", "lat")]
nn_indices <- get.knn(coords, k = 5)$nn.index


all_data$nn_avg_target <- apply(nn_indices, 1, function(idx) {
  mean(all_data$target_variable[idx], na.rm = TRUE)
})


predictor_columns <- setdiff(names(all_data), 
                             c("sn", "target_variable"))[-c(1,2)]



# predictions 
Predict_data <- predict_data %>%
  # Prepare prediction data
  dplyr::select(
    lon = longitude,
    lat = latitude,
    mean_monthly_rainfall = mean_rainfall,
    elevation = elevation_100m,
    settlement_type = settlement_type.y,
    distance_to_water = distance_to_water_100m,
    all_of(setdiff(predictor_columns, c("lon", "lat", 
                                        "mean_monthly_rainfall", "elevation", 
                                        "settlement_type", "distance_to_water", 
                                        "nn_avg_target")))) %>%
  dplyr::select(all_of(setdiff(predictor_columns, "nn_avg_target")))



pred_coords <- Predict_data[, c("lon", "lat")]
nn <- get.knnx(data = coords, query = pred_coords, k = 5)

nn_indices <- nn$nn.index
nn_distances <- nn$nn.dist







# Compute nearest neighbor predictions for prediction data
pred_coords <- predict_data[, c("longitude", "latitude")]
nn <- get.knnx(data = coords, query = pred_coords, k = 5)

nn_indices <- nn$nn.index
nn_distances <- nn$nn.dist


# Functions 
split_data <- function(aggregated_data){
  # Split indices, # Create splits and  returns a list of the 
  # three datasets
  
  data <- aggregated_data %>%
    sample_frac(1)
  
  
  num_row <- nrow(data)
  train_index <- 1:floor(num_row * train_prop)
  test_index <- (max(train_index) + 1):(max(train_index) + floor(num_row * test_prop))
  val_index <- (max(test_index) + 1):num_row
  
  
  train_data <- data[train_index, ]
  test_data <- data[test_index, ]
  val_data <- data[val_index, ]
  
  return(list(train_data, 
              test_data, 
              val_data))
  
}


specificity_loss <- function(preds, dtrain, thresh = 0.125) {
  ## Return a loss that increases as specificity decreases
  
  labels <- get_field(dtrain, "label")
  threshold <- thresh  
  preds_binary <- ifelse(preds > threshold, 1, 0)
  
  tn <- sum((preds_binary == 0) & (labels < threshold))
  fp <- sum((preds_binary == 1) & (labels < threshold))
  
  specificity <- tn / (tn + fp)
  
  return(list(name = "specificity_loss", 
              value = -specificity, 
              higher_better = TRUE))
}


parameter_search_space <- function(param_grid, dtrains, validss){
  
  best_rmse <- Inf
  best_params <- list()
  
  for (i in 1:nrow(param_grid)) {
    params <- as.list(param_grid[i, ])
    params$objective <- "regression"
    params$metric <- "rmse"
    
    
    model <- lgb.train(
      # Train the model with validation data
      params = params,
      data = dtrains,
      nrounds = 100,
      valids = validss,
      eval = specificity_loss,
      early_stopping_rounds = 50,
      verbose = -1
    )
    
    
    rmse <- min(unlist(model$record_evals$valid$rmse$eval))
    
    if (rmse < best_rmse) {
      best_rmse <- rmse
      best_params <- params
    }
  }
  
  return(list(best_rmse,
              best_params))
  
}





target_column <- "target_variable"


param_grid <- expand.grid(
  learning_rate = c(0.01, 0.05, 0.1),
  num_leaves = c(31, 63, 127),
  max_depth = c(-1, 7, 15),
  min_data_in_leaf = c(20, 50, 100),
  feature_fraction = c(0.8, 0.9, 1.0),
  bagging_fraction = c(0.8, 0.9, 1.0),
  bagging_freq = c(0, 5)
)


data_aggregated <- lapply(seq_along(1:1000), function(x) split_data(all_data))

train_data = lapply(seq_along(data_aggregated), function(x) data_aggregated[[x]][[1]])
test_data = lapply(seq_along(data_aggregated), function(x) data_aggregated[[x]][[2]])
val_data = lapply(seq_along(data_aggregated), function(x) data_aggregated[[x]][[3]])



# preprocess the training, validation and testing

lightgbm_data <-lapply(seq_along(train_data), function (x) train_data[[x]] %>%
  dplyr::select(all_of(c(target_column, predictor_columns))) %>%
  na.omit())

X <- lapply(seq_along(lightgbm_data), function(x) as.matrix(lightgbm_data[[x]][, predictor_columns]))
y <- lapply(seq_along(lightgbm_data), function(x)lightgbm_data[[x]][[target_column]])


lightgbm_data_test <- lapply(seq_along(test_data), function (x) test_data[[x]] %>%
  dplyr::select(all_of(c(target_column, predictor_columns))) %>%
  na.omit())

X_test <-lapply(seq_along(val_data), function(x) as.matrix(lightgbm_data_test[[x]][, predictor_columns]))


lightgbm_data_val <- lapply(seq_along(val_data), function (x) val_data[[x]]%>%
  dplyr::select(all_of(c(target_column, predictor_columns))) %>%
  na.omit())


X_val_matrix <- lapply(seq_along(lightgbm_data_val), function (x) as.matrix(lightgbm_data_val[[x]][, predictor_columns]))
y_val <- lapply(seq_along(lightgbm_data_val), function(x) lightgbm_data_val[[x]][[target_column]])



# start parameter search space for each model 

dtrain <- lapply(seq_along(X), function (x)lgb.Dataset(data = X[[x]], 
                                                       label = y[[x]],
                      params = list(feature_pre_filter = FALSE)))



dval <- lapply(seq_along(X_val_matrix), function(x) lgb.Dataset(data = X_val_matrix[[x]],
                    label = y_val[[x]], 
                    params = list(feature_pre_filter = FALSE)))


valids <- lapply(seq_along(dtrain), function (x)list(train = dtrain[[x]], valid = dval[[x]]))



best_paramss <- lapply(seq_along(dtrain), function (x)
  parameter_search_space(param_grid, 
                         dtrains = dtrain[[x]],
                                    valids[[x]]))


params <- list(
  learning_rate = best_params$learning_rate,
  num_leaves = best_params$num_leaves,
  max_depth = best_params$max_depth,
  min_data_in_leaf = best_params$min_data_in_leaf,
  feature_fraction = best_params$feature_fraction,
  bagging_fraction = best_params$bagging_fraction,
  bagging_freq = best_params$bagging_freq,
  objective= best_params$objective,
  metric = best_params$metric
)




Ward_predict_data <- data.frame(wardname = NULL,
                                ward = NULL,
                                iteration= NULL,
                                brier_score= NULL,
                                rmse= NULL, 
                                log_loss= NULL,
                                predicted_prevalence= NULL,
                                ranks= NULL)


#start loop here 

    model <- lgb.train(
      params = params,
      data = dtrain,
      nrounds = 1000,
      valids = list(train = dtrain, valid = dval),
      early_stopping_rounds = 50,
      verbose = -1
    )
    
    
    # importance <- lgb.importance(model, percentage = TRUE)
    # lgb.plot.importance(importance, top_n = 25, measure = "Gain")
    
    # testing
    probabilities <- predict(model, newdata  = X_test)
    epsilon <- 1e-15
    predicted_probabilities <- pmin(pmax(probabilities, epsilon), 1 - epsilon)
    
    
    log_loss <- -mean(test_data$target_variable * log(predicted_probabilities) + 
                        (1 - test_data$target_variable) * log(1 - predicted_probabilities))
    
    
    
    brier_score <- mean((predicted_probabilities - test_data$target_variable)^2)
    
    
    pred_values <- predict(model, newdata = X_val_matrix)
    
    
    rmse <- sqrt(mean((pred_values - val_data$target_variable)^2))
    
    
    Predict_data$nn_avg_target <- sapply(1:nrow(predict_data),  function(i) {
      # Calculate weighted or unweighted averages of the training `
      # target_variable` for prediction data
      # Neighbors from training data
      # Use `target_variable`
      # Weighted average using inverse distances
      # Small constant to avoid division by zero
      # Normalize weights
      
      neighbor_indices <- nn_indices[i, ]  
      neighbor_values <- train_data$target_variable[neighbor_indices]  
      weights <- 1 / (nn_distances[i, ] + 1e-6)  
      weights <- weights / sum(weights)  
      sum(weights * neighbor_values, na.rm = TRUE)
    })
    
    X_pred <- as.matrix(Predict_data[, predictor_columns])
    
    # Generate predictions
    predictions <- pmax(0, predict(model, X_pred))
    predict_data$predictions <- predictions
    
    
    predict_data$nn_avg_prediction <- sapply(1:nrow(predict_data), function(i) {
      neighbor_indices <- nn_indices[i, ]
      neighbor_predictions <- train_data$target_variable[neighbor_indices]
      weights <- 1 / (nn_distances[i, ] + 1e-6)
      weights <- weights / sum(weights)
      sum(weights * neighbor_predictions, na.rm = TRUE)
    })
    
    
    Ward_predictdata <- predict_data %>%
      # Aggregate predictions by ward
      # mutate() %>% 
      # group_by(wardname) %>%
      transmute( wardname = wardname, 
                 ward = ward,
                 iteration = ii, 
                 brier_score = brier_score, 
                 rmse = rmse,
                 log_loss = log_loss,
                 predicted_prevalence = predictions)
    
    Ward_predict_data <- rbind(Ward_predict_data, Ward_predictdata)







write.csv(Ward_predict_data, 
          paste0("C:/Users/laure/Downloads/ward_level_prevalence_optimal_params_1000",".csv")) 


# my_model <- saveRDS(model, "model_better.rds")



# Plotting##########################


gridded_map <- Ward_predict_data %>% 
  group_by(ward, wardname) %>% 
  summarise(mean_predicted_prevalence = median(predicted_prevalence), 
            sd_predicted_prevalence = sd(predicted_prevalence))

Kano_shapefile <- sf::st_read("C:/Users/laure/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_shapefile/gridded_shapefile/gridded/Kano/Kano.shp")


# Make geometries valid and remove duplicate vertices
Kano_shapefile <- st_make_valid(Kano_shapefile) %>% 
  st_transform(crs = st_crs(Kano_shapefile)) %>% 
  inner_join(gridded_map)



# spatial_data <- st_as_sf(kano_prevalence, coords = c("longitude", "latitude"), crs = 4326)
Kano_shapefile <- st_transform(Kano_shapefile, crs = 4326)

# Kano_shapefile <- st_transform(Kano_shapefile, crs = st_crs(spatial_data))


# kano_choropleth_data <- st_join(Kano_shapefile, spatial_data)

# Plot the choropleth map
ggplot(Kano_shapefile) +
  geom_sf(aes(fill = mean_predicted_prevalence)) +  
  scale_fill_gradient(low = "yellow", high = "red", name = "Malaria Probability") +  
  labs(
    title = "Malaria Probability in Kano",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_void()



ward_map <- Ward_predict_data %>% 
  group_by(wardname) %>% 
  summarise(mean_predicted_prevalence = median(predicted_prevalence), 
            sd_predicted_prevalence = sd(predicted_prevalence)) %>% 
  mutate(ranks = rank(mean_predicted_prevalence, ties.method = "first"))


View(mean_predicted_prevalence)
