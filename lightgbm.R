rm(list = ls())

# Load necessary libraries
library(lightgbm)
library(sf)
library(dplyr)
library(tidyr)
library(Matrix)
library(FNN)
library(ggplot2)

set.seed(611)

# Load spatial data
spatial_data <- st_read("C:/Users/laure/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/analysis_docs/Kano_Predictions_New/kano_environmental_data.csv")
predict_data <- read.csv("C:/Users/laure/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/analysis_docs/kano_environmental_data_predictions.csv")


# Proportions for each set
train_prop <- 0.7
test_prop <- 0.15
val_prop <- 0.15


# Aggregate spatial data
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
              value = specificity, 
              higher_better = TRUE))
}

# all_data = all_data[-c(1,2,3)]

data_aggregated <- split_data(all_data)

train_data = data_aggregated[[1]]; test_data = data_aggregated[[2]]; val_data = data_aggregated[[3]]


target_column <- "target_variable"

lightgbm_data <- train_data %>%
  dplyr::select(all_of(c(target_column, predictor_columns))) %>%
  na.omit()

lightgbm_data_test <- test_data %>%
  dplyr::select(all_of(c(target_column, predictor_columns))) %>%
  na.omit()

lightgbm_data_val <- val_data %>%
  dplyr::select(all_of(c(target_column, predictor_columns))) %>%
  na.omit()

X <- as.matrix(lightgbm_data[, predictor_columns])

X_test <- as.matrix(lightgbm_data_test[, predictor_columns])

X_val_matrix <- as.matrix(lightgbm_data_val[, predictor_columns])


y <- lightgbm_data[[target_column]]
y_val <- lightgbm_data_val[[target_column]]

lgb_data <- lgb.Dataset(data = X, label = y)


params <- list(
  # Set parameters for LightGBM
    objective = "regression",
    metric = "rmse",
    learning_rate = 0.02,
    max_depth = 6,           # Moderate tree depth
    num_leaves = 64,         # Should be ≤ 2^max_depth
    min_data_in_leaf = 20,   # Regularization to prevent overfitting
    feature_fraction = 0.8,  # Use 80% of features for each iteration
    bagging_fraction = 0.8,  # Use 80% of data for each iteration
    bagging_freq = 5         # Perform bagging every 5 iterations
  )



# model_train <- lgb.train(
#   params = list(objective = "regression"),
#   data = lgb_data,
#   nrounds = 10000,
#   eval = specificity_loss,
#   verbose = 1
# )




## Predict on the validation set

dtrain <- lgb.Dataset(data = X , 
                      label = y)



dval <- lgb.Dataset(data = X_val_matrix,
                    label = y_val)




valids <- list(train = dtrain, 
               valid = dval)




model <- lgb.train(
  # Train the model with validation data
  params = params,
  data = dtrain,
  nrounds = 10000,
  valids = valids,
  eval = specificity_loss,
  early_stopping_rounds = 10000
)



importance <- lgb.importance(model, percentage = TRUE)
lgb.plot.importance(importance, top_n = 25, measure = "Gain")

# testing
probabilities <- predict(model, newdata  = X_test)
epsilon <- 1e-15
predicted_probabilities <- pmin(pmax(probabilities, epsilon), 1 - epsilon)


# log_loss <- -mean(test_data$target_variable * log(predicted_probabilities) + 
#                     (1 - test_data$target_variable) * log(1 - predicted_probabilities))
# 
# 
# 
# # roc_curve <- roc(test_data$target_variable, predicted_probabilities)
# brier_score <- mean((predicted_probabilities - test_data$target_variable)^2)


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


Predict_data$nn_avg_target <- sapply(1:nrow(predict_data),  function(i) {
  # Calculate weighted or unweighted averages of the training `
  # target_variable` for prediction data
  # Neighbors from training data
  # Use `target_variable`
  neighbor_indices <- nn_indices[i, ]  
  neighbor_values <- data_aggregated$target_variable[neighbor_indices]  
  
  # Weighted average using inverse distances
  # Small constant to avoid division by zero
  # Normalize weights
  weights <- 1 / (nn_distances[i, ] + 1e-6)  
  weights <- weights / sum(weights)  
  sum(weights * neighbor_values, na.rm = TRUE)
})


X_pred <- as.matrix(Predict_data[, predictor_columns])

# Generate predictions
predictions <- pmax(0, predict(model, X_pred))
predict_data$predictions <- predictions

# Compute nearest neighbor predictions for prediction data
pred_coords <- predict_data[, c("longitude", "latitude")]
nn <- get.knnx(data = coords, query = pred_coords, k = 5)

nn_indices <- nn$nn.index
nn_distances <- nn$nn.dist

predict_data$nn_avg_prediction <- sapply(1:nrow(predict_data), function(i) {
  neighbor_indices <- nn_indices[i, ]
  neighbor_predictions <- data_aggregated$target_variable[neighbor_indices]
  weights <- 1 / (nn_distances[i, ] + 1e-6)
  weights <- weights / sum(weights)
  sum(weights * neighbor_predictions, na.rm = TRUE)
})


Ward_predict_data <- predict_data %>%
  # Aggregate predictions by ward
  group_by(wardname) %>%
  summarise(
    predicted_prevalence = max(predictions),
    .groups = "drop"
  ) %>%
  mutate(ranks = rank(predicted_prevalence, 
                      ties.method = "first"))



 write.csv(Ward_predict_data, "C:/Users/laure/Downloads/ward_level_prevalence_optimal_params_better.csv") 


# saveRDS(model, "model_better.rds")
my_model <- readRDS("model_better.rds")

