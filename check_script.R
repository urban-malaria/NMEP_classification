## PRIORITIZE WARDS AND POPULATION ESTIMATE FUNCTION


ii = 3

data = combined_wards2
population_col = "Population"
rank_col = "ranks"
class_col = colums[ii]
ward_col = "WardName"
target_percentage = 30

# prioritize_wards <- function(data, 
#                              population_col, 
#                              rank_col, 
#                              class_col, 
#                              ward_col, 
#                              target_percentage
#                              ) {
  
  
  total_population <- lapply(seq_along(data), function (x) sum(data[[x]][[population_col]], na.rm = TRUE))
  
  selected_wards <- c()
  cumulative_population <- 0
  ward_populations <- c()
  ward_percentages <- c()
  WardCode <- c()
  
  data_sorted <- lapply(seq_along(data), function (x) data[[x]][!is.na(data[[x]][[population_col]]) & !is.na(data[[x]][[rank_col]]), ])
  
  
  data_sorted <- lapply(seq_along(data), function (x) (data_sorted[[x]] %>% 
                          distinct(WardName.x, urbanPercentage, .keep_all = T))[order(data_sorted[[x]][[rank_col]]), ])
  
  
  for (i in 1:nrow(data_sorted)) {
    ward <- data_sorted[i, ]
    
    if (is.na(ward[[class_col]]) || ward[[class_col]] == "Rural") {
      next
    }
    
    selected_wards <- c(selected_wards, ward[[ward_col]])
    ward_population <- ward[[population_col]]
    cumulative_population <- cumulative_population + ward_population
    current_percentage <- (ward_population / total_population) * 100
    WardCode <- c(WardCode, ward$WardCode)
    
    ward_populations <- c(ward_populations, ward_population)
    ward_percentages <- c(ward_percentages, round(current_percentage, 2))
    
    if (!is.na(current_percentage) && (cumulative_population / total_population) * 100 >= target_percentage) {
      break
    }
  }
  
  result <- data.frame(
    SelectedWards = selected_wards,
    WardCode = WardCode,
    WardPopulation = ward_populations,
    WardPercentage = ward_percentages,
    CumulativePopulation = cumsum(ward_populations),
    CumulativePercentage = round(cumsum(ward_populations) / total_population * 100, 2)
  )
  
  return(result)
}
