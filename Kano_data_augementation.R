
#Laurette to change the pathways after the pressure 


Kano_shpefile <- sf::st_read(DriveDir, "data/nigeria/nigeria_shapefiles/shapefiles/ShinyApp_shapefiles/Kano_Ibadan/Kano-Ibadan.shp") %>% 
  dplyr::filter(StateCode == "KN") %>%
  filter(WardName %in% c(unique(model_data$ward), "Fagge D2"))

#change pathways

urbanextent <-  sf::st_read("C:/Users/laure/Downloads/Kano_State_urban_percentage.geojson") %>% 
  select(WardName, urbanPercentage) %>% 
  filter(WardName %in% c(unique(model_data$ward), "Fagge D2")) 



urban_data <- sf::st_join(Kano_shpefile, urbanextent) %>% 
  select(ward = WardName.x, urbanPercentage) %>% 
  sf::st_drop_geometry() %>% 
  mutate(ward = ifelse(ward == "Fagge D2", "Fagge", ward))


model_data <- read.csv("C:/Users/laure/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/analysis_docs/kano_environmental_data.csv") 
  



kano_choropleth_data <- model_data  %>%
  inner_join( urban_data) 

write.csv(kano_choropleth_data, "C:/Users/laure/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/analysis_docs/kano_environmental_data.csv", row.names = F) 


model_predictions <- read.csv("C:/Users/laure/Downloads/kano_environmental_data_predictions.csv")[-1]


urbanextent_all<-  sf::st_read("C:/Users/laure/Downloads/Kano_State_urban_percentage.geojson")%>% 
  select(WardName, urbanPercentage) %>% 
  filter(WardName %in% c(unique(model_predictions$wardname))) %>% 
  group_by(WardName) %>% 
  summarise(urbanPercentage  = mean(urbanPercentage, na.rm = T)) %>% 
  st_drop_geometry()


tpr <- read.csv("C:/Users/laure/Downloads/kanotpr (1).csv")[-1] 

morphology <- read.csv("C:/Users/laure/Urban Malaria Proj Dropbox/urban_malaria/projects/urban_microstratification/Building Morphology/Summary Metrics/Kano_metric_stats_all.csv")
morphology$WardName = Kano_shpefile$WardName

settlement_type <- read.csv("C:/Users/laure/Downloads/settlement_blocks_extracted(1).csv") %>% 
  group_by(WardName) %>% 
  summarise(settlement_type.y = mean(settlement_type, na.rm = T))


pred <- inner_join(model_predictions, urbanextent_all, by = c("wardname" = "WardName")) %>% 
  st_drop_geometry() %>% 
  inner_join(tpr, by = c("wardname" = "WardName")) %>% 
  inner_join(morphology, by = c("wardname" = "WardName")) %>% 
  inner_join(settlement_type, by = c("wardname" = "WardName"))%>% 
  select(-c( X, FID.x,FID.y ))


write.csv(pred, "C:/Users/laure/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/analysis_docs/kano_environmental_data_predictions.csv", row.names = F) 


Kano_shpefiles <- sf::st_read("C:/Users/laure/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/nigeria_shapefiles/shapefiles/ShinyApp_shapefiles/Kano_Ibadan/Kano-Ibadan.shp") %>% 
  dplyr::filter(StateCode == "KN")


Kano_shapefile <- sf::st_transform(Kano_shpefiles, crs = 4326)

Kano_shapefile <- sf::st_transform(Kano_shapefile, crs = sf::st_crs(urbanextent_all))


urban_data <- sf::st_join(Kano_shapefile, urbanextent_all) %>% 
  select(ward = WardName.x, urbanPercentage) %>% 
  sf::st_drop_geometry() 





model_data <- read.csv("C:/Users/laure/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/analysis_docs/kano_environmental_data.csv") %>% 
  dplyr::inner_join( urbanextent_all, by = c("ward" = "WardName")) %>% 
  inner_join(tpr, by = c("ward" = "WardName")) %>% 
  inner_join(morphology, by = c("ward" = "WardName")) %>% 
  select(-c( X, FID, city))


write.csv(model_data, "C:/Users/laure/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/analysis_docs/kano_environmental_data.csv", row.names = F) 

