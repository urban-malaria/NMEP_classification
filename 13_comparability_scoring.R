#### Normalization and Scoring

##get new shapefiles

ibadan_shp <- st_read(file.path(ShpfilesDir, "Ibadan", "Ibadan.shp"))
ibadan_wards_shp <- ibadan_shp %>% 
  filter(WardName == "Agugu" | WardName == "Bashorun" | WardName == "Challenge" | 
           WardName == "Olopomewa")
ggplot()+
  geom_sf(data = ibadan_wards_shp, aes(geometry = geometry))

st_write(ibadan_wards_shp, file.path(ShpfilesDir, "IB_4_Wards/Ibadan_4.shp"))


kano_shp <- st_read(file.path(ShpfilesDir, "Kano", "Kano.shp"))
kano_wards_shp <- kano_shp %>% 
  filter(WardName == "Dorayi" | WardName == "Fagge D2" | WardName == "Giginyu" | 
           WardName == "Gobirawa" | WardName == "Zango")
st_write(kano_wards_shp, file.path(ShpfilesDir, "KN_5_Wards/Kano_5.shp"))



#data files
ibadan_variables <- read.csv(file.path(OutputsDir, "NMEP Malaria Risk Scores", "ibadan_field_variables.csv"))
kano_variables <- read.csv(file.path(OutputsDir, "NMEP Malaria Risk Scores", "kano_field_variables.csv"))

ibadan_wards_shp <- st_read(file.path(ShpfilesDir, "IB_4_Wards/Ibadan_4.shp"))
kano_wards_shp <- st_read( file.path(ShpfilesDir, "KN_5_Wards/Kano_5.shp"))

#plot initial maps

covariates <- c("malaria_positive", "net_own", "net_use2")

new_names <- c(malaria_positive = "TPR",
               net_own = "Net Ownership",
               net_use2 = "Net Use")
#Ibadan

ibadan_variables <- ibadan_variables %>%
  left_join(ibadan_wards_shp %>% dplyr::select(WardName, geometry), by = c("Ward" = "WardName"))


for (covariate in covariates) {
  plotting_data <- ibadan_variables %>%
    reshape2::melt(id.vars = c("Ward", "geometry")) %>% 
    filter(variable == covariate) %>% 
    mutate(variable = new_names[variable])
  
  p0 <- ggplot()+
    geom_sf(data = plotting_data, aes(geometry = geometry, fill = value), 
            color = "gray")+
    scale_fill_continuous(low = "lightyellow", high = "darkred")+
    geom_text_repel(
      data = ibadan_variables,
      aes(label =  Ward, geometry = geometry),color ='black',
      stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
    labs(title = paste(new_names[covariate] , "in Ibadan"),
         fill = new_names[covariate],
         x = "", y = "")+
    map_theme()
  
  print(p0)
 ggsave(filename = paste0(OutputsDir, "/NMEP Malaria Risk Scores/Plots/Ibadan/","values_", covariate, ".png"), plot = p0, width = 8, height = 6)
}

#Kano
kano_variables <- kano_variables %>%
  left_join(kano_wards_shp %>% dplyr::select(WardName, geometry), by = c("Ward" = "WardName"))


for (covariate in covariates) {
  plotting_data <- kano_variables %>%
    reshape2::melt(id.vars = c("Ward", "geometry")) %>% 
    filter(variable == covariate) %>% 
    mutate(variable = new_names[variable])
  
  p0 <- ggplot()+
    geom_sf(data = plotting_data, aes(geometry = geometry, fill = value), 
            color = "gray")+
    scale_fill_continuous(low = "lightyellow", high = "darkred")+
    geom_text_repel(
      data = kano_variables,
      aes(label =  Ward, geometry = geometry),color ='black',
      stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
    labs(title = paste(new_names[covariate] , "in Kano"),
         fill = new_names[covariate],
         x = "", y = "")+
    map_theme()
  
  print(p0)
  ggsave(filename = paste0(OutputsDir, "/NMEP Malaria Risk Scores/Plots/Kano/","values_", covariate, ".png"), plot = p0, width = 8, height = 6)
}

    
    
    