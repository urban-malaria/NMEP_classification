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



# colour palettes for the map 
palettes_00 <- list(rev(RColorBrewer::brewer.pal(5, "OrRd")))[[1]][5:1]
palettes <- list(rev(RColorBrewer::brewer.pal(5, "RdYlBu")))

#data files
ibadan_variables <- read.csv(file.path(OutputsDir, "NMEP Malaria Risk Scores", "ibadan_field_variables.csv"))
kano_variables <- read.csv(file.path(OutputsDir, "NMEP Malaria Risk Scores", "kano_field_variables.csv"))

ibadan_wards_shp <- st_read(file.path(ShpfilesDir, "IB_4_Wards/Ibadan_4.shp"))
kano_wards_shp <- st_read( file.path(ShpfilesDir, "KN_5_Wards/Kano_5.shp"))

#plot initial maps

ibadan_variables <- ibadan_variables %>%
  left_join(ibadan_wards_shp %>% dplyr::select(WardName, geometry), by = c("Ward" = "WardName"))

ib_covariates <- c("malaria_positive", "net_own", "net_use2")

new_names <- c(malaria_positive = "TPR",
               net_own = "Net Ownership",
               net_use2 = "Net Use")

for (covariate in ib_covariates) {
  plotting_data <- ibadan_variables %>%
    reshape2::melt(id.vars = c("Ward", "geometry")) %>% 
    filter(variable == covariate) %>% 
    mutate(variable = new_names[variable])
  
  p0 <- ggplot()+
    geom_sf(data = plotting_data, aes(geometry = geometry, fill = value), 
            color = "gray")+
    scale_fill_continuous(low = "lightblue", high = "darkblue")+
    labs(title = paste("Inital Distribution", new_names[covariate]),
         fill = new_names[covariate])+
    map_theme()
  
  print(p0)
  ggsave(filename = paste0(OutputsDir, "/NMEP Malaria Risk Scores/Plots/Ibadan/","values_", covariate, ".png"), plot = p0, width = 8, height = 6)
}


###############################NORMALIZE ##########################################
normalize <- function(x) {
  ifelse(is.na(x), NA, (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

IB_normalized <- ibadan_variables %>%
  mutate(prev_normal = normalize(malaria_positive),
    net_own_normal = normalize(net_own),
    net_use_normal = normalize(net_use2))  

IB_model <- IB_normalized %>% 
  mutate (
    model01 = prev_normal - net_own_normal -net_use_normal,
    model02 = prev_normal - net_own_normal,
    model03 = prev_normal - net_use_normal,
    model04 = - net_own_normal - net_own_normal,
    model05 = - net_own_normal,
    model06 = - net_use_normal)




    
    
    