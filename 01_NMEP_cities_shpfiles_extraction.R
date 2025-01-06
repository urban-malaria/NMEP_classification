# ==========================================================================================================================================
## Script Name: NMEP Cities/States Exploration
## Purpose: This script processes shapefiles for Nigerian cities and states, generating individual shapefiles for specific cities and wards. 
## It also creates visualizations of wards in selected cities and saves them as PDF plots. Additionally, it handles the processing of 
## Kano State shapefiles.
# ==========================================================================================================================================

rm(list=ls())
source("~/NMEP_classification/load_path.R", echo = T)


nigeria_shp<- st_read(file.path(ShpfilesDir2, "Nigeria", "Nigeria_Wards.shp"))

#generate individual city shape files from nigeria_shp

#Abeokuta
abeokuta_shp <- nigeria_shp %>% 
  filter(LGACode == "28001" | LGACode == "28002") %>%  #Abeokuta-North, Abeokuta-South
  mutate(LGAName = ifelse(LGACode == "28001", "Abeokuta-North",
                          ifelse(LGACode == "28002", "Abeokuta-South", NA)))
st_write(abeokuta_shp, file.path(ShpfilesDir, "Abeokuta", "Abeokuta.shp" ))

#Katsina
katsina_shp <- nigeria_shp %>% 
  filter(LGACode == "421" | LGACode == "402" | LGACode == "426" |LGACode == "427" | LGACode == "430") %>% #Katsina, Batagarawa, Mani, Mashi, Rimi
  mutate(LGAName = ifelse(LGACode == "421", "Katsina",
                          ifelse(LGACode == "402", "Batagawara",
                                 ifelse(LGACode == "426", "Mani",
                                        ifelse(LGACode == "427", "Mashi",
                                               ifelse(LGACode == "430", "Rimi", NA))))))
st_write(katsina_shp, file.path(ShpfilesDir, "Katsina", "Katsina.shp" ))

#Dutse
dutse_shp <- nigeria_shp %>% 
  filter(LGACode == "18012") %>% #Dutse
  mutate(LGAName = "Dutse")
st_write(dutse_shp, file.path(ShpfilesDir, "Dutse", "Dutse.shp" ))

#Damaturu
damaturu_shp <- nigeria_shp %>% 
  filter(LGACode == "703") %>% 
  mutate(LGAName = "Damaturu")
st_write(damaturu_shp, file.path(ShpfilesDir, "Damaturu", "Damaturu.shp" ))

#Gombe
gombe_shp <- nigeria_shp %>% 
  filter(LGACode == "16006") %>% 
  mutate(LGAName = "Gombe")
st_write(gombe_shp, file.path(ShpfilesDir, "Gombe", "Gombe.shp" ))

#Jalingo
jalingo_shp <- nigeria_shp %>% 
  filter(LGACode == "35007") %>% 
  mutate(LGAName = "Jalingo")
st_write(jalingo_shp, file.path(ShpfilesDir, "Jalingo", "Jalingo.shp" ))

#Asaba
asaba_shp <- nigeria_shp %>% 
  filter(LGACode == "10015") %>% 
  mutate(LGAName = "Oshimili-South")
st_write(asaba_shp, file.path(ShpfilesDir, "Asaba", "Asaba.shp" ))

#Sapele
sapele_shp <- nigeria_shp %>% 
  filter(LGACode == "10017") %>% 
  mutate(LGAName = "Sapele")
st_write(sapele_shp, file.path(ShpfilesDir, "Sapele", "Sapele.shp" ))

ggplot()+
  geom_sf(data = sapele_shp, aes(geometry = geometry, fill = LGAName))+
  ggrepel::geom_text_repel(data = sapele_shp, aes(label = WardName, geometry = geometry),
                           color = "black", stat = "sf_coordinates",
                           min.segment.length = 0, size = 4, force = 1)+
  labs(title = "Wards in Sapele",
       x = "",
       y = "",
       fill = "LGA")+
  map_theme()

#plot wards in each city
cities <- c("Abeokuta", "Asaba", "Damaturu", "Dutse", "Gombe", "Ilorin", "Jalingo", "Kano", "Katsina", 
            "Minna", "Osogbo", "Warri", "Zaria")

for (city in cities) {
  
  city_shp <- st_read(file.path(ShpfilesDir, city, paste0(city, ".shp")))
  
  city_map <- ggplot()+
    geom_sf(data = city_shp, aes(geometry = geometry, fill = LGAName))+
    ggrepel::geom_text_repel(data = city_shp, aes(label = WardName, geometry = geometry),
                             color = "black", stat = "sf_coordinates",
                             min.segment.length = 0, size = 4, force = 1)+
    labs(title = paste("Wards in", city),
         x = "",
         y = "",
         fill = "LGA")+
    map_theme()
  
ggsave(filename = file.path(plots_dir, "cities", paste0(city, ".pdf")), plot = city_map, 
       width = 8, height = 6)
}


# Kano State

kano_state_shp <- nigeria_shp %>% 
  filter(StateCode == "KN")  #All Kano State Wards = 484 wards

ggplot()+
  geom_sf(data = kano_state_shp, aes(geometry = geometry))+
  map_theme()

st_write(kano_state_shp, file.path(ShpfilesDir,"all_reprioritization_nmep_states",
                                   "Kano State", "Kano_State.shp" ))
