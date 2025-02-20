# ==========================================================================================================================================
## Script Name: NMEP Cities/States Exploration
## Purpose: This script processes shapefiles for Nigerian cities and states, generating individual shapefiles for specific cities and wards. 
## It also creates visualizations of wards in selected cities and saves them as PDF plots. Additionally, it handles the processing of 
## Kano State shapefiles.
# ==========================================================================================================================================

# clear the environment
rm(list = ls())

# load custom paths
source("~/NMEP_classification/load_path.R", echo = TRUE)

# read nigeria ward shapefiles
nigeria_shp <- st_read(file.path(ShpfilesDir2, "Nigeria", "Nigeria_Wards.shp"))

## -----------------------------------------------------------------------------------------------------------------------------------------
### Generate Individual City Shapefiles from nigeria_shp
## -----------------------------------------------------------------------------------------------------------------------------------------

# abeokuta shapefile
abeokuta_shp <- nigeria_shp %>%
  filter(LGACode == "28001" | LGACode == "28002") %>%  # abeokuta-north, abeokuta-south
  mutate(LGAName = ifelse(LGACode == "28001", "Abeokuta-North",
                          ifelse(LGACode == "28002", "Abeokuta-South", NA)))
st_write(abeokuta_shp, file.path(ShpfilesDir, "Abeokuta", "Abeokuta.shp"))

# katsina shapefile
katsina_shp <- nigeria_shp %>%
  filter(LGACode %in% c("421", "402", "426", "427", "430")) %>%  # katsina, batagarawa, mani, mashi, rimi
  mutate(LGAName = ifelse(LGACode == "421", "Katsina",
                          ifelse(LGACode == "402", "Batagarawa",
                                 ifelse(LGACode == "426", "Mani",
                                        ifelse(LGACode == "427", "Mashi",
                                               ifelse(LGACode == "430", "Rimi", NA))))))
st_write(katsina_shp, file.path(ShpfilesDir, "Katsina", "Katsina.shp"))

# dutse shapefile
dutse_shp <- nigeria_shp %>%
  filter(LGACode == "18012") %>%  # dutse
  mutate(LGAName = "Dutse")
st_write(dutse_shp, file.path(ShpfilesDir, "Dutse", "Dutse.shp"))

# damaturu shapefile
damaturu_shp <- nigeria_shp %>%
  filter(LGACode == "703") %>%
  mutate(LGAName = "Damaturu")
st_write(damaturu_shp, file.path(ShpfilesDir, "Damaturu", "Damaturu.shp"))

# gombe shapefile
gombe_shp <- nigeria_shp %>%
  filter(LGACode == "16006") %>%
  mutate(LGAName = "Gombe")
st_write(gombe_shp, file.path(ShpfilesDir, "Gombe", "Gombe.shp"))

# jalingo shapefile
jalingo_shp <- nigeria_shp %>%
  filter(LGACode == "35007") %>%
  mutate(LGAName = "Jalingo")
st_write(jalingo_shp, file.path(ShpfilesDir, "Jalingo", "Jalingo.shp"))

# asaba shapefile
asaba_shp <- nigeria_shp %>%
  filter(LGACode == "10015") %>%
  mutate(LGAName = "Oshimili-South")
st_write(asaba_shp, file.path(ShpfilesDir, "Asaba", "Asaba.shp"))

# sapele shapefile
sapele_shp <- nigeria_shp %>%
  filter(LGACode == "10017") %>%
  mutate(LGAName = "Sapele")
st_write(sapele_shp, file.path(ShpfilesDir, "Sapele", "Sapele.shp"))

# plot wards in sapele
ggplot() +
  geom_sf(data = sapele_shp, aes(geometry = geometry, fill = LGAName)) +
  ggrepel::geom_text_repel(data = sapele_shp, aes(label = WardName, geometry = geometry),
                           color = "black", stat = "sf_coordinates",
                           min.segment.length = 0, size = 4, force = 1) +
  labs(title = "Wards in Sapele",
       x = "", y = "", fill = "LGA") +
  map_theme()

# plot wards for each city
cities <- c("Abeokuta", "Asaba", "Damaturu", "Dutse", "Gombe", "Ilorin", "Jalingo", "Kano", "Katsina",
            "Minna", "Osogbo", "Warri", "Zaria")

for (city in cities) {
  
  # read shapefile for each city
  city_shp <- st_read(file.path(ShpfilesDir, city, paste0(city, ".shp")))
  
  # create map of wards
  city_map <- ggplot() +
    geom_sf(data = city_shp, aes(geometry = geometry, fill = LGAName)) +
    ggrepel::geom_text_repel(data = city_shp, aes(label = WardName, geometry = geometry),
                             color = "black", stat = "sf_coordinates",
                             min.segment.length = 0, size = 4, force = 1) +
    labs(title = paste("Wards in", city),
         x = "", y = "", fill = "LGA") +
    map_theme()
  
  # save the plot
  ggsave(filename = file.path(plots_dir, "cities", paste0(city, ".pdf")),
         plot = city_map, width = 8, height = 6)
}

# plot all wards in kano state
kano_state_shp <- nigeria_shp %>%
  filter(StateCode == "KN")  # all kano state wards = 484 wards

ggplot() +
  geom_sf(data = kano_state_shp, aes(geometry = geometry)) +
  map_theme()

# save kano state shapefile
st_write(kano_state_shp, file.path(ShpfilesDir, "all_reprioritization_nmep_states",
                                   "Kano State", "Kano_State.shp"))
