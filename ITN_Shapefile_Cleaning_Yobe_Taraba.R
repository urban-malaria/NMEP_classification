library(dplyr)
library(tidyverse)
library(sf)

nga_shape <- st_read(file.path(dhsDir,"nigeria/nigeria_ward_boundaries/ng_wrds", "ng_wrds.shp"))

names(nga_shape)

ShpKano <- nga_shape %>%
  filter(StateCode == "KN")

ShpKaduna <- nga_shape %>%
  filter(StateCode == "KD")

ShpKatsina <- nga_shape %>%
  filter(StateCode == "KT")

ShpNiger <- nga_shape %>%
  filter(StateCode == "NI")

ShpDelta <- nga_shape %>%
  filter(StateCode == "DE" & !WardName=="Egodor")

ShpYobe <- nga_shape %>%
  filter(StateCode == "YO")

ShpTaraba <- nga_shape %>%
  filter(StateCode == "TA")


#Check wards that are not merging

shpybngeom <- st_drop_geometry(ShpYobe)

shpdtyb <-anti_join(ITN_distribution_total_ward_yobe_2022_recoded,shpybngeom, by = c("Ward" = "WardName" ))

shpdtyb2 <-anti_join(shpybngeom,ITN_distribution_total_ward_yobe_2022_recoded, by = c("WardName" = "Ward" ))


ITNYobe <- ITN_distribution_total_ward_yobe_2022_recoded[order(ITN_distribution_total_ward_yobe_2022_recoded$Ward), ]

#sort the ward name and filter out one extra ward with no correspondence in ITN Data

shp_sorted <- ShpYobe[order(ShpYobe$WardName), ] %>%
  filter(!WardCode == 71303)

shpdata <- cbind(shp_sorted, ITNYobe)

write_csv(ITNYobe,"ITNYobe.csv")
write_csv(shp_sorted, "ShapYobe.csv")


write_csv(shpdtyb,"Nomatch1.csv")
write_csv(shpdtyb2, "Nomatch2.csv")



ShpYobe <- ShpYobe %>%
  mutate(WardName = case_match(WardName,
                               "Bare Bari" ~ "Bare-Bari",
                               "Chillariye" ~ "Chilariya",
                               "Chukuriwa" ~ "Chikuiwa",
                               "Danani Lawanti" ~ "Danani/Lawanti",
                               "Darin Langawa" ~ "Darin/Lang",
                               "Dogon Nini" ~ "Dogo Nini",
                               "Dole" ~ "Dole Machina",
                               "Fajiganari" ~ "Faji Ganari",
                               "Falimaram" ~ "Falimiram",
                               "Fika Anze" ~ "Fika/Anze",
                               "Gadaka/She" ~ "Gadaka/Shembire",
                               "Goneri" ~ "Goniri",
                               "Gotala Gotumba" ~ "Gotala",
                               "Guji Metalari" ~ "Guji/Metalari",
                               "Hausawa Asibiti" ~ "Hausawa Asibity",
                               "Jaji Maji" ~ "Jajimaji",
                               "Jawa Garun Dole" ~ "Jawa/Garun Dole",
                               "Juluri Damnawa" ~ "Julluri",
                               "Karausuwa Garin Guna" ~ "Karasuwa Garin Guna",
                               "Kollere Kafaje" ~ "Kollere PHCC",
                               "Koriyel" ~ "Koryel",
                               "Lantewa" ~ "Lantaiwa",
                               "Ma'anna" ~ "Ma'Anna",
                               "Maisandari/Waziri Ibrahim" ~ "Maisandari Waziri Ibrahim",
                               "Maja Kura" ~ "Majakura",
                               "Marmari Gudugurka" ~ "Mari-Mari/Gud",
                               "Mayori" ~ "Moyori",
                               "Mozogun" ~ "Mazagon",
                               "Murfakalam" ~ "Murfa Kalam",
                               "Ngalda/Dumb" ~ "Ngalda/Dumbulwa",
                               "Ngilewa" ~ "Tsohon Nguru",
                               "Njiwaji/Gwange" ~ "Njiwaji Gwange",
                               "S G Kanuri" ~ "Sabon Gari Kanuri",
                               "Sabon Gari" ~ "Sabongari",
                               "Shoye" ~ "Shoye/Garin Aba",
                               "Sungul Koka" ~ "Koka/Sungul",
                               "Turmi Malori" ~ "Turmi/Maluri",
                               "Yerimaram" ~ "Yarimaram",
                               "Zangaya Mazawaun" ~ "Zangaya/Mazawun",
                               .default = WardName  # leave unmatched values unchanged
  ))
st_write(ShpYobe, "/Users/macbookpro/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/NMEP_nigeria_shapefiles/states/Yobe/Yobe.shp", delete_layer = TRUE)




shpybngeomt <- st_drop_geometry(ShpTaraba)

shpdtybt <-anti_join(ITN_taraba,shpybngeomt, by = c("Ward" = "WardName" ))

shpdtyb2t <-anti_join(shpybngeomt,ITN_taraba, by = c("WardName" = "Ward" ))


ITN_taraba <- ITN_taraba[order(ITN_taraba$Ward), ]

shp_sortedt <- ShpTaraba[order(ShpTaraba$WardName), ] #%>%
  #filter(!WardCode == 71303)

shpdatat <- cbind(shp_sortedt, ITN_taraba)

write_csv(ITN_taraba,"ITNTaraba.csv")
write_csv(shp_sortedt, "ShapTaraba.csv")


write_csv(shpdtyb,"Nomatch1t.csv")
write_csv(shpdtyb2, "Nomatch2t.csv")




# 2. Filter the shapefile for the two wards using their WardCode
wards_two <- ShpTaraba %>%
  filter(WardCode %in% c("TRSBAL10", "TRSDGA10"))

wards_two_centroids <- st_centroid(wards_two)

# 3. Plotting to compare 2 suntai to decide which is Lapo
ggplot() +
  geom_sf(data = wards_two, fill = "lightblue", color = "darkblue", size = 1) +
  geom_sf_text(data = wards_two_centroids, aes(label = WardCode), 
               color = "black", fontface = "bold", size = 4) +
  ggtitle("Plot of Two Wards: TRSBAL10 & TRSDGA10 (Suntai)") +
  theme_minimal()


ShpTaraba <- ShpTaraba %>%
  mutate(WardName = case_match(WardCode,
                               "TRSBBB01" ~ "Dampar I",
                               "TRSBBB02" ~ "Dampar II",
                               "TRSBBB03" ~ "Dampar III",
                               "TRSYRR02" ~ "Gongon",
                               "TRSDGA10" ~ "Lapo",
                               .default = WardName  # leave unmatched values unchanged
  ))

#Verify if it perfectly merges
shpybngeomt <- st_drop_geometry(ShpTaraba)

shpdtybt <-anti_join(ITN_taraba,shpybngeomt, by = c("Ward" = "WardName" ))

shpdtyb2t <-anti_join(shpybngeomt,ITN_taraba, by = c("WardName" = "Ward" ))


st_write(ShpTaraba, "/Users/macbookpro/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/NMEP_nigeria_shapefiles/states/Taraba/Taraba.shp", delete_layer = TRUE)


