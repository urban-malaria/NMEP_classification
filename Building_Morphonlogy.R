install.packages("raster")

#directory path to dropbox

Drive <- Sys.getenv("HOME")

#Drive <- gsub("\\\\", "/", Drive)
DriveDir <- file.path(Drive, "Urban Malaria Proj Dropbox", "urban_malaria")
dhsDir <- file.path(DriveDir, "data")
# dropbox <- file.path(dhsDir, "/nigeria/urban_microstratification/shiny_app_data")
# results <- file.path(DriveDir, "projects/Manuscripts/ongoing/ShinyAppDevelopment/images/R")

# packges to use
list_of_packages <- c("stringr","ggplot2", "dplyr", "purrr", "haven", "tidyverse",
                      "readxl", "patchwork", "tidyr", "factoextra", "MASS", "broom",
                      "glm2", "viridis", "ggwordcloud", "sf", "raster")
read_install_pacakges <- function(packages = list_of_packages
){
  new_packages <- packages[!(list_of_packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new_packages)
  return(sapply(list_of_packages, require, character.only = TRUE))
}
read_install_pacakges()


library(dplyr)
library(tidyverse)
library(sf)
library(raster)
library(ggplot2)
library(openxlsx)

#####Initializing Folder Path
floodrasterpath <- "nigeria/Raster_files/flooding_2023"
shapefilepath <-  "nigeria/nigeria_shapefiles/shapefiles/ShinyApp_shapefiles/Kano_household"
predictionpoint <- "nigeria/kano_shapefile/gridded_shapefile/gridded/Kano"


####load Household shape file
points_sp <- st_read(file.path(dhsDir ,shapefilepath, "Kano_household.shp"))

###Load prediction point shape file
prediction_point <- st_read(file.path(dhsDir , predictionpoint, "Kano.shp"))


flood <- raster(file.path(dhsDir , floodrasterpath, "Flooded_Areas_Nigeria_2023.tif"))


st_crs(nga_shape) <- "4326"

#HH Raster
points_flood <- extract(flood, nga_shape, fun = mean, df = TRUE)

#PREDICTION RASTERS
predicted_points_flood <- extract(flood, prediction_point, fun = mean, df = TRUE)

library(terra)

#HH Raster
nga_buildingheight <- extract(buildingheightraster, ShpKano, fun = mean, df = TRUE)

#PREDICTION RASTERS
predicted_points_buildingheight <- extract(buildingheightraster, prediction_point, fun = mean, df = TRUE)



#Assign extracted values to a column in shapefile
points_sp$flood <- points_flood$VH

#Assign extracted values to a column in shapefile
prediction_point$flood <- predicted_points_flood$VH

st_write(points_sp)


ggplot(prediction_point) +
  geom_sf()+
  geom_sf(data = points_sp, aes(colour = flood, geometry=geometry)) +
  scale_colour_viridis_c(option = "C") +
  #geom_text(data = points_sp, aes(label = ward, geometry=geometry) )+
  labs(title = "Mean Flood Intensity", fill = "Flood Intensity") +
  theme_minimal()


ggplot(prediction_point) +
  geom_sf()+
  geom_sf(aes(fill = flood)) +
  scale_fill_viridis_c(option = "C") +
  labs(title = "Mean Flood Intensity", fill = "Flood Intensity") +
  theme_minimal()




#############Load Kano Metro Shape file##########
kanoshp <- st_read(file.path(DriveDir, "projects/urban_microstratification/Abidjan_Nigeria/Documents/AI_based_settlement_type_classification/Deliverables/inputs/Kano/Kano_metro_ward_sixLGAs/Kano_metro_ward_sixLGAs.shp"))
flooddata2024 <- read.xlsx( file.path(DriveDir, "/Users/macbookpro/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_flood_data/Kano Flood Assessment data (18 September 2024).xlsx"))

kanoshp_flood <- left_join(kanoshp, flooddata2024, by=c("WardName" = "Ward"))

ggplot(kanoshp_flood) +
  geom_sf()+
  geom_sf(aes(fill = `Affected.households`)) +
  scale_fill_viridis_c(option = "C") +
  labs(title = "Affected Households by Flood", fill = "Number of Affected Households") +
  theme_minimal()



environmentaldata <- read_csv( file.path(dhsDir , "nigeria/kano_ibadan_epi/new_field_data/analysis_docs/kano_environmental_data.csv") )
environmentaldata$flood <- points_sp$flood
environmentaldata$building_height <- points_buildingheight$building_height

predictiondata <- read_csv( file.path(dhsDir , "nigeria/kano_ibadan_epi/new_field_data/analysis_docs/kano_environmental_data_predictions.csv") )

predictiondata$flood <- prediction_point$flood
predictiondata$building_height <- predicted_points_buildingheight$building_height

flood2024hh <- flooddata2024 %>%
  select(Ward, Affected.households)%>%
  rename(No_of_hh_affected_by_flood_2024 = Affected.households)%>%
  group_by(Ward)%>%
  summarise(total_hh_affected_by_flood_2024 = sum(No_of_hh_affected_by_flood_2024))%>%
  mutate(Ward = recode(Ward, 
                "Hotoro (NNPC)"= "Hotoro South",
                "Unguwa  Uku"="Unguwa Uku Cikin Gari",
                "Kauyen  Alu"="Unguwa Uku Kauyen Alu"
                ))

#colnames(flood2024hh$Affected.households) <- "No_of_hh_affected_by_flood_2024"

environmentaldatav1 <- environmentaldata %>%
  left_join(flood2024hh, by = c("ward"="Ward"))%>%
  rename(flooding2023 = flood)

predictiondata <- predictiondata %>%
  rename(flooding2023 = flood)


write_csv(environmentaldatav1, file.path(dhsDir , "nigeria/kano_ibadan_epi/new_field_data/analysis_docs/kano_environmental_data.csv") )

write_csv(df_separatedv1, file.path(dhsDir , "nigeria/kano_ibadan_epi/new_field_data/analysis_docs/kano_environmental_data_predictions.csv") )



# Load tidyr package
library(tidyr)


# Separate the text and numbers
df_separated <- predictiondata %>%
  separate(ward, into = c("wardname", "grid_number"), sep = "_", extra = "merge")

# Print the result
df_separated_ <- df_separated %>%
  select(wardname) 


df_separatedv1 <- combinedpred %>%
  left_join(flood2024hh, by = c("wardname"="Ward"))%>%
  distinct()



combinedpred <- cbind(predictiondata, df_separated_)
predictiondata$No_of_hh_affected_by_flood_2024 <- df_separatedv1$No_of_hh_affected_by_flood_2024



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
  filter(StateCode == "DE")

ShpYobe <- nga_shape %>%
  filter(StateCode == "YO")

ShpTaraba <- nga_shape %>%
  filter(StateCode == "TA")


st_write(ShpKano,file.path(dhsDir,"nigeria/nigeria_ward_boundaries/ng_wrds/kano_state.shp") )

st_write(ShpKatsina,file.path(dhsDir,"nigeria/nigeria_ward_boundaries/ng_wrds/katsina_state.shp") )

############Kano Foot print##########

############Set up foot package
install.packages("devtools")
library(devtools)
# Path to the package folder
install(file.path(dhsDir, "rasters/foot-master/") )


library (foot)
library(sf)

kano_city_fp <- read.csv(file.path(dhsDir,"nigeria/building_footprints/OpenBuildings", "Kano.csv"))
kano_city_fp <- st_as_sf(kano_city_fp, wkt = "geometry")
st_crs(kano_city_fp) <- 4326

kano_fp <- kano_fp %>%
  mutate(FID = row_number())

kano_stats <- calculate_footstats(kano_fp, #kano_shp of 66 wards
                                  zone = "FID",
                                  what =list(list("area"), list("perimeter"), list("compact"), list("angle"), list("shape"),
                                             list("settled")
                                             #list("nndist")
                                  ),
                                  how =list(list("mean","median","sd", "max", "min", "cv", "sum", "iqr"), #area
                                            list("mean","median","sd", "max", "min", "cv", "sum", "iqr"), #perimeter
                                            list("mean","median","sd", "max", "min", "cv", "iqr"), #compact
                                            list("mean","median","sd", "max", "min", "cv", "entropy", "iqr"), #angle
                                            list("mean","median","sd", "max", "min", "cv", "iqr"), #shape
                                            list("binary", "count") #settled
                                            #list("mean","median","sd", "max", "min", "cv", "nnindex", "iqr")
                                  ), #nndist
                                  controlZone = list(zoneName = "FID",
                                                     method = "centroid"), verbose = TRUE)

kano_c <- cbind(kano_fp,kano_stats) %>%
  dplyr::select(- FID.1)

write.csv(kano_c, file.path(dhsDir,"nigeria/building_footprints", "Summary Metrics", "kano_stats.csv"))




kano_city_fp <- read.csv(file.path(dhsDir,"nigeria/building_footprints/OpenBuildings", "Kano.csv"))
kano_city_fp <- st_as_sf(kano_city_fp, wkt = "geometry")
st_crs(kano_city_fp) <- 4326

kano_city_fp <- kano_city_fp %>%
  mutate(FID = row_number())
Kano_stats_nni <- calculate_footstats(kano_city_fp, #kano_shp of 66 wards
                                         zone = "FID",
                                          what ="all", 
                                          how ="all",
                                          controlZone = list(zoneName = "FID",
                                                             method = "centroid"), verbose = TRUE)

Kano_c <- cbind(Kano_fp,Kano_stats_nni) %>%
  dplyr::select(- FID.1)

st_drop_geometry(Kano_c)
write.csv(Kano_c, file.path(dhsDir,"Nigeria/building_footprints", "Summary Metrics", "Kano_city_settlement_stats.csv"))





######Warri###############
library(foot)
warri_fp <- read.csv(file.path(dhsDir,"nigeria/building_footprints/OpenBuildings", "Warri.csv"))
warri_fp <- st_as_sf(warri_fp, wkt = "geometry")
st_crs(warri_fp) <- 4326

warri_ward <- st_join(warri_fp, ShpDelta)

warri_fp <- warri_fp %>%
  mutate(FID = row_number())

warri_stats <- calculate_footstats(warri_ward, #warri_shp of 66 wards
                                  zone = "WardName",
                                  what =list(#list("area"), list("perimeter"), list("compact"), list("angle"), list("shape"),
                                            #list("settled"),
                                             list("nndist")),
                                  how =list(#list("mean","median","sd", "max", "min", "cv", "sum", "iqr"), #area
                                  #           list("mean","median","sd", "max", "min", "cv", "sum", "iqr"), #perimeter
                                  #           list("mean","median","sd", "max", "min", "cv", "iqr"), #compact
                                  #           list("mean","median","sd", "max", "min", "cv", "entropy", "iqr"), #angle
                                  #           list("mean","median","sd", "max", "min", "cv", "iqr"), #shape
                                  #           list("binary", "count"), #settled
                                            list("mean","median","sd", "max", "min", "cv", "nnindex", "iqr")), #nndist
                                  controlZone = list(zoneName = "WardName",
                                                     method = "centroid"), verbose = TRUE)

write.csv(warri_stats, file.path(dhsDir,"nigeria/building_footprints", "Summary Metrics", "warri_nni_stats.csv"))







######Asaba###############
library(foot)
asaba_fp <- read.csv(file.path(dhsDir,"nigeria/building_footprints/OpenBuildings", "Asaba.csv"))
asaba_fp <- st_as_sf(asaba_fp, wkt = "geometry")
st_crs(asaba_fp) <- 4326

asaba_fp <- asaba_fp %>%
  mutate(FID = row_number())

asaba_stats <- calculate_footstats(asaba_fp, #asaba_shp of 66 wards
                                   zone = "FID",
                                   what =list(list("area"), list("perimeter"), list("compact"), list("angle"), list("shape"),
                                              list("settled")
                                              #list("nndist")
                                              ),
                                   how =list(list("mean","median","sd", "max", "min", "cv", "sum", "iqr"), #area
                                             list("mean","median","sd", "max", "min", "cv", "sum", "iqr"), #perimeter
                                             list("mean","median","sd", "max", "min", "cv", "iqr"), #compact
                                             list("mean","median","sd", "max", "min", "cv", "entropy", "iqr"), #angle
                                             list("mean","median","sd", "max", "min", "cv", "iqr"), #shape
                                             list("binary", "count") #settled
                                             #list("mean","median","sd", "max", "min", "cv", "nnindex", "iqr")
                                             ), #nndist
                                   controlZone = list(zoneName = "FID",
                                                      method = "centroid"), verbose = TRUE)
Asaba_c <- cbind(asaba_fp,asaba_stats) %>%
  dplyr::select(- FID.1)

write.csv(Asaba_c, file.path(dhsDir,"nigeria/building_footprints", "Summary Metrics", "asaba_stats.csv"))


######Ilorin###############
library(foot)
library(sf)
ilorin_fp <- read.csv(file.path(dhsDir,"nigeria/building_footprints/OpenBuildings", "Ilorin.csv"))
ilorin_fp <- st_as_sf(ilorin_fp, wkt = "geometry")
st_crs(ilorin_fp) <- 4326

ilorin_fp <- ilorin_fp %>%
  mutate(FID = row_number())

ilorin_stats <- calculate_footstats(ilorin_fp, #ilorin_shp of 66 wards
                                   zone = "FID",
                                   what =list(list("area"), list("perimeter"), list("compact"), list("angle"), list("shape"),
                                              list("settled")
                                              #list("nndist")
                                   ),
                                   how =list(list("mean","median","sd", "max", "min", "cv", "sum", "iqr"), #area
                                             list("mean","median","sd", "max", "min", "cv", "sum", "iqr"), #perimeter
                                             list("mean","median","sd", "max", "min", "cv", "iqr"), #compact
                                             list("mean","median","sd", "max", "min", "cv", "entropy", "iqr"), #angle
                                             list("mean","median","sd", "max", "min", "cv", "iqr"), #shape
                                             list("binary", "count") #settled
                                             #list("mean","median","sd", "max", "min", "cv", "nnindex", "iqr")
                                   ), #nndist
                                   controlZone = list(zoneName = "FID",
                                                      method = "centroid"), verbose = TRUE)
ilorin_c <- cbind(ilorin_fp,ilorin_stats) %>%
  dplyr::select(- FID.1)

write.csv(ilorin_c, file.path(dhsDir,"nigeria/building_footprints", "Summary Metrics", "ilorin_stats.csv"))







######ibadan###############
library(foot)
ibadan_fp <- read.csv(file.path(dhsDir,"nigeria/building_footprints/OpenBuildings", "Ibadan.csv"))
ibadan_fp <- st_as_sf(ibadan_fp, wkt = "geometry")
st_crs(ibadan_fp) <- 4326

ibadan_fp <- ibadan_fp %>%
  mutate(FID = row_number())

ibadan_stats <- calculate_footstats(ibadan_fp, #ibadan_shp of 66 wards
                                    zone = "FID",
                                    what =list(list("area"), list("perimeter"), list("compact"), list("angle"), list("shape"),
                                               list("settled")
                                               #list("nndist")
                                    ),
                                    how =list(list("mean","median","sd", "max", "min", "cv", "sum", "iqr"), #area
                                              list("mean","median","sd", "max", "min", "cv", "sum", "iqr"), #perimeter
                                              list("mean","median","sd", "max", "min", "cv", "iqr"), #compact
                                              list("mean","median","sd", "max", "min", "cv", "entropy", "iqr"), #angle
                                              list("mean","median","sd", "max", "min", "cv", "iqr"), #shape
                                              list("binary", "count") #settled
                                              #list("mean","median","sd", "max", "min", "cv", "nnindex", "iqr")
                                    ), #nndist
                                    controlZone = list(zoneName = "FID",
                                                       method = "centroid"), verbose = TRUE)
ibadan_c <- cbind(ibadan_fp,ibadan_stats) %>%
  dplyr::select(- FID.1)

write.csv(ibadan_c, file.path(dhsDir,"nigeria/building_footprints", "Summary Metrics", "ibadan_stats.csv"))



######katsina###############
library(foot)
katsina_fp <- read.csv(file.path(dhsDir,"nigeria/building_footprints/OpenBuildings", "Katsina.csv"))
katsina_fp <- st_as_sf(katsina_fp, wkt = "geometry")
st_crs(katsina_fp) <- 4326

katsina_fp <- katsina_fp %>%
  mutate(FID = row_number())

katsina_stats <- calculate_footstats(katsina_fp, #katsina_shp of 66 wards
                                    zone = "FID",
                                    what =list(list("area"), list("perimeter"), list("compact"), list("angle"), list("shape"),
                                               list("settled")
                                               #list("nndist")
                                    ),
                                    how =list(list("mean","median","sd", "max", "min", "cv", "sum", "iqr"), #area
                                              list("mean","median","sd", "max", "min", "cv", "sum", "iqr"), #perimeter
                                              list("mean","median","sd", "max", "min", "cv", "iqr"), #compact
                                              list("mean","median","sd", "max", "min", "cv", "entropy", "iqr"), #angle
                                              list("mean","median","sd", "max", "min", "cv", "iqr"), #shape
                                              list("binary", "count") #settled
                                              #list("mean","median","sd", "max", "min", "cv", "nnindex", "iqr")
                                    ), #nndist
                                    controlZone = list(zoneName = "FID",
                                                       method = "centroid"), verbose = TRUE)
katsina_c <- cbind(katsina_fp,katsina_stats) %>%
  dplyr::select(- FID.1)

write.csv(katsina_c, file.path(dhsDir,"nigeria/building_footprints", "Summary Metrics", "katsina_stats.csv"))







######jalingo###############
library(foot)
jalingo_fp <- read.csv(file.path(dhsDir,"nigeria/building_footprints/OpenBuildings", "Jalingo.csv"))
jalingo_fp <- st_as_sf(jalingo_fp, wkt = "geometry")
st_crs(jalingo_fp) <- 4326

jalingo_fp <- jalingo_fp %>%
  mutate(FID = row_number())

jalingo_stats <- calculate_footstats(jalingo_fp, #jalingo_shp of 66 wards
                                     zone = "FID",
                                     what =list(list("area"), list("perimeter"), list("compact"), list("angle"), list("shape"),
                                                list("settled")
                                                #list("nndist")
                                     ),
                                     how =list(list("mean","median","sd", "max", "min", "cv", "sum", "iqr"), #area
                                               list("mean","median","sd", "max", "min", "cv", "sum", "iqr"), #perimeter
                                               list("mean","median","sd", "max", "min", "cv", "iqr"), #compact
                                               list("mean","median","sd", "max", "min", "cv", "entropy", "iqr"), #angle
                                               list("mean","median","sd", "max", "min", "cv", "iqr"), #shape
                                               list("binary", "count") #settled
                                               #list("mean","median","sd", "max", "min", "cv", "nnindex", "iqr")
                                     ), #nndist
                                     controlZone = list(zoneName = "FID",
                                                        method = "centroid"), verbose = TRUE)
jalingo_c <- cbind(jalingo_fp,jalingo_stats) %>%
  dplyr::select(- FID.1)

write.csv(jalingo_c, file.path(dhsDir,"nigeria/building_footprints", "Summary Metrics", "jalingo_stats.csv"))




######gombe###############
library(foot)
gombe_fp <- read.csv(file.path(dhsDir,"nigeria/building_footprints/OpenBuildings", "Gombe.csv"))
gombe_fp <- st_as_sf(gombe_fp, wkt = "geometry")
st_crs(gombe_fp) <- 4326

gombe_fp <- gombe_fp %>%
  mutate(FID = row_number())

gombe_stats <- calculate_footstats(gombe_fp, #gombe_shp of 66 wards
                                     zone = "FID",
                                     what =list(list("area"), list("perimeter"), list("compact"), list("angle"), list("shape"),
                                                list("settled")
                                                #list("nndist")
                                     ),
                                     how =list(list("mean","median","sd", "max", "min", "cv", "sum", "iqr"), #area
                                               list("mean","median","sd", "max", "min", "cv", "sum", "iqr"), #perimeter
                                               list("mean","median","sd", "max", "min", "cv", "iqr"), #compact
                                               list("mean","median","sd", "max", "min", "cv", "entropy", "iqr"), #angle
                                               list("mean","median","sd", "max", "min", "cv", "iqr"), #shape
                                               list("binary", "count") #settled
                                               #list("mean","median","sd", "max", "min", "cv", "nnindex", "iqr")
                                     ), #nndist
                                     controlZone = list(zoneName = "FID",
                                                        method = "centroid"), verbose = TRUE)
gombe_c <- cbind(gombe_fp,gombe_stats) %>%
  dplyr::select(- FID.1)

write.csv(gombe_c, file.path(dhsDir,"nigeria/building_footprints", "Summary Metrics", "gombe_stats.csv"))



######damaturu###############
library(foot)
damaturu_fp <- read.csv(file.path(dhsDir,"nigeria/building_footprints/OpenBuildings", "Damaturu.csv"))
damaturu_fp <- st_as_sf(damaturu_fp, wkt = "geometry")
st_crs(damaturu_fp) <- 4326

damaturu_fp <- damaturu_fp %>%
  mutate(FID = row_number())

damaturu_stats <- calculate_footstats(damaturu_fp, #damaturu_shp of 66 wards
                                     zone = "FID",
                                     what =list(list("area"), list("perimeter"), list("compact"), list("angle"), list("shape"),
                                                list("settled")
                                                #list("nndist")
                                     ),
                                     how =list(list("mean","median","sd", "max", "min", "cv", "sum", "iqr"), #area
                                               list("mean","median","sd", "max", "min", "cv", "sum", "iqr"), #perimeter
                                               list("mean","median","sd", "max", "min", "cv", "iqr"), #compact
                                               list("mean","median","sd", "max", "min", "cv", "entropy", "iqr"), #angle
                                               list("mean","median","sd", "max", "min", "cv", "iqr"), #shape
                                               list("binary", "count") #settled
                                               #list("mean","median","sd", "max", "min", "cv", "nnindex", "iqr")
                                     ), #nndist
                                     controlZone = list(zoneName = "FID",
                                                        method = "centroid"), verbose = TRUE)
damaturu_c <- cbind(damaturu_fp,damaturu_stats) %>%
  dplyr::select(- FID.1)

write.csv(damaturu_c, file.path(dhsDir,"nigeria/building_footprints", "Summary Metrics", "damaturu_stats.csv"))



######dutse###############
library(foot)
library(sf)
dutse_fp <- read.csv(file.path(dhsDir,"nigeria/building_footprints/OpenBuildings", "Dutse.csv"))
dutse_fp <- st_as_sf(dutse_fp, wkt = "geometry")
st_crs(dutse_fp) <- 4326

dutse_fp <- dutse_fp %>%
  mutate(FID = row_number())

dutse_stats <- calculate_footstats(dutse_fp, #dutse_shp of 66 wards
                                     zone = "FID",
                                     what =list(list("area"), list("perimeter"), list("compact"), list("angle"), list("shape"),
                                                list("settled")
                                                #list("nndist")
                                     ),
                                     how =list(list("mean","median","sd", "max", "min", "cv", "sum", "iqr"), #area
                                               list("mean","median","sd", "max", "min", "cv", "sum", "iqr"), #perimeter
                                               list("mean","median","sd", "max", "min", "cv", "iqr"), #compact
                                               list("mean","median","sd", "max", "min", "cv", "entropy", "iqr"), #angle
                                               list("mean","median","sd", "max", "min", "cv", "iqr"), #shape
                                               list("binary", "count") #settled
                                               #list("mean","median","sd", "max", "min", "cv", "nnindex", "iqr")
                                     ), #nndist
                                     controlZone = list(zoneName = "FID",
                                                        method = "centroid"), verbose = TRUE)
dutse_c <- cbind(dutse_fp,dutse_stats) %>%
  dplyr::select(- FID.1)

write.csv(dutse_c, file.path(dhsDir,"nigeria/building_footprints", "Summary Metrics", "dutse_stats.csv"))



######abeokuta###############
library(foot)
abeokuta_fp <- read.csv(file.path(dhsDir,"nigeria/building_footprints/OpenBuildings", "Abeokuta.csv"))
abeokuta_fp <- st_as_sf(abeokuta_fp, wkt = "geometry")
st_crs(abeokuta_fp) <- 4326

abeokuta_fp <- abeokuta_fp %>%
  mutate(FID = row_number())

abeokuta_stats <- calculate_footstats(abeokuta_fp, #abeokuta_shp of 66 wards
                                     zone = "FID",
                                     what =list(list("area"), list("perimeter"), list("compact"), list("angle"), list("shape"),
                                                list("settled")
                                                #list("nndist")
                                     ),
                                     how =list(list("mean","median","sd", "max", "min", "cv", "sum", "iqr"), #area
                                               list("mean","median","sd", "max", "min", "cv", "sum", "iqr"), #perimeter
                                               list("mean","median","sd", "max", "min", "cv", "iqr"), #compact
                                               list("mean","median","sd", "max", "min", "cv", "entropy", "iqr"), #angle
                                               list("mean","median","sd", "max", "min", "cv", "iqr"), #shape
                                               list("binary", "count") #settled
                                               #list("mean","median","sd", "max", "min", "cv", "nnindex", "iqr")
                                     ), #nndist
                                     controlZone = list(zoneName = "FID",
                                                        method = "centroid"), verbose = TRUE)
abeokuta_c <- cbind(abeokuta_fp,abeokuta_stats) %>%
  dplyr::select(- FID.1)

write.csv(abeokuta_c, file.path(dhsDir,"nigeria/building_footprints", "Summary Metrics", "abeokuta_stats.csv"))


###############################Settlement block Kano #######################

library(sf)


settlement_blocks <- st_read(file.path(dhsDir, "nigeria/Raster_files/nigeria_settlement_classification", "blocks_V1.1",
                                       "Nigeria_Blocks_V1.shp")) %>% 
  filter(landuse =='Residential')

shape <- st_read("Users/macbookpro/Downloads/Kano_State/Kano_State.shp")

shape <- nga_shape
st_crs(shape) <- 4326

st_crs(settlement_blocks)

shape <- st_transform(shape, crs = st_crs(settlement_blocks))

kano_settlement <- st_join(shape, settlement_blocks, 
                            join = sf::st_overlaps)

ko_bar_dat <- kano_settlement
sf::st_geometry(ko_bar_dat) <- NULL

ko_bar_overall = ko_bar_dat %>% 
  dplyr::select(type) %>%  
  group_by(type) %>%  
  summarise(number = n())

settlement_type_kano = kano_settlement %>% 
  dplyr::select(WardName, settle_type = type) %>% 
  group_by(WardName, settle_type) %>% 
  summarise(number = n())

ggplot()+
  geom_sf(data = shape)+
  geom_sf(data = settlement_type_kano, aes(geometry = geometry, fill = number))+
  scale_fill_continuous(low = "lightyellow", high = "brown", na.value = "grey")+
  facet_wrap(~settle_type)+
  labs(title = "Settlement Types in kano")#+
  #map_theme()

sf::st_geometry(settlement_type_kano) <- NULL 

kano_settlement_types <- settlement_type_kano %>% 
  mutate(number = ifelse(is.na(number), 0,  number)) %>% 
  pivot_wider(names_from = settle_type, values_from = number)

plotting <- shape %>% 
  inner_join(settlement_type_kano) %>% 
  group_by(WardName) %>% 
  mutate(total_settlement = sum(number),
         proportion_settlement_type = number / total_settlement) %>% 
  ungroup() %>% 
  mutate(class_stmnt_number = cut(number, c(0,2,4,6,8,10,15,20,30,50, 78), include.lowest = T),
         class_stmnt_proportion = cut(proportion_settlement_type, seq(0,1,0.2)))

plotting_v2 = plotting %>% 
  mutate(grp = ifelse(settle_type == "A" | settle_type == "B" | settle_type == "M", "Poor", "Good")) %>%
  group_by(WardName, grp, total_settlement) %>% 
  summarise(grp_number = sum(number)) %>% 
  mutate(proportion_settlement_type_grp = grp_number / total_settlement,
         grp_proportion = cut(proportion_settlement_type_grp, seq(0, 1, 0.2), include.lowest = T))

plotting_00 <- plotting_v2 %>% 
  filter(grp == "Poor") %>% 
  dplyr::select(WardName, settlement_type = proportion_settlement_type_grp) %>% 
  st_drop_geometry()

palettes <- list((RColorBrewer::brewer.pal(10, "RdYlBu")))


wardvariableskano <- read_csv(file.path(DriveDir , "projects/urban_microstratification/Shiny App/five_extractions/Kano_wards_variables.csv") )%>%
  left_join( plotting_00, by = "WardName" )

wardvariablesniger <- read_csv(file.path(DriveDir , "projects/urban_microstratification/Shiny App/five_extractions/Niger_wards_variables.csv") )%>%
  left_join( plotting_00, by = "WardName" )

wardvariableskaduna <- read_csv(file.path(DriveDir , "projects/urban_microstratification/Shiny App/five_extractions/Kaduna_wards_variables.csv") )%>%
  left_join( plotting_00, by = "WardName" )

wardvariableskatsina <- read_csv(file.path(DriveDir , "projects/urban_microstratification/Shiny App/five_extractions/Katsina_wards_variables.csv") )%>%
  left_join( plotting_00, by = "WardName" )

wardvariablestaraba <- read_csv(file.path(DriveDir , "projects/urban_microstratification/Shiny App/five_extractions/Taraba_wards_variables.csv") )%>%
  left_join( plotting_00, by = "WardName" )

wardvariablesyobe <- read_csv(file.path(DriveDir , "projects/urban_microstratification/Shiny App/five_extractions/Yobe_wards_variables.csv") )%>%
  left_join( plotting_00, by = "WardName" )

wardvariablesdelta <- read_csv(file.path(DriveDir , "projects/urban_microstratification/Shiny App/five_extractions/Delta_wards_variables.csv") )%>%
  left_join( plotting_00, by = "WardName" )



write_csv(wardvariableskano, file.path(DriveDir , "projects/urban_microstratification/Shiny App/five_extractions/Kano_wards_variables.csv") )

write_csv(wardvariablesniger, file.path(DriveDir , "projects/urban_microstratification/Shiny App/five_extractions/Niger_wards_variables.csv") )

write_csv(wardvariableskaduna, file.path(DriveDir , "projects/urban_microstratification/Shiny App/five_extractions/Kaduna_wards_variables.csv") )

write_csv(wardvariableskatsina, file.path(DriveDir , "projects/urban_microstratification/Shiny App/five_extractions/Katsina_wards_variables.csv") )

write_csv(wardvariablestaraba, file.path(DriveDir , "projects/urban_microstratification/Shiny App/five_extractions/Taraba_wards_variables.csv") )

write_csv(wardvariablesyobe, file.path(DriveDir , "projects/urban_microstratification/Shiny App/five_extractions/Yobe_wards_variables.csv") )

write_csv(wardvariablesdelta, file.path(DriveDir , "projects/urban_microstratification/Shiny App/five_extractions/Delta_wards_variables.csv") )




fielddata <- read_csv(file.path(dhsDir , "nigeria/kano_ibadan_epi/new_field_data/analysis_docs/kano_environmental_data.csv") )

predictiondata <- read_csv(file.path(dhsDir , "nigeria/kano_ibadan_epi/new_field_data/analysis_docs/kano_environmental_data_predictions.csv") )

write_csv(plotting_00, file.path(DriveDir , "projects/urban_microstratification/Shiny App/five_extractions/settlement_blocks_extracted_all_ng_wards.csv") )


kano_variables <- left_join(fielddata, plotting_00, by = c("ward" = "WardName") )


write_csv(kano_variables, file.path(dhsDir , "nigeria/kano_ibadan_epi/new_field_data/analysis_docs/kano_environmental_data.csv") )

write_csv(df_separatedv1, file.path(dhsDir , "nigeria/kano_ibadan_epi/new_field_data/analysis_docs/kano_environmental_data_predictions.csv") )









###########################NNI

############Niger Foot print##########
library (foot)

Niger_fp <- read.csv("/Users/macbookpro/Downloads/Niger_open_buildings_v3_polygons_your_own_wkt_polygon.csv")
Niger_fp <- st_as_sf(Niger_fp, wkt = "geometry")
st_crs(Niger_fp) <- 4326

Niger_fp <- Niger_fp %>%
  mutate(FID = row_number())

Niger_stats_settled <- calculate_footstats(Niger_fp, #kano_shp of 66 wards
                                  zone = "FID",
                                  what =list(list("settled")), 
                                  how =list(list("binary", "count")),
                                  controlZone = list(zoneName = "FID",
                                                     method = "centroid"), verbose = TRUE)

Niger_c <- cbind(Niger_fp,Niger_stats_settled) %>%
  dplyr::select(- FID.1)

write.csv(Niger_c, file.path(dhsDir,"nigeria/building_footprints", "Summary Metrics", "Niger_settlement_stats.csv"))








############Yobe Foot print##########
library (foot)

Yobe_fp <- read.csv("/Users/macbookpro/Downloads/Yobe_open_buildings_v3_polygons_your_own_wkt_polygon.csv")
Yobe_fp <- st_as_sf(Yobe_fp, wkt = "geometry")
st_crs(Yobe_fp) <- 4326

yobe_wards <- st_join(Yobe_fp, ShpYobe)

Yobe_fp <- Yobe_fp %>%
  mutate(FID = row_number())

Yobe_stats_settled <- calculate_footstats(Yobe_fp, #kano_shp of 66 wards
                                           zone = "FID",
                                           what =list(list("settled")), 
                                           how =list(list("binary", "count")),
                                           controlZone = list(zoneName = "FID",
                                                              method = "centroid"), verbose = TRUE)

Yobe_c <- cbind(Yobe_fp,Yobe_stats_settled) %>%
  dplyr::select(- FID.1)

write.csv(Yobe_c, file.path(dhsDir,"Nigeria/building_footprints", "Summary Metrics", "Yobe_settlement_stats.csv"))


#############################Ward level nni

############Yobe Foot print##########
library (foot)

Yobe_fp <- read.csv("/Users/macbookpro/Downloads/Yobe_open_buildings_v3_polygons_your_own_wkt_polygon.csv")
Yobe_fp <- st_as_sf(Yobe_fp, wkt = "geometry")


Yobe_fp <- Yobe_fp %>%
  mutate(FID = row_number())

st_crs(Yobe_fp) <- 4326

yobe_wards <- st_intersection(Yobe_fp, ShpYobe)

yobe_wards <- yobe_wards %>%
  mutate(FID = row_number())



yobe_wards_centroid <- yobe_wards %>%
group_by(WardName) %>%
  summarise(avg_area_in_meters = mean(area_in_meters, na.rm = TRUE),
            avg_confidence_interval = mean(confidence, na.rm = TRUE),
            avg_lat = mean(latitude, na.rm = TRUE),
            avg_long = mean(longitude, na.rm = TRUE),
            full_plus_code_ = list(full_plus_code)
            )



write_dta(yobe_wards,"Yobe_wards.dta")

write_dta(yobe_wards_centroid,"Yobe_wards_centroid.dta")

yobe_wards <- yobe_wards %>%
  mutate(FID = row_number())



Yobe_stats_nni <- calculate_footstats(Yobe_fp, #kano_shp of 66 wards
                            zone = ShpYobe,
                            what =list(#list("area"), list("perimeter"), list("compact"), list("angle"), list("shape"),
                              #list("settled"),
                              list("nndist")),
                            how =list(#list("mean","median","sd", "max", "min", "cv", "sum", "iqr"), #area
                              #           list("mean","median","sd", "max", "min", "cv", "sum", "iqr"), #perimeter
                              #           list("mean","median","sd", "max", "min", "cv", "iqr"), #compact
                              #           list("mean","median","sd", "max", "min", "cv", "entropy", "iqr"), #angle
                              #           list("mean","median","sd", "max", "min", "cv", "iqr"), #shape
                              #           list("binary", "count"), #settled
                              list("mean","median","sd", "max", "min", "cv", "nnindex", "iqr")), #nndist
                            controlZone = list(zoneName = "FID",
                                               method = "centroid"), verbose = TRUE)






























############Taraba Foot print##########
library (foot)

Taraba_fp <- read.csv("/Users/macbookpro/Downloads/Taraba_open_buildings_v3_polygons_your_own_wkt_polygon.csv")
Taraba_fp <- st_as_sf(Taraba_fp, wkt = "geometry")
st_crs(Taraba_fp) <- 4326

Taraba_fp <- Taraba_fp %>%
  mutate(FID = row_number())

Taraba_stats_settled <- calculate_footstats(Taraba_fp, #kano_shp of 66 wards
                                          zone = "FID",
                                          what =list(list("settled")), 
                                          how =list(list("binary", "count")),
                                          controlZone = list(zoneName = "FID",
                                                             method = "centroid"), verbose = TRUE)

Taraba_c <- cbind(Taraba_fp,Taraba_stats_settled) %>%
  dplyr::select(- FID.1)

write.csv(Taraba_c, file.path(dhsDir,"Nigeria/building_footprints", "Summary Metrics", "Taraba_settlement_stats.csv"))




############Kaduna Foot print##########
library (foot)

Kaduna_fp <- read.csv("/Users/macbookpro/Downloads/Kaduna_open_buildings_v3_polygons_your_own_wkt_polygon.csv")
Kaduna_fp <- st_as_sf(Kaduna_fp, wkt = "geometry")
st_crs(Kaduna_fp) <- 4326

Kaduna_fp <- Kaduna_fp %>%
  mutate(FID = row_number())

Kaduna_stats_settled <- calculate_footstats(Kaduna_fp, #kano_shp of 66 wards
                                            zone = "FID",
                                            what =list(list("settled")), 
                                            how =list(list("binary", "count")),
                                            controlZone = list(zoneName = "FID",
                                                               method = "centroid"), verbose = TRUE)

Kaduna_c <- cbind(Kaduna_fp,Kaduna_stats_settled) %>%
  dplyr::select(- FID.1)

write.csv(Kaduna_c, file.path(dhsDir,"Nigeria/building_footprints", "Summary Metrics", "Kaduna_settlement_stats.csv"))








############Delta Foot print##########
library (foot)

Delta_fp <- read.csv("/Users/macbookpro/Downloads/Delta_open_buildings_v3_polygons_your_own_wkt_polygon.csv")
Delta_fp <- st_as_sf(Delta_fp, wkt = "geometry")
st_crs(Delta_fp) <- 4326

Delta_fp <- Delta_fp %>%
  mutate(FID = row_number())

Delta_stats_settled <- calculate_footstats(Delta_fp, #kano_shp of 66 wards
                                            zone = "FID",
                                            what =list(list("settled")), 
                                            how =list(list("binary", "count")),
                                            controlZone = list(zoneName = "FID",
                                                               method = "centroid"), verbose = TRUE)

Delta_c <- cbind(Delta_fp,Delta_stats_settled) %>%
  dplyr::select(- FID.1)

write.csv(Delta_c, file.path(dhsDir,"Nigeria/building_footprints", "Summary Metrics", "Delta_settlement_stats.csv"))








############Katsina Foot print##########
library (foot)

Katsina_fp <- read.csv("/Users/macbookpro/Downloads/Katsina_open_buildings_v3_polygons_your_own_wkt_polygon.csv")
Katsina_fp <- st_as_sf(Katsina_fp, wkt = "geometry")
st_crs(Katsina_fp) <- 4326

Katsina_fp <- Katsina_fp %>%
  mutate(FID = row_number())

Katsina_stats_settled <- calculate_footstats(Katsina_fp, #kano_shp of 66 wards
                                            zone = "FID",
                                            what =list(list("settled")), 
                                            how =list(list("binary", "count")),
                                            controlZone = list(zoneName = "FID",
                                                               method = "centroid"), verbose = TRUE)

Katsina_c <- cbind(Katsina_fp,Katsina_stats_settled) %>%
  dplyr::select(- FID.1)

write.csv(Katsina_c, file.path(dhsDir,"Nigeria/building_footprints", "Summary Metrics", "Katsina_settlement_stats.csv"))



############Kano Foot print##########
library (foot)

Kano_fp <- read.csv("/Users/macbookpro/Downloads/Kano_open_buildings_v3_polygons_your_own_wkt_polygon.csv")
Kano_fp <- st_as_sf(Kano_fp, wkt = "geometry")
st_crs(Kano_fp) <- 4326

Kano_fp <- Kano_fp %>%
  mutate(FID = row_number())

Kano_stats_settled <- calculate_footstats(Kano_fp, #kano_shp of 66 wards
                                             zone = "FID",
                                             what =list(list("nni")), 
                                             how =list(list("binary", "count")),
                                             controlZone = list(zoneName = "FID",
                                                                method = "centroid"), verbose = TRUE)

Kano_c <- cbind(Kano_fp,Kano_stats_settled) %>%
  dplyr::select(- FID.1)

write.csv(Kano_c, file.path(dhsDir,"Nigeria/building_footprints", "Summary Metrics", "Kano_settlement_stats.csv"))



###############################Settlement block Kano #######################

library(sf)


settlement_blocks <- st_read(file.path(dhsDir, "nigeria/Raster_files/nigeria_settlement_classification", "blocks_V1.1",
                                       "Nigeria_Blocks_V1.shp")) %>% 
  filter(landuse =='Residential')

shape <- st_read("Users/macbookpro/Downloads/Kano_State/Kano_State.shp")

shape <- nga_shape
st_crs(shape) <- 4326

st_crs(settlement_blocks)

shape <- st_transform(shape, crs = st_crs(settlement_blocks))

kano_settlement <- st_join(shape, settlement_blocks, 
                           join = sf::st_overlaps)

ko_bar_dat <- kano_settlement
sf::st_geometry(ko_bar_dat) <- NULL

ko_bar_overall = ko_bar_dat %>% 
  dplyr::select(type) %>%  
  group_by(type) %>%  
  summarise(number = n())

settlement_type_kano = kano_settlement %>% 
  dplyr::select(WardName, settle_type = type) %>% 
  group_by(WardName, settle_type) %>% 
  summarise(number = n())

ggplot()+
  geom_sf(data = shape)+
  geom_sf(data = settlement_type_kano, aes(geometry = geometry, fill = number))+
  scale_fill_continuous(low = "lightyellow", high = "brown", na.value = "grey")+
  facet_wrap(~settle_type)+
  labs(title = "Settlement Types in kano")#+
#map_theme()

sf::st_geometry(settlement_type_kano) <- NULL 

kano_settlement_types <- settlement_type_kano %>% 
  mutate(number = ifelse(is.na(number), 0,  number)) %>% 
  pivot_wider(names_from = settle_type, values_from = number)

plotting <- shape %>% 
  inner_join(settlement_type_kano) %>% 
  group_by(WardName) %>% 
  mutate(total_settlement = sum(number),
         proportion_settlement_type = number / total_settlement) %>% 
  ungroup() %>% 
  mutate(class_stmnt_number = cut(number, c(0,2,4,6,8,10,15,20,30,50, 78), include.lowest = T),
         class_stmnt_proportion = cut(proportion_settlement_type, seq(0,1,0.2)))

plotting_v2 = plotting %>% 
  mutate(grp = ifelse(settle_type == "A" | settle_type == "B" | settle_type == "M", "Poor", "Good")) %>%
  group_by(WardName, grp, total_settlement) %>% 
  summarise(grp_number = sum(number)) %>% 
  mutate(proportion_settlement_type_grp = grp_number / total_settlement,
         grp_proportion = cut(proportion_settlement_type_grp, seq(0, 1, 0.2), include.lowest = T))

plotting_00 <- plotting_v2 %>% 
  filter(grp == "Poor") %>% 
  dplyr::select(WardName, settlement_type = proportion_settlement_type_grp) %>% 
  st_drop_geometry()

palettes <- list((RColorBrewer::brewer.pal(10, "RdYlBu")))




########################wkt extraction



# Load required libraries
library(sf)


# Define the path to the shapefile
shapefile_path <- file.path(dhsDir,"nigeria/nigeria_shapefiles/shapefiles/ShinyApp_shapefiles/all_reprioritization_nmep_states/STATES/Kano/Kano_State.shp")

# Read the shapefile
gdf <- st_read(shapefile_path)

# Check the CRS
print(st_crs(gdf))

# Transform CRS to EPSG 4326
gdf <- st_transform(gdf, crs = 4326)

# Check the transformed CRS
print(st_crs(gdf))

# Combine polygons into a single geometry
combined_polygon <- st_union(gdf$geometry)

# Create a new GeoDataFrame (sf object) with the combined polygon
combined_gdf <- st_sf(geometry = st_sfc(combined_polygon), crs = st_crs(gdf))

# Print the combined polygon geometry
print(combined_polygon)

write.csv(combined_polygon, file.path(dhsDir,"nigeria/nigeria_shapefiles/shapefiles/ShinyApp_shapefiles/all_reprioritization_nmep_states/STATES/Kano/Kano_State_combined.csv"))



# Kaduna combined polygon
library(sf)


# Define the path to the shapefile
Kadunashapefile_path <- file.path(dhsDir,"nigeria/nigeria_shapefiles/shapefiles/ShinyApp_shapefiles/all_reprioritization_nmep_states/STATES/Kaduna/Kaduna_State.shp")

# Read the shapefile
Kaduna_gdf <- st_read(Kadunashapefile_path)

# Check the CRS
print(st_crs(Kaduna_gdf))

# Transform CRS to EPSG 4326
Kaduna_gdf <- st_transform(Kaduna_gdf, crs = 4326)

# Check the transformed CRS
print(st_crs(Kaduna_gdf))

# Combine polygons into a single geometry
Kaduna_combined_polygon <- st_union(Kaduna_gdf$geometry)

# Create a new GeoDataFrame (sf object) with the combined polygon
Kaduna_combined_gdf <- st_sf(geometry = st_sfc(Kaduna_combined_polygon), crs = st_crs(Kaduna_gdf))

# Print the combined polygon geometry
print(Kaduna_combined_polygon)

write.table(Kaduna_combined_gdf, file.path(dhsDir,"nigeria/nigeria_shapefiles/shapefiles/ShinyApp_shapefiles/all_reprioritization_nmep_states/STATES/Kaduna/Kaduna_State_polygon.txt"))



# Delta combined polygon
library(sf)


# Define the path to the shapefile
Deltashapefile_path <- file.path(dhsDir,"nigeria/nigeria_shapefiles/shapefiles/ShinyApp_shapefiles/all_reprioritization_nmep_states/STATES/Delta/Delta_State.shp")

# Read the shapefile
Delta_gdf <- st_read(Deltashapefile_path)

# Check the CRS
print(st_crs(Delta_gdf))

# Transform CRS to EPSG 4326
Delta_gdf <- st_transform(Delta_gdf, crs = 4326)

# Check the transformed CRS
print(st_crs(Delta_gdf))

# Combine polygons into a single geometry
Delta_combined_polygon <- st_union(Delta_gdf$geometry)

# Create a new GeoDataFrame (sf object) with the combined polygon
Delta_combined_gdf <- st_sf(geometry = st_sfc(Delta_combined_polygon), crs = st_crs(Delta_gdf))

# Print the combined polygon geometry
print(Delta_combined_polygon)

write.table(Delta_combined_gdf, file.path(dhsDir,"nigeria/nigeria_shapefiles/shapefiles/ShinyApp_shapefiles/all_reprioritization_nmep_states/STATES/Delta/Delta_State_polygon.txt"))






# Katsina combined polygon
library(sf)


# Define the path to the shapefile
Katsinashapefile_path <- file.path(dhsDir,"nigeria/nigeria_shapefiles/shapefiles/ShinyApp_shapefiles/all_reprioritization_nmep_states/STATES/Katsina/Katsina_State.shp")

# Read the shapefile
Katsina_gdf <- st_read(Katsinashapefile_path)

# Check the CRS
print(st_crs(Katsina_gdf))

# Transform CRS to EPSG 4326
Katsina_gdf <- st_transform(Katsina_gdf, crs = 4326)

# Check the transformed CRS
print(st_crs(Katsina_gdf))

# Combine polygons into a single geometry
Katsina_combined_polygon <- st_union(Katsina_gdf$geometry)

# Create a new GeoDataFrame (sf object) with the combined polygon
Katsina_combined_gdf <- st_sf(geometry = st_sfc(Katsina_combined_polygon), crs = st_crs(Katsina_gdf))

# Print the combined polygon geometry
print(Katsina_combined_polygon)

write.table(Katsina_combined_gdf, file.path(dhsDir,"nigeria/nigeria_shapefiles/shapefiles/ShinyApp_shapefiles/all_reprioritization_nmep_states/STATES/Katsina/Katsina_State_polygon.txt"))





# Taraba combined polygon
library(sf)


# Define the path to the shapefile
Tarabashapefile_path <- file.path(dhsDir,"nigeria/nigeria_shapefiles/shapefiles/ShinyApp_shapefiles/all_reprioritization_nmep_states/STATES/Taraba/Taraba_State.shp")

# Read the shapefile
Taraba_gdf <- st_read(Tarabashapefile_path)

# Check the CRS
print(st_crs(Taraba_gdf))

# Transform CRS to EPSG 4326
Taraba_gdf <- st_transform(Taraba_gdf, crs = 4326)

# Check the transformed CRS
print(st_crs(Taraba_gdf))

# Combine polygons into a single geometry
Taraba_combined_polygon <- st_union(Taraba_gdf$geometry)

# Create a new GeoDataFrame (sf object) with the combined polygon
Taraba_combined_gdf <- st_sf(geometry = st_sfc(Taraba_combined_polygon), crs = st_crs(Taraba_gdf))

# Print the combined polygon geometry
print(Taraba_combined_polygon)

write.table(Taraba_combined_gdf, file.path(dhsDir,"nigeria/nigeria_shapefiles/shapefiles/ShinyApp_shapefiles/all_reprioritization_nmep_states/STATES/Taraba/Taraba_State_polygon.txt"))



# Niger combined polygon
library(sf)


# Define the path to the shapefile
Nigershapefile_path <- file.path(dhsDir,"nigeria/nigeria_shapefiles/shapefiles/ShinyApp_shapefiles/all_reprioritization_nmep_states/STATES/Niger/Niger_State.shp")

# Read the shapefile
Niger_gdf <- st_read(Nigershapefile_path)

# Check the CRS
print(st_crs(Niger_gdf))

# Transform CRS to EPSG 4326
Niger_gdf <- st_transform(Niger_gdf, crs = 4326)

# Check the transformed CRS
print(st_crs(Niger_gdf))

# Combine polygons into a single geometry
Niger_combined_polygon <- st_union(Niger_gdf$geometry)

# Create a new GeoDataFrame (sf object) with the combined polygon
Niger_combined_gdf <- st_sf(geometry = st_sfc(Niger_combined_polygon), crs = st_crs(Niger_gdf))

# Print the combined polygon geometry
print(Niger_combined_polygon)

write.table(Niger_combined_gdf, file.path(dhsDir,"nigeria/nigeria_shapefiles/shapefiles/ShinyApp_shapefiles/all_reprioritization_nmep_states/STATES/Niger/Niger_State_polygon.txt"))



# Yobe combined polygon
library(sf)


# Define the path to the shapefile
Yobeshapefile_path <- file.path(dhsDir,"nigeria/nigeria_shapefiles/shapefiles/ShinyApp_shapefiles/all_reprioritization_nmep_states/STATES/Yobe/Yobe_State.shp")

# Read the shapefile
Yobe_gdf <- st_read(Yobeshapefile_path)

# Check the CRS
print(st_crs(Yobe_gdf))

# Transform CRS to EPSG 4326
Yobe_gdf <- st_transform(Yobe_gdf, crs = 4326)

# Check the transformed CRS
print(st_crs(Yobe_gdf))

# Combine polygons into a single geometry
Yobe_combined_polygon <- st_union(Yobe_gdf$geometry)

# Create a new GeoDataFrame (sf object) with the combined polygon
Yobe_combined_gdf <- st_sf(geometry = st_sfc(Yobe_combined_polygon), crs = st_crs(Yobe_gdf))

# Print the combined polygon geometry
print(Yobe_combined_polygon)

write.table(Yobe_combined_gdf, file.path(dhsDir,"nigeria/nigeria_shapefiles/shapefiles/ShinyApp_shapefiles/all_reprioritization_nmep_states/STATES/Yobe/Yobe_State_polygon.txt"))





# Print all items in the list
for (i in seq_along(Katsina_combined_gdf$geometry)) {
  cat("Point", i, ":\n")
  print(Katsina_combined_gdf$geometry[[i]])
  cat("\n")
}



######damaturu###############
library(foot)
damaturu_fp <- read.csv(file.path(dhsDir,"nigeria/building_footprints/OpenBuildings", "Damaturu.csv"))
damaturu_fp <- st_as_sf(damaturu_fp, wkt = "geometry")
st_crs(damaturu_fp) <- 4326

damaturu_fp <- damaturu_fp %>%
  mutate(FID = row_number())

damaturu_stats_id <- calculate_footstats(damaturu_fp, #damaturu_shp of 66 wards
                                      zone = "WardName",
                                      what =list(
                                                 list("nndist")
                                      ),
                                      how =list(                                                
                                        list("cv", "nnindex", "iqr")
                                      ), #nndist
                                      controlZone = list(zoneName = "WardName",
                                                         method = "centroid"), verbose = TRUE)

damaturu_c <- cbind(damaturu_fp,damaturu_stats) %>%
  dplyr::select(- FID.1)

damaturu_joined <- st_join(damaturu_c, ShpYobe)

write.csv(damaturu_c, file.path(dhsDir,"nigeria/building_footprints", "Summary Metrics", "damaturu_nni_stats.csv"))



kano_city_fp <- read.csv(file.path(dhsDir,"nigeria/building_footprints/OpenBuildings", "Kano.csv"))
kano_city_fp <- st_as_sf(kano_city_fp, wkt = "geometry")
st_crs(kano_city_fp) <- 4326

kano_city_fp <- kano_city_fp %>%
  mutate(FID = row_number())


