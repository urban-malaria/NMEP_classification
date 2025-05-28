
source("/Users/grace/Desktop/UMP/NMEP_classification_my_fork/load_path.R", echo = T)
PackageDataDir <- file.path(DriveDir, "data/nigeria/R_package_data")

## =========================================================================================================================================
### Extractions
## =========================================================================================================================================

# read in state shapefiles
adamawa_shp <- st_read(file.path(PackageDataDir, "shapefiles/Adamawa/GeoPoDe_NGA_Geometry_20250519143931/boundary_ward_default/boundary_ward_default.shp"))
kwara_shp <- st_read(file.path(PackageDataDir, "shapefiles/Kwara/GeoPoDe_NGA_Geometry_20250519144714/boundary_ward_default/boundary_ward_default.shp"))
osun_shp <- st_read(file.path(PackageDataDir, "shapefiles/Osun/GeoPoDe_NGA_Geometry_20250519145007/boundary_ward_default/boundary_ward_default.shp"))

# save the three shapefiles for use in the r package
st_write(adamawa_shp, file.path(PackageDataDir, "shapefiles/Adamawa/Adamawa.shp"))
st_write(kwara_shp, file.path(PackageDataDir, "shapefiles/Kwara/Kwara.shp"))
st_write(osun_shp, file.path(PackageDataDir, "shapefiles/Osun/Osun.shp"))

# set paths to raster data
raster_paths <- list(
  evi_path = file.path(RastersDir, "Updated_Covariates", "2023-24_EVI_MOD13A1"),
  ndvi_path = file.path(RastersDir, "Updated_Covariates", "2023-24_NDVI_MOD13A1"),
  rainfall_path = file.path(RastersDir, "monthly rainfall 2023-24"),
  h2o_distance_path = file.path(RastersDir, "distance_to_water_bodies", "distance_to_water.tif"),
  elevation_path = file.path(RastersDir, "Elevation", "ELE.tif"),
  rh_2023 = file.path(RastersDir, "temp_humidity_2023_grib", "relative_humidity_2023.grib"),
  rh_2024 = file.path(RastersDir, "temp_humidity_2023_grib", "relative_humidity_2024.grib"),
  temp_2023 = file.path(RastersDir, "temp_humidity_2023_grib", "temperature_2023.grib"),
  temp_2024 = file.path(RastersDir, "temp_humidity_2023_grib", "temperature_2024.grib"),
  housing_quality_path = file.path(RastersDir, "housing",
                                   "2019_Nature_Africa_Housing_2015_NGA.tiff"),
  ndwi_path = file.path(RastersDir, "global_surface_water",
                        "Nigeria_NDWI_2023.tif"),
  ndmi_path = file.path(RastersDir, "global_surface_water",
                        "NDMI_Nigeria_2023.tif"),
  pfpr_path = file.path(PfDir, "pf_parasite_rate",
                        "202406_Global_Pf_Parasite_Rate_NGA_2022.tiff"),
  lights_path = file.path(RastersDir, "night_time_light_2023"),
  surface_soil_wetness_path = file.path(RastersDir, "surface_soil_wetness"),
  flood_path = file.path(RastersDir, "flooding/flooding_2023"),
  surface_h20_path = file.path(RastersDir, "global_surface_water"),
  settlement_block = file.path(RastersDir, "nigeria_settlement_classification", "blocks_V1.1",
                               "Nigeria_Blocks_V1.shp"),
  output_dir = file.path(DriveDir, "projects/urban_microstratification/NetSmartR/extractions")
)

# extract raster variables for each of the states
osun_extracted <- extract_raster_data("Osun", osun_shp, raster_paths)
kwara_extracted <- extract_raster_data("Kwara", kwara_shp, raster_paths)
adamawa_extracted <- extract_raster_data("Adamawa", adamawa_shp, raster_paths)

## =========================================================================================================================================
### TPR Data
## put three states' tpr data in the correct format for the R package
## =========================================================================================================================================

# read in tpr data
tpr <- readxl::read_excel(file.path(DataDir, "nigeria/nigeria_hmis/3_state_tpr.xlsx"))

# rename columns
tpr <- tpr %>% 
  rename(HF = `Health Faccility`,
         gen_attd = `General Attendance`,
         outpatient = `Out-patient Attendance`,
         inpatient = `Inpatient Admissions`,
         fever_u5 = `Persons with fever <5yrs`,
         fever_a5 = `Persons with fever  â‰¥5yrs (excl PW)`,
         fever_pw = `Persons with fever Preg Women (PW)`,
         rdt_u5 = `Persons presenting with fever & tested by RDT <5yrs`,
         rdt_pos_u5 = `Persons tested positive for malaria by RDT <5yrs`,
         rdt_a5 = `Persons presenting with fever & tested by RDT  â‰¥5yrs (excl PW)`,
         rdt_pos_a5 = `Persons tested positive for malaria by RDT  â‰¥5yrs (excl PW)`,
         rdt_pw = `Persons presenting with fever & tested by RDT Preg Women (PW)`,
         rdt_pos_pw = `Persons tested positive for malaria by RDT Preg Women (PW)`,
         micro_u5 = `Persons presenting with fever and tested by Microscopy <5yrs`,
         micro_pos_u5 = `Persons tested positive for malaria by Microscopy <5yrs`,
         micro_a5 = `Persons presenting with fever and tested by Microscopy  â‰¥5yrs (excl PW)`,
         micro_pos_a5 = `Persons tested positive for malaria by Microscopy  â‰¥5yrs (excl PW)`,
         micro_pw = `Persons presenting with fever and tested by Microscopy Preg Women (PW)`,
         micro_pos_pw = `Persons tested positive for malaria by Microscopy Preg Women (PW)`,
         deaths_u5 = `Deaths < 5 Malaria - U5`) %>% 
         mutate(National = str_replace_all(National, "ng", ""),
         State = str_remove(State, "^[a-zA-Z]{2}\\s+"),
         LGA = str_remove(LGA, "^[a-zA-Z]{2}\\s+"),
         LGA = str_replace_all(LGA, "Local Government Area", ""),
         Ward = str_remove(Ward, "^[a-zA-Z]{2}\\s+"),
         Ward = str_replace_all(Ward, "Ward", ""),
         HF = str_remove(HF, "^[a-zA-Z]{2}\\s+"))

tpr$Ward <- str_trim(tpr$Ward)

tpr <- tpr %>% 
  mutate(tested_u5 = (rdt_u5 + micro_u5),
                 tested_a5 = (rdt_a5 + micro_a5),
                 pos_u5 = (rdt_pos_u5 + micro_pos_u5),
                 pos_a5 = (rdt_pos_a5 + micro_pos_a5),
                 tested_pw = (rdt_pw + micro_pw),
                 pos_pw = (rdt_pos_pw + micro_pos_pw))

# filter by state
adamawa_tpr <- tpr %>% dplyr::filter(State == "Adamawa State")
kwara_tpr <- tpr %>% dplyr::filter(State == "Kwara State")
osun_tpr <- tpr %>% dplyr::filter(State == "Osun State")

# ADAMAWA: summarize and calculate u5 TPR by ward and LGA
adamawa_tpr_summary <- adamawa_tpr %>%
  group_by(Ward, LGA) %>%
  summarise(
    u5_tpr = sum(pos_u5, na.rm = TRUE) / sum(tested_u5, na.rm = TRUE),           # combined TPR
    u5_tpr_rdt = sum(rdt_pos_u5, na.rm = TRUE) / sum(rdt_u5, na.rm = TRUE),     # RDT TPR 
    u5_tpr_micro = sum(micro_pos_u5, na.rm = TRUE) / sum(micro_u5, na.rm = TRUE), # Microscopy TPR 
    u5_tpr2 = sum(pos_u5, na.rm = TRUE) / sum(fever_u5, na.rm = TRUE),           # TPR in U5 children with fever
    u5_tpr2_rdt = sum(rdt_pos_u5, na.rm = TRUE) / sum(fever_u5, na.rm = TRUE),  # 
    u5_tpr2_micro = sum(micro_pos_u5, na.rm = TRUE) / sum(fever_u5, na.rm = TRUE), 
    u5_tpr3 = sum(pos_u5, na.rm = TRUE) / sum(gen_attd, na.rm = TRUE),           # TPR U5 children in proportion to general hospital attendance
    u5_tpr3_rdt = sum(rdt_pos_u5, na.rm = TRUE) / sum(gen_attd, na.rm = TRUE),  
    u5_tpr3_micro = sum(micro_pos_u5, na.rm = TRUE) / sum(gen_attd, na.rm = TRUE) 
  ) %>% 
  ungroup() %>% 
  rename(WardName = Ward)

# KWARA: summarize and calculate u5 TPR by ward and LGA
kwara_tpr_summary <- kwara_tpr %>%
  group_by(Ward, LGA) %>%
  summarise(
    u5_tpr = sum(pos_u5, na.rm = TRUE) / sum(tested_u5, na.rm = TRUE),           # combined TPR
    u5_tpr_rdt = sum(rdt_pos_u5, na.rm = TRUE) / sum(rdt_u5, na.rm = TRUE),     # RDT TPR 
    u5_tpr_micro = sum(micro_pos_u5, na.rm = TRUE) / sum(micro_u5, na.rm = TRUE), # Microscopy TPR 
    u5_tpr2 = sum(pos_u5, na.rm = TRUE) / sum(fever_u5, na.rm = TRUE),           # TPR in U5 children with fever
    u5_tpr2_rdt = sum(rdt_pos_u5, na.rm = TRUE) / sum(fever_u5, na.rm = TRUE),  # 
    u5_tpr2_micro = sum(micro_pos_u5, na.rm = TRUE) / sum(fever_u5, na.rm = TRUE), 
    u5_tpr3 = sum(pos_u5, na.rm = TRUE) / sum(gen_attd, na.rm = TRUE),           # TPR U5 children in proportion to general hospital attendance
    u5_tpr3_rdt = sum(rdt_pos_u5, na.rm = TRUE) / sum(gen_attd, na.rm = TRUE),  
    u5_tpr3_micro = sum(micro_pos_u5, na.rm = TRUE) / sum(gen_attd, na.rm = TRUE) 
  ) %>% 
  ungroup() %>% 
  rename(WardName = Ward)

# OSUN: summarize and calculate u5 TPR by ward and LGA
osun_tpr_summary <- osun_tpr %>%
  group_by(Ward, LGA) %>%
  summarise(
    u5_tpr = sum(pos_u5, na.rm = TRUE) / sum(tested_u5, na.rm = TRUE),           # combined TPR
    u5_tpr_rdt = sum(rdt_pos_u5, na.rm = TRUE) / sum(rdt_u5, na.rm = TRUE),     # RDT TPR 
    u5_tpr_micro = sum(micro_pos_u5, na.rm = TRUE) / sum(micro_u5, na.rm = TRUE), # Microscopy TPR 
    u5_tpr2 = sum(pos_u5, na.rm = TRUE) / sum(fever_u5, na.rm = TRUE),           # TPR in U5 children with fever
    u5_tpr2_rdt = sum(rdt_pos_u5, na.rm = TRUE) / sum(fever_u5, na.rm = TRUE),  # 
    u5_tpr2_micro = sum(micro_pos_u5, na.rm = TRUE) / sum(fever_u5, na.rm = TRUE), 
    u5_tpr3 = sum(pos_u5, na.rm = TRUE) / sum(gen_attd, na.rm = TRUE),           # TPR U5 children in proportion to general hospital attendance
    u5_tpr3_rdt = sum(rdt_pos_u5, na.rm = TRUE) / sum(gen_attd, na.rm = TRUE),  
    u5_tpr3_micro = sum(micro_pos_u5, na.rm = TRUE) / sum(gen_attd, na.rm = TRUE) 
  ) %>% 
  ungroup() %>% 
  rename(WardName = Ward)

## =========================================================================================================================================
### Clean ward names in TPR data to match the extracted data
## =========================================================================================================================================

## -----------------------------------------------------------------------------------------------------------------------------------------
### Clean Adamawa
## -----------------------------------------------------------------------------------------------------------------------------------------

adamawa_extracted <- read.csv(file.path(PackageDataDir, "extractions/Adamawa_wards_variables.csv"))
adamawa_code_lookup <- adamawa_extracted %>% dplyr::select(WardName, WardCode)

# identify mismatches between tpr data and extracted data
tpr_unique <- unique(adamawa_tpr_summary$WardName)
extracted_unique <- unique(adamawa_extracted$WardName)
missing_in_extracted <- setdiff(tpr_unique, extracted_unique)
missing_in_tpr <- setdiff(extracted_unique, tpr_unique)
cat("Wards in TPR data but not in extracted data:\n")
print(missing_in_extracted)
cat("\nWards in extracted data but not in TPR data:\n")
print(missing_in_tpr)

# recode adamawa wards
adamawa_tpr_summary <- adamawa_tpr_summary %>%
  mutate(WardName = recode(WardName,
                       "Mayo-Nguli" = "Mayo Nguli",
                       "Nasarawo" = "Nassarawo (Mubi South LGA)",
                       "Nassarawo" = "Nassarawo (Yola North LGA)",
                       "Yelwa  (Mubi North)" = "Yelwa (Mubi North LGA)",
                       "Yelwa" = "Yelwa (Yola North LGA)",
                       "Dubwange" = "Dubange",
                       "Wambilim/Tilli" = "Wambilimi/Tili",
                       "Yelli" = "Yeli",
                       "Girei I" = "Girei 1",
                       "Girei II" = "Girei 2",
                       "Bazza/Margi" = "Bazza Margi",
                       "Wuro-Bokki" = "Wuro Bokki",
                       "Vih/Bokka" = "Vih/Boka",
                       "Bole-Yoldepate" = "Bole Yolde Pate",
                       "Bolki" = "Bwalki",
                       "Ribadu" = "Ribadu (Fufore LGA)",
                       "Ribadu  (Mayo-Belwa)" = "Ribadu (Mayo-Belwa LGA)",
                       "Uki-Tuki" = "Uki Tuki",
                       "Ga'anda" = "Gaanda",
                       "Nyibango" = "Nyibago",
                       "Hosheri-Zum" = "Hoserezum",
                       "Minkisi/Wurongiki" = "Ninkisi/Wuro Ngiki",
                       "Tumbarangabili" = "Tumbara/Ngabili",
                       "Mbulo" = "Mbullo",
                       "Futudou/Futuless" = "Futuless",
                       "Gabun" = "Gabon",
                       "Gamadio" = "Gamadiyo",
                       "Gangfada" = "Gang Fada",
                       "Gurum Pawo" = "Gurumpawo",
                       "Mayo-Kalaye" = "Mayo Kalaye",
                       "Garta/Ghumchi" = "Garta",
                       "Mayo-Lope" = "Mayo Lope",
                       "Gaya Sikalmi" = "Gaya-Sikalmi",
                       "Mayo-Ine" = "Mayo Inne",
                       "Mafarang" = "Mayo Farang",
                       "Purokayo" = "Purakayo",
                       "Gudu/Mboi" = "Gudu Mboi",
                       "Wuro-Dole" = "Wuro Dole",
                       "Sina/Kamale" = "Sina Kamale",
                       "Soaru A" = "Sorau A",
                       "Soaru B" = "Sorau B",
                       "Waltadi" = "Waltandi",
                       "Besto" = "Betso",
                       "Wulla" = "Wula",
                       "Ngbebongun" = "Ngbebogun",
                       "Lamurde  (Lamurde)" = "Lamurde",
                       "Lamurde  (Mubi South)" = "Lamorde",
                       "Mayo-Bani" = "Mayo Bani",
                       "Wagga  (Madagali)" = "Waga-Chakawa"
  ))

# add ward code to adamawa tpr data
adamawa_tpr_summary <- adamawa_tpr_summary %>% 
  left_join(adamawa_code_lookup, by = "WardName")

adamawa_tpr_summary <- adamawa_tpr_summary %>% 
  dplyr::select(WardCode, WardName, LGA, u5_tpr, u5_tpr_rdt, u5_tpr_micro, u5_tpr2, u5_tpr2_rdt, u5_tpr2_micro, u5_tpr3, u5_tpr3_rdt, u5_tpr3_micro)

# save to R package data folder
write.csv(adamawa_tpr_summary, file = file.path(PackageDataDir, "TPR/adamawatpr_updated.csv"), row.names = FALSE)

## -----------------------------------------------------------------------------------------------------------------------------------------
### Clean Kwara
## -----------------------------------------------------------------------------------------------------------------------------------------

kwara_extracted <- read.csv(file.path(PackageDataDir, "extractions/Kwara_wards_variables.csv"))
kwara_code_lookup <- kwara_extracted %>% dplyr::select(WardName, WardCode)

# identify mismatches between tpr data and extracted data
tpr_unique <- unique(kwara_tpr_summary$WardName)
extracted_unique <- unique(kwara_extracted$WardName)
missing_in_extracted <- setdiff(tpr_unique, extracted_unique)
missing_in_tpr <- setdiff(extracted_unique, tpr_unique)
cat("Wards in TPR data but not in extracted data:\n")
print(missing_in_extracted)
cat("\nWards in extracted data but not in TPR data:\n")
print(missing_in_tpr)

kwara_tpr_summary <- kwara_tpr_summary %>%
  mutate(WardName = recode(WardName,
                       "Aboto-Odo-Ode" = "Aboto/Odoode",
                       "Agbeyangi/Abadamu" = "Agbeyangi/Gbadamu",
                       "Agbona/Elebue Fata" = "Agbona/Fata",
                       "Agunjin" = "Agwijin",
                       "Ajannaku" = "Ajanaku/Malete",
                       "Ajasse 1" = "Ajase 1",
                       "Ajasse 2" = "Ajase 2",
                       "Alapa/Onire/Odegiwa" = "Onire/Odegiwa",
                       "Balogun Fulani  3" = "Balogun Fulani 3",
                       "Banni" = "Bani",
                       "Bode/Babane" = "Bode Babane",
                       "Budo Egba" = "Budo-Egba",
                       "Efue/Berikodi" = "Efue/Berikodo",
                       "Ejidongari" = "Egidogari",
                       "Erin Ile North" = "Erin-North",
                       "Erin Ile South" = "Erin-South",
                       "Esie/Ijan" = "Esie-Ijan",
                       "Gambari/Ayekale" = "Gambar/Ayekale",
                       "Idofian   2" = "Idofian 2",
                       "Idofian  1" = "Idofian 1",
                       "Idofin Odo Ashe" = "Idofin Odoashe",
                       "Igbaja   1" = "Igbaja 1",
                       "Igbaja  2" = "Igbaja 2",
                       "Igbaja  3" = "Igbaja 3",
                       "Igbana 2" = "Idofin/Igbana 2",
                       "Igbo-Idun" = "Igboidun",
                       "Igbonna" = "Igbomma",
                       "Ijara Isin" = "Ijara",
                       "Ikotun" = "Ikotun-Kwara",
                       "Ila-Oja" = "Ila- Oja",
                       "Ilale/Erinmope/Imoji" = "Ilale /Erin/Imoji",
                       "Ile-Ere" = "Ile-Ire",
                       "Ilemona" = "Ilemoma",
                       "Imode/Egosi" = "Imade/Egosi",
                       "Irra" = "Ira",
                       "Isalu-Isin  2" = "Isanlu 2",
                       "Isanlu-Isin  1" = "Isanlu 1",
                       "Kemanji" = "Kemaji",
                       "Kenu/Taberu" = "Kenu/Tabera",
                       "Kpura/Yakira" = "Kpura/Yakiru",
                       "Lafiagi  1" = "Lafiagi 1",
                       "Magaji Geri" = "M/Ngeri",
                       "Marafa/Pepete" = "Marafa/Pepele",
                       "Maya/Ile Apa" = "Maya/Ileapa",
                       "Megida" = "Magida",
                       "Obbo Aiyegunle 1" = "Obbo-Aiyegunle 1",
                       "Obbo Ile/Ora" = "Obbo-Ile",
                       "Odo-owa  1" = "Odo-Owa 1",
                       "Ojomu Central A" = "Ojomo Central A",
                       "Ojuekun/Zarumi" = "Oju-Ekun",
                       "Oke Ode  1" = "Oke-Ode 1",
                       "Oke Ode  3" = "Oke-Ode 3",
                       "Oke Ogun" = "Oka-Ogun",
                       "Oke Oyi/Oke Ose" = "Oke Oyi/Oke-Ose/Alalubosa",
                       "Oke-Ode  2" = "Oke-Ode 2",
                       "Okeso/Afeyin" = "Okeso",
                       "Okeweru/Yowere II" = "Okeweru/Yow 2",
                       "Oko Erin" = "Oko-Erin",
                       "Omupo 1" = "Omupo",
                       "Oro Ago" = "Oro-Ago",
                       "Osi  I" = "Osi 1",
                       "Osi  II" = "Osi 2",
                       "Otte/Balla" = "Otte/Ballah",
                       "Owu-Isin" = "Owu",
                       "Pakunmon" = "Pakuma",
                       "Patigi 1" = "Pategi 1",
                       "Patigi 3" = "Pategi 3",
                       "Patigi 4" = "Pategi Iv",
                       "Sabaja/Pamo/Oba" = "Sabaja/Pama/Oba",
                       "Share  IV" = "Share Iv",
                       "Share  V" = "Share 5",
                       "Shinau/Tumbuyan" = "Sinawu/Tumbiya",
                       "Shonga 1" = "Tsonga 1",
                       "Shonga 2" = "Tsonga 2",
                       "Shonga 3" = "Tsonga 3",
                       "Sosoki/Yowere I" = "Sosoki/Yowere",
                       "Wumi Ayaki" = "Womi Ayaki",
                       "Yashikira" = "Yashikira 1",
                       "Zango" = "Zango 1",
                       "Gure/Giwasoro" = "Yashikira 2"
  ))

# add ward code to adamawa tpr data
kwara_tpr_summary <- kwara_tpr_summary %>% 
  left_join(kwara_code_lookup, by = "WardName")

kwara_tpr_summary <- kwara_tpr_summary %>% 
  dplyr::select(WardCode, WardName, LGA, u5_tpr, u5_tpr_rdt, u5_tpr_micro, u5_tpr2, u5_tpr2_rdt, u5_tpr2_micro, u5_tpr3, u5_tpr3_rdt, u5_tpr3_micro)

# save to R package data folder
write.csv(kwara_tpr_summary, file = file.path(PackageDataDir, "TPR/kwaratpr_updated.csv"), row.names = FALSE)

## -----------------------------------------------------------------------------------------------------------------------------------------
### Clean Osun
## -----------------------------------------------------------------------------------------------------------------------------------------

osun_extracted <- read.csv(file.path(PackageDataDir, "extractions/Osun_wards_variables.csv"))
osun_code_lookup <- osun_extracted %>% dplyr::select(WardName, WardCode)

# identify mismatches between tpr data and extracted data
tpr_unique <- unique(osun_tpr_summary$WardName)
extracted_unique <- unique(osun_extracted$WardName)
missing_in_extracted <- setdiff(tpr_unique, extracted_unique)
missing_in_tpr <- setdiff(extracted_unique, tpr_unique)
cat("Wards in TPR data but not in extracted data:\n")
print(missing_in_extracted)
cat("\nWards in extracted data but not in TPR data:\n")
print(missing_in_tpr)

osun_tpr_summary <- osun_tpr_summary %>%
  mutate(WardName = recode(WardName,
      "1 - Telemu" = "Telemu",
      "Ilaji  7" = "Ilaji",
      "Lagere" = "Ijugbe/Amola",
      "Igbaye/Imuleke" = "Igbayi/Emuleke",
      "10 - Asa/Ajagunlase" = "Asa/Ajagunlase",
      "11 -Araromi" = "Araromi/Owu",
      "2 - Ile-Mowu/Asamu" = "Ilemowu/Asamu",
      "2 - Otun Balogun" = "Otun Balogun",
      "3 - Ogbaagba I" = "Ogbagba 1",
      "4 - Ogbaagba II" = "Ogbagba 2",
      "5 - Ikire-Ile/Iwara" = "Ikire-Ile/Iwara",
      "6 - Ikonifin Isero" = "Ikonifin/Isero",
      "7 - Ile-Ogo/Obamoro" = "Ile Ogo/Obamoro",
      "8 - Bode Osi" = "Bode-Osi",
      "9 - Ajagba/Iwo Oke" = "Ajagba/Iwo-Oke",
      "Aagba 5" = "Aagba",
      "Adeo/3" = "Adeo/Bara B",
      "Adeti (8)" = "Adeti",
      "Afolu/7" = "Afolu/Elerin A",
      "Alapomu II" = "Alapomu 2",
      "Alekuwodu" = "Alekuwodo",
      "Aludundun" = "Aludundun/Owode Ikirun",
      "Amobi 9" = "Amobi",
      "Anaye" = "Anaye/Iloro/Roye",
      "Anlugbua" = "Obalufon/Anlugbua",
      "Anwo/2" = "Anwo/Elerin B",
      "Ara 1" = "Ara I",
      "Ara 2" = "Ara 2",
      "Aragan (6)" = "Aragan",
      "Aromiwe/4" = "Aromiwe/Olobu D",
      "Arowojobe" = "Arowojobe/Arowojobe",
      "Asaobi/8" = "Isaobi",
      "Asaoni" = "Asa-Oni",
      "Ashi/Asaba" = "Asi/Asaba",
      "Asipa/Akin" = "Asipa/Akinlalu",
      "Asunmo (10)" = "Asunmo",
      "Ataoja A" = "Ataoja 1/A",
      "Ataoja C" = "Ataoja 3/C",
      "Ataoja D" = "Ataoja Iv",
      "Awooro" = "Aworo",
      "Ayesan" = "Ogbon Ayesan",
      "Baale" = "Baale Okuku",
      "Baara Ejemu" = "Bara-Ejemu",
      "Baba Kekere" = "Babakekere",
      "Babanla Gate" = "Babanla/Agate",
      "Buari isibo" = "Buari Isibo",
      "Cooperative /9" = "Co-Op",
      "Edun 1" = "Edunabon I",
      "Edun 2" = "Edunabon 2",
      "Eegan Aaje" = "Ega-Aaje/Osun-Eesa",
      "Egbe-idi (2)" = "Egbe-Idi",
      "Eko-Ende/Eko-Ajala" = "Eko Ende/Eko Ajala",
      "Ereja (9)" = "Ereja",
      "Erin-Ijesa" = "Erin Jesa",
      "Erinmo" = "Erinmo Jesa",
      "Eripa 7" = "Eripa",
      "Esa Odo/9" = "Esa Odo",
      "Esa oke/7" = "Esa Oke",
      "Eyngbo/8" = "Eyingbo/Olobu C",
      "Faaji/Opete" = "Faji/Opete",
      "Fiditi" = "Fiditi/Ikire J",
      "Gbogbo 4" = "Gbogbo/Ileogbo IV",
      "Gbongan Rural" = "Gbogon Rural",
      "Iba II" = "Iba 2/II",
      "Ibala/5" = "Ibala/Eesun",
      "Ibodi/4" = "Ibodi",
      "Ibokun/1" = "Ibokun",
      "Idi-Ape 2" = "Idi Ape",
      "Idi-ogun Ileogbo 3" = "Idi-Ogun",
      "Ido-Osun" = "Ido Osun",
      "Ifelodun/5" = "Ifelodun",
      "Ifeodan" = "Ife-Odan",
      "Ifewara 1/10" = "Ifewara 1",
      "Ifewara 2/11" = "Ifewara 2",
      "Igbajo 1 4" = "Igbajo 1/Gbeleru Obaala I",
      "Igbajo 2 5" = "Igbajo 2",
      "Igbajo 3 6" = "Igbajo 3",
      "Igbogi (3)" = "Igbogi",
      "Ijebu-Ijesa" = "Ijebu Jesa",
      "Ijeda/Iloko" = "Iloko Ijeda",
      "Ijimoba" = "Ijimoba/Inisa I/Aato/Igbon",
      "Ijugbe" = "Ijegbe/Oke-Eso/Oke-Owu Ijugbe",
      "Ikija 1" = "Ikija 1",
      "Ilahun/Ikinyinwa 3" = "Ilahun/Ikinyinwa",
      "Ilaje (7)" = "Ilaje",
      "Ilaji 7" = "Ilaji",
      "Ilare 4" = "Ilare 4",
      "Ilare 1" = "Ilare I",
      "Ilare/10" = "Ilare",
      "Ilase/4" = "Ilase",
      "Iperi/Eyindi" = "Iperin/Eyindi",
      "Ipetu 1" = "Ipetu Ijesa I",
      "Ipetu 2" = "Ipetu Ijesa 2",
      "Ipetu Ile/2" = "Ipetu Ile",
      "Ira Ikeji" = "Ikeji Ira",
      "Iremo 5" = "Iremo 5",
      "Iremo 1" = "Iremo I",
      "Iremo 2" = "Iremo 2/Iremo II Eleyele",
      "Iresi 1 9" = "Iresi 1",
      "Iresi 2 10" = "Iresi 2",
      "Irojo" = "Irojo/Ilerin",
      "Isale Asa 9" = "Isale Asa",
      "Isale Ikirun" = "Isale Ikirun/Isale/Oke Afo",
      "Isale Offa 2" = "College/Egbada Road/Isale Offa",
      "Isale Oyo 4" = "Isale Oyo",
      "Isale-Oba 1" = "Isale Oba I",
      "Isale-Oba 2" = "Isale Oba 2",
      "Isale-Oba 3" = "Isale Oba 3",
      "Isale-Oba 4" = "Isale Oba Iv",
      "Isedo" = "Isedo 1",
      "Iso Ege 7" = "Iso-Ege/Ada I",
      "Isokun (5)" = "Isokun",
      "Ita-Ofa (4)" = "Ita-Ofa/Omofe/Idasa",
      "Itagunmodi/6" = "Itagunmodi",
      "Itisin/Ilemo" = "Itisin",
      "Iwoye" = "Iwoye Jesa",
      "Iwoye/Ekuro/Idoo" = "Ekuro/Idoo",
      "Jagun A" = "Jagun 1",
      "Jagun B" = "Jagun 2/B",
      "JagunJagun" = "Jagun/Jagun",
      "Jagunosi" = "Jagun Osi Baale",
      "Konda/5" = "Konda/Bara A",
      "Moore" = "More",
      "Moore Ojaja" = "Moreojaja",
      "Muroko/9" = "Muroko",
      "Oba Ojomu" = "Oba/Ojomo",
      "Oba- Oke" = "Oba-Oke",
      "Obale" = "Obaale",
      "Odeomu Rural" = "Ode Omu Rural",
      "Odogbo" = "Odogbo/Ayegunle",
      "Ojaosun" = "Oja-Osun",
      "Ojomu" = "Oba/Ojomo",
      "Oke Afo" = "Oke-Afo",
      "Oke Amola" = "Oke Amola/Amola Ikirun",
      "Oke Aree 11" = "Oke-Aree",
      "Oke Baale 8" = "Oke-Baale",
      "Oke Balogun" = "Okebalogun/Elejigbo 'D'/Ejemu",
      "Oke Irun 8" = "Oke-Run",
      "Oke Ode 2" = "Oke Ode",
      "Oke Omi 1" = "Oke Omi",
      "Oke Otan 3" = "Oke-Otan",
      "Oke-Adan 1" = "Oke Adan 1",
      "Oke-Adan 2" = "Oke Adan 2",
      "Oke-Adan 3" = "Oke Adan 3",
      "Oke-Eran/6" = "Oke Eran/Elerin E",
      "Oke-Iroko 1" = "Oke-Iroko/Eesa Ikirun",
      "Oke-Iyin (10)" = "Oke Iyin",
      "Oke-Oba 1" = "Oke Oba I",
      "Oke-Oba 2" = "Oke Oba 2",
      "Oke-Opo" = "Oke-Opo/Isare",
      "Oke-Osun 10" = "Oke Osun",
      "Oke-Sa" = "Okesa",
      "Okebode/7" = "Okebode",
      "Okeogi 10" = "Oke Ogi/Ada II",
      "Okerewe 1" = "Okerewe I",
      "Okiti/Molufon" = "Okiti/Molofun/Olufon Orolu 'J'",
      "Olla" = "Ola",
      "Oloba-atapara" = "Oloba/Atapara",
      "Olobu/1" = "Olobu/Olobu A",
      "Ologun-agbaakin" = "Ologun/Agbaakin",
      "Olonde" = "Olonde/Olonde Ikirun",
      "Olorunsogo/Ofatedo/Okini" = "Olorunsogo/Offatedo",
      "Oloti 1" = "Oloti",
      "Oloyan/Elemoso" = "Olonyan",
      "Olukotun" = "Olukotun Inisha",
      "Olunisa" = "Oluinisa",
      "Oosa 1" = "Oosa",
      "Oosaadifa" = "Oosa Adifa",
      "Oosi" = "Osi",
      "Ooye" = "Ooye/Olufon Orolu 'E'",
      "Orisunbare" = "Orisunmbare/Balogun",
      "Oritasabo" = "Orita Sabo/Owode  II",
      "Ororuwo 6" = "Ororuwo",
      "Osolo/Oparin" = "Osolo/Oparimo",
      "Osu/1" = "Osu I",
      "Osu/2" = "Osu 2",
      "Osu/3" = "Osu 3",
      "Osun Eesa" = "Ega-Aaje/Osun-Eesa",
      "Ota Efun" = "Ota-Efun",
      "Otan Ile/8" = "Otan Ile",
      "Oyere 1" = "Oyere I",
      "Sabo" = "Sabo/Owoope",
      "Sabo agbongbe 1" = "Sabo Agbengbe 1",
      "Sabo-agbongbe 2" = "Sabo Agbengbe 2",
      "Sagba-abogunde" = "Sagba/Abogunde",
      "Seriki/9" = "Seriki/Tokede/Elerin C",
      "Tokede/11" = "Seriki/Tokede/Elerin C",
      "Wasimi" = "Wasinmi",
      "imesi Ile/6" = "Imesi Ile",
      "Aagba  5" = "Aagba",
      "Aare" = "Ogbon Aare",
      "Ara  1" = "Ara I",
      "Ara  2" = "Ara 2",
      "Araromi  3" = "Araromi/Olufon Orolu 'C'",
      "Eesa Otun" = "Esa/Otunbale",
      "Eleesi" = "Elesi/Olufon Orolu 'G'",
      "Eripa  7" = "Eripa",
      "Gbogbo  4" = "Gbogbo/Ileogbo  IV",
      "Idi-ogun Ileogbo  3" = "Idi-Ogun",
      "Igbajo 1  4" = "Igbajo 1/Gbeleru Obaala I",
      "Igbajo 2  5" = "Igbajo 2",
      "Igbajo 3  6" = "Igbajo 3",
      "Ijabe/Ilaodo" = "Ilaodo/Ijabe",
      "Ikija  1" = "Ikija 1",
      "Ilare  4" = "Ilare 4",
      "Iremo  5" = "Iremo 5",
      "Iresi 1  9" = "Iresi 1",
      "Iresi 2  10" = "Iresi 2",
      "Isale Asa  9" = "Isale Asa",
      "Isale Offa  2" = "College/Egbada Road/Isale Offa",
      "Isale Oyo  4" = "Isale Oyo",
      "Isale-Oba 8" = "Isale Oba",
      "Iso Ege  7" = "Iso-Ege/Ada I",
      "Itaakogun -  1" = "Itakogun",
      "Oke Aree  11" = "Oke-Aree",
      "Oke Baale  8" = "Oke-Baale",
      "Oke Irun  8" = "Oke-Run",
      "Oke Ode  2" = "Oke Ode",
      "Oke Omi  1" = "Oke Omi",
      "Oke Otan  3" = "Oke-Otan",
      "Oke-Iroko  1" = "Oke-Iroko/Eesa Ikirun",
      "Oke-Osun  10" = "Oke Osun",
      "Okeada" = "Oke Ada",
      "Okeba" = "Eweta/Okeba Ikirun",
      "Okeogi  10" = "Oke Ogi/Ada II",
      "Oloti  1" = "Oloti",
      "Orita Sabo/Owode II" = "Orita Sabo/Owode  II",
      "Ororuwo  6" = "Ororuwo",
      "Sokoto" = "Sokoto/Forest Reserve  II",
      "Omisore 6" = "Omisore/Ileogbo  II"))
      
osun_tpr_summary <- osun_tpr_summary %>%
  dplyr::filter(!(WardName == "Balogun" & LGA == "Ayedaade "))

osun_tpr_summary <- osun_tpr_summary %>%
  mutate(
    WardName = str_trim(WardName),
    LGA = str_trim(LGA)
  ) %>%
  mutate(
    WardName = case_when(
      WardName == "Popo" & LGA == "Ejigbo" ~ "Popo (Ejigbo LGA)",
      WardName == "Popo" & LGA == "Osogbo" ~ "Popo/Otun Jagun 'A'",
      WardName == "Popo 5" & LGA == "Ayedire" ~ "Popo (Ayedire LGA)",
      WardName == "Otun Balogun" & LGA == "Ayedaade" ~ "Balogun (Ayedade LGA)",
      WardName == "Balogun" & LGA == "Ifedayo" ~ "Balogun (Ifedayo LGA)",
      WardName == "Balogun/10" & LGA == "Irepodun" ~ "Balogun Jagba/Elerin D",
      WardName == "Araromi" & LGA == "Orolu" ~ "Araromi/Olufon Orolu 'C'",
      TRUE ~ WardName
    )
  )

# add ward code to adamawa tpr data
osun_tpr_summary <- osun_tpr_summary %>% 
  left_join(osun_code_lookup, by = "WardName")

osun_tpr_summary <- osun_tpr_summary %>% 
  dplyr::select(WardCode, WardName, LGA, u5_tpr, u5_tpr_rdt, u5_tpr_micro, u5_tpr2, u5_tpr2_rdt, u5_tpr2_micro, u5_tpr3, u5_tpr3_rdt, u5_tpr3_micro)

# save to R package data folder
write.csv(osun_tpr_summary, file = file.path(PackageDataDir, "TPR/osuntpr_updated.csv"), row.names = FALSE)


## =========================================================================================================================================
### Map TPR calculation methods
## =========================================================================================================================================

methods <- c("u5_tpr", "u5_tpr_rdt", "u5_tpr_micro", 
             "u5_tpr2", "u5_tpr2_rdt", "u5_tpr2_micro", 
             "u5_tpr3", "u5_tpr3_rdt", "u5_tpr3_micro")

# state shapefiles
adamawa_shp <- st_read(file.path(PackageDataDir, "shapefiles/Adamawa/Adamawa.shp"))
kwara_shp <- st_read(file.path(PackageDataDir, "shapefiles/Kwara/Kwara.shp"))
osun_shp <- st_read(file.path(PackageDataDir, "shapefiles/Osun/Osun.shp"))

map_tpr <- function(state_shapefile, state_tpr_data, methods) {
  
  # join shapefile and data
  tpr_shp <- state_shapefile %>%
    left_join(state_tpr_data, by = "WardName")
  
  # create a list to store plots
  plot_list <- list()
  
  # loop over each method
  for (method in methods) {
    # convert method name to character (if it's a symbol or object)
    method_name <- as.character(substitute(method))
    if (inherits(method, "name")) method <- as.character(method)
    
    # make the map
    p <- ggplot(tpr_shp) +
      geom_sf(aes_string(fill = method)) +
      scale_fill_gradientn(
        colors = c("grey80", "pink", "darkred"),  # grey for 0, pink to red
        values = scales::rescale(c(0, 0.01, 1)),  # emphasize grey near 0
        na.value = "grey80",
        limits = c(0, NA),  # force 0 to appear in scale
      ) +
      labs(title = paste("TPR:", method), fill = "TPR") +
      theme_manuscript()
    
    # add to list
    plot_list[[method]] <- p
  }
  
  return(plot_list)
}

map_tpr_include_zero <- function(state_shapefile, state_tpr_data, methods) {
  
  # join shapefile and data
  tpr_shp <- state_shapefile %>%
    left_join(state_tpr_data, by = "WardName")
  
  # create a list to store plots
  plot_list <- list()
  
  # loop over each method
  for (method in methods) {
    # convert method name to character (if it's a symbol or object)
    method_name <- as.character(substitute(method))
    if (inherits(method, "name")) method <- as.character(method)
    
    # make the map
    p <- ggplot(tpr_shp) +
      geom_sf(aes_string(fill = method)) +
      scale_fill_gradientn(
        colors = c("pink", "darkred"),  # grey for 0, pink to red
        na.value = "grey80",
      ) +
      labs(title = paste("TPR:", method), fill = "TPR") +
      theme_manuscript()
    
    # add to list
    plot_list[[method]] <- p
  }
  
  return(plot_list)
}

adamawa_maps <- map_tpr(adamawa_shp, adamawa_tpr_summary, methods)
kwara_maps <- map_tpr(kwara_shp, kwara_tpr_summary, methods)
osun_maps <- map_tpr(osun_shp, osun_tpr_summary, methods)
adamawa_maps_include_zero <- map_tpr_include_zero(adamawa_shp, adamawa_tpr_summary, methods)
kwara_maps_include_zero <- map_tpr_include_zero(kwara_shp, kwara_tpr_summary, methods)
osun_maps_include_zero <- map_tpr_include_zero(osun_shp, osun_tpr_summary, methods)

library(patchwork)
FigDir <-  file.path("/Users/grace/Urban Malaria Proj Dropbox/urban_malaria/projects/urban_microstratification/NetSmartR/tpr_plots")

adamawa_grid <- wrap_plots(adamawa_maps, ncol = 3)
kwara_grid <- wrap_plots(kwara_maps, ncol = 3)
osun_grid <- wrap_plots(osun_maps, ncol = 3)
adamawa_grid_include_zero <- wrap_plots(adamawa_maps_include_zero, ncol = 3)
kwara_grid_include_zero <- wrap_plots(kwara_maps_include_zero, ncol = 3)
osun_grid_include_zero <- wrap_plots(osun_maps_include_zero, ncol = 3)

ggsave(file.path(FigDir, "Adamawa_TPR_Maps.pdf"), adamawa_grid, width = 14, height = 10)
ggsave(file.path(FigDir, "Kwara_TPR_Maps.pdf"), kwara_grid, width = 14, height = 10)
ggsave(file.path(FigDir, "Osun_TPR_Maps.pdf"), osun_grid, width = 14, height = 10)
ggsave(file.path(FigDir, "Adamawa_TPR_Maps_include_zero.pdf"), adamawa_grid_include_zero, width = 14, height = 10)
ggsave(file.path(FigDir, "Kwara_TPR_Maps_include_zero.pdf"), kwara_grid_include_zero, width = 14, height = 10)
ggsave(file.path(FigDir, "Osun_TPR_Maps_include_zero.pdf"), osun_grid_include_zero, width = 14, height = 10)



