### Explore new HMIS data
rm(list=ls())

source("~/NMEP_classification/load_path.R", echo = T)

library(readxl)

hims <- read_excel(file.path(DataDir, "nigeria/nigeria_hmis", 
                          "1. National Raw Data NMEP Malaria Data_2023_2022_10062024.xlsx"), sheet = 2)

head(hims)

hims_delta <- hims %>% 
  filter(State == "de Delta State")

head(hims_delta)

hims_delta_clean <- hims_delta %>% 
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
         malaria_u5 = `Persons with clinically diagnosed Malaria <5yrs`,
         malaria_a5 = `Persons with clinically diagnosed Malaria  â‰¥5yrs (excl PW)`,
         malaria_pw = `Persons with clinically diagnosed Malaria Preg Women (PW)`) %>%
  mutate(National = str_replace_all(National, "ng", ""),
  State = str_replace_all(State, "de", ""),
  LGA = str_replace_all(LGA, "de ", ""),
  LGA = str_replace_all(LGA, "Local Government Area", ""),
  Ward = str_replace_all(Ward, "de ", ""),
  Ward = str_replace_all(Ward, "Ward", ""),
  HF = str_replace_all(HF, "de", ""))

hims_delta_clean$Ward <- str_trim(hims_delta_clean$Ward)

hims_delta_clean <- hims_delta_clean %>% 
  mutate(tested_u5 = (rdt_u5 + micro_u5),
         pos_u5 = (rdt_pos_u5 + micro_pos_u5),
         tested_a5 = (rdt_a5 + micro_a5),
         pos_a5 = (rdt_pos_a5 + micro_pos_a5),
         tested_pw = (rdt_pw + micro_pw),
         pos_pw = (rdt_pos_pw + micro_pos_pw)) %>% 
  mutate(Ward= case_when(
    Ward == "Idumuje" ~ "Idumuje-Unor",
    Ward == "Issele-Nkpitime" ~ "Issele-Mkpitime",
    Ward == "Onicha-Olone" ~ "Onicha-Olona",
    Ward == "Agidiase" ~ "Agiadiasi",
    Ward == "Ewulu/Ishagu" ~ "Ewulu/Isheagu",
    Ward == "Ekaametagbene/Kalafio" ~"Kolafiogbene/Ekametagbene",
    Ward == "Ugbeinma/Okoloba" ~ "Ogbeinma/Okoloba",
    Ward == "Bolou Ndoro" ~ "Bolou-Ndoro",
    Ward == "Eku" ~ "Eku/Agbon VI",
    Ward == "Igun" ~ "Igun/Agbon V",
    Ward == "Isiokolo" ~ "Isiokolo/Agbon VIII",
    Ward == "Okpara" ~ "Okpara/Agbon  I",
    Ward == "Orhroakpo" ~ "Orhaorpo/Agbon  IV",
    Ward == "Oria" ~ "Oria/Abraka  III",
    Ward == "Ovu" ~ "Ovu/Agbon  II",
    Ward == "Urhuvie" ~ "Urhuovie/Abraka  II",
    Ward == "Ogharefe 1" ~ "Ogharefe 1/Oghara I",
    Ward == "Ogharefe 2" ~ "Ogharefe 2/Oghara II",
    Ward == "Ogharefe 3" ~ "Ogharefe 3/Oghara III",
    Ward == "Boji-Boji 1" ~ "Boji-Boji Owa 1/Owa III",
    Ward == "Owa-Alero" ~ "Owa-Alero/Owa II",
    Ward == "Owa-Alizomor" ~ "Owa-Alizomor/Owa  VI",
    Ward == "Umune" ~ "Umunede",
    Ward == "Abavo" ~ "Abavo Central",
    Ward == "Agbor Nta" ~ "Agbor-Nta/Agbor Town I",
    Ward == "Agbor Obi" ~ "Agbor-Obi 1/Agbor Town II",
    Ward == "Boji-Boji 1 Agbor" ~ "Boji-Boji I/ Agbor 7",
    Ward == "Boji-Boji 2 Agbor" ~ "Boji-Boji II/ Agbor 8",
    Ward == "Oyoko" ~ "Oyoko/Abavo I",
    Ward == "Ozonogogo ward" ~ "Ozanogogo/Ihuozomor (Ozanogogo Alisimie)",
    Ward == "Udomi/Azuowa" ~ "Udomi-Azuowa/Abavo II",
    Ward == "Iye1" ~ "Iyede 1",
    Ward == "Iye2" ~ "Iyede 2",
    Ward == "Oye" ~ "Oyede",
    Ward == "Aviara" ~ "Avsiara",
    Ward == "Eme" ~ "Emede",
    Ward == "Igbi" ~ "Igbide",
    Ward == "Azagba/Ibedeni" ~ "Ibedeni/Azagba",
    Ward == "Beneku/Utchi/Okpai" ~ "Okpi/Utchi/Beneku",
    Ward == "Onyah/Umuolu" ~ "Umuolu/Onya",
    Ward == "Onicha Okwuani" ~ "Onitcha Ukwuani",
    Ward == "Utagba-Uno" ~ "Utagba Uno",
    Ward == "Adagbrassa" ~ "Adagbarassa",
    Ward == "Evwrigen" ~ "Evwriyen",
    Ward == "Okwabu" ~ "Okwabude",
    Ward == "Oviri Okpe" ~ "Oviri-Okpe",
    Ward == "Achala/Ezukwu/Ogboli" ~ "Ezukwu/Ogboli/Achalla",
    Ward == "Akwukwu/Atuma" ~ "Atuma Akwukwu-Igbo",
    Ward == "Ogbeowele" ~ "Ogbowele",
    Ward == "Ukala/Aninwalo" ~ "Ukalla/Anwalo",
    Ward == "Akuebulu" ~ "Akuebolu",
    Ward == "Cable Point ward" ~ "Cable Point",
    Ward == "Oko-Amakom" ~ "Oko Anala/Amakom",
    Ward == "Agaloma" ~ "Agoloma",
    Ward == "Bulu Angiama" ~ "Buluangiama",
    Ward == "Turu Angiama" ~ "Toruangiama",
    Ward == "Ugborhen" ~ "Ugborhen/Okokporo",
    Ward == "Egini" ~ "Egini/Ovwian II",
    Ward == "Orhuruworun" ~ "Orhuwhorun",
    Ward == "Otor-Udu" ~ "Otor Udu/Udu I",
    Ward == "Owhro" ~ "Owhrode/Udu II",
    Ward == "Afiesere" ~ "Afiesere/Ereumukohwaren",
    Ward == "Agbarha" ~ "Agbarha-Otor",
    Ward == "Agbarho2" ~ "Agbarho 2",
    Ward == "Otor-Ogo" ~ "Otor-Ogor",
    Ward == "Olumo 1" ~ "Olomu 1",
    Ward == "Olumo 2" ~ "Olomu 2",
    Ward == "Olumo 3" ~ "Olomu 3-Effurun-Otor",
    Ward == "Ogbe Obiaruku" ~ "Ogbeobiaruku",
    Ward == "Okuzu" ~ "Okuzu/Obiaruku  II",
    Ward == "Ugboreke" ~ "Ugboroke",
    Ward == "Urhumarho" ~ "Urhumarhu",
    Ward == "Abigborode" ~ "Abigborodo",
    Ward == "Itsekelewu" ~ "Tsekelewu",
    Ward == "Ogbudugbudu" ~ "Okbudugbudu",
    Ward == "Okere ward" ~ "Okere",
    Ward == "Ogidingbe" ~ "Ogidigben",
    Ward == "Okererenkoko" ~ "Okerenkoko",
    Ward == "Abraka" ~ "Abraka I",
    Ward == "Afiesere" ~ "Afiesere/Ereumukohwaren",
    # Ward == "Old" ~ "New",
    TRUE ~ Ward
  ))


###
facilities <- hims_delta_clean %>%
  dplyr::select(HF, ownership, Ward) %>%
  distinct() %>% 
  group_by(ownership) %>%
  summarise(facility_count = n())


ggplot(facilities, aes(x = ownership, y = facility_count)) +
  geom_bar(stat = "identity") +
  theme_manuscript() +
  labs(title = "Health Facilities in Delta State",
       x = "Ownership",
       y = "Count")


facilities2 <- hims_delta_clean %>%
  dplyr::select(HF, ownership, Ward, LGA) %>%
  distinct() %>% 
  group_by(Ward, LGA) %>%
  summarise(facility_count = n())

delta_shp <- st_read(file.path(ShpfilesDir, "Delta", "Delta_wards.shp"))


delta_ward_facilities <- left_join(delta_shp, facilities2, by = c("WardName" = "Ward"))


na_wards_count <- delta_ward_facilities %>%
  filter(is.na(facility_count)) %>%
  summarise(na_count = n())
print(na_wards_count) #47 wards

na_wards <- delta_ward_facilities %>%
  filter(is.na(facility_count)) %>%
  dplyr::select(WardName)


ggplot()+
  geom_sf(data = delta_ward_facilities, aes(geometry =  geometry, fill = facility_count))+
  labs(title = "Surveyed Health Facilities in Delta Wards ",
       fill = "Count")+
  map_theme()



##tpr

u5_tpr_delta <- hims_delta_clean %>% 
  dplyr::select(Ward, periodname, fever_u5, rdt_u5, rdt_pos_u5, micro_u5, micro_pos_u5, malaria_u5, gen_attd) %>% 
  mutate(tested = rdt_u5 + micro_u5,
         positive = rdt_pos_u5 + micro_pos_u5) %>% 
  group_by(Ward) %>%
  summarise(total_tested = sum(tested, na.rm = TRUE),
    total_positive = sum(positive, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(u5_tpr = total_tested/total_positive)
 

u5_tpr_delta_2 <- hims_delta_clean %>% 
  dplyr::select(Ward, fever_u5, rdt_u5, rdt_pos_u5, micro_u5, micro_pos_u5, malaria_u5, gen_attd, tested_u5, pos_u5) %>% 
  group_by(Ward) %>% 
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
  ungroup()
  

ward_tpr <- left_join(delta_shp, u5_tpr_delta_2, by = c("WardName" = "Ward"))
  
ggplot()+
  geom_sf(data = ward_tpr, #%>% 
            #mutate(u5_tpr = ifelse(is.nan(u5_tpr), 0, u5_tpr)), 
          aes(geometry = geometry, fill = u5_tpr2))+
  scale_fill_continuous(low = "pink", high = "maroon",na.value = "black")+
  labs(title = "TPR in children under 5 in Delta Wards, 2023",
       fill = "TPR",
       caption = "Black wards have no data. Either no children were tested or no facility was recorded in the ward ")+
  map_theme()



delta_wards <- read.csv(file.path(OutputsDir, "delta_wards.csv"))

delta_wards2 <- delta_wards %>%
  left_join(u5_tpr_delta_2 %>% 
              dplyr::select(Ward, u5_tpr, u5_tpr2, u5_tpr3), by = c("WardName" = "Ward"))

write.csv(delta_wards2, file.path(OutputsDir,"delta_wards.csv"), append = T)


delta_variables <- read.csv(file.path(ShpfilesDir, "delta_variables.csv"))
delta_variables2 <- delta_variables %>% 
  left_join(delta_wards2 %>% 
              dplyr::select(WardName, totalArea, urbanArea, urbanPercentage, urban_gee, u5_tpr2), by = "WardName")

write.csv(delta_variables2, file.path(OutputsDir,"delta_variables_shiny.csv"))

