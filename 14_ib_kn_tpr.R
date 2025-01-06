# ==========================================================================================================================================
# Script Name: Kano and Ibadan TPR
# Purpose: 
# ==========================================================================================================================================

source("~/NMEP_classification/load_path.R", echo = T)

library(readxl)

##### SHAPE FILE EDIT

# kano_ibadan_shp <- st_read(file.path(OutputsDir,"NMEP Malaria Risk Scores", "Kano_Ibadan/Kano-Ibadan.shp"))
# 
# kano_ibadan_shp2 <- kano_ibadan_shp %>% 
#   filter(WardName == "Fagge D2" | WardName == "Dorayi" |WardName == "Giginyu" |WardName == "Gobirawa"| WardName == "Zango"|
#            WardName == "Agugu" | WardName == "Bashorun" | WardName == "Challenge" | WardName == "Olopomewa" )
# 
# st_write(kano_ibadan_shp2, file.path(OutputsDir,"NMEP Malaria Risk Scores", "KN-IB/KN-IB.shp" ))

################### ROUTINE HEALTH DATA ANALYSIS ##############################

hmis <- read_excel(file.path(DataDir, "nigeria/nigeria_hmis", 
                             "1. National Raw Data NMEP Malaria Data_2023_2022_10062024.xlsx"), sheet = 2)
head(hmis)
print(unique(hmis$periodname))

hmis_ib_kn <- hmis %>% 
  filter(State == "oy Oyo State" | State == "kn Kano State" )

head(hmis_ib_kn)

hmis_ib_kn_clean <- hmis_ib_kn %>% 
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
         State = str_remove(State, "^[a-zA-Z]{2}\\s+"),
         LGA = str_remove(LGA, "^[a-zA-Z]{2}\\s+"),
         LGA = str_replace_all(LGA, "Local Government Area", ""),
         Ward = str_remove(Ward, "^[a-zA-Z]{2}\\s+"),
         Ward = str_replace_all(Ward, "Ward", ""),
         HF = str_remove(HF, "^[a-zA-Z]{2}\\s+"))

hmis_ib_kn_clean$Ward <- str_trim(hmis_ib_kn_clean$Ward)

hmis_ib_kn_clean <- hmis_ib_kn_clean %>% 
  filter(Ward == "Fagge D 2" | Ward == "Dorayi" |Ward == "Giginyu" |Ward == "Gobirawa"| Ward == "Zango"|
           Ward == "Agugu" | Ward == "Basorun" | Ward == "Challenge" | Ward == "Olopomewa" ) %>% 
  mutate(Ward= case_when(
    Ward == "Basorun" ~ "Bashorun",
    Ward == "Fagge D 2" ~ "Fagge D2",
    TRUE ~ Ward
  )) %>%
  mutate(tested_u5 = (rdt_u5 + micro_u5),
         pos_u5 = (rdt_pos_u5 + micro_pos_u5),
         tested_a5 = (rdt_a5 + micro_a5),
         pos_a5 = (rdt_pos_a5 + micro_pos_a5),
         tested_pw = (rdt_pw + micro_pw),
         pos_pw = (rdt_pos_pw + micro_pos_pw))


###
facilities <- hmis_ib_kn_clean %>%
  dplyr::select(HF, ownership, Ward, level) %>%
  distinct() %>% 
  group_by(Ward, ownership, level) %>%
  summarise(facility_count = n())

ggplot(facilities, aes(x = Ward, y = facility_count, fill = level)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Health Facilities Surveyed in Kano and Ibadan Wards", 
       x = "Ward", y = "Facility Count", fill = "Level") +
  theme_manuscript() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


facilities2 <- hmis_ib_kn_clean %>%
  dplyr::select(HF, ownership, Ward, LGA) %>%
  distinct() %>% 
  group_by(Ward, LGA) %>%
  summarise(facility_count = n())

ib_kn_shp <- st_read(file.path(ShpfilesDir, "Kano_Ibadan", "Kano-Ibadan.shp"))

all_ward_facilities <- left_join(ib_kn_shp, facilities2, by = c("WardName" = "Ward"))


ggplot()+
  geom_sf(data = all_ward_facilities %>% 
            filter(WardName == "Agugu"| WardName == "Bashorun"| WardName == "Challenge" | WardName == "Olopomewa"), 
          aes(geometry =  geometry, fill = facility_count))+
  labs(title = "Surveyed Health Facilities in Ibadan Wards ",
       fill = "Count")+
  map_theme()

ggplot()+
  geom_sf(data = all_ward_facilities %>% 
            filter(WardName == "Dorayi"| WardName == "Fagge D2"| WardName == "Giginyu" | WardName == "Gobirawa"| WardName == "Zango"), 
          aes(geometry =  geometry, fill = facility_count))+
  labs(title = "Surveyed Health Facilities in Kano Wards ",
       fill = "Count")+
  map_theme()


##tpr

u5_tpr_2 <- hmis_ib_kn_clean %>% 
  dplyr::select(HF, level, Ward, fever_u5, rdt_u5, rdt_pos_u5, micro_u5, micro_pos_u5, malaria_u5, gen_attd, tested_u5, pos_u5) %>% 
  group_by(Ward, HF, level) %>% #HF and level added
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


#Plot

# u5_tpr_long <- u5_tpr %>% 
#   pivot_longer(cols = -Ward, names_to = "Metric", values_to = "Value")
# ggplot(u5_tpr_long, aes(x = Ward, y = Value, fill = Metric)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   labs(title = "TPR Metrics Across Wards", x = "Ward", y = "TPR",
#        caption = "u5_tpr = u5_tpr/u5 children tested
#        u5_tpr_2 = u5_tpr/u5 children with fever
#        u5_tpr_3 = u5_tpr/ all people attending clinic
#        _rdt = RDT only
#        _micro = Microscopy only") +
#   theme_manuscript() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))


u5_tpr_long2 <- u5_tpr_2 %>%
  pivot_longer(cols = starts_with("u5_tpr"), 
               names_to = "Metric",       
               values_to = "Value")   

filtered <- u5_tpr_long2 %>%
  filter(Metric == "u5_tpr2")

ggplot(filtered, aes(x = Value, fill = level)) +
  #geom_density(alpha = 0.4, adjust = 1) +
  geom_histogram(binwidth = 0.1, color = "black", alpha = 0.3, position = "dodge") +
  labs(title = "Distribution of U5_TPR 2 (Children Presenting with Fever) by Ward",
       x = "U5_TPR",
       y = "Frequency",
       fill = "Level") +
  facet_wrap(~ Ward) +
  theme_manuscript()

ggplot(filtered, aes(x = Ward, y= Value, colour = level)) +
  geom_point(size = 1.5)+
  labs(title = "Distribution of U5_TPR 2 (Children Presenting with Fever)",
       x = "Ward",
       y = "U5_TPR",
       color = "Level") +
  theme_manuscript()

filtered2 <- u5_tpr_long2 %>%
  filter(Metric == "u5_tpr2" ) %>% 
  filter(level != "Tertiary Health Facility") %>% 
  filter(Ward == "Agugu" | Ward == "Bashorun" | Ward == "Challenge" | Ward == "Olopomewa")

ggplot(filtered2, aes(x = Value, fill = Ward)) +
  #geom_density(alpha = 0.4, adjust = 1) +
  geom_histogram(binwidth = 0.1, color = "black", alpha = 0.3, position = "dodge") +
  labs(title = "Distribution of U5_TPR 2 (Children Presenting with Fever) Ibadan",
       x = "U5_TPR",
       y = "Density",
       fill = "Ward") +
  facet_wrap(~ level) +
  theme_manuscript()


filtered3 <- u5_tpr_long2 %>%
  filter(Metric == "u5_tpr2" ) %>% 
  filter(level != "Tertiary Health Facility") %>% 
  filter(Ward == "Dorayi" | Ward == "Fagge D2" | Ward == "Giginyu" | Ward == "Gobirawa" | Ward == "Zango")

ggplot(filtered3, aes(x = Value, fill = Ward)) +
  #geom_density(alpha = 0.4, adjust = 1) +
  geom_histogram(binwidth = 0.1, color = "black", alpha = 0.3, position = "dodge") +
  labs(title = "Distribution of U5_TPR 2 (Children Presenting with Fever) Kano",
       x = "U5_TPR",
       y = "Density",
       fill = "Ward") +
  facet_wrap(~ level) +
  theme_manuscript()

ggplot(filtered3, aes(x = Ward, y= Value, colour = level)) +
  geom_point(size = 2)+
  labs(title = "Distribution of U5_TPR 2 (Children Presenting with Fever) Kano",
       x = "Ward",
       y = "U5_TPR",
       fill = "Ward") +
  theme_manuscript()


##
all_ward_tpr_plot <-left_join(ib_kn_shp, u5_tpr, by = c("WardName" = "Ward")) %>% 
  filter(WardName == "Fagge D2" | WardName == "Dorayi" |WardName == "Giginyu" |WardName == "Gobirawa"| WardName == "Zango"|
           WardName == "Agugu" | WardName == "Bashorun" | WardName == "Challenge" | WardName == "Olopomewa" )

all_ward_tpr <- all_ward_tpr_plot %>% 
  st_drop_geometry()

write.csv(all_ward_tpr, file.path(OutputsDir, "NMEP Malaria Risk Scores", "u5_tpr_kn_ib.csv"))

ibadan_tpr_plot <- all_ward_tpr_plot %>% 
  filter(StateCode == "OY") %>% 
  dplyr::select(u5_tpr, u5_tpr2, u5_tpr3) %>% 
  pivot_longer(cols = c(u5_tpr, u5_tpr2, u5_tpr3), names_to = "TPR_type", values_to = "value")

ggplot()+
  geom_sf(data = ibadan_tpr_plot, 
          aes(geometry = geometry, fill = value))+
  facet_wrap(~TPR_type)+
  scale_fill_continuous(low = "pink", high = "maroon",na.value = "black")+
  labs(title = "TPR in children under 5 in Ibadan Wards, 2023",
       fill = "TPR",
       caption = "u5_tpr = TPR amongst children tested
       u5_tpr2 = TPR amongst U5 children presenting with fever
       u5_tpr3 = TPR amongst general clinic attendance")+
  map_theme()

ggplot()+
  geom_sf(data = all_ward_tpr_plot %>% 
            filter(StateCode == "OY"), aes(geometry = geometry, fill = u5_tpr2)) +
  scale_fill_continuous(low = "pink", high = "maroon",na.value = "black")+
  labs(title = "TPR in children under 5 (amongst those who presented with fever) in Ibadan Wards, 2023",
       fill = "TPR")+
  map_theme()


##save

kano_ibadan_wards <- read.csv(file.path(OutputsDir,"NMEP Malaria Risk Scores", "kano_ibadan_wards.csv"))

kano_ibadan_wards2 <- kano_ibadan_wards %>%
  left_join(u5_tpr %>% 
              dplyr::select(Ward, u5_tpr, u5_tpr2, u5_tpr3), by = c("WardName" = "Ward"))

write.csv(kano_ibadan_wards2, file.path(OutputsDir,"NMEP Malaria Risk Scores", "kano_ibadan_wards.csv"), append = T)


##########KANO ROUTINE DATA
kano_routine <- read.csv(file.path(DataDir, "nigeria/kano_ibadan/kano_routine_data", "kano_routine.csv"))
print(unique(kano_routine$year))

kano_routine_2019 <- kano_routine %>% 
  filter(year == "2019" | year == "2020"| year == "2021") %>% 
  rename(State = adm1,
         Ward = adm2,
         HF = hfname,
         gen_attd = attendance,
         rdt_u5 = test_rdt_u5,
         rdt_pos_u5 = conf_rdt_u5 ,
         mirco_u5 = test_mic_u5,
         micro_pos_u5 = conf_mic_u5)


### NATIONAL HEALTH FACILITY DATA 2020
hmis_2020 <- read_excel(file.path(DataDir, "nigeria/nigeria_hmis", 
                                  "211026_Malaria Data 2020_HF_edited.xlsx")) %>% 
  dplyr::select(-state, -lga, -ward) %>% 
  rename(HF = facility_name) %>% 
  mutate(State = str_remove(State, "^[a-zA-Z]{2}\\s+"),
         State = str_replace(State, "State", ""),
         LGA = str_remove(LGA, "^[a-zA-Z]{2}\\s+"),
         Ward = str_remove(Ward, "^[a-zA-Z]{2}\\s+"),
         Ward = str_replace_all(Ward, "Ward", ""),
         HF = str_remove(HF, "^[a-zA-Z]{2}\\s+"))
hmis_2020$State <- str_trim(hmis_2020$State)
hmis_2020$HF <- str_trim(hmis_2020$HF)
hmis_2020$Ward <- str_trim(hmis_2020$Ward)

hmis_2020_kn_ib <- hmis_2020 %>% 
  filter(State == "Kano" | State == "Oyo") %>% 
  filter(Ward == "Fagge D 2" | Ward == "Dorayi" |Ward == "Giginyu" |Ward == "Gobirawa"| Ward == "Zango"|
           Ward == "Agugu" | Ward == "Basorun" | Ward == "Challenge" | Ward == "Olopomewa") %>% 
  mutate(Ward= case_when(
    Ward == "Basorun" ~ "Bashorun",
    Ward == "Fagge D 2" ~ "Fagge D2",
    TRUE ~ Ward
  )) %>% 
  mutate(year = "2020")
  
  

## MERGE 3 DATA SETS FOR KANO AND IBADAN
