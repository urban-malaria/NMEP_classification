
StateShpDir <- file.path(ShpfilesDir, "all_reprioritization_nmep_states/STATES")
delta_shp <- st_read(file.path(StateShpDir,"Delta", "Delta_State.shp"))

delta_variables <- read.csv(file.path(OutputsDir, "Final Extractions", "delta_plus.csv")) %>% 
  dplyr::select(X, WardName, WardCode, urbanPercentage) %>% 
  mutate(
    classification_20 = ifelse(urbanPercentage > 20, "Urban", "Rural"),
    classification_30 = ifelse(urbanPercentage > 30, "Urban", "Rural"),
    classification_50 = ifelse(urbanPercentage > 50, "Urban", "Rural"),
    classification_75 = ifelse(urbanPercentage > 75, "Urban", "Rural")
  )


#delta_ranks <- read.csv(file.path(OutputsDir, "Delta_rankings2.csv")) %>%
delta_ranks <- read.csv(file.path(OutputsDir, "rankings", "Delta_rankings.csv")) %>% 
  dplyr::select(WardCode, WardName, ranks) #Ward, Rank
delta_ranks$WardName <- str_trim(delta_ranks$WardName)
delta_ranks$ranks <- str_trim(delta_ranks$ranks)


delta_itn_data <- readxl::read_excel(
  file.path(ITNDir, "pbi_distribution_Delta.xlsx"), 
  sheet = 3)

delta_itn_clean <- delta_itn_data %>% 
  rename(population = N_FamilyMembers,
         Ward = AdminLevel3) %>% 
  dplyr::select(population, Ward) %>% 
  group_by(Ward) %>% 
  summarise(Population = sum(population, na.rm =T)) %>% 
  mutate(Ward = case_when(
    Ward == "Aballa/ Inyi/ Onuaboh" ~	"Aballa/Inyi/Onuaboh",
    Ward == "Abbi 1" ~	"Abbi 8",
    Ward == "Abbi 2" ~	"Abbi 9",
    Ward == "Aboh/ Akarai" ~	"Aboh/Akarai",
    Ward == "Abraka" ~	"Abraka I",
    Ward == "Achalla/Ezukwu/Ogboli" ~	"",
    Ward == "Afor/ Obikwele" ~	"Afor/Obikwele",
    Ward == "Agbarho I" ~	"Agbarho 1",
    Ward == "Agbarho II" ~	"Agbarho 2",
    Ward == "Agbor Obi" ~	"Agbor-Obi 1/Agbor Town II",
    Ward == "Agbor Obi Ward 2" ~	"",
    Ward == "Agbor-Nta" ~	"Agbor-Nta/Agbor Town I",
    Ward == "Agidiasi" ~	"Agiadiasi",
    Ward == "Ajudabo" ~	"Ajudaibo",
    Ward == "Akugbene 3" ~	"Akugbene III",
    Ward == "Akwuebulu" ~	"Akuebolu",
    Ward == "Alihagwu" ~	"Alihagwu/Ihiuiyase I",
    Ward == "Arigborodo" ~	"Abigborodo",
    Ward == "Ashaka/ Ushie" ~	"Ashaka/Ushie",
    Ward == "Boji Boji 1" ~	"Boji-Boji Owa 1/Owa III",
    Ward == "Boji Boji Agbor 1" ~	"Boji-Boji I/ Agbor 7",
    Ward == "Boji Boji Agbor 2" ~	"Boji-Boji II/ Agbor 8",
    Ward == "Boji Boji Owa 2" ~	"Boji-Boji Owa 2/Owa IV",
    Ward == "Egbo" ~	"Egbo/Agbon  VII",
    Ward == "Egini" ~	"Egini/Ovwian II",
    Ward == "Egodor" ~	"",
    Ward == "Ejeme/ Egbudu" ~	"Ejeme/Egbudu",
    Ward == "Ekametagbene/Kalafio" ~	"",
    Ward == "Ekpan I" ~	"Ekpan 9",
    Ward == "Ekpan II" ~	"Ekpan 10",
    Ward == "Eku" ~	"Eku/Agbon  VI",
    Ward == "Enerhen 1" ~	"Enerhen 3",
    Ward == "Enerhen I" ~	"",
    Ward == "Enerhen II" ~	"Enerhen 4",
    Ward == "Ewulu/ Isheagu" ~	"Ewulu/Isheagu",
    Ward == "Ibedeni/ Azagba" ~	"Ibedeni/Azagba",
    Ward == "Iberede/Onu/Iyede-Ame" ~	"Ibrede/Onu/Iyede-Ame",
    Ward == "Idumuje-Umor" ~	"Idumuje-Unor",
    Ward == "Igun" ~	"Igun/Agbon  V",
    Ward == "Irri 1" ~	"Irri 10",
    Ward == "Irri 2" ~	"Irri 11",
    Ward == "Isiokolo" ~	"Isiokolo/Agbon  VIII",
    Ward == "Kokori" ~	"Kokori/Agbon  III",
    Ward == "Mandangho" ~	"Madangho",
    Ward == "Ogbe - Obiaruku" ~	"Ogbeobiaruku",
    Ward == "Ogbe-Udu" ~	"Ogbe Udu",
    Ward == "Ogbudugbudu" ~	"Okbudugbudu",
    Ward == "Ogharefe 1" ~	"Ogharefe 1/Oghara I",
    Ward == "Ogharefe 2" ~	"Ogharefe 2/Oghara II",
    Ward == "Ogharefe 3" ~	"Ogharefe 3/Oghara III",
    Ward == "Oghareki 1" ~	"",
    Ward == "Oghareki 2" ~	"",
    Ward == "Ogor" ~	"",
    Ward == "Ogume 1" ~	"Ogume 6",
    Ward == "Ogume 2" ~	"Ogume 7",
    Ward == "Oko-Ogbele" ~	"Oko Ogbele",
    Ward == "Okpanam/ Ugbolu" ~	"Okpanam/Ugbolu",
    Ward == "Okpara" ~	"Okpara/Agbon  I",
    Ward == "Okuzu" ~	"Okuzu/Obiaruku  II",
    Ward == "Olomu 3" ~	"Olomu 3-Effurun-Otor",
    Ward == "Orhaorpo" ~	"Orhaorpo/Agbon  IV",
    Ward == "Oria" ~	"Oria/Abraka  III",
    Ward == "Orogun I" ~	"Orogun 1",
    Ward == "Orogun II" ~	"Orogun 2-Erhobaro",
    Ward == "Otor-Udu" ~	"Otor Udu/Udu I",
    Ward == "Ovu" ~	"Ovu/Agbon  II",
    Ward == "Owa- Alizomor" ~	"Owa-Alizomor/Owa  VI",
    Ward == "Owa Oyibu" ~	"Owa-Oyibu",
    Ward == "Owa-Alero" ~	"Owa-Alero/Owa II",
    Ward == "Owanta" ~	"Owanta/Owa  V",
    Ward == "Owhe Ward 1" ~	"Owhe 1",
    Ward == "Owhe Ward 2" ~	"Owhe 2",
    Ward == "Owhe Ward 3" ~	"Owhe 3",
    Ward == "Owhrode" ~	"Owhrode/Udu II",
    Ward == "Oyede Ward" ~	"Oyede",
    Ward == "Oyoko" ~	"Oyoko/Abavo I",
    Ward == "Ozanogogo" ~	"Ozanogogo/Ihuozomor (Ozanogogo Alisimie)",
    Ward == "Ozoro Ward 1" ~	"Ozoro 1",
    Ward == "Ozoro Ward 2" ~	"Ozoro 2",
    Ward == "Ozoro Ward 3" ~	"Ozoro 3",
    Ward == "Udomi-Azuowa" ~	"Udomi-Azuowa/Abavo II",
    Ward == "Urhuovie" ~	"Urhuovie/Abraka  II",
    TRUE ~ Ward))


combined_wards <- left_join(delta_variables, delta_ranks, by = c("WardCode", "WardName"))

combined_wards2 <- left_join(combined_wards, delta_itn_clean, by = c("WardName" = "Ward")) 

prioritized_delta1 <- prioritize_wards(data = combined_wards2, 
                                       population_col = "Population", 
                                       rank_col = "ranks", 
                                       class_col = "classification_20", 
                                       ward_col = "WardName", 
                                       target_percentage = 30) 
write.csv(prioritized_delta1, file.path(OutputsDir, "NMEP Presentation Reprioritization Tables", "Delta", "delta_scenario1.csv"))



prioritized_delta2 <- prioritize_wards(data = combined_wards2, 
                                       population_col = "Population", 
                                       rank_col = "ranks", 
                                       class_col = "classification_30", 
                                       ward_col = "WardName", 
                                       target_percentage = 30)
write.csv(prioritized_delta2, file.path(OutputsDir, "NMEP Presentation Reprioritization Tables", "Delta", "delta_scenario2.csv"))




prioritized_delta3 <- prioritize_wards(data = combined_wards2, 
                                       population_col = "Population", 
                                       rank_col = "ranks", 
                                       class_col = "classification_50", 
                                       ward_col = "WardName", 
                                       target_percentage = 30)
write.csv(prioritized_delta3, file.path(OutputsDir, "NMEP Presentation Reprioritization Tables", "Delta", "delta_scenario3.csv"))



prioritized_delta4 <- prioritize_wards(data = combined_wards2, 
                                       population_col = "Population", 
                                       rank_col = "ranks", 
                                       class_col = "classification_75", 
                                       ward_col = "WardName", 
                                       target_percentage = 30)
write.csv(prioritized_delta4, file.path(OutputsDir, "NMEP Presentation Reprioritization Tables", "Delta", "delta_scenario4.csv"))


############## 50%
combined_plot2 <- delta_shp %>% 
  left_join(prioritized_delta3, by = c("WardCode")) %>% 
  mutate(status = ifelse(is.na(WardPopulation), "Not Reprioritized", "Reprioritized"))

combined_plot3 <-combined_plot2 %>%  left_join(delta_ranks, by = "WardCode") %>%  filter(status == "Reprioritized") %>%  
  mutate(new_rank = rank(ranks))

p3<- ggplot(data = filter(lGAshp, State == "Delta")) +
  geom_sf(fill = "red") +
  geom_sf(data = combined_plot3, aes(geometry = geometry), fill = "green") +
  # scale_fill_manual(values = c("Not Reprioritized" = "red", "Reprioritized" = "green"),
  #                   name = "Status") +
  geom_text_repel(data = filter(lGAshp, State == "Delta"),
                  aes(label =  LGA, geometry = geometry),color ='black',
                  stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1, max.overlaps = Inf)+
  labs(title = "Reprioritization Scenario 3",
       caption = "conditions: 1. composite scores from EVI, u5_tpr, distance to water bodies, settlement type, and flood intensity, 
       2. Have at least 50% urban area")+
  map_theme() +
  xlab("")+
  ylab("")


p3_ward = ggplot()+
  geom_sf(data = combined_plot3, aes(geometry = geometry), fill = "green")+
  geom_text_repel(data = combined_plot3,
                  aes(label =  paste0(new_rank,". ", WardName.x, " (", round(WardPopulation*1.1, 0), ")"), geometry = geometry),color ='black',
                  stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1, max.overlaps = Inf)+
  map_theme() +
  xlab("")+
  ylab("")


p3_all = p3 + p3_ward

############## 75%
combined_plot2 <- delta_shp %>% 
  left_join(prioritized_delta4, by = c("WardCode")) %>% 
  mutate(status = ifelse(is.na(WardPopulation), "Not Reprioritized", "Reprioritized"))

combined_plot3 <-combined_plot2 %>%  left_join(delta_ranks, by = "WardCode") %>%  filter(status == "Reprioritized") %>%  
  mutate(new_rank = rank(ranks))


p4<- ggplot(data = filter(lGAshp, State == "Delta")) +
  geom_sf(fill = "red") +
  geom_sf(data = combined_plot3, aes(geometry = geometry), fill = "green") +
  # scale_fill_manual(values = c("Not Reprioritized" = "red", "Reprioritized" = "green"),
  #                   name = "Status") +
  geom_text_repel(data = filter(lGAshp, State == "Delta"),
                  aes(label =  LGA, geometry = geometry),color ='black',
                  stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1, max.overlaps = Inf)+
  labs(title = "Reprioritization Scenario 4",
       caption = "conditions: 1. composite scores from EVI, u5_tpr, distance to water bodies, settlement type, and flood intensity, 
       2. Have at least 75% urban area")+
  map_theme() +
  xlab("")+
  ylab("")

p4_ward = ggplot()+
  geom_sf(data = combined_plot3, aes(geometry = geometry), fill = "green")+
  geom_text_repel(data = combined_plot3,
                  aes(label =  paste0(WardName.x, " (", round(WardPopulation*1.1, 0), ")"), geometry = geometry),color ='black',
                  stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1, max.overlaps = Inf)+
  map_theme() +
  xlab("")+
  ylab("")


p4_all = p4 + p4_ward

p_high_urban = p3_all/p4_all

ggsave(paste0(FigDir,"/", "urban_50_75", "/", Sys.Date(),"_Delta_reprioritization_scenarios.pdf"), p_high_urban, width = 13, height = 14)


