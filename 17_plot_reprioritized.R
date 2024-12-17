### PRIORITZED AREAS

#source("~/NMEP_classification/load_path.R")


#DELTA

StateShpDir <- file.path(ShpfilesDir, "all_reprioritization_nmep_states/STATES")

Yobe_shp <- st_read(file.path(StateShpDir,"Yobe", "Yobe_State.shp"))

Yobe_variables <- read.csv(file.path(OutputsDir, "Final Extractions", "Yobe_plus.csv")) %>% 
  distinct(WardCode, .keep_all = TRUE) %>% 
  dplyr::select(X, WardName, urbanPercentage) %>% 
  mutate(
    classification_20 = ifelse(urbanPercentage > 20, "Urban", "Rural"),
    classification_30 = ifelse(urbanPercentage > 30, "Urban", "Rural"),
    classification_50 = ifelse(urbanPercentage > 50, "Urban", "Rural"),
    classification_75 = ifelse(urbanPercentage > 75, "Urban", "Rural")
  ) 



Yobe_ranks <- read.csv(file.path(OutputsDir, "rankings", "Yobe_rankings.csv")) %>% 
  dplyr::select(WardName, rank)
Yobe_ranks$Ward <- str_trim(Yobe_ranks$WardName)
Yobe_ranks$Rank <- str_trim(Yobe_ranks$rank)


ITNDir <- file.path(DataDir, "nigeria/ITN_distribution/household_member_ward_summaries_Itn_distribution")
Yobe_itn_data <- readxl::read_excel(
  file.path(ITNDir, "ITN_distribution_total_ward_yobe_2022.xlsx"))

Yobe_itn_clean <- Yobe_itn_data %>% 
  rename(population =`Sum of N_FamilyMembers`) %>% 
  dplyr::select(population, Ward) %>% 
  group_by(Ward) %>% 
  summarise(Population = sum(population, na.rm =T)) #%>% 
  # mutate(Ward = case_when(
  #   Ward == "Aballa/ Inyi/ Onuaboh" ~	"Aballa/Inyi/Onuaboh",
  #   Ward == "Abbi 1" ~	"Abbi 8",
  #   Ward == "Abbi 2" ~	"Abbi 9",
  #   Ward == "Aboh/ Akarai" ~	"Aboh/Akarai",
  #   Ward == "Abraka" ~	"Abraka I",
  #   Ward == "Achalla/Ezukwu/Ogboli" ~	"",
  #   Ward == "Afor/ Obikwele" ~	"Afor/Obikwele",
  #   Ward == "Agbarho I" ~	"Agbarho 1",
  #   Ward == "Agbarho II" ~	"Agbarho 2",
  #   Ward == "Agbor Obi" ~	"Agbor-Obi 1/Agbor Town II",
  #   Ward == "Agbor Obi Ward 2" ~	"",
  #   Ward == "Agbor-Nta" ~	"Agbor-Nta/Agbor Town I",
  #   Ward == "Agidiasi" ~	"Agiadiasi",
  #   Ward == "Ajudabo" ~	"Ajudaibo",
  #   Ward == "Akugbene 3" ~	"Akugbene III",
  #   Ward == "Akwuebulu" ~	"Akuebolu",
  #   Ward == "Alihagwu" ~	"Alihagwu/Ihiuiyase I",
  #   Ward == "Arigborodo" ~	"Abigborodo",
  #   Ward == "Ashaka/ Ushie" ~	"Ashaka/Ushie",
  #   Ward == "Boji Boji 1" ~	"Boji-Boji Owa 1/Owa III",
  #   Ward == "Boji Boji Agbor 1" ~	"Boji-Boji I/ Agbor 7",
  #   Ward == "Boji Boji Agbor 2" ~	"Boji-Boji II/ Agbor 8",
  #   Ward == "Boji Boji Owa 2" ~	"Boji-Boji Owa 2/Owa IV",
  #   Ward == "Egbo" ~	"Egbo/Agbon  VII",
  #   Ward == "Egini" ~	"Egini/Ovwian II",
  #   Ward == "Egodor" ~	"",
  #   Ward == "Ejeme/ Egbudu" ~	"Ejeme/Egbudu",
  #   Ward == "Ekametagbene/Kalafio" ~	"",
  #   Ward == "Ekpan I" ~	"Ekpan 9",
  #   Ward == "Ekpan II" ~	"Ekpan 10",
  #   Ward == "Eku" ~	"Eku/Agbon  VI",
  #   Ward == "Enerhen 1" ~	"Enerhen 3",
  #   Ward == "Enerhen I" ~	"",
  #   Ward == "Enerhen II" ~	"Enerhen 4",
  #   Ward == "Ewulu/ Isheagu" ~	"Ewulu/Isheagu",
  #   Ward == "Ibedeni/ Azagba" ~	"Ibedeni/Azagba",
  #   Ward == "Iberede/Onu/Iyede-Ame" ~	"Ibrede/Onu/Iyede-Ame",
  #   Ward == "Idumuje-Umor" ~	"Idumuje-Unor",
  #   Ward == "Igun" ~	"Igun/Agbon  V",
  #   Ward == "Irri 1" ~	"Irri 10",
  #   Ward == "Irri 2" ~	"Irri 11",
  #   Ward == "Isiokolo" ~	"Isiokolo/Agbon  VIII",
  #   Ward == "Kokori" ~	"Kokori/Agbon  III",
  #   Ward == "Mandangho" ~	"Madangho",
  #   Ward == "Ogbe - Obiaruku" ~	"Ogbeobiaruku",
  #   Ward == "Ogbe-Udu" ~	"Ogbe Udu",
  #   Ward == "Ogbudugbudu" ~	"Okbudugbudu",
  #   Ward == "Ogharefe 1" ~	"Ogharefe 1/Oghara I",
  #   Ward == "Ogharefe 2" ~	"Ogharefe 2/Oghara II",
  #   Ward == "Ogharefe 3" ~	"Ogharefe 3/Oghara III",
  #   Ward == "Oghareki 1" ~	"",
  #   Ward == "Oghareki 2" ~	"",
  #   Ward == "Ogor" ~	"",
  #   Ward == "Ogume 1" ~	"Ogume 6",
  #   Ward == "Ogume 2" ~	"Ogume 7",
  #   Ward == "Oko-Ogbele" ~	"Oko Ogbele",
  #   Ward == "Okpanam/ Ugbolu" ~	"Okpanam/Ugbolu",
  #   Ward == "Okpara" ~	"Okpara/Agbon  I",
  #   Ward == "Okuzu" ~	"Okuzu/Obiaruku  II",
  #   Ward == "Olomu 3" ~	"Olomu 3-Effurun-Otor",
  #   Ward == "Orhaorpo" ~	"Orhaorpo/Agbon  IV",
  #   Ward == "Oria" ~	"Oria/Abraka  III",
  #   Ward == "Orogun I" ~	"Orogun 1",
  #   Ward == "Orogun II" ~	"Orogun 2-Erhobaro",
  #   Ward == "Otor-Udu" ~	"Otor Udu/Udu I",
  #   Ward == "Ovu" ~	"Ovu/Agbon  II",
  #   Ward == "Owa- Alizomor" ~	"Owa-Alizomor/Owa  VI",
  #   Ward == "Owa Oyibu" ~	"Owa-Oyibu",
  #   Ward == "Owa-Alero" ~	"Owa-Alero/Owa II",
  #   Ward == "Owanta" ~	"Owanta/Owa  V",
  #   Ward == "Owhe Ward 1" ~	"Owhe 1",
  #   Ward == "Owhe Ward 2" ~	"Owhe 2",
  #   Ward == "Owhe Ward 3" ~	"Owhe 3",
  #   Ward == "Owhrode" ~	"Owhrode/Udu II",
  #   Ward == "Oyede Ward" ~	"Oyede",
  #   Ward == "Oyoko" ~	"Oyoko/Abavo I",
  #   Ward == "Ozanogogo" ~	"Ozanogogo/Ihuozomor (Ozanogogo Alisimie)",
  #   Ward == "Ozoro Ward 1" ~	"Ozoro 1",
  #   Ward == "Ozoro Ward 2" ~	"Ozoro 2",
  #   Ward == "Ozoro Ward 3" ~	"Ozoro 3",
  #   Ward == "Udomi-Azuowa" ~	"Udomi-Azuowa/Abavo II",
  #   Ward == "Urhuovie" ~	"Urhuovie/Abraka  II",
  #   TRUE ~ Ward))


combined_wards <- left_join(Yobe_variables, Yobe_ranks, by = c("WardName" = "Ward"))

combined_wards2 <- left_join(combined_wards, delta_itn_clean, by = c("WardName" = "Ward")) 

prioritized_delta1 <- prioritize_wards(data = combined_wards2, 
                                      population_col = "Population", 
                                      rank_col = "Rank", 
                                      class_col = "classification_20", 
                                      ward_col = "WardName", 
                                      target_percentage = 30) 

prioritized_delta2 <- prioritize_wards(data = combined_wards2, 
                                       population_col = "Population", 
                                       rank_col = "Rank", 
                                       class_col = "classification_30", 
                                       ward_col = "WardName", 
                                       target_percentage = 30) 


combined_plot <-  delta_shp %>% 
  left_join(combined_wards2, by = "WardName")
combined_plot$Rank <- as.numeric(as.character(combined_plot$Rank))


ggplot()+
  geom_sf(data = combined_plot, aes(geometry = geometry, fill = Rank))+
  scale_fill_gradient(na.value = "grey") +
  labs(title = "Risk Map in Delta State") +
  map_theme()

  
combined_plot2 <- delta_shp %>% 
  left_join(prioritized_delta1, by = c("WardName" = "SelectedWards")) %>% 
  mutate(status = ifelse(is.na(WardPopulation), "Not Reprioritized", "Reprioritized"))


ggplot(data = combined_plot2) +
  geom_sf(aes(geometry = geometry, fill = status)) +
  scale_fill_manual(values = c("Not Reprioritized" = "red", "Reprioritized" = "green"),
                    name = "Status") +
  labs(title = "Reprioritization Scenario 1",
       caption = "Suggested wards are low the lowest-ranking wards under these conditions:
       1. Using composite scores from EVI, u5_tpr, rainfall, urbanPercentage, distance to water bodies and housing quality
       2. Have at least 20% urban area")+
  map_theme()




