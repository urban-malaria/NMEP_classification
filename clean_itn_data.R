# ==========================================================================================================================================
# Script Name: Clean ITN ward names
# Purpose: Clean ward names in ITN datasets and resave them for use in the R package/reprioritization.
#          Ensures consistency of names with extracted data.
# Author: Grace Legris, Research Data Analyst, gracebea@gmail.com
# ==========================================================================================================================================

source("load_path.R")

## =========================================================================================================================================
### KATSINA
## =========================================================================================================================================

# read in ITN data and extracted data
katsina_itn_data <- readxl::read_excel(file.path(ITNDir, "pbi_distribution_Katsina.xlsx"), sheet = 1)
katsina_itn_clean <- katsina_itn_data %>%
  rename(Ward = `AdminLevel3`) %>%
  mutate(Ward = case_when(
    Ward == "Ang Musa" ~ "Anguwan Musa",
    Ward == "Ba'Awa" ~ "Ba'awa",
    Ward == "Babba Mutum" ~ "Babban Mutum",
    Ward == "Bagagadi" ~ "Bugagadi",
    Ward == "Barde/Kkw" ~ "Barde Kwantakwaran",
    Ward == "Borin Dawa" ~ "Borindawa",
    Ward == "Dan Alhaji" ~ "Dan Alhaji Yangayya",
    Ward == "Dan Kum" ~ "Dankum",
    Ward == "Dan'Aunai" ~ "Dan Aunai",
    Ward == "Danali" ~ "Dan Ali",
    Ward == "Danjanku" ~ "Danjanku-Karachi",
    Ward == "Danmarabu" ~ "Dan Murabu",
    Ward == "Danmusa A" ~ "Dan Musa A",
    Ward == "Danmusa B" ~ "Dan Musa B",
    Ward == "Dantakari" ~ "Dantankari",
    Ward == "Darini/ Magaji Abu" ~ "Darini Magaji Abu",
    Ward == "Daunaka/Bakin Kwari" ~ "Daunaka",
    Ward == "Dawan Musa" ~ "Dawa Musa",
    Ward == "Dudunni" ~ "Duddunni",
    Ward == "Dungun Muazu" ~ "Dugu Muazu",
    Ward == "Dutsin Kura" ~ "Dutsen Kura",
    Ward == "Dutsinma A" ~ "Dutsin Ma A",
    Ward == "Dutsinma B" ~ "Dutsin Ma B",
    Ward == "Duwan/Makau" ~ "Duwan Makau",
    Ward == "Fakuwa/Kafin Dangi" ~ "Fakuwa Kafin Dangi",
    Ward == "Gafia" ~ "Gafiya",
    Ward == "Gora Dansaka" ~ "Gora Dan Saka",
    Ward == "Kahutu A" ~ "Kahuta A",
    Ward == "Kahutu B" ~ "Kahuta B",
    Ward == "Kandawa /Jobe" ~ "Kandawa",
    Ward == "Karadua" ~ "Karaduwa",
    Ward == "Katare" ~ "Ketare",
    Ward == "Kawarin Malama" ~ "K Malamai",
    Ward == "Kudi 1" ~ "Kudu 1",
    Ward == "Kudi 2" ~ "Kudu 2",
    Ward == "Kudi 3" ~ "Kudu 3",
    Ward == "Kunduru/Gyaza" ~ "Kunduru Gyaza",
    Ward == "Kurami/Yankwani" ~ "Kurami Yankwani",
    Ward == "Magama/Masabo/Kurabau" ~ "Magama Masabo Kurabau",
    Ward == "Maiadua A" ~ "Mai'adua A",
    Ward == "Maiadua B" ~ "Mai'adua B",
    Ward == "Maiadua C" ~ "Mai'adua C",
    Ward == "Maigora" ~ "Mai Gora",
    Ward == "Mairuwa" ~ "Mairua",
    Ward == "Maje/Karare" ~ "Maje Karare",
    Ward == "Muduri" ~ "Mudiri",
    Ward == "Na Alma" ~ "Na'alma",
    Ward == "Ruma" ~ "Rumah",
    Ward == "Sabongari" ~ "Sabon Gari",
    Ward == "Sarikin Yara B" ~ "Sarkin Yara B",
    Ward == "Tabanni" ~ "Tabanni-Yarraddau",
    Ward == "Tafashiya/ Nassarawa" ~ "Tafashiya Nasarawa",
    Ward == "Tama/Daye" ~ "Tama",
    Ward == "Tsa/Magam" ~ "Tsa Magam",
    Ward == "Tsagem / Takusheyi" ~ "Tsagem Takusheyi",
    Ward == "Uban Dawaki A" ~ "Ubandawaki A",
    Ward == "Uban Dawaki B" ~ "Ubandawaki B",
    Ward == "Yanmama" ~ "Yarmama",
    Ward == "Yargayya" ~ "Yangayya",
    Ward == "Yau Yau/ Mallamawa" ~ "Yau-Yau Malamawa",
    Ward == "Yaya/Bidore" ~ "Yaya",
    TRUE ~ Ward
  )) %>%
  dplyr::select(population = `N_Nets`, Ward) %>%
  group_by(Ward) %>%
  summarise(Population = sum(population, na.rm = TRUE)) %>%
  ungroup()

katsina_extracted_data <- read.csv(file.path(ExtractedDir, "Katsina_wards_variables.csv"))


katsina_extracted_data <- katsina_extracted_data %>%
  arrange(WardName)
katsina_itn_clean <- katsina_itn_clean %>%
  arrange(Ward)

# identify mismatches
itn_unique <- unique(katsina_itn_clean$Ward)
extracted_unique <- unique(katsina_extracted_data$WardName)
missing_in_extracted <- setdiff(itn_unique, extracted_unique)
missing_in_itn <- setdiff(extracted_unique, itn_unique)
cat("Wards in ITN data but not in extracted data:\n")
print(missing_in_extracted)
cat("\nWards in extracted data but not in ITN data:\n")
print(missing_in_itn)

writexl::write_xlsx(katsina_itn_clean, file.path(ITNDir, "cleaned", "pbi_distribution_Katsina_clean.xlsx"))

## =========================================================================================================================================
### DELTA
## =========================================================================================================================================

# read in ITN data and extracted data
delta_itn_data <- readxl::read_excel(file.path(ITNDir, "pbi_distribution_Delta.xlsx"), sheet = 1)
delta_extracted_data <- read.csv(file.path(ExtractedDir, "Delta_wards_variables.csv"))

delta_itn_clean <- delta_itn_data %>%
  rename(Ward = `Row Labels`) %>%
  mutate(Ward = case_when(
    Ward == "Aballa/ Inyi/ Onuaboh" ~ "Aballa/Inyi/Onuaboh",
    Ward == "Aboh/ Akarai" ~ "Aboh/Akarai",
    Ward == "Achalla/Ezukwu/Ogboli" ~ "Ezukwu/Ogboli/Achalla",
    Ward == "Afor/ Obikwele" ~ "Afor/Obikwele",
    Ward == "Agbarho I" ~ "Agbarho 1",
    Ward == "Agbarho II" ~ "Agbarho 2",
    Ward == "Agbor Obi" ~ "Agbor-Obi 1/Agbor Town II",
    Ward == "Agbor-Nta" ~ "Agbor-Nta/Agbor Town I",
    Ward == "Agidiasi" ~ "Agiadiasi",
    Ward == "Ajudabo" ~ "Ajudaibo",
    Ward == "Akugbene 3" ~ "Akugbene III",
    Ward == "Akwuebulu" ~ "Akuebolu",
    Ward == "Alihagwu" ~ "Alihagwu/Ihiuiyase I",
    Ward == "Arigborodo" ~ "Abigborodo",
    Ward == "Ashaka/ Ushie" ~ "Ashaka/Ushie",
    Ward == "Boji Boji 1" ~ "Boji-Boji I/ Agbor 7",
    Ward == "Boji Boji Agbor 1" ~ "Boji-Boji I/ Agbor 7",
    Ward == "Boji Boji Agbor 2" ~ "Boji-Boji II/ Agbor 8",
    Ward == "Boji Boji Owa 2" ~ "Boji-Boji Owa 2/Owa IV",
    Ward == "Egbo" ~ "Egbo/Agbon  VII",
    Ward == "Egini" ~ "Egini/Ovwian II",
    Ward == "Ejeme/ Egbudu" ~ "Ejeme/Egbudu",
    Ward == "Ekametagbene/Kalafio" ~ "Kolafiogbene/Ekametagbene",
    Ward == "Eku" ~ "Eku/Agbon  VI",
    Ward == "Ewulu/ Isheagu" ~ "Ewulu/Isheagu",
    Ward == "Ibedeni/ Azagba" ~ "Ibedeni/Azagba",
    Ward == "Iberede/Onu/Iyede-Ame" ~ "Ibrede/Onu/Iyede-Ame",
    Ward == "Idumuje-Umor" ~ "Idumuje-Unor",
    Ward == "Igun" ~ "Igun/Agbon  V",
    Ward == "Isiokolo" ~ "Isiokolo/Agbon  VIII",
    Ward == "Kokori" ~ "Kokori/Agbon  III",
    Ward == "Mandangho" ~ "Madangho",
    Ward == "Ogbe - Obiaruku" ~ "Ogbeobiaruku",
    Ward == "Ogbe-Udu" ~ "Ogbe Udu",
    Ward == "Ogbudugbudu" ~ "Okbudugbudu",
    Ward == "Ogharefe 1" ~ "Ogharefe 1/Oghara I",
    Ward == "Ogharefe 2" ~ "Ogharefe 2/Oghara II",
    Ward == "Ogharefe 3" ~ "Ogharefe 3/Oghara III",
    Ward == "Ogor" ~ "Otor-Ogor",
    Ward == "Oko-Ogbele" ~ "Oko Ogbele",
    Ward == "Okpanam/ Ugbolu" ~ "Okpanam/Ugbolu",
    Ward == "Okpara" ~ "Okpara/Agbon  I",
    Ward == "Okuzu" ~ "Okuzu/Obiaruku  II",
    Ward == "Olomu 3" ~ "Olomu 3-Effurun-Otor",
    Ward == "Oria" ~ "Oria/Abraka  III",
    Ward == "Orogun I" ~ "Orogun 1",
    Ward == "Orogun II" ~ "Orogun 2-Erhobaro",
    Ward == "Otor-Udu" ~ "Otor Udu/Udu I",
    Ward == "Ovu" ~ "Ovu/Agbon  II",
    Ward == "Owa Oyibu" ~ "Owa-Oyibu",
    Ward == "Owa- Alizomor" ~ "Owa-Alizomor/Owa  VI",
    Ward == "Owa-Alero" ~ "Owa-Alero/Owa II",
    Ward == "Owanta" ~ "Owanta/Owa  V",
    Ward == "Owhe Ward 1" ~ "Owhe 1",
    Ward == "Owhe Ward 2" ~ "Owhe 2",
    Ward == "Owhe Ward 3" ~ "Owhe 3",
    Ward == "Owhrode" ~ "Owhrode/Udu II",
    Ward == "Oyede Ward" ~ "Oyede",
    Ward == "Oyoko" ~ "Oyoko/Abavo I",
    Ward == "Ozanogogo" ~ "Ozanogogo/Ihuozomor (Ozanogogo Alisimie)",
    Ward == "Ozoro Ward 1" ~ "Ozoro 1",
    Ward == "Ozoro Ward 2" ~ "Ozoro 2",
    Ward == "Ozoro Ward 3" ~ "Ozoro 3",
    Ward == "Udomi-Azuowa" ~ "Udomi-Azuowa/Abavo II",
    Ward == "Urhuovie" ~ "Urhuovie/Abraka  II",
    Ward == "Orhaorpo" ~ "Orhaorpo/Agbon  IV",
    Ward == "Abraka" ~ "Abraka I",
    TRUE ~ Ward)) %>% 
  dplyr::select(population = `Sum of N_Nets`, Ward) %>%
  group_by(Ward) %>%
  summarise(Population = sum(population, na.rm = TRUE)) %>%
  ungroup()


# identify mismatches
itn_unique <- unique(delta_itn_clean$Ward)
extracted_unique <- unique(delta_extracted_data$WardName)
missing_in_extracted <- setdiff(itn_unique, extracted_unique)
missing_in_itn <- setdiff(extracted_unique, itn_unique)
cat("Wards in ITN data but not in extracted data:\n")
print(missing_in_extracted)
cat("\nWards in extracted data but not in ITN data:\n")
print(missing_in_itn)

writexl::write_xlsx(delta_itn_clean, file.path(ITNDir, "cleaned", "pbi_distribution_Delta_clean.xlsx"))


## =========================================================================================================================================
### KADUNA
## =========================================================================================================================================

# read in ITN data and extracted data
kaduna_itn_data <- readxl::read_excel(file.path(ITNDir, "pbi_distribution_Kaduna_old.xlsx"))
kaduna_extracted_data <- read.csv(file.path(ExtractedDir, "Kaduna_wards_variables.csv"))

kaduna_itn_clean <- kaduna_itn_data %>%
  rename(Ward = `AdminLevel3`) %>%
  mutate(Ward = case_when(
    Ward == "Awan" ~ "Awon",
    Ward == "Badikko" ~ "Badiko",
    Ward == "Bitaro" ~ "Dura Bitaro",
    Ward == "Budah" ~ "Buda",
    Ward == "D/Abba" ~ "Dutsen Abba",
    Ward == "Dadiriba" ~ "Dadi Riba",
    Ward == "Dammahawayi" ~ "Danmahawayi",
    Ward == "Danalhaji" ~ "Dan Alhaji",
    Ward == "Dandamisa" ~ "Dan Damisa",
    Ward == "Dogon Dawa" ~ "Dogondawa",
    Ward == "Dutse Wai" ~ "Dutsen Wai",
    Ward == "Garu/Mariri" ~ "Garu Mariri",
    Ward == "Gure/Kahugu" ~ "Gure Kahugu",
    Ward == "Gyallesu" ~ "Gyellesu",
    Ward == "Jama'A" ~ "Jama A",
    Ward == "K/Magani" ~ "Kasuwan Magani",
    Ward == "K/Wali North" ~ "Kauran Wali North",
    Ward == "Kagarko  North" ~ "Kagarko North",
    Ward == "Kagarko  South" ~ "Kagarko South",
    Ward == "Kamanton" ~ "Kamantan",
    Ward == "Kannikon" ~ "Kaninkon",
    Ward == "Karreh" ~ "Kareh",
    Ward == "Kukui" ~ "Kukuyi",
    Ward == "Lazuru/  Tuddai" ~ "Lazuru Tuddai",
    Ward == "Limachin Kona" ~ "Limancin Kona",
    Ward == "Madekiya" ~ "Madakiya",
    Ward == "Maiburji" ~ "Maiburiji",
    Ward == "Mallagum" ~ "Malagum",
    Ward == "Mayere" ~ "Meyari",
    Ward == "Mucha" ~ "Muchiya",
    Ward == "N/Doya" ~ "Nasarawa Wandoya",
    Ward == "Panhuaya" ~ "Panhauya",
    Ward == "Panshanu" ~ "Fanshanu",
    Ward == "Ramin/Kura" ~ "Raminkura",
    Ward == "Sab Chem" ~ "Sabchem",
    Ward == "Sab-Zuro" ~ "Sabzuro",
    Ward == "Sabon Gari" ~ "Sabon Garin",
    Ward == "Sam Ban" ~ "Samban",
    Ward == "Saya-Saya" ~ "Saya Saya",
    Ward == "Tantatu" ~ "Tantattu",
    Ward == "U/Dosa" ~ "Unguwan Dosa",
    Ward == "U/Rimi" ~ "Unguwar Rimi",
    Ward == "U/Sanusi" ~ "Unguwan Sanusi",
    Ward == "U/Sarki" ~ "Unguwar Sarki",
    Ward == "U/Shanu" ~ "Unguwar Shanu",
    Ward == "Ung Fatika" ~ "Unguwar Fatika",
    Ward == "Ung Juma" ~ "Unguwar Juma",
    Ward == "Ungwan Gabbas" ~ "Unguwar Gabas",
    Ward == "Ungwar Gaiya" ~ "Unguwar Gaya",
    Ward == "Ungwar Rimi" ~ "Unguwan Rimi",
    Ward == "Zagon Aya" ~ "Zangon Aya",
    Ward == "K/Wali South" ~ "Kauran Wali South",
    Ward == "T/Nupawa" ~ "Tudun Nupawa",
    Ward == "Riga Chikun" ~ "Rigachikun",
    TRUE ~ Ward)) %>%
  dplyr::select(population = `N_Nets`, Ward) %>%
  group_by(Ward) %>%
  summarise(Population = sum(population, na.rm = TRUE)) %>%
  ungroup()

# identify mismatches
itn_unique <- unique(kaduna_itn_clean$Ward)
extracted_unique <- unique(kaduna_extracted_data$WardName)
missing_in_extracted <- setdiff(itn_unique, extracted_unique)
missing_in_itn <- setdiff(extracted_unique, itn_unique)
cat("Wards in ITN data but not in extracted data:\n")
print(missing_in_extracted)
cat("\nWards in extracted data but not in ITN data:\n")
print(missing_in_itn)

writexl::write_xlsx(kaduna_itn_clean, file.path(ITNDir, "cleaned", "pbi_distribution_Kaduna_clean.xlsx"))


## =========================================================================================================================================
### NIGER
## =========================================================================================================================================

# read in ITN data and extracted data
niger_itn_data <- readxl::read_excel(file.path(ITNDir, "pbi_distribution_Niger.xlsx"), sheet = 1)
niger_extracted_data <- read.csv(file.path(ExtractedDir, "Niger_wards_variables.csv"))

niger_itn_clean <- niger_itn_data %>%
  rename(population = `Sum of N_Nets`,
         Ward = `Row Labels`) %>%
  dplyr::select(population, Ward) %>%
  group_by(Ward) %>%
  summarise(Population = sum(population, na.rm = T)) %>%
  ungroup() %>%
  mutate(Ward = case_when(
    Ward == "Adunu" ~ "Adono",
    Ward == "Akare" ~ "Akari",
    Ward == "Anaba" ~ "Anaba/Ibelu West",
    Ward == "Badeggi" ~ "Badegi",
    Ward == "Bagama B" ~ "Bagama b",
    Ward == "Bonu" ~ "Bono",
    Ward == "Chanchaga" ~ "Chachanga",
    Ward == "Cheniyan" ~ "Chenian",
    Ward == "Ciki Gari" ~ "Cikin Gari",
    Ward == "Danrangi" ~ "Danragi",
    Ward == "Dokko" ~ "Doko",
    Ward == "Dokodza" ~ "Dokoza",
    Ward == "Ebbo/Gbachinku" ~ "Ebbo",
    Ward == "Ecwa/Gwada" ~ "Egwa/Gwada",
    Ward == "Edozhigi" ~ "Edozigi",
    Ward == "Fahzi" ~ "Fazhi",
    Ward == "Gazhe" ~ "Gazhe 1",
    Ward == "Gupa/Abugi" ~ "Gupa",
    Ward == "Gwarjiko" ~ "Gwarijiko",
    Ward == "Kafin Koro" ~ "Kafinkoro",
    Ward == "Kuchi Bussu" ~ "Kucibusu",
    Ward == "Kura" ~ "Kura/Auna East",
    Ward == "Kurebe/Kushaka" ~ "Kuregbe",
    Ward == "Kuso Tachin" ~ "Kusotacin",
    Ward == "Labohzi" ~ "Labozhi",
    Ward == "Lokogoma" ~ "Lokogwoma",
    Ward == "Magamadaji" ~ "Magamadaji/Ibelu North",
    Ward == "Mantabefyan" ~ "Manbe Tafyan",
    Ward == "Moregi" ~ "Muregi",
    Ward == "Muye" ~ "Muye/Egba",
    Ward == "Busuru" ~ "Bunsuru",
    Ward == "Dan Dauda" ~ "Dandaudu",
    Ward == "Dasun" ~ "Dassun",
    Ward == "Gusoro/Zumba" ~ "Gussoro/Zumba",
    Ward == "Iku 1" ~ "Iku 1/Iku South I",
    Ward == "Iku 2" ~ "Iku 2/Iku South II",
    Ward == "Kafinkoro" ~ "Kafin Koro",
    Ward == "Kawna (Tegina West)" ~ "Tegina West",
    Ward == "Kucibusu" ~ "Kuchi Bussu",
    Ward == "Kundu (Gunna South)" ~ "Gunna South",
    Ward == "Kuregbe" ~ "Kurebe/Kushaka",
    Ward == "Kusheriki North" ~ "Kusherki North",
    Ward == "Kusheriki South" ~ "Kusherki South",
    Ward == "Kusotacin" ~ "Kuso Tachin",
    Ward == "Kwaki" ~ "Kwaki/Chikuba",
    Ward == "Madaka (Kakuri)" ~ "Kakuri",
    Ward == "Maikujeri (Kongoma West)" ~ "Kongoma West",
    Ward == "Manigi" ~ "Manigi/Dapangi/Makera",
    Ward == "Masaba A" ~ "Masaba A/Masaga I",
    Ward == "Masaga B" ~ "Masaga B/Masaga  II",
    Ward == "Muregi" ~ "Moregi",
    Ward == "Muye/Egba" ~ "Muye",
    Ward == "Nikuchi Tunga Mallam" ~ "Tungamallam/Nikuchi",
    Ward == "Nuwankota" ~ "Nuwan Kuta",
    Ward == "Pina" ~ "Pina/Kolu",
    Ward == "Rabba Ndayoko" ~ "Raba Ndayako",
    Ward == "Sabon Gari Ushe" ~ "Sabon Gari Wuse",
    Ward == "Sahonrami" ~ "Sahonrami/Saho-Rami",
    Ward == "Salka" ~ "Salka/Auna East Central",
    Ward == "Sarkin Pawa" ~ "Sarki Power",
    Ward == "Sommazhiko" ~ "Sojmajiko",
    Ward == "T/Wada North" ~ "T/Wada N",
    Ward == "T/Wada South" ~ "T/Wada S",
    Ward == "Tungan Jika" ~ "Tungajika/Auna South",
    Ward == "Umaru Majlgi A" ~ "Umaru Majagi A/Umaru Majigi  I",
    Ward == "Umaru Majlgi B" ~ "Umaru Majagi B/Umaru Majigi  II",
    Ward == "Yangalu" ~ "Yangalu/Ibelu East",
    Ward == "Yekila(Gunna)" ~ "Gunna Central",
    Ward == "Kafinkoro" ~ "Kafin Koro",
    TRUE ~ Ward
  )) %>%
  mutate(num = 1:n())

# identify mismatches
itn_unique <- unique(niger_itn_clean$Ward)
extracted_unique <- unique(niger_extracted_data$WardName)
missing_in_extracted <- setdiff(itn_unique, extracted_unique)
missing_in_itn <- setdiff(extracted_unique, itn_unique)
cat("Wards in ITN data but not in extracted data:\n")
print(missing_in_extracted)
cat("\nWards in extracted data but not in ITN data:\n")
print(missing_in_itn)

writexl::write_xlsx(niger_itn_clean, file.path(ITNDir, "cleaned", "pbi_distribution_Niger_clean.xlsx"))

## =========================================================================================================================================
### TARABA
## =========================================================================================================================================

# read in ITN data and extracted data
taraba_itn_data <- readxl::read_excel(file.path(ITNDir, "pbi_distribution_Taraba.xlsx"))
taraba_extracted_data <- read.csv(file.path(ExtractedDir, "Taraba_wards_variables.csv"))

# clean ITN data
taraba_itn_clean <- taraba_itn_data %>%
  rename(Ward = `Row Labels`, num_nets = `Sum of N_Nets`) %>%
  dplyr::select(Ward, num_nets) %>%
  group_by(Ward) %>%
  summarise(num_nets = sum(num_nets, na.rm = TRUE)) %>%
  mutate(Ward = case_when(
    Ward == "Dampar I" ~ "Dampar 1",
    Ward == "Dampar II" ~ "Dampar 2",
    Ward == "Dampar III" ~ "Dampar 3",
    Ward == "Lissam I" ~ "Lissam 1",
    Ward == "Lissam Ii" ~ "Lissam 2",
    Ward == "Nwonyo I" ~ "Nwonyo 1",
    Ward == "Nwonyo II" ~ "Nwonyo 2",
    Ward == "Rimi Uku I" ~ "Rimi Uku 1",
    Ward == "Rimi Uku II" ~ "Rimi Uku 2",
    Ward == "Sarkin Kudu I" ~ "Sarkin Kudu 1",
    Ward == "Sarkin Kudu II" ~ "Sarkin Kudu 2",
    Ward == "Sarkin Kudu III" ~ "Sarkin Kudu 3",
    Ward == "Kussum Badakoshi" ~ "Kussum/Badakoshi",
    Ward == "Panti-Sawa" ~ "Pantisawa",
    Ward == "Zangonkombi" ~ "Zangon Kombi",
    Ward == "Kassala-Sembe" ~ "Kachala-Sembe",
    Ward == "Kpambo Purfi" ~ "Kpambo Puri",
    Ward == "Kotsensi" ~ "Kosensi",
    Ward == "Mbamaga" ~ "Mbamnga",
    Ward == "Gangdole" ~ "Gang Dole",
    Ward == "Ganglari" ~ "Gang Lari",
    Ward == "Gangmata" ~ "Gang Mata",
    Ward == "Gangtiba" ~ "Gang Tiba",
    Ward == "Jenkaigama" ~ "Jen Kaigama",
    Ward == "Wurojam" ~ "Wuro Jam",
    Ward == "Danadda" ~ "Donadda",
    Ward == "Kpambo" ~ "Kpambo 1",
    Ward == "Gongon" ~ "Gongong",
    Ward == "Manang" ~ "Mannang",
    Ward == "Lama" ~ "Lamma",
    Ward == "Maidanu" ~ "Mai Idanu",
    TRUE ~ Ward
  )) %>%
  filter(Ward != "(blank)" &
           Ward != "Grand Total" &
           !is.na(Ward) &
           Ward != "")

# identify mismatches
itn_unique <- unique(taraba_itn_clean$Ward)
extracted_unique <- unique(taraba_extracted_data$WardName)
missing_in_extracted <- setdiff(itn_unique, extracted_unique)
missing_in_itn <- setdiff(extracted_unique, itn_unique)
cat("Wards in ITN data but not in extracted data:\n")
print(missing_in_extracted)
cat("\nWards in extracted data but not in ITN data:\n")
print(missing_in_itn)

writexl::write_xlsx(taraba_itn_clean, file.path(ITNDir, "cleaned", "pbi_distribution_Taraba_clean.xlsx"))

## =========================================================================================================================================
### YOBE
## =========================================================================================================================================

# read in ITN data and extracted data
yobe_itn_data <- readxl::read_excel(file.path(ITNDir, "pbi_distribution_Yobe.xlsx"))
yobe_extracted_data <- read.csv(file.path(ExtractedDir, "Yobe_wards_variables.csv"))

yobe_itn_clean <- yobe_itn_data %>%
  rename(population = `N_Nets`,
         Ward = `AdminLevel3`) %>%
  mutate(Ward = case_when(
    # Ward name standardization
    Ward == "Asheikiri" ~ "Asheikri",
    Ward == "Balanguwa" ~ "Bulanguwa",
    Ward == "Bare-Bari" ~ "Bare Bari",
    Ward == "Chikuiwa" ~ "Chukuriwa",
    Ward == "Chilariya" ~ "Chillariye",
    Ward == "Danani/Lawanti" ~ "Danani Lawanti",
    Ward == "Darin/Lang" ~ "Darin Langawa",
    Ward == "Dogo Nini" ~ "Dogon Nini",
    Ward == "Dole Machina" ~ "Dole",
    Ward == "Faji Ganari" ~ "Fajiganari",
    Ward == "Falimiram" ~ "Falimaram",
    Ward == "Fika/Anze" ~ "Fika Anze",
    Ward == "Gadaka/Shembire" ~ "Gadaka/She",
    Ward == "Goniri" ~ "Goneri",
    Ward == "Gotala" ~ "Gotala Gotumba",
    Ward == "Guba/Dapso" ~ "Guba Dapso",
    Ward == "Guji/Metalari" ~ "Guji Metalari",
    Ward == "Hausawa Asibity" ~ "Hausawa Asibiti",
    Ward == "Jajimaji" ~ "Jaji Maji",
    Ward == "Jawa/Garun Dole" ~ "Jawa Garun Dole",
    Ward == "Julluri" ~ "Juluri Damnawa",
    Ward == "Karasuwa Garin Guna" ~ "Karausuwa Garin Guna",
    Ward == "Koka/Sungul" ~ "Sungul Koka",
    Ward == "Kollere PHCC" ~ "Kollere Kafaje",
    Ward == "Konkonmma" ~ "Konkomma",
    Ward == "Koryel" ~ "Koriyel",
    Ward == "Kumagannam" ~ "Kumaganam",
    Ward == "Lantaiwa" ~ "Lantewa",
    Ward == "Ma'Anna" ~ "Ma'anna",
    Ward == "Maisandari Waziri Ibrahim" ~ "Maisandari/Waziri Ibrahim",
    Ward == "Majakura" ~ "Maja Kura",
    Ward == "Mari-Mari/Gud" ~ "Marmari Gudugurka",
    Ward == "Mazagon" ~ "Mozogun",
    Ward == "Moyori" ~ "Mayori",
    Ward == "Murfa Kalam" ~ "Murfakalam",
    Ward == "Ngalda/Dumbulwa" ~ "Ngalda/Dumb",
    Ward == "Njiwaji Gwange" ~ "Njiwaji/Gwange",
    Ward == "Sabon Gari Kanuri" ~ "S G Kanuri",
    Ward == "Sabongari" ~ "Sabon Gari",
    Ward == "Sasawa Kabaru" ~ "Sasawa/Kabaru",
    Ward == "Shoye/Garin Aba" ~ "Shoye",
    Ward == "Turmi/Maluri" ~ "Turmi Malori",
    Ward == "Yarimaram" ~ "Yerimaram",
    Ward == "Zangaya/Mazawun" ~ "Zangaya Mazawaun",
    TRUE ~ Ward)) %>%
  dplyr::select(population, Ward) %>%
  group_by(Ward) %>%
  summarise(Population = sum(population, na.rm = T)) %>%
  ungroup() %>%
  mutate(num = 1:n())

# identify mismatches
itn_unique <- unique(yobe_itn_clean$Ward)
extracted_unique <- unique(yobe_extracted_data$WardName)
missing_in_extracted <- setdiff(itn_unique, extracted_unique)
missing_in_itn <- setdiff(extracted_unique, itn_unique)
cat("Wards in ITN data but not in extracted data:\n")
print(missing_in_extracted)
cat("\nWards in extracted data but not in ITN data:\n")
print(missing_in_itn)

writexl::write_xlsx(yobe_itn_clean, file.path(ITNDir, "cleaned", "pbi_distribution_Yobe_clean.xlsx"))
