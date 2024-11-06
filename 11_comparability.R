## malaria comparability risk scores

getwd()
source("~/NMEP_classification/load_path.R", echo = T)


# install.packages("svyby")
# library(svyby)

library(labelled)
library(srvyr)
library(survey)

FieldDataDir <- file.path(DataDir, "nigeria/kano_ibadan_epi/new_field_data")
IbadanFieldData <- file.path(FieldDataDir, "240922_Ibadan_latest_data")
                          
#load shapefiles

# kano_shp <- st_read(file.path(ShpfilesDir, "Kano", "Kano.shp"))
# ibadan_shp <- st_read(file.path(ShpfilesDir, "Ibadan", "Ibadan.shp"))


#custom function
convert_labels_to_factors <- function(df) {
  df %>%
    mutate(across(where(is.labelled), as_factor))
}

#Ibadan                          
ibadan_data <- read_dta(file.path(IbadanFieldData, 
                                 "ibadan_long_wetseason_household_members_with_ind_nets.dta"))

ibadan_data_clean <- ibadan_data %>% 
  dplyr::select(sn, hl1, hl4, hl5, hl6, hl9,
                q300i, q301, q302, ln, rdt,
                ward, enumaration_area, ward_weight, ea_settlement_weight, 
                hhs_weights, hh_number, overall_hh_weight,
                bi1, name_lga, bi2, bi3___1, bi3___2, bi3___3, bi5, bi6, bi7_long, bi7_lat,
                nh101a, nh105, nh106, nh107, nh107a, count, net_ownership_and_ch_v_3, number_nets, slept_under_net) %>%
  mutate(settlement_type = case_when(
    bi3___1 == 1 ~ "Formal",
    bi3___2 == 1 ~ "Informal",
    bi3___3 == 1 ~ "Slum",
    TRUE ~ NA)) %>% 
  dplyr::select(-bi3___1, -bi3___2, -bi3___3) %>% 
  filter(!is.na(settlement_type)) %>%  #remove 15
  filter(!ward == "") %>%  #remove 425 ind.c ward info + 9 c settlement
  mutate(unique_id = paste(sn, hl1, sep = "_"))

#descriptive plots

#survey population
surveyed_by_settlement <- ibadan_data_clean %>%
  group_by(ward, settlement_type) %>%
  summarise(count_settlements = n()) %>%
  arrange(ward)


ggplot(surveyed_by_settlement, aes(x = ward, y = count_settlements, fill = settlement_type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = count_settlements), 
            position = position_dodge(width = 0.9), 
            vjust = -0.3, 
            size = 3) +
  labs(title = "Survey Population Ibadan",
       x = "Ward", y = "Number", fill = "Settlement Type")+
  theme_manuscript()



#malaria prevalence

malaria_prevalence <- ibadan_data_clean %>%
  filter(!is.na(q302))%>%
  filter(!q302 == 3) %>% 
  group_by(ward, q302) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(ward) %>%
  mutate(proportion = count / sum(count))
malaria_prevalence$q302 <- as.factor(malaria_prevalence$q302)


ggplot(malaria_prevalence, aes(x = ward, y = proportion, fill = q302)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = count), 
            position = position_stack(vjust = 0.5), size = 3) +
  scale_y_continuous(labels = scales::percent) +  
  labs(title = "Malaria Test Positivity Rate - Ibadan",
       x = "Ward", y = "Proportion of Test Results", fill = "Test Result") +
 theme_manuscript()


#net ownership and net access

net_data <- ibadan_data_clean %>%
  filter(!is.na(nh101a)) %>%  
  filter(nh101a != 3) %>%  
  group_by(ward, settlement_type) %>%
  summarise(total = n(),
    own_nets = sum(nh101a == 1, na.rm = TRUE),
    use_nets = sum(slept_under_net == 1 & nh101a == 1, na.rm = TRUE)) 
  

net_data_long <- net_data %>%
  pivot_longer(cols = c(own_nets, use_nets), 
               names_to = "metric", 
               values_to = "count")

ggplot(net_data_long, aes(x = ward, y = count, fill = metric)) +
  geom_bar(stat = "identity", position = "dodge") + 
  facet_wrap(~ settlement_type) +
  labs(
    title = "Net Ownership and Net Use Ibadan",
    x = "Ward",
    y = "Count",
    fill = ""
  ) +
  theme_manuscript()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#net_data_proportions
net_data_proportions <- net_data %>% 
  mutate(net_own_proportion = own_nets / sum(own_nets)) %>% 
  mutate(
    net_ownership_percent = (own_nets / total) * 100,
    net_use_percent = (use_nets / own_nets) * 100)

##### weighted


#weighted
survey_design <- svydesign(ids = ~enumaration_area, 
                           weights = ~overall_hh_weight, 
                           data = ibadan_data_clean)

surveyed_by_settlement_weighted <- svyby(
  ~enumaration_area, 
  ~ward + settlement_type, 
  survey_design, 
  svytotal, 
  na.rm = TRUE
)

# Convert to a data frame for plotting
surveyed_by_settlement_weighted_df <- as.data.frame(surveyed_by_settlement_weighted)

names(surveyed_by_settlement_weighted_df)[names(surveyed_by_settlement_weighted_df) == "(enumaration_area)"] <- "count_settlements"


ggplot(surveyed_by_settlement_weighted_df, aes(x = ward, y = count_settlements, fill = settlement_type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  # geom_text(aes(label = count_settlements), 
  #           position = position_dodge(width = 0.9), 
  #           vjust = -0.3, 
  #           size = 3) +
  labs(title = "Survey Population Ibadan Weighted",
       x = "Ward", y = "Number", fill = "Settlement Type")+
  theme_manuscript()

