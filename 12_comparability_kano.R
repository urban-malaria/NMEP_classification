# ==========================================================================================================================================
# Script Name: Kano Comparability
# Authors: Hephzibah, Grace
# Purpose: Processes and analyzes data related to malaria in Kano, focusing on net use and ownership, survey population, and malaria prevalence. 
# Creates several visualizations, including the malaria test positivity rate by ward and net ownership/access by settlement type. 
# Calculates weighted values for each ward and conducts regression analyses on net use and malaria positivity, focusing on different 
# factors such as settlement type and ward. Finally, the code combines shapefiles for Kano and Ibadan to visualize geographic data.
# ==========================================================================================================================================

rm(list = ls())

source("~/NMEP_classification/load_path.R", echo = T)

# grace
source("/Users/grace/Desktop/UMP/NMEP_classification_my_fork/load_path.R", echo = T)

KanoFieldData <- file.path(FieldDataDir, "241106_Kano_latest_data")
KanoDry <- file.path(FieldDataDir, "Kano Dry Season Data_latest_Nov2024",
                     "Kano dry season survey data")

kano_data_dry<- read_dta(file.path(KanoDry, "long_dryseason_household_membersV00.dta"))

####################################################################################
## Manipulate column did you sleep under nets

# read in data
kano_data <- read_dta(file.path(FieldDataDir, "Kano Wet Season Data Sept. 2024",
                                "long_wetseason_household_membersV2_678_cols.dta"))

kn_household_data <- read_dta(file.path(FieldDataDir, "Kano Wet Season Data Sept. 2024",
                                "KN_Merged_Long_Data_with_Net_use.dta"))

max_splits <- max(str_count(kn_household_data$nh114a, "\\s*(,|AND|and|/|&)\\s*") + 1)


# Separate the columns nh114 and nh114a directly
# Process the data with corrected separate statements
kn_household_slept_net <- kn_household_data %>%
  rename(redcap_repeat_instrument = redcap_repeat_instrument_y...679,
         redcap_repeat_instance = redcap_repeat_instance_y...680) %>% 
  dplyr::select(sn, redcap_repeat_instrument, redcap_repeat_instance, nh113, nh113a, nh114, nh114a) %>% 
  mutate(nh114a = ifelse(sn == 11010, "1", nh114a),
         nh114a = ifelse(nh114a == "83", "3", nh114a)) %>% 
  separate(nh114, into = paste0("nh114_part", 1:max_splits), sep = ",", fill = "right", extra = "drop") %>%
  separate(nh114a, into = paste0("nh114a_part", 1:max_splits), sep = "\\s*( |,|AND|and|/|&)\\s*", fill = "right", extra = "drop") %>%
  dplyr::select(-c(nh114_part1, nh114_part2, nh114_part3, nh114_part4, nh114_part5))


kn_household_slept_net_melted <- reshape2::melt(kn_household_slept_net, id.vars = c("sn", "redcap_repeat_instrument", 
                                                                          "redcap_repeat_instance", "nh113", "nh113a")) 

kn_household_slept_net_melted$new_line_number <- as.numeric(kn_household_slept_net_melted$value) 

kn_household_slept_net_melted <- kn_household_slept_net_melted %>% 
  mutate(slept_under_net = ifelse(!is.na(new_line_number), 1, 0)) %>% 
  drop_na(nh113)

net_sleeping_columns <- left_join(kano_data, kn_household_slept_net_melted, by = c("sn" = "sn", 
                                                                                   "redcap_repeat_instance" ="new_line_number"))

#cleaning names for saving 
net_sleeping_columns_filtered <- net_sleeping_columns[, !grepl("\\.y", names(net_sleeping_columns))]

names(net_sleeping_columns_filtered)[grepl("\\.x", names(net_sleeping_columns_filtered))] <- 
  gsub("\\.x", "", names(net_sleeping_columns_filtered)[grepl("\\.x", names(net_sleeping_columns_filtered))])

names(net_sleeping_columns_filtered) <- gsub("[^[:alnum:]]", "_", names(net_sleeping_columns_filtered))


write_dta(net_sleeping_columns_filtered, file.path(KanoFieldData, "kano_long_wetseason_household_members_with_ind_nets.dta") ) 

##################################################################################################################################################
########################## COMPARABILITY ANALYSIS #####################################################
####################################################################################################

kano_data_full <- read_dta(file.path(KanoFieldData, "kano_long_wetseason_household_members_with_ind_nets.dta"))

kano_data_clean <- kano_data_full %>% 
  dplyr::select(sn, hl1, hl4, hl5, hl6, #identifiers
                q302, #test result
                Wardn, #settle_type, 
                settlement1, enumeration_area, 
                ward_weight, ea_settlement_weight, overall_hh_weight, ind_weights_hh,  #weights
                nh101a, nh105, slept_under_net, #net ownership/ use
                bi12i # interview visit 1 date
  ) %>%
  filter(!is.na(settlement1)) %>%  
  filter(!Wardn == "") %>%  
  filter(!is.na(q302)) %>%  #remove non-tested individuals, included this line after running surveyed population
  mutate(unique_id = paste(sn, hl1, sep = "_")) %>% 
  rename(Ward = Wardn,
         settlement_type = settlement1,
         ea = enumeration_area, 
         ea_weight = ea_settlement_weight,
         hh_weight = overall_hh_weight,
         ind_weight = ind_weights_hh,
         net_ownership = nh101a,
         net_use = slept_under_net
  ) %>% 
  mutate(net_use = ifelse(is.na(net_use), 0, net_use),
         net_own = ifelse(net_ownership == 1, 1, 0),
         malaria_positive = ifelse(q302 == 1, 1, 0)) %>% #convert all to 0 and 1
  mutate(net_use2 = case_when(
    net_own == 0 ~ NA_real_, net_own == 1 ~ net_use)) %>% 
  mutate(net_use3 = case_when(
    net_own == 0 & net_use == 1 ~ NA_real_,
    net_own == 0 & net_use == 0 ~ 0,
    net_own == 1 ~ net_use))  ##net use cleaned for model


#Surveyed Population

surveyed_by_settlement_kn <- kano_data_clean %>%
  group_by(Ward, settlement_type) %>%
  summarise(count_settlements = n()) %>%
  arrange(Ward)

print(sum(surveyed_by_settlement_kn$count_settlements)) #17213

ggplot(surveyed_by_settlement_kn, aes(x = Ward, y = count_settlements, fill = settlement_type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = count_settlements), 
            position = position_dodge(width = 0.9), 
            vjust = -0.3, 
            size = 3) +
  labs(title = "Survey Population Kano",
       x = "Ward", y = "Number", fill = "Settlement Type")+
  theme_manuscript()

#malaria prevalence by ward

malaria_prevalence_kn <- kano_data_clean %>%
  filter(!is.na(q302)) %>%  #remove non-tested individuals
  filter(!q302 == 3) %>%  #remove odd options
  group_by(Ward, malaria_positive) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(Ward) %>%
  mutate(proportion = count / sum(count))
malaria_prevalence_kn$malaria_positive <- factor(malaria_prevalence_kn$malaria_positive, levels = c(0, 1), labels = c("Negative", "Positive"))


ggplot(malaria_prevalence_kn, aes(x = Ward, y = proportion, fill = malaria_positive)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = round(proportion, 3)), 
            position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Malaria Test Positivity Rate - Kano",
       x = "Ward", y = "Proportion of Test Results", fill = "Test Result") +
  theme_manuscript()

##net ownership/ access

net_data_kn <- kano_data_clean %>%
  filter(!is.na(net_ownership)) %>%  
  filter(net_ownership != 3) %>%  
  group_by(Ward) %>% #group by household? settlement_type_new
  summarise(total = n(),
            own_nets = sum(net_ownership == 1, na.rm = TRUE),
            use_nets = sum(net_use == 1 & net_ownership == 1, na.rm = TRUE))

print(sum(net_data_kn$total)) #17159

net_data_long_kn <- net_data_kn %>%
  pivot_longer(cols = c(own_nets, use_nets), 
               names_to = "metric", 
               values_to = "count")

ggplot(net_data_long_kn, aes(x = Ward, y = count, fill = metric)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Net Ownership and Net Use Kano",
    x = "Ward",
    y = "Count",
    fill = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme_manuscript()


##################Get weighted values for each ward ##################################

design <- svydesign(
  id = ~sn + ea,
  strata = ~Ward + settlement_type,
  weights = ~hh_weight,
  data = kano_data_clean,
  nest = T
)

#prevalence
weighted_ward_tpr_kn <- svyby(~malaria_positive, ~Ward, design,
                           svymean, na.rm = T)

#net ownership
weighted_ward_net_own_kn <- svyby(~net_own, ~Ward, design,
                               svymean, na.rm = T)

#net use in people that own nets
weighted_ward_net_use3_kn <- svyby(~net_use3, ~Ward, design, svymean, na.rm = T) #net use in people that own nets


#combine
kano_list <- data.frame(Ward = c("Dorayi", "Fagge", "Giginyu", "Gobirawa", "Zango"))
all_kn <- list(kano_list, weighted_ward_tpr_kn, weighted_ward_net_own_kn, weighted_ward_net_use3_kn)
kano_summary <- reduce(all_kn, left_join, by = "Ward")
kano_summary <- kano_summary %>% 
  mutate(Ward = ifelse(Ward == "Fagge", "Fagge D2", Ward))

write.csv(kano_summary, file.path(OutputsDir, "NMEP Malaria Risk Scores", "kano_field_variables.csv"))


############################REGRESSIONS##########################################

###### WEIGHTED ODDS########

kano_individuals <- get_model_results(kano_data_clean)

kano_individuals <- kano_individuals %>% 
  filter(term != "(Intercept)")

ggplot(kano_individuals, aes(x = oddsratio, y = term, colour = model)) +
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = ci_high, xmin = ci_low), size = .5, height =.2, position = position_dodge(width = 0.5)) +
  geom_point(size = 2, alpha = 0.5, position = position_dodge(width = 0.5)) +
  #facet_wrap(~model)+ 
  labs(x = "Odds Ratio",
       y = "Predictors",
       title = "Net Use and Net Ownership on Positive Malaria Test in Kano",
       colour = "Model")+
  theme_manuscript()

#by ward

ward_data_kn <- kano_data_clean %>% group_split(Ward)
names(ward_data_kn) <- c("Dorayi", "Fagge", "Giginyu", "Gobirawa", "Zango")  

ward_results_kn <- ward_data_kn %>%
  map_df(~ get_model_results(.x), .id = "Ward")


ggplot(ward_results_kn %>% 
         filter(term != "(Intercept)"), 
       aes(x = oddsratio, y = Ward, colour = term))+
  geom_point(size = 2, position = position_dodge(width = 0.5))+
  geom_vline(aes(xintercept = 1), size = 0.25, linetype = "dashed")+
  geom_errorbarh(aes(xmax = ci_high, xmin = ci_low), size = 0.5, height = 0.2, position = position_dodge(width = 0.5))+
  facet_grid(~model, scales = "free")+
  labs(title = "Net Ownership and Net Use on Positive Malaria Test in Kano Wards",
       x = "Odds Ratio",
       y = "Ward",
       color = "Type") +
  theme_manuscript()

#by settlement type

settle_data_kn <- kano_data_clean %>% group_split(settlement_type)
names(settle_data_kn) <- c("Formal", "Informal", "Slum")  

settle_results_kn <- settle_data_kn %>%
  map_df(~ get_model_results(.x), .id = "settlement_type")


ggplot(settle_results_kn %>% 
         filter(term != "(Intercept)"), 
       aes(x = oddsratio, y = settlement_type, colour = term))+
  geom_point(size = 2, position = position_dodge(width = 0.5))+
  geom_vline(aes(xintercept = 1), size = 0.25, linetype = "dashed")+
  geom_errorbarh(aes(xmax = ci_high, xmin = ci_low), size = 0.5, height = 0.2, position = position_dodge(width = 0.5))+
  facet_grid(~model, scales = "free")+
  labs(title = "Net Ownership and Net Use on Positive Malaria Test in Kano Settlements",
       x = "Odds Ratio",
       y = "Settlement Type",
       color = "Type") +
  theme_manuscript()

#### combine Kano and Ibadan shapefiles
ibadan_shp <- st_read(file.path(ShpfilesDir, "IB_4_Wards", "Ibadan_4.shp"))
kano_shp <- st_read(file.path(ShpfilesDir, "KN_5_Wards", "Kano_5.shp"))

combined_shp <- rbind(ibadan_shp, kano_shp)
ggplot()+
  geom_sf(data = combined_shp, aes(geometry = geometry))+
  #coord_equal()+
  map_theme()

st_write(combined_shp, file.path(ShpfilesDir, "Kano_Ibadan", "Kano-Ibadan.shp", append = T))


########################## Calculate combined wet and dry season TPR####################

kano_data_dry_cols <- kano_data_dry %>% 
  dplyr::select(sn, hl1, Ward, bi3, q301, q302, ea_name_new, hhs_weights)


kano_data_wet_cols <- kano_data_full %>% 
  dplyr::select(sn, hl1, Ward, )

### weighted
design <- svydesign(
  id = ~sn + ea,
  strata = ~Ward + settlement_type,
  weights = ~hh_weight,
  data = kano_data_clean,
  nest = T
)

#prevalence
weighted_ward_tpr_kn <- svyby(~malaria_positive, ~Ward, design,
                              svymean, na.rm = T)
st_write(combined_shp, file.path(ShpfilesDir, "Kano_Ibadan", "Kano-Ibadan.shp", append = T))

