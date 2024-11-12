## malaria comparability risk scores

getwd()
source("~/NMEP_classification/load_path.R", echo = T)

install.packages("gtsummary")
library(gtsummary)

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

#ibadan_data_bernard <- read_dta(file.path(IbadanFieldData, "ibadan_long_wetseason_household_members.dta"))

ibadan_data_clean <- ibadan_data %>% 
  dplyr::select(sn, hl1, hl4, hl5, hl6, #identifiers
                q302, #test result
                ward, settlement_type_new, enumaration_area, 
                ward_weight, ea_settlement_weight, overall_hh_weight, ind_weights_hh,  #weights
                nh101a, slept_under_net #net ownership/ use
                ) %>%
  filter(!is.na(settlement_type_new)) %>%  
  filter(!ward == "") %>%  #remove 425 ind.c ward info 
  mutate(unique_id = paste(sn, hl1, sep = "_")) %>% 
  rename(Ward = ward,
         settlement_type = settlement_type_new,
         ea = enumaration_area, 
         ea_weight = ea_settlement_weight,
         hh_weight = overall_hh_weight,
         ind_weight = ind_weights_hh,
         net_ownership = nh101a,
         net_use = slept_under_net) %>% 
  mutate(net_use = ifelse(is.na(net_use), 2, net_use))  #turn NAs to negative
 

#descriptive plots

#survey population 
surveyed_by_settlement <- ibadan_data_clean %>%
  group_by(Ward, settlement_type) %>%
  summarise(count_settlements = n()) %>%
  arrange(Ward)

print(sum(surveyed_by_settlement$count_settlements)) #9003

ggplot(surveyed_by_settlement, aes(x = Ward, y = count_settlements, fill = settlement_type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = count_settlements), 
            position = position_dodge(width = 0.9), 
            vjust = -0.3, 
            size = 3) +
  labs(title = "Survey Population Ibadan",
       x = "Ward", y = "Number", fill = "Settlement Type")+
  theme_manuscript()


#malaria prevalence/ test positivity rate

malaria_prevalence <- ibadan_data_clean %>%
  filter(!is.na(q302)) %>%  #remove non-tested individuals
  filter(!q302 == 3) %>%  #remove odd options
  #group_by(bi6, q302) %>%
  group_by(Ward, q302) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(Ward) %>%
  mutate(proportion = count / sum(count))
malaria_prevalence$q302 <- factor(malaria_prevalence$q302, levels = c(1, 2), labels = c("Positive", "Negative"))


ggplot(malaria_prevalence, aes(x = Ward, y = proportion, fill = q302)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = round(proportion, 3)), 
            position = position_stack(vjust = 0.5), size = 3) +
  scale_y_continuous(labels = scales::percent) +  
  labs(title = "Malaria Test Positivity Rate - Ibadan",
       x = "Ward", y = "Proportion of Test Results", fill = "Test Result") +
  theme_manuscript()


##ward tpr
ward_tpr <- malaria_prevalence %>%
  filter(q302 == "Positive") %>%  
  dplyr::select(Ward, tpr = proportion) 

household_tpr <- ibadan_data_clean %>%
  filter(!is.na(q302)) %>%
  filter(q302 != 3) %>% 
  group_by(sn) %>% 
  summarise(
    tested = n(),
    positive = sum(q302 == 1),
    negative = sum(q302 == 2),
    hh_weight = first(hh_weight) 
  ) %>% 
  mutate(tpr = positive / tested)


###########net ownership and net access

net_data <- ibadan_data_clean %>%
  filter(!is.na(net_ownership)) %>%  
  filter(net_ownership != 3) %>%  
  group_by(Ward) %>% #group by household? settlement_type_new
  summarise(total = n(),
    own_nets = sum(net_ownership == 1, na.rm = TRUE),
    use_nets = sum(net_use == 1 & net_ownership == 1, na.rm = TRUE))

print(sum(net_data$total))

#
net_data_long <- net_data %>%
  pivot_longer(cols = c(own_nets, use_nets), 
               names_to = "metric", 
               values_to = "count")

ggplot(net_data_long, aes(x = Ward, y = count, fill = metric)) +
  geom_bar(stat = "identity", position = "dodge") + 
  #facet_wrap(~ settlement_type_new) +
  labs(
    title = "Net Ownership and Net Use Ibadan",
    x = "Ward",
    y = "Count",
    fill = ""
  ) +
  theme_manuscript()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##
net_data_proportions <- net_data %>% 
  mutate(net_own_proportion = own_nets / total,
         net_use_proportion = use_nets/ own_nets,
         overall_net_use_proportion = use_nets/total,
         all = total/total) 

ggplot(net_data_proportions, aes(x = Ward)) +
  geom_line(aes(y = total, group = 1, color = "Individuals Surveyed"))+
  geom_line(aes(y = own_nets, group = 1, color = "Net Owners"))+
  geom_line(aes(y = use_nets, group = 1, color = "Net Users"))+
  labs(title = "Net Ownership and Usage in Ibadan",
       x = "Wards",
       y = "Number",
       colour = "Labels")+
  theme_manuscript()


##household 
household_net_data <- ibadan_data_clean %>%
  filter(!is.na(nh101a)) %>%  
  filter(nh101a != 3) %>%  
  group_by(sn, ward) %>% #group by household? settlement_type_new
  summarise(total = n(),
            own_nets = sum(nh101a == 1, na.rm = TRUE),
            use_nets = sum(slept_under_net == 1 & nh101a == 1, na.rm = TRUE)) %>% 
  mutate(net_own_proportion = own_nets / total,
         net_use_proportion = use_nets/ own_nets,
         overall_net_use_proportion = use_nets/total) 



############################################################################
###### REGRESSIONS####################


##ward summaries

ib_variables <- read.csv(file.path(OutputsDir, "NMEP Malaria Risk Scores", "ib_field_variables.csv"))

##unweighted
model <- glm(tpr ~ net_own_prop +overall_net_use_prop, 
             data = ib_variables)
summary(model)

p1 <- ggplot(ib_variables, aes(x = net_own_prop, y = tpr, colour = Ward)) +
  geom_point() + 
  geom_smooth(method = "glm", se = TRUE, color = "red") + 
  labs(title = "Relationship between Net Ownership and Calculated TPR (Unweighted)",
       x = "Net Ownership Proportion",
       y = "Test Positivity Rate (TPR)") +
  theme_manuscript()

p2 <- ggplot(ib_variables, aes(x = net_use_prop, y = tpr, colour = Ward)) +
  geom_point() + 
  geom_smooth(method = "glm", se = TRUE, color = "red") + 
  labs(title = "Relationship between Net Use and Calculated TPR (Unweighted)",
       x = "Net Use Proportion",
       y = "Test Positivity Rate (TPR)") +
  theme_manuscript()

grid.arrange(p1, p2)

## weighted

model0 <- glm(estimated_prev ~ net_own_prop + overall_net_use_prop, 
             data = ib_variables, family = "binomial", weights = ward_weight)
summary(model0)




###### using raw data/ individual level data

ibadan_data_clean$q302 <- as.factor(ibadan_data_clean$q302)  
ibadan_data_clean$net_ownership <- as.factor(ibadan_data_clean$net_ownership)  
ibadan_data_clean$net_use <- as.factor(ibadan_data_clean$net_use) 

ibadan_data_clean <- ibadan_data_clean %>%
  filter(!is.na(q302)) %>%
  filter(q302 != 3) 

model2 <- glm(q302 ~ net_ownership + net_use, 
             family = "binomial", 
             data = ibadan_data_clean)

summary(model2)

predicted_probabilities <- predict(model2, type = "response")
ibadan_data_clean$predicted_prob <- predicted_probabilities

#net ownership
ggplot(ibadan_data_clean, aes(x = as.factor(net_ownership), y = predicted_prob)) +
  geom_boxplot() + 
  labs(
    title = "Probability of Positive Test Result by Net Ownership",
    x = "Net Ownership",
    y = "Predicted Probability of Positive Test"
  ) +
  theme_manuscript()

#net use
ggplot(ibadan_data_clean, aes(x = as.factor(net_use), y = predicted_prob)) +
  geom_boxplot() + 
  labs(
    title = "Probability of Positive Test Result by Net Use",
    x = "Net Use",
    y = "Predicted Probability of Positive Test"
  ) +
  theme_manuscript()



#########################

#weighted unsummarized

model3 <- glm(q302 ~ net_ownership + net_use, data = ibadan_data_clean,
              family = "binomial", weights = ind_weight)

summary(model3)


individual <- svydesign(ids = ~ea,
                             weights = ~ind_weight,
                             data = ibadan_data_clean)


model4 <- svyglm(q302 ~ net_ownership + net_use,
              family = "binomial", design = individual)
summary(model4)


##weighted for each ward
agugu_data <- ibadan_data_clean %>% 
  filter(Ward == "AGUGU")

agugu <- svydesign(ids = ~ea,
                        weights = ~ind_weight,
                        data = agugu_data)

model_agugu <- svyglm(q302 ~ net_ownership + net_use,
                 family = "binomial", design = agugu)
summary(model_agugu)













































# model<- lm(estimated_prev ~ net_own_prop + overall_net_use_prop, 
#              data = ib_variables)
# summary(model)
# #not statistically significant, increased ownership decreases prev. but net use increases prev
# 
# 
# model1 <- glm(estimated_prev ~ tpr + net_own_prop + net_use_prop + overall_net_use_prop, 
#              data = ib_variables, family = "quasibinomial")
# summary(model1)
# #multicollinearity
# #correlation matrix - very high correlations
# cor(ib_variables[, c("tpr", "net_own_prop", "net_use_prop", "overall_net_use_prop")])
# 
# 
# model2 <- glm(estimated_prev ~ net_own_prop + net_use_prop, 
#              data = ib_variables, family = "quasibinomial")
# summary(model2)
# #not statistically significant similar to model
# 
# 
# model3 <- glm(estimated_prev ~ net_own_prop + overall_net_use_prop, 
#               data = ib_variables, family = "quasibinomial")
# summary(model3)
# tbl_regression(model3)
# 
# #not statistically significant, similar to model and model2
# 
# 
# 
# ##### weighted
# 
# 
# survey_design <- svydesign(ids = ~enumaration_area,
#                          weights = ~overall_hh_weight,
#                          data = ibadan_data_clean)
# 
# 
# net_data_weighted <- ibadan_data_clean %>%
#   filter(!is.na(nh101a)) %>%  
#   filter(nh101a != 3) %>%  
#   group_by(ward) %>%  
#   reframe(
#     weighted_own_nets = svytotal(~(nh101a == 1), survey_design, na.rm = T),  
#     weighted_use_nets = svytotal(~(slept_under_net == 1 & nh101a == 1), survey_design, na.rm = T)  
#     #weighted_own_prop = svymean(~(nh101a == 1), survey_design, na.rm = T),
#     #weighted_use_prop = svymean(~(slept_under_net == 1 & nh101a == 1), survey_design, na.rm = T)
#     )
# 
# 
# 
# ward_design <- svydesign(ids = ~ward, #~enumaration_area,sn
#                          weights = ~ward_weight, #ea_settlment_weight, ~overall_hh_weight
#                          data = ibadan_data_clean)
# 
# 
# ward_net_data_weighted <- ibadan_data_clean %>%
#   filter(!is.na(nh101a)) %>%  
#   filter(nh101a != 3) %>%  
#   group_by(ward) %>% 
#   reframe(total = n(),
#           own_nets = svytotal(~(nh101a == 1), ward_design, na.rm = T),
#           use_nets = svytotal(~(slept_under_net == 1 & nh101a == 1), ward_design, na.rm = T)) %>%  
#   mutate(net_own_proportion = own_nets / total,
#          net_use_proportion = use_nets/ own_nets,
#          overall_net_use_proportion = use_nets/total) 
# 
# 
# 
# household_net_data_weighted <- ibadan_data_clean %>%
#   filter(!is.na(nh101a)) %>%  
#   filter(nh101a != 3) %>%  
#   group_by(sn, ward) %>% 
#   reframe(total = n(),
#           own_nets = svytotal(~(nh101a == 1), survey_design),
#           use_nets = svytotal(~(slept_under_net == 1 & nh101a == 1), survey_design)) %>%  
#   mutate(net_own_proportion = own_nets / total,
#          net_use_proportion = use_nets/ own_nets,
#          overall_net_use_proportion = use_nets/total) 
# 
# 
# 
# ##simple linear regression
# 
# ibadan_data_clean$q302 <- as.factor(ibadan_data_clean$q302)
# 
# ibadan_data_clean$q302_binary <- ifelse(ibadan_data_clean$q302 == 1, 1, 0, na.rm = T)
# ibadan_data_clean$net_use_binary <- ifelse(ibadan_data_clean$slept_under_net == 1, 1, 0, na.rm = T)
# 
# model <- glm(q302_binary ~ net_use_binary, data = ibadan_data_clean, family = binomial)
# summary(model)
# 
# cor(ibadan_data_clean$q302_binary, ibadan_data_clean$net_use_binary, use = "complete.obs")

