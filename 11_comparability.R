# ==========================================================================================================================================
## Script Name: Malaria Comparability Risk Scores
## Purpose: Processes and analyzes data from Ibadan, focusing on malaria prevalence, net ownership, and net usage. 
# It includes data cleaning, descriptive plots, weighted calculations, and regression models to explore the relationships 
# between net ownership/use and malaria test positivity rates at the household and ward levels.
# ==========================================================================================================================================

getwd()
source("~/NMEP_classification/load_path.R", echo = T)

#Ibadan 

IbadanFieldData <- file.path(FieldDataDir, "240922_Ibadan_latest_data")

ibadan_data <- read_dta(file.path(IbadanFieldData, 
                                 "ibadan_long_wetseason_household_members_with_ind_nets.dta"))

ibadan_data_clean <- ibadan_data %>% 
  dplyr::select(sn, hl1, hl4, hl5, hl6, #identifiers
                q302, #test result
                ward, settlement_type_new, enumaration_area, 
                ward_weight, ea_settlement_weight, overall_hh_weight, ind_weights_hh,  #weights
                nh101a, slept_under_net #net ownership/ use
                ) %>%
  filter(!is.na(settlement_type_new)) %>%  
  filter(!ward == "") %>%  #remove 425 ind.c ward info 
  filter(!is.na(q302)) %>%  #remove non-tested individuals, included this line after running surveyed population
  mutate(unique_id = paste(sn, hl1, sep = "_")) %>% 
  rename(Ward = ward,
         settlement_type = settlement_type_new,
         ea = enumaration_area, 
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
    net_own == 0 ~ NA_real_, net_own == 1 ~ net_use)) %>%  #net use in those with nets
  mutate(net_use3 = case_when(
    net_own == 0 & net_use == 1 ~ NA_real_,
    net_own == 0 & net_use == 0 ~ 0,
    net_own == 1 ~ net_use))  ##net use cleaned for model

  # mutate(net_use2 = case_when(
  # net_own == 1 & net_use == 0 ~ 0,  
  # net_own == 0 & net_use == 0 ~ NA_real_, TRUE ~ net_use  ))
  # 

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
  group_by(Ward, malaria_positive) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(Ward) %>%
  mutate(proportion = count / sum(count))
malaria_prevalence$q302 <- factor(malaria_prevalence$q302, levels = c(1, 2), labels = c("Positive", "Negative"))


ggplot(malaria_prevalence, aes(x = Ward, y = proportion, fill = malaria_positive)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = round(proportion, 3)), 
            position = position_stack(vjust = 0.5), size = 3) +
  #scale_filled_continuous(labels = scales::percent) +  
  labs(title = "Malaria Test Positivity Rate - Ibadan",
       x = "Ward", y = "Proportion of Test Results", fill = "Test Result") +
  theme_manuscript()


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

net_data_kn <- ibadan_data_clean %>%
  filter(!is.na(net_ownership)) %>%  
  filter(net_ownership != 3) %>%  
  group_by(Ward) %>% #group by household? settlement_type_new
  summarise(total = n(),
    own_nets = sum(net_ownership == 1, na.rm = TRUE),
    use_nets = sum(net_use == 1 & net_ownership == 1, na.rm = TRUE))

print(sum(net_data$total))


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


###weighted calculations and Ibadan summary
design <- svydesign(
  id = ~sn + ea,
  strata = ~Ward + settlement_type,
  weights = ~hh_weight,
  data = ibadan_data_clean,
  nest = T
)

weighted_ward_tpr <- svyby(~malaria_positive, ~Ward, design,
                           svymean, na.rm = T)
weighted_ward_net_own <- svyby(~net_own, ~Ward, design,
                               svymean, na.rm = T)

weighted_ward_net_use_overall <- svyby(~net_use, ~Ward, design,
                     svymean, na.rm = T)

weighted_ward_net_use3 <- svyby(~net_use3, ~Ward, design, svymean, na.rm = T)

ibadan_list <- data.frame(Ward = c("AGUGU", "BASHORUN", "CHALLENGE", "OLOGUNERU"))
all_ib <- list(ibadan_list, weighted_ward_tpr, weighted_ward_net_own, weighted_ward_net_use3)
ibadan_summary <- reduce(all_ib, left_join, by = "Ward")
ibadan_summary <- ibadan_summary %>%
  mutate(Ward = str_to_title(Ward),
         Ward = ifelse(Ward == "Ologuneru", "Olopomewa", Ward)) 

write.csv(ibadan_summary, file.path(OutputsDir, "NMEP Malaria Risk Scores", "ibadan_field_variables.csv"))

  

################################################################################
########################### REGRESSIONS ########################################
################################################################################

ib_variables <- read.csv(file.path(OutputsDir, "NMEP Malaria Risk Scores", "ib_field_variables.csv"))


##unweighted
model <- glm(tpr ~ net_own_prop +overall_net_use_prop, 
             data = ib_variables)
summary(model)

## weighted

model0 <- glm(weighted_tpr ~ weighted_net_own + weighted_net_use_overall, 
              data = ib_variables, weights = ward_weight)
summary(model0)


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

#odds ratio

model_results <- broom::tidy(model) %>% 
  mutate(oddsratio = round(exp(estimate), 3),
         ci_low = round(exp(estimate - (1.96 * std.error)), 3),
         ci_high = round(exp(estimate + (1.96 * std.error)), 3),
         model = "unadjusted")


ggplot(model_results, aes(x = oddsratio, y = term))+
  geom_vline(aes(xintercept = 1), size =.25, linetype = "dashed")+
  geom_errorbarh(aes(xmax = ci_high, xmin = ci_low), size = .5, height =.2)+
  geom_point(size = 3.5)+
  labs(x = "Odds Ratio",
       y = "Predictors",
       title = "Odds Ratio (Ward Aggregate)")+
  theme_manuscript()
  


##### overall net use
model02 <- glm(malaria_positive ~ net_own + net_use, 
               family = "binomial", data = ibadan_data_clean)
summary(model02)

predict_data <- expand.grid(net_own = c(0, 1), net_use = c(0, 1))
predict_data$predicted_prob <- predict(model02, newdata = predict_data, type = "response")


ggplot(predict_data, aes(x = factor(net_own), y = predicted_prob, fill = factor(net_use))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Predicted Probability of Malaria Positivity",
       x = "Net Ownership (0 = No, 1 = Yes)",
       y = "Predicted Probability",
       fill = "Net Usage (0 = No, 1 = Yes)") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()


### net use particular
model03 <- glm(malaria_positive ~ net_own + net_use2, 
               family = "binomial", data = ibadan_data_clean)
summary(model03)

predict_data2 <- expand.grid(net_own = c(0, 1), net_use2 = c(0, 1))
predict_data2$predicted_prob <- predict(model03, newdata = predict_data2, type = "response")

ggplot(predict_data2, aes(x = factor(net_own), y = predicted_prob, fill = factor(net_use2))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Predicted Probability of Malaria Positivity",
       x = "Net Ownership (0 = No, 1 = Yes)",
       y = "Predicted Probability",
       fill = "Net Usage (0 = No, 1 = Yes)") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()

###########
#weighted

model3 <- glm(q302 ~ net_ownership + net_use, data = ibadan_data_clean,
              family = "binomial", weights = ind_weight)

summary(model3)


individual <- svydesign(ids = ~ea,
                             weights = ~ind_weight,
                             data = ibadan_data_clean)


model4 <- svyglm(q302 ~ net_ownership + net_use,
              family = "binomial", design = individual)
summary(model4)


model03 <- glm(malaria_positivity)

####################################################################################
# Association Analysis:  Odds Ratio

# malaria_reg <- ibadan_data_clean %>% 
#   dplyr::select(net_use, net_own, net_use2, malaria_positive, ind_weight)


model04 <- glm(malaria_positive ~ net_own + net_use3, 
               family = binomial(link = "logit"), data = ibadan_data_clean, weights = ind_weight)

summary(model04)

model04_results <- broom::tidy(model04) %>% 
  mutate(oddsratio = round(exp(estimate), 3),
         ci_low = round(exp(estimate - (1.96 * std.error)), 3),
         ci_high = round(exp(estimate + (1.96 * std.error)), 3),
         model = "adjusted",
         method = "glm")

model05 <- glm(malaria_positive ~ net_own, family = binomial(link = "logit"),
               data = ibadan_data_clean, weights = ind_weight)

model05_results <- broom::tidy(model05) %>% 
  mutate(oddsratio = round(exp(estimate), 3),
         ci_low = round(exp(estimate - (1.96 * std.error)), 3),
         ci_high = round(exp(estimate + (1.96 * std.error)), 3),
         model = "unadjusted",
         method = "glm")

print(model05)


model06 <- glm(malaria_positive ~ net_use3, 
               family = binomial(link = "logit"), data = ibadan_data_clean, weights = ind_weight)
summary(model06)


model06_results <- broom::tidy(model06) %>% 
  mutate(oddsratio = round(exp(estimate), 3),
         ci_low = round(exp(estimate - (1.96 * std.error)), 3),
         ci_high = round(exp(estimate + (1.96 * std.error)), 3),
         model = "unadjusted",
         method = "glm")

##with svyglm()

malaria_reg2<- ibadan_data_clean %>% 
  dplyr::select(net_use, net_own, net_use2, net_use3, malaria_positive, ind_weight, Ward, 
                settlement_type, sn, ea, hh_weight)


indvidual_design <- svydesign(
  id = ~sn + ea,
  strata = ~Ward + settlement_type,
  weights = ~ind_weight,
  data = malaria_reg2,
  nest = T
)

model07 <- svyglm(malaria_positive ~ net_own + net_use3,
                  family = "binomial", design = indvidual_design)

model07_results <- broom::tidy(model07) %>% 
  mutate(oddsratio = round(exp(estimate), 3),
         ci_low = round(exp(estimate - (1.96 * std.error)), 3),
         ci_high = round(exp(estimate + (1.96 * std.error)), 3),
         model = "adjusted",
         method = "svyglm")

model08 <- svyglm(malaria_positive ~ net_own,
                  family = "binomial", design = indvidual_design)

model08_results <- broom::tidy(model08) %>% 
  mutate(oddsratio = round(exp(estimate), 3),
         ci_low = round(exp(estimate - (1.96 * std.error)), 3),
         ci_high = round(exp(estimate + (1.96 * std.error)), 3),
         model = "unadjusted",
         method = "svyglm")

model09 <- svyglm(malaria_positive ~ net_use3,
                  family = "binomial", design = indvidual_design)

model09_results <- broom::tidy(model09) %>% 
  mutate(oddsratio = round(exp(estimate), 3),
         ci_low = round(exp(estimate - (1.96 * std.error)), 3),
         ci_high = round(exp(estimate + (1.96 * std.error)), 3),
         model = "unadjusted",
         method = "svyglm")


all <- rbind(model04_results, model05_results, model06_results, model07_results, model08_results,
             model09_results)

ggplot(all %>% 
         filter(term != "(Intercept)"), aes(x = oddsratio, y = term, colour = method)) +
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = ci_high, xmin = ci_low), size = .5, height =.2, na.rm = T, position = position_dodge(width = 0.5)) +
  geom_point(size = 2, alpha = 1, na.rm = T, position = position_dodge(width = 0.5)) +
  facet_wrap(~model)+ 
  labs(x = "Odds Ratio", 
       y = "Predictors",
       title = "Odds Ratio of Net Ownership and Net Use on Positive Malaria Test in Ibadan",
       colour = "Method")+
  theme_manuscript()


################
#odds ratio by ward
ward_data <- ibadan_data_clean %>% group_split(Ward)
names(ward_data) <- c("Agugu", "Bashorun", "Challenge", "Olopomewa")  

ward_results <- ward_data %>%
  map_df(~ get_model_results(.x), .id = "Ward")


ggplot(ward_results, aes(x = oddsratio, y = term, color = model)) +
  geom_vline(aes(xintercept = 1), size = 0.25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = ci_high, xmin = ci_low), size = 0.5, height = 0.2, position = position_dodge(width = 0.5)) +
  geom_point(size = 2, position = position_dodge(width = 0.5)) +
  facet_wrap(~Ward, scales = "free") + 
  labs(title = "Odds Ratios for Net Ownership and Net Use by Ward",
       x = "Odds Ratio",
       y = "Predictor",
       color = "Type") +
  theme_manuscript()


ggplot(ward_results %>% 
         filter(term != "(Intercept)"), 
       aes(x = oddsratio, y = Ward, colour = term))+
  geom_point(size = 2, position = position_dodge(width = 0.5))+
  geom_vline(aes(xintercept = 1), size = 0.25, linetype = "dashed")+
  geom_errorbarh(aes(xmax = ci_high, xmin = ci_low), size = 0.5, height = 0.2, position = position_dodge(width = 0.5))+
  facet_grid(~model, scales = "free")+
  labs(title = "Net Ownership and Net Use on Positive Malaria Test in Ibadan Wards",
       x = "Odds Ratio",
       y = "Ward",
       color = "Type") +
  theme_manuscript()


### settlement type

settlement_data <- ibadan_data_clean %>% group_split(settlement_type)
names(settlement_data) <- c("Formal", "Informal", "Slum")  

settlement_results <- settlement_data %>%
  map_df(~ get_model_results(.x), .id = "settlement_type")


ggplot(settlement_results, aes(x = oddsratio, y = term, color = model)) +
  geom_vline(aes(xintercept = 1), size = 0.25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = ci_high, xmin = ci_low), size = 0.5, height = 0.2, position = position_dodge(width = 0.5)) +
  geom_point(size = 2, position = position_dodge(width = 0.5)) +
  facet_wrap(~settlement_type, scales = "free") + 
  labs(title = "Odds Ratios for Net Ownership and Net Use by Settlement Type",
       x = "Odds Ratio",
       y = "Predictor",
       color = "Type") +
  theme_manuscript()


ggplot(settlement_results %>% 
         filter(term != "(Intercept)"), 
       aes(x = oddsratio, y = settlement_type, colour = term))+
  geom_point(size = 2, position = position_dodge(width = 0.5))+
  geom_vline(aes(xintercept = 1), size = 0.25, linetype = "dashed")+
  geom_errorbarh(aes(xmax = ci_high, xmin = ci_low), size = 0.5, height = 0.2, position = position_dodge(width = 0.5))+
  facet_grid(~model, scales = "free")+
  labs(title = "Net Ownership and Net Use on Positive Malaria Test in Ibadan Settlements",
       x = "Odds Ratio",
       y = "Settlement Type",
       color = "Type") +
  theme_manuscript()




























