# ==========================================================================================================================================
# Script Name: Kano Comparability in Children Under 5
# Author: Grace Legris
# Purpose: Conduct malaria prevalence analyses
# ==========================================================================================================================================

rm(list = ls())

## =========================================================================================================================================
### Setup
## =========================================================================================================================================

#source("~/NMEP_classification/load_path.R", echo = T)

# grace path
source("/Users/grace/Desktop/UMP/NMEP_classification_my_fork/load_path.R", echo = T)

KanoFieldData <- file.path(FieldDataDir, "241106_Kano_latest_data")

# read in data
kano_data <- read_dta(file.path(FieldDataDir, "Kano Wet Season Data Sept. 2024",
                                "long_wetseason_household_membersV2_678_cols.dta"))

kn_household_data <- read_dta(file.path(FieldDataDir, "Kano Wet Season Data Sept. 2024",
                                        "KN_Merged_Long_Data_with_Net_use.dta"))

max_splits <- max(str_count(kn_household_data$nh114a, "\\s*(,|AND|and|/|&)\\s*") + 1)

# data cleaning
kano_data_full <- read_dta(file.path(KanoFieldData, "kano_long_wetseason_household_members_with_ind_nets.dta"))

kano_data_clean <- kano_data_full %>% 
  dplyr::select(sn, hl1, hl4, hl5, hl6, #identifiers
                q302, #test result
                Wardn, #settle_type, 
                settlement1, enumeration_area, 
                ward_weight, ea_settlement_weight, overall_hh_weight, ind_weights_hh,  #weights
                nh101a, nh105, slept_under_net, #net ownership/ use
                bi12i, # interview visit 1 date
                bi9
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


## =========================================================================================================================================
### GRACE'S CODE: Analyze U5 Malaria Prevalence and Examine Associations with Net Use, Net Ownership, and Nighttime Lights
### UNWEIGHTED ANALYSIS
## =========================================================================================================================================

# subset cleaned Kano data for children under 5 (under 60 months) only
# use the date of birth, along with the date of interview to calculate the age 0-60 months
# also exclude observations where DOB was reported as after the first interview date
kano_data_clean_u5 <- kano_data_clean %>%
  mutate(
    age_years = as.numeric((bi9 - hl6) / 365) 
  ) %>%
  filter(age_years < 5 & age_years > 0)
  # filter(age_years < 0) %>%
  # dplyr::select(hl5, hl6, bi9, age_years)

## -----------------------------------------------------------------------------------------------------------------------------------------
### Malaria TPR Among U5 Children
## -----------------------------------------------------------------------------------------------------------------------------------------

# calculate malaria prevalence by ward (children under 5 only)
malaria_prevalence_kn_u5 <- kano_data_clean_u5 %>%
  filter(!is.na(q302)) %>%  # remove individuals without malaria test results
  filter(q302 != 3) %>%  # remove invalid or odd response options
  group_by(Ward, malaria_positive) %>%  # group data by ward and test result
  summarise(count = n()) %>%  # count the number of cases in each group
  ungroup() %>%  # ungroup to avoid issues with further calculations
  group_by(Ward) %>%  # group by ward again to calculate proportions
  mutate(proportion = count / sum(count))  # calculate the proportion for each result type

# convert malaria_positive to a factor with descriptive labels
malaria_prevalence_kn_u5$malaria_positive <- factor(
  malaria_prevalence_kn_u5$malaria_positive,
  levels = c(0, 1),
  labels = c("Negative", "Positive")
)

# create percentage label for plot
malaria_prevalence_kn_u5 <- malaria_prevalence_kn_u5 %>%
  mutate(
    label = proportion * 100
  ) %>%
  mutate(
    label = paste0(round(label, 1), "%")
  )

color = c("#f2a5a1", "#c55c80")

# plot malaria test positivity rate by ward (children under 5 only)
tpr_plot <- ggplot(malaria_prevalence_kn_u5, aes(x = Ward, y = proportion, fill = malaria_positive)) +
  geom_bar(stat = "identity", position = "stack") +  # stacked bar chart
  scale_fill_manual(name = "Test Result", labels = c("Negative", "Positive"), values = color) +
  geom_text(aes(label = label),  # add proportion labels to bars
            position = position_stack(vjust = 0.5), 
            size = 5,
            color = "white") +
  labs(
    title = "Malaria Test Positivity Rate - Kano",
    subtitle = "Children Under Five Only",
    x = "Ward",
    y = "Proportion of Test Results",
    fill = "Test Result"
  ) +
  theme_manuscript() + 
  theme(plot.subtitle = element_text(hjust = 0.5))

ggsave(filename = paste0(u5PlotsDir, "/", Sys.Date(), '_tpr_by_ward.pdf'), plot = tpr_plot, width = 12, height = 8)

## -----------------------------------------------------------------------------------------------------------------------------------------
### Net Ownership and Usage Among U5 Children
## -----------------------------------------------------------------------------------------------------------------------------------------

# calculate net ownership and usage statistics by ward (children under 5 only)
net_data_kn_u5 <- kano_data_clean_u5 %>%
  filter(!is.na(net_ownership)) %>%  # remove households without net ownership data
  filter(net_ownership != 3) %>%  # remove invalid responses
  group_by(Ward) %>%  # group data by ward
  summarise(
    total = n(),  # total number of households
    own_nets = sum(net_ownership == 1, na.rm = TRUE),  # count of households owning nets
    use_nets = sum(net_use == 1 & net_ownership == 1, na.rm = TRUE)  # count of households using owned nets
  ) %>%
  mutate(
    proportion_own_nets = own_nets / total,  # proportion of households owning nets
    proportion_use_nets = use_nets / total  # proportion of households using nets
  )

# print the total number of households
print(sum(net_data_kn_u5$total))  # output the total count (931 in this case)

# reshape data to long format for easier plotting
net_data_long_kn_u5 <- net_data_kn_u5 %>%
  pivot_longer(
    cols = c(own_nets, use_nets),  # columns to pivot
    names_to = "metric",  # new column for metric names
    values_to = "count"  # new column for values
  )

net_colors = c("#01377D", "#26B170")

# add proportion to counts df
net_data_long_kn_u5 <- net_data_long_kn_u5 %>%
  mutate(
    proportion = count / total,
    label = proportion * 100,
    label = paste0(round(label, 1), "%")
  )

# plot of net use and ownership counts that includes proportions labeled on bars
net_use_own_plot <- ggplot(net_data_long_kn_u5, aes(x = Ward, y = count, fill = metric)) +
  geom_bar(stat = "identity", position = "dodge") +  # side-by-side bar chart
  scale_fill_manual(name = "", labels = c("Own Nets", "Use Nets"), values = net_colors) +
  geom_text(aes(label = count, y = count / 2),  # place text at the midpoint of the bar
            position = position_dodge(width = 0.9),  # align labels with dodged bars
            size = 5,
            color = "white") + 
  geom_text(aes(label = paste0("(", label, ")"), y = (count / 2) - 7),  # place text at the midpoint of the bar
            position = position_dodge(width = 0.9),  # align labels with dodged bars
            size = 4,
            color = "white") +
  labs(
    title = "Counts of Net Ownership and Net Use - Kano",
    subtitle = "Children Under Five Only",
    x = "Ward",
    y = "Count",
    fill = ""  # leave legend title blank
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # rotate x-axis labels
  theme_manuscript() +
  theme(plot.subtitle = element_text(hjust = 0.5))

net_use_own_plot
ggsave(filename = paste0(u5PlotsDir, "/", Sys.Date(), '_net_use_own_plot.pdf'), plot = net_use_own_plot, width = 12, height = 8)

## =========================================================================================================================================
### GRACE'S CODE: WEIGHTED ANALYSIS
## =========================================================================================================================================

## -----------------------------------------------------------------------------------------------------------------------------------------
### Get Weighted Values for Each Ward
## -----------------------------------------------------------------------------------------------------------------------------------------

# define survey design for weighted calculations
u5_design <- svydesign(
  id = ~sn + ea,  # specify sampling units
  strata = ~Ward + settlement_type,  # stratify by ward and settlement type
  weights = ~hh_weight,  # apply household weights
  data = kano_data_clean_u5,  # input dataset
  nest = TRUE  # ensure proper nesting of survey levels
)

# calculate weighted malaria prevalence by ward
weighted_ward_tpr_kn_u5 <- svyby(~malaria_positive, ~Ward, u5_design, svymean, na.rm = TRUE)

# calculate weighted net ownership by ward
weighted_ward_net_own_kn_u5 <- svyby(~net_own, ~Ward, u5_design, svymean, na.rm = TRUE)

# calculate weighted net use among households that own nets by ward
weighted_ward_net_use3_kn_u5 <- svyby(~net_use3, ~Ward, u5_design, svymean, na.rm = TRUE)

# combine weighted results with a list of wards
kano_list <- data.frame(Ward = c("Dorayi", "Fagge", "Giginyu", "Gobirawa", "Zango"))
all_kn_u5 <- list(kano_list, weighted_ward_tpr_kn_u5, weighted_ward_net_own_kn_u5, weighted_ward_net_use3_kn_u5)
kano_summary_u5 <- reduce(all_kn_u5, left_join, by = "Ward")  # merge all datasets by ward

# update ward name for "Fagge"
kano_summary_u5 <- kano_summary_u5 %>%
  mutate(Ward = ifelse(Ward == "Fagge", "Fagge D2", Ward))

# save combined summary to csv
write.csv(kano_summary_u5, file.path(OutputsDir, "NMEP Malaria Risk Scores", "u5_kano_field_variables.csv"))

## -----------------------------------------------------------------------------------------------------------------------------------------
### Weighted Regressions
## -----------------------------------------------------------------------------------------------------------------------------------------

# get weighted odds ratios for individual-level analysis
# get_model_results applies survey design and gets adjusted model, unadjusted (net own only), and unadjusted (net use only)
kano_individuals_u5 <- get_model_results(kano_data_clean_u5)

# remove intercept term from results
kano_individuals_u5 <- kano_individuals_u5 %>%
  filter(term != "(Intercept)")

net_colors_adjusted = c("#FF817E", "#01377D", "#26B170")

# plot odds ratios for individual-level analysis
individual_OR_plot <- ggplot(kano_individuals_u5, aes(x = oddsratio, y = term, colour = model)) +
  geom_vline(aes(xintercept = 1), size = 0.25, linetype = "dashed") +  # add reference line at OR = 1
  geom_errorbarh(aes(xmax = ci_high, xmin = ci_low), size = 0.5, height = 0.2, position = position_dodge(width = 0.5)) +  # error bars
  geom_point(size = 2, alpha = 0.5, position = position_dodge(width = 0.5)) +  # plot points
  labs(
    x = "Odds Ratio",
    y = "Predictors",
    title = "Net Use and Net Ownership \non Positive Malaria Test in Kano",
    subtitle = "Children Under Five Only",
    colour = "Model"
  ) +
  scale_y_discrete(
    breaks = c("net_use3", "net_own"),
    labels = c("Net Use", "Net Ownership")  # custom labels for y-axis
  ) +
  scale_colour_manual(
    values = net_colors_adjusted,
    labels = c("Adjusted", "Unadjusted (Net Ownership Only)", "Unadjusted (Net Use Only)")
  ) +
  theme_manuscript() +
  theme(plot.subtitle = element_text(hjust = 0.5))

ggsave(filename = paste0(u5PlotsDir, "/", Sys.Date(), '_individual_ORs.pdf'), plot = individual_OR_plot, width = 12, height = 8)

# analyze odds ratios by ward
ward_data_kn_u5 <- kano_data_clean_u5 %>% group_split(Ward)  # split data by ward
names(ward_data_kn_u5) <- c("Dorayi", "Fagge", "Giginyu", "Gobirawa", "Zango")  # name each group

ward_results_kn_u5 <- ward_data_kn_u5 %>%
  map_df(~ get_model_results(.x), .id = "Ward")  # calculate models for each ward

# change model names for plotting
ward_results_kn_u5 <- ward_results_kn_u5 %>%
  mutate(
    model = case_when(
      model == "adjusted" ~ "Adjusted",
      model == "unadjusted_net_own" ~ "Unadjusted (Net Ownership Only)",
      model == "unadjusted_net_use" ~ "Unadjusted (Net Use Only)",
    )
  )

# # cap CI at (1, 10) (Fagge CIs are super wide) and OR at 10
# ward_results_kn_u5 <- ward_results_kn_u5 %>%
#   mutate(
#     ci_low = ifelse(ci_low < 1, ci_low, 1),
#     ci_high = ifelse(ci_high > 10, 10, ci_high),
#     oddsratio = ifelse(oddsratio > 10, 10, oddsratio)
#   )

# plot odds ratios by ward
adjusted_ward_OR_plot <- ggplot(ward_results_kn_u5 %>% filter(term != "(Intercept)"),
       aes(x = oddsratio, y = Ward, colour = term)) +
  geom_point(size = 2, position = position_dodge(width = 0.5)) +  # plot points
  geom_vline(aes(xintercept = 1), size = 0.25, linetype = "dashed") +  # add reference line
  geom_errorbarh(aes(xmax = ci_high, xmin = ci_low), size = 0.5, height = 0.2, position = position_dodge(width = 0.5)) +  # error bars
  facet_grid(~model, scales = "free") +  # facet by model
  labs(
    title = "Net Ownership and Net Use on \nPositive Malaria Test in Kano Wards",
    subtitle = "Children Under Five Only",
    x = "Odds Ratio",
    y = "Ward",
    color = "Type"
  ) +
  scale_colour_manual(
    values = net_colors,
    labels = c("Net Ownership", "Net Use")
  ) +
  theme_manuscript() + 
  theme(plot.subtitle = element_text(hjust = 0.5))
adjusted_ward_OR_plot
ggsave(filename = paste0(u5PlotsDir, "/", Sys.Date(), '_adjusted_ward_ORs.pdf'), plot = adjusted_ward_OR_plot, width = 12, height = 8)


# analyze odds ratios by settlement type
settle_data_kn_u5 <- kano_data_clean_u5 %>% group_split(settlement_type)  # split data by settlement type
names(settle_data_kn_u5) <- c("Formal", "Informal", "Slum")  # name each group

settle_results_kn_u5 <- settle_data_kn_u5 %>%
  map_df(~ get_model_results(.x), .id = "settlement_type")  # calculate models for each settlement type

# change model names for plotting
settle_results_kn_u5 <- settle_results_kn_u5 %>%
  mutate(
    model = case_when(
      model == "adjusted" ~ "Adjusted",
      model == "unadjusted_net_own" ~ "Unadjusted (Net Ownership Only)",
      model == "unadjusted_net_use" ~ "Unadjusted (Net Use Only)",
    )
  )

# plot odds ratios by settlement type
settlement_type_OR_plot <- ggplot(settle_results_kn_u5 %>% filter(term != "(Intercept)"),
       aes(x = oddsratio, y = settlement_type, colour = term)) +
  geom_point(size = 2, position = position_dodge(width = 0.5)) +  # plot points
  geom_vline(aes(xintercept = 1), size = 0.25, linetype = "dashed") +  # add reference line
  geom_errorbarh(aes(xmax = ci_high, xmin = ci_low), size = 0.5, height = 0.2, position = position_dodge(width = 0.5)) +  # error bars
  facet_grid(~model, scales = "free") +  # facet by model
  labs(
    title = "Net Ownership and Net Use on Positive Malaria Test in Kano Settlements",
    subtitle = "Children Under Five Only",
    x = "Odds Ratio",
    y = "Settlement Type",
    color = "Type"
  ) +
  scale_colour_manual(
    values = net_colors,
    labels = c("Net Ownership", "Net Use")
  ) +
  theme_manuscript()  + 
  theme(plot.subtitle = element_text(hjust = 0.5))

ggsave(filename = paste0(u5PlotsDir, "/", Sys.Date(), '_settlementtype_ORs.pdf'), plot = settlement_type_OR_plot, width = 12, height = 8)
