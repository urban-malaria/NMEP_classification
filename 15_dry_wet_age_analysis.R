# ==========================================================================================================================================
# Script Name: NMEP Request Analyses
# Author: Grace Legris, Research Data Analyst
# Date: 12/11/24
# Purpose: Conduct malaria prevalence analyses
# ==========================================================================================================================================

rm(list = ls())

## =========================================================================================================================================
### Setup
## =========================================================================================================================================

# script with directories, functions, etc
source("/Users/grace/Desktop/UMP/NMEP_classification_my_fork/load_path.R", echo = T)

# read in long wet and dry season data
dry <- read_dta(file.path(FieldDataDir, "Kano Dry Season Data_latest_Nov2024", "Kano dry season survey data", "long_dryseason_household_membersV00.dta"))
#wet <- read_dta(file.path(FieldDataDir, "Kano Wet Season Data Sept. 2024", "long_wetseason_household_membersV2_678_cols.dta"))

wet <- read_dta(file.path(KanoFieldData, "kano_long_wetseason_household_members_with_ind_nets.dta"))

# data cleaning: DRY
dry <- dry %>%
  mutate(
    settlement_type = case_when(  # recode settlement type variable to text labels
      bi3 == 1 ~ "Formal",
      bi3 == 2 ~ "Informal",
      bi3 == 3 ~ "Slum",
      TRUE ~ NA_character_  # handle any unexpected values
    )
  ) %>%
  dplyr::select(sn, hl1, hl4, hl5, hl6, agebin, # identifiers
                women_q223, # if woman is currently pregnant
                women_q702, # woman's malaria test result
                q302, # test result
                Ward, # ward name 
                settlement_type, # settlement type
                enumeration_area, 
                ward_weight, ea_settlement_weight, hhs_weights,
                overall_hh_weight, ind_weights_hh,  # weights not in dry df
                #nh101a, nh105, slept_under_net, # dry season data does not include net ownership/use
                bi12i, # interview visit 1 date
                # bi9
  ) %>%
  filter(!is.na(settlement_type)) %>%  
  filter(!Ward == "") %>%  
  filter(!is.na(q302)) %>%  #remove non-tested individuals, included this line after running surveyed population
  mutate(unique_id = paste(sn, hl1, sep = "_")) %>% 
  rename(ea = enumeration_area,
         ea_weight = ea_settlement_weight,
         hh_weight = overall_hh_weight,
         ind_weight = ind_weights_hh,
         #net_ownership = nh101a, # dry season data does not include net ownership/use
         #net_use = slept_under_net # dry season data does not include net ownership/use
         sex = hl4,
         age = hl5,
         dob = hl6,
         pregnant = women_q223,
         visit1_date = bi12i,
         ward = Ward
  ) %>% 
  mutate(#net_use = ifelse(is.na(net_use), 0, net_use), # dry season data does not include net ownership/use
         #net_own = ifelse(net_ownership == 1, 1, 0), # dry season data does not include net ownership/use
         malaria_positive = ifelse(q302 == 1, 1, 0), # convert all to 0 and 1
         woman_malaria_positive = ifelse(women_q702 == 1, 1, 0)) %>% # same for woman's test result var
  mutate(age_cat = case_when(
    age <= 5 ~ "under 5",
    age >= 6 & age <= 10 ~ "6–10",
    age >= 11 & age <= 17 ~ "11–17",
    age >= 18 & age <= 30 ~ "18–30",
    age > 30 ~ "31+"
  ))
  # mutate(net_use2 = case_when( # dry season data does not include net ownership/use
  #   net_own == 0 ~ NA_real_, net_own == 1 ~ net_use)) %>% 
  # mutate(net_use3 = case_when(
  #   net_own == 0 & net_use == 1 ~ NA_real_,
  #   net_own == 0 & net_use == 0 ~ 0,
  #   net_own == 1 ~ net_use))  ##net use cleaned for model

# data cleaning: WET
wet <- wet %>%
  dplyr::select(sn, hl1, hl4, hl5, hl6, agebin, #identifiers
                women_q223, # if woman is currently pregnant
                women_q702, # woman's malaria test result
                q302, #test result
                Wardn, #settle_type, 
                settlement1, enumeration_area, 
                ward_weight, ea_settlement_weight, overall_hh_weight, ind_weights_hh,  #weights
                nh101a, nh105, slept_under_net, #net ownership/ use
                bi12i, # interview visit 1 date
                #bi9
  ) %>%
  filter(!is.na(settlement1)) %>%  
  filter(!Wardn == "") %>%  
  filter(!is.na(q302)) %>%  #remove non-tested individuals, included this line after running surveyed population
  mutate(unique_id = paste(sn, hl1, sep = "_")) %>% 
  rename(ward = Wardn,
         settlement_type = settlement1,
         ea = enumeration_area, 
         ea_weight = ea_settlement_weight,
         hh_weight = overall_hh_weight,
         ind_weight = ind_weights_hh,
         net_ownership = nh101a,
         net_use = slept_under_net,
         sex = hl4,
         age = hl5,
         dob = hl6,
         pregnant = women_q223,
         visit1_date = bi12i,
         net_use_frequency = nh105
  ) %>% 
  mutate(net_use = ifelse(is.na(net_use), 0, net_use), # set all NA values of net_use to 0 (did not use)
         net_own = ifelse(net_ownership == 1, 1, 0),
         malaria_positive = ifelse(q302 == 1, 1, 0), # convert all to 0 and 1
         woman_malaria_positive = ifelse(women_q702 == 1, 1, 0)) %>% # same for woman's test result var
  # net_use2 is a cleaned version of net_use where: 
  # if the person does not own a net (net_own == 0), their net_use value is set to NA (since they couldn’t use a net they don’t own).
  # if they own a net (net_own == 1), their net_use value remains unchanged.
  mutate(net_use2 = case_when(
    net_own == 0 ~ NA_real_, net_own == 1 ~ net_use)) %>% 
  # net_use3 is further cleaning of net_use2, with more specific handling:
  # if the person does not own a net and reported using a net (net_use == 1), their value is set to NA (impossible situation).
  # if the person does not own a net and reported not using a net (net_use == 0), their value is set to 0 (logical case).
  # if the person owns a net (net_own == 1), their net_use value remains unchanged.
  mutate(net_use3 = case_when(
    net_own == 0 & net_use == 1 ~ NA_real_,
    net_own == 0 & net_use == 0 ~ 0,
    net_own == 1 ~ net_use)) %>% ##net use cleaned for model
  mutate(age_cat = case_when(
    age <= 5 ~ "under 5",
    age >= 6 & age <= 10 ~ "6–10",
    age >= 11 & age <= 17 ~ "11–17",
    age >= 18 & age <= 30 ~ "18–30",
    age > 30 ~ "31+"
  ))

## -----------------------------------------------------------------------------------------------------------------------------------------
### Combine Dry and Wet Data
## -----------------------------------------------------------------------------------------------------------------------------------------

# reorder columns in dry and wet dfs so they match (and exclude duplicate vars), add column to note dry/wet data
dry <- dry %>%
  mutate(season = "Dry") %>%
  dplyr::select(sn, hl1, sex, age, dob, age_cat, pregnant, ward, settlement_type, visit1_date, unique_id, malaria_positive, 
         woman_malaria_positive, season, ea, ward_weight, ea_weight, hhs_weights, hh_weight, ind_weight) %>%
  mutate(sn = as.character(sn))

wet <- wet %>%
  mutate(season = "Wet") %>%
  dplyr::select(sn, hl1, sex, age, dob, age_cat, pregnant, ward, settlement_type, visit1_date, unique_id, malaria_positive,
         woman_malaria_positive, season, ea, ward_weight, ea_weight, hh_weight, ind_weight, net_use_frequency,
         net_use, net_own, net_use2, net_use3)

# combine the dry and wet data
combined_df <- bind_rows(dry, wet)

## =========================================================================================================================================
### Sample Sizes by Age Group, Season, and Ward
## =========================================================================================================================================

# custom order for tables
age_order <- c("under 5", "6–10", "11–17", "18–30", "31+")

# 1) table of sample sizes in each age group in dry, wet, and combined
dry_counts <- table(dry$age_cat)
wet_counts <- table(wet$age_cat)
combined_counts <- table(combined_df$age_cat)
age_cat_season <- data.frame(
  Age = factor(names(dry_counts), levels = age_order),  # Age categories
  Dry = as.numeric(dry_counts),     # Counts from the dry dataset
  Wet = as.numeric(wet_counts),     # Counts from the wet dataset
  Combined = as.numeric(combined_counts)  # Counts from the combined dataset
)
age_cat_season <- age_cat_season[order(age_cat_season$Age), ]
# make table have age categories as columns and datasets as rows to match other sample size tables
age_cat_season <- t(age_cat_season[, -1])
age_cat_season <- data.frame(age_cat_season)
# reset age categories
age_cat_season <- age_cat_season %>%
  rename(
    "under 5" = "X1",
    "6–10" = "X2",
    "11–17" = "X3",
    "18–30" = "X4",
    "31+" = "X5"
  ) %>%
  dplyr::select("under 5", "6–10", "11–17", "18–30", "31+")
# make row names their own column
age_cat_season <- rownames_to_column(age_cat_season, var = "Season")

# 2) table of sample sizes in each age group by ward and season
dry_ward_counts <- table(dry$ward, dry$age_cat)
dry_ward_df <- as.data.frame.matrix(dry_ward_counts)
dry_ward_df <- tibble::rownames_to_column(dry_ward_df, var = "Ward")
dry_ward_df <- dry_ward_df[, c("Ward", age_order)]
wet_ward_counts <- table(wet$ward, wet$age_cat)
wet_ward_df <- as.data.frame.matrix(wet_ward_counts)
wet_ward_df <- tibble::rownames_to_column(wet_ward_df, var = "Ward")
wet_ward_df <- wet_ward_df[, c("Ward", age_order)]
combined_ward_counts <- table(combined_df$ward, combined_df$age_cat)
combined_ward_df <- as.data.frame.matrix(combined_ward_counts)
combined_ward_df <- tibble::rownames_to_column(combined_ward_df, var = "Ward")
combined_ward_df <- combined_ward_df[, c("Ward", age_order)]

# 2) table of sample sizes in each age group by settlement type and season
dry_settlement_counts <- table(dry$settlement_type, dry$age_cat)
dry_settlement_df <- as.data.frame.matrix(dry_settlement_counts)
dry_settlement_df <- tibble::rownames_to_column(dry_settlement_df, var = "Settlement Type")
dry_settlement_df <- dry_settlement_df[, c("Settlement Type", age_order)]
wet_settlement_counts <- table(wet$settlement_type, wet$age_cat)
wet_settlement_df <- as.data.frame.matrix(wet_settlement_counts)
wet_settlement_df <- tibble::rownames_to_column(wet_settlement_df, var = "Settlement Type")
wet_settlement_df <- wet_settlement_df[, c("Settlement Type", age_order)]
combined_settlement_counts <- table(combined_df$settlement_type, combined_df$age_cat)
combined_settlement_df <- as.data.frame.matrix(combined_settlement_counts)
combined_settlement_df <- tibble::rownames_to_column(combined_settlement_df, var = "Settlement Type")
combined_settlement_df <- combined_settlement_df[, c("Settlement Type", age_order)]

# place all sample size tables in a word document for export
# create a Word document and add titles for each table
doc <- read_docx()
doc <- doc %>%
  body_add_par("Sample Size in each Age Group by Season", style = "heading 1") %>%
  body_add_table(value = age_cat_season, style = "table_template")
doc <- doc %>%
  body_add_par("Sample Size in each Age Group by Ward: Dry Season", style = "heading 1") %>%
  body_add_table(value = dry_ward_df, style = "table_template")
doc <- doc %>%
  body_add_par("Sample Size in each Age Group by Ward: Wet Season", style = "heading 1") %>%
  body_add_table(value = wet_ward_df, style = "table_template")
doc <- doc %>%
  body_add_par("Sample Size in each Age Group by Ward: Dry and Wet Seasons Combined", style = "heading 1") %>%
  body_add_table(value = combined_ward_df, style = "table_template")
doc <- doc %>%
  body_add_par("Sample Size in each Age Group by Settlement Type: Dry Season", style = "heading 1") %>%
  body_add_table(value = dry_settlement_df, style = "table_template")
doc <- doc %>%
  body_add_par("Sample Size in each Age Group by Settlement Type: Wet Season", style = "heading 1") %>%
  body_add_table(value = wet_settlement_df, style = "table_template")
doc <- doc %>%
  body_add_par("Sample Size in each Age Group by Settlement Type: Dry and Wet Seasons Combined", style = "heading 1") %>%
  body_add_table(value = combined_settlement_df, style = "table_template")

# save the document
file_path <- file.path(NMEPOutputs, "sample_size_by_age.docx")
print(doc, target = file_path)


## =========================================================================================================================================
### Plots for Malaria TPR by Ward and by Age Group
## =========================================================================================================================================

color_palette = c("#f2a5a1", "#c55c80")

# dfs for each age group: combined wet and dry data
u5 <- combined_df %>%
  dplyr::filter(age_cat == "under 5")
six_ten <- combined_df %>%
  dplyr::filter(age_cat == "6–10")
eleven_seventeen <- combined_df %>%
  dplyr::filter(age_cat == "11–17")
eighteen_thirty <- combined_df %>%
  dplyr::filter(age_cat == "18–30")
thirtyone_plus <- combined_df %>%
  dplyr::filter(age_cat == "31+")

# dfs for each age group: wet data only
u5_wet <- wet %>%
  dplyr::filter(age_cat == "under 5")
six_ten_wet <- wet %>%
  dplyr::filter(age_cat == "6–10")
eleven_seventeen_wet <- wet %>%
  dplyr::filter(age_cat == "11–17")
eighteen_thirty_wet <- wet %>%
  dplyr::filter(age_cat == "18–30")
thirtyone_plus_wet <- wet %>%
  dplyr::filter(age_cat == "31+")

# dfs for each age group: dry data only
u5_dry <- dry %>%
  dplyr::filter(age_cat == "under 5")
six_ten_dry <- dry %>%
  dplyr::filter(age_cat == "6–10")
eleven_seventeen_dry <- dry %>%
  dplyr::filter(age_cat == "11–17")
eighteen_thirty_dry <- dry %>%
  dplyr::filter(age_cat == "18–30")
thirtyone_plus_dry <- dry %>%
  dplyr::filter(age_cat == "31+")

## -----------------------------------------------------------------------------------------------------------------------------------------
### Function to create tpr plots for each age group
## -----------------------------------------------------------------------------------------------------------------------------------------

create_tpr_plot <- function(data, title, subtitle, age_group) {
  # calculate malaria prevalence by ward
  tpr_data <- data %>%
    group_by(ward, malaria_positive) %>%  # group data by ward and test result
    summarise(count = n(), .groups = "drop") %>%  # count the number of cases in each group
    group_by(ward) %>%  # group by ward again to calculate proportions
    mutate(proportion = count / sum(count))  # calculate the proportion for each result type
  
  # convert malaria_positive to a factor with descriptive labels
  tpr_data$malaria_positive <- factor(
    tpr_data$malaria_positive,
    levels = c(0, 1),
    labels = c("Negative", "Positive")
  )

  # create percentage label for plot
  tpr_data <- tpr_data %>%
    mutate(
      label = paste0(round(proportion * 100, 1), "%")  # percentage labels
    )
  
  # create the plot
  tpr_plot <- ggplot(tpr_data, aes(x = ward, y = proportion, fill = malaria_positive)) +
    geom_bar(stat = "identity", position = "stack") +  # stacked bar chart
    scale_fill_manual(name = "Test Result", values = color_palette) +  # manual fill colors
    geom_text(aes(label = label, y = proportion / 2),  # place text at the midpoint of the bar
              #position = position_dodge(width = 0.8),  # align labels with dodged bars
              size = 4,
              color = "white") + 
    geom_text(aes(label = paste0("(", count, ")"), y = (proportion / 2) - 0.1),  # place text at the midpoint of the bar
              #position = position_dodge(width = 0.6),  # align labels with dodged bars
              size = 3,
              color = "white") +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Ward",
      y = "Proportion of Test Results",
      fill = "Test Result"
    ) +
    theme_manuscript() +  # Adjust theme as needed
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate ward names if needed
    )
  
  # save the plot with descriptive name
  assign(paste0(age_group, "_tpr_plot"), tpr_plot, envir = .GlobalEnv)
  
  return(tpr_plot)
}

## -----------------------------------------------------------------------------------------------------------------------------------------
### Apply tpr function to each age group and save plots - COMBINED WET AND DRY DATA
## -----------------------------------------------------------------------------------------------------------------------------------------

# u5
create_tpr_plot(
  data = u5,
  title = "Malaria Test Positivity Rate in Kano: \nWet and Dry Season Combined",
  subtitle = "Children Under 5",
  age_group = "u5"
)
ggsave(filename = paste0(NMEPOutputs, "/tpr_plots/", "/combined/", Sys.Date(), '_u5_tpr_plot.pdf'), plot = u5_tpr_plot, width = 6, height = 4)

# 6-10
create_tpr_plot(
  data = six_ten,
  title = "Malaria Test Positivity Rate in Kano: \nWet and Dry Season Combined",
  subtitle = "Children Aged 6–10",
  age_group = "six_ten"
)
ggsave(filename = paste0(NMEPOutputs, "/tpr_plots/", "/combined/", Sys.Date(), '_6_10_tpr_plot.pdf'), plot = six_ten_tpr_plot, width = 6, height = 4)


# 11-17
create_tpr_plot(
  data = eleven_seventeen,
  title = "Malaria Test Positivity Rate in Kano: \nWet and Dry Season Combined",
  subtitle = "Children Aged 11–17",
  age_group = "eleven_seventeen"
)
ggsave(filename = paste0(NMEPOutputs, "/tpr_plots/", "/combined/", Sys.Date(), '_11_17_tpr_plot.pdf'), plot = eleven_seventeen_tpr_plot, width = 6, height = 4)


# 18-30
create_tpr_plot(
  data = eighteen_thirty,
  title = "Malaria Test Positivity Rate in Kano: \nWet and Dry Season Combined",
  subtitle = "Adults Aged 18–30",
  age_group = "eighteen_thirty"
)
ggsave(filename = paste0(NMEPOutputs, "/tpr_plots/", "/combined/", Sys.Date(), '_18_30_tpr_plot.pdf'), plot = eighteen_thirty_tpr_plot, width = 6, height = 4)


# 30+
create_tpr_plot(
  data = thirtyone_plus,
  title = "Malaria Test Positivity Rate in Kano: \nWet and Dry Season Combined",
  subtitle = "Adults Aged 31+",
  age_group = "thirtyone_plus"
)
ggsave(filename = paste0(NMEPOutputs, "/tpr_plots/", "/combined/", Sys.Date(), '_31_plus_tpr_plot.pdf'), plot = thirtyone_plus_tpr_plot, width = 6, height = 4)


# save all plots in a word doc
doc <- read_docx()
doc <- doc %>% body_add_gg(value = u5_tpr_plot, style = "centered", width = 6, height = 4)
doc <- doc %>% body_add_gg(value = six_ten_tpr_plot, style = "centered", width = 6, height = 4)
doc <- doc %>% body_add_gg(value = eleven_seventeen_tpr_plot, style = "centered", width = 6, height = 4)
doc <- doc %>% body_add_gg(value = eighteen_thirty_tpr_plot, style = "centered", width = 6, height = 4)
doc <- doc %>% body_add_gg(value = thirtyone_plus_tpr_plot, style = "centered", width = 6, height = 4)

# save the document
file_path <- file.path(NMEPOutputs, "/tpr_plots/", "/combined/", "tpr_by_age.docx")
print(doc, target = file_path)

## -----------------------------------------------------------------------------------------------------------------------------------------
### Apply tpr function to each age group and save plots - WET DATA ONLY
## -----------------------------------------------------------------------------------------------------------------------------------------

# u5
create_tpr_plot(
  data = u5_wet,
  title = "Malaria Test Positivity Rate in Kano: \nWet Season Only",
  subtitle = "Children Under 5",
  age_group = "u5"
)
ggsave(filename = paste0(NMEPOutputs, "/tpr_plots/", "/wet/", Sys.Date(), '_u5_tpr_plot.pdf'), plot = u5_tpr_plot, width = 6, height = 4)

# 6-10
create_tpr_plot(
  data = six_ten_wet,
  title = "Malaria Test Positivity Rate in Kano: \nWet Season Only",
  subtitle = "Children Aged 6–10",
  age_group = "six_ten"
)
ggsave(filename = paste0(NMEPOutputs, "/tpr_plots/", "/wet/", Sys.Date(), '_6_10_tpr_plot.pdf'), plot = six_ten_tpr_plot, width = 6, height = 4)


# 11-17
create_tpr_plot(
  data = eleven_seventeen_wet,
  title = "Malaria Test Positivity Rate in Kano: \nWet Season Only",
  subtitle = "Children Aged 11–17",
  age_group = "eleven_seventeen"
)
ggsave(filename = paste0(NMEPOutputs, "/tpr_plots/", "/wet/", Sys.Date(), '_11_17_tpr_plot.pdf'), plot = eleven_seventeen_tpr_plot, width = 6, height = 4)


# 18-30
create_tpr_plot(
  data = eighteen_thirty_wet,
  title = "Malaria Test Positivity Rate in Kano: \nWet Season Only",
  subtitle = "Adults Aged 18–30",
  age_group = "eighteen_thirty"
)
ggsave(filename = paste0(NMEPOutputs, "/tpr_plots/", "/wet/", Sys.Date(), '_18_30_tpr_plot.pdf'), plot = eighteen_thirty_tpr_plot, width = 6, height = 4)


# 30+
create_tpr_plot(
  data = thirtyone_plus_wet,
  title = "Malaria Test Positivity Rate in Kano: \nWet Season Only",
  subtitle = "Adults Aged 31+",
  age_group = "thirtyone_plus"
)
ggsave(filename = paste0(NMEPOutputs, "/tpr_plots/", "/wet/", Sys.Date(), '_31_plus_tpr_plot.pdf'), plot = thirtyone_plus_tpr_plot, width = 6, height = 4)


# save all plots in a word doc
doc <- read_docx()
doc <- doc %>% body_add_gg(value = u5_tpr_plot, style = "centered", width = 6, height = 4)
doc <- doc %>% body_add_gg(value = six_ten_tpr_plot, style = "centered", width = 6, height = 4)
doc <- doc %>% body_add_gg(value = eleven_seventeen_tpr_plot, style = "centered", width = 6, height = 4)
doc <- doc %>% body_add_gg(value = eighteen_thirty_tpr_plot, style = "centered", width = 6, height = 4)
doc <- doc %>% body_add_gg(value = thirtyone_plus_tpr_plot, style = "centered", width = 6, height = 4)

# save the document
file_path <- file.path(NMEPOutputs, "/tpr_plots/", "/wet/", "tpr_by_age.docx")
print(doc, target = file_path)

## -----------------------------------------------------------------------------------------------------------------------------------------
### Apply tpr function to each age group and save plots - DRY DATA ONLY
## -----------------------------------------------------------------------------------------------------------------------------------------

# u5
create_tpr_plot(
  data = u5_dry,
  title = "Malaria Test Positivity Rate in Kano: \nDry Season Only",
  subtitle = "Children Under 5",
  age_group = "u5"
)
ggsave(filename = paste0(NMEPOutputs, "/tpr_plots/", "/dry/", Sys.Date(), '_u5_tpr_plot.pdf'), plot = u5_tpr_plot, width = 6, height = 4)

# 6-10
create_tpr_plot(
  data = six_ten_dry,
  title = "Malaria Test Positivity Rate in Kano: \nDry Season Only",
  subtitle = "Children Aged 6–10",
  age_group = "six_ten"
)
ggsave(filename = paste0(NMEPOutputs, "/tpr_plots/", "/dry/", Sys.Date(), '_6_10_tpr_plot.pdf'), plot = six_ten_tpr_plot, width = 6, height = 4)

# 11-17
create_tpr_plot(
  data = eleven_seventeen_dry,
  title = "Malaria Test Positivity Rate in Kano: \nDry Season Only",
  subtitle = "Children Aged 11–17",
  age_group = "eleven_seventeen"
)
ggsave(filename = paste0(NMEPOutputs, "/tpr_plots/", "/dry/", Sys.Date(), '_11_17_tpr_plot.pdf'), plot = eleven_seventeen_tpr_plot, width = 6, height = 4)

# 18-30
create_tpr_plot(
  data = eighteen_thirty_dry,
  title = "Malaria Test Positivity Rate in Kano: \nDry Season Only",
  subtitle = "Adults Aged 18–30",
  age_group = "eighteen_thirty"
)
ggsave(filename = paste0(NMEPOutputs, "/tpr_plots/", "/dry/", Sys.Date(), '_18_30_tpr_plot.pdf'), plot = eighteen_thirty_tpr_plot, width = 6, height = 4)

# 30+
create_tpr_plot(
  data = thirtyone_plus_dry,
  title = "Malaria Test Positivity Rate in Kano: \nDry Season Only",
  subtitle = "Adults Aged 31+",
  age_group = "thirtyone_plus"
)
ggsave(filename = paste0(NMEPOutputs, "/tpr_plots/", "/dry/", Sys.Date(), '_31_plus_tpr_plot.pdf'), plot = thirtyone_plus_tpr_plot, width = 6, height = 4)

# save all plots in a word doc
doc <- read_docx()
doc <- doc %>% body_add_gg(value = u5_tpr_plot, style = "centered", width = 6, height = 4)
doc <- doc %>% body_add_gg(value = six_ten_tpr_plot, style = "centered", width = 6, height = 4)
doc <- doc %>% body_add_gg(value = eleven_seventeen_tpr_plot, style = "centered", width = 6, height = 4)
doc <- doc %>% body_add_gg(value = eighteen_thirty_tpr_plot, style = "centered", width = 6, height = 4)
doc <- doc %>% body_add_gg(value = thirtyone_plus_tpr_plot, style = "centered", width = 6, height = 4)

# save the document
file_path <- file.path(NMEPOutputs, "/tpr_plots/", "/dry/", "tpr_by_age.docx")
print(doc, target = file_path)

## -----------------------------------------------------------------------------------------------------------------------------------------
### Line Plots for TPR by Age Group and Ward
## -----------------------------------------------------------------------------------------------------------------------------------------

line_palette <- brewer.pal(5, "Set2")

# define the function to create the line plot
create_tpr_line_plot <- function(data, title, season) {
  # calculate TPR for each age group and ward
  summarized_data <- data %>%
    group_by(age_cat, ward) %>%
    summarize(
      tpr = mean(malaria_positive, na.rm = TRUE), # calculate mean positivity rate
      .groups = "drop"
    )
  
  # calculate TPR for each age group (not separated by ward) for "overall" line on plot
  overall <- data %>%
    group_by(age_cat) %>%
    summarize(
      tpr = mean(malaria_positive, na.rm = TRUE),
      .groups = "drop"
    )
  
  # remove age_cat = NA for both
  summarized_data <- summarized_data %>% dplyr::filter(!is.na(age_cat))
  overall <- overall %>% dplyr::filter(!is.na(age_cat))
  
  # put in age order
  summarized_data <- summarized_data %>%
    mutate(age_cat = factor(age_cat, levels = age_order)) %>%
    arrange(age_cat)
  overall <- overall %>%
    mutate(age_cat = factor(age_cat, levels = age_order)) %>%
    arrange(age_cat)
  
  overall$age_cat <- factor(overall$age_cat, 
                            levels = c("under 5", "6–10", "11–17", "18–30", "31+"), 
                            ordered = TRUE)
  
  # create the line plot
  line_plot <- ggplot() +
    geom_line(data = summarized_data, # ward-specific lines
              aes(x = age_cat, y = tpr, color = ward, group = ward), 
              size = 1) +
    geom_point(data = summarized_data, 
               aes(x = age_cat, y = tpr, color = ward, group = ward), 
               size = 2) +
    geom_line(data = overall, # overall lines
              aes(x = age_cat, y = tpr, group = 1),
              color = "black", size = 1) +
    geom_point(data = overall, 
               aes(x = age_cat, y = tpr), 
               color = "black", size = 2) + 
    scale_color_manual(values = line_palette) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(
      title = title,
      x = "Age Group",
      y = "Malaria Test Positivity Rate (%)",
      color = "Ward"
    ) +
    theme_manuscript() +
    theme(
      text = element_text(size = 12),
      plot.title = element_text(size = 10),
      legend.position = "bottom"
    )
  
  # save the plot with descriptive name
  assign(paste0(season, "_line_plot"), line_plot, envir = .GlobalEnv)
  
  # Return the plot object for further use if needed
  return(line_plot)
}

# apply function for dry, wet, and combined
dry_plot <- create_tpr_line_plot(
  data = dry, 
  title = "Malaria Test Positivity Rate by Age Group: Dry Season",
  season = "dry"
)
ggsave(filename = paste0(NMEPOutputs, "/tpr_plots/", Sys.Date(), '_dry_line_plot.pdf'), plot = dry_line_plot, width = 6, height = 4)

wet_plot <- create_tpr_line_plot(
  data = wet, 
  title = "Malaria Test Positivity Rate by Age Group: Wet Season",
  season = "wet"
)
ggsave(filename = paste0(NMEPOutputs, "/tpr_plots/", Sys.Date(), '_wet_line_plot.pdf'), plot = wet_line_plot, width = 6, height = 4)

combined_plot <- create_tpr_line_plot(
  data = combined_df, 
  title = "Malaria Test Positivity Rate by Age Group: Combined Wet + Dry Seasons",
  season = "combined"
)
ggsave(filename = paste0(NMEPOutputs, "/tpr_plots/", Sys.Date(), '_combined_line_plot.pdf'), plot = combined_line_plot, width = 6, height = 4)

## =========================================================================================================================================
### Plots for Net Data by Ward and by Age Group - Calculate Proportion of Net Ownership and Net Use
# net use counts and proportions calculated using those who reported using a net AND owning a net
## =========================================================================================================================================

## -----------------------------------------------------------------------------------------------------------------------------------------
### Function to create net plots for each age group
## -----------------------------------------------------------------------------------------------------------------------------------------

net_colors = c("#01377D", "#26B170")

create_net_plot <- function(data, title, subtitle, age_group) {
  
  # filter for only wet data (dry data does not include net data)
  data <- data %>%
    dplyr::filter(season == "Wet")
  
  # calculate net ownership and usage statistics by ward (children under 5 only)
  net_data <- data %>%
    filter(!is.na(net_own)) %>%  # remove households without net ownership data
    filter(net_own != 3) %>%  # remove invalid responses
    group_by(ward) %>%  # group data by ward
    summarise(
      total = n(),  # total number of households
      own_nets = sum(net_own == 1, na.rm = TRUE),  # count of households owning nets
      use_nets = sum(net_use == 1 & net_own == 1, na.rm = TRUE)  # count of households using owned nets
    ) %>%
    mutate(
      proportion_own_nets = own_nets / total,  # proportion of households owning nets
      proportion_use_nets = use_nets / total  # proportion of households using nets
    )
  
  # reshape data to long format for easier plotting
  net_data_long <- net_data %>%
    pivot_longer(
      cols = c(own_nets, use_nets),  # columns to pivot
      names_to = "metric",  # new column for metric names
      values_to = "count"  # new column for values
    )
  
  # add proportion to counts df
  net_data_long <- net_data_long %>%
    mutate(
      proportion = count / total,
      percent = proportion * 100,
      label = paste0(round(proportion * 100, 1), "%"),
    )
  
  # plot of net use and ownership counts that includes proportions labeled on bars
  net_plot <- ggplot(net_data_long, aes(x = ward, y = percent, fill = metric)) +
    geom_bar(stat = "identity", position = "dodge") +  # side-by-side bar chart
    scale_fill_manual(name = "", labels = c("Own Nets", "Use Nets"), values = net_colors) +
    geom_text(
      aes(label = label, y = percent + 8),  # place proportion label slightly above the bar
      position = position_dodge(width = 0.9),  # align labels with dodged bars
      size = 2.5,
      color = "black"
    ) +
    geom_text(
      aes(label = paste0("(", count, ")"), y = percent + 3),  # place count label above proportion label
      position = position_dodge(width = 0.9),  # align labels with dodged bars
      size = 2.5,
      color = "black"
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Ward",
      y = "Proportion",
      fill = ""  # leave legend title blank
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # rotate x-axis labels
    theme_manuscript() +
    theme(plot.subtitle = element_text(hjust = 0.5)) +
    ylim(0, 100)
  
  # save the plot with descriptive name
  assign(paste0(age_group, "_net_plot"), net_plot, envir = .GlobalEnv)
  
  return(net_plot)
}

## -----------------------------------------------------------------------------------------------------------------------------------------
### Apply net function to each age group and save plots
## -----------------------------------------------------------------------------------------------------------------------------------------

# u5
create_net_plot(
  data = u5,
  title = "Net Use and Net Ownership in Kano",
  subtitle = "Children Under 5",
  age_group = "u5"
)
ggsave(filename = paste0(NMEPOutputs, "/net_data/", Sys.Date(), '_u5_net_plot.pdf'), plot = u5_net_plot, width = 6, height = 4)

# 6-10
create_net_plot(
  data = six_ten,
  title = "Net Use and Net Ownership in Kano",
  subtitle = "Children Aged 6–10",
  age_group = "six_ten"
)
ggsave(filename = paste0(NMEPOutputs, "/net_data/", Sys.Date(), '_6_10_net_plot.pdf'), plot = six_ten_net_plot, width = 6, height = 4)

# 11-17
create_net_plot(
  data = eleven_seventeen,
  title = "Net Use and Net Ownership in Kano",
  subtitle = "Children Aged 11–17",
  age_group = "eleven_seventeen"
)
ggsave(filename = paste0(NMEPOutputs, "/net_data/", Sys.Date(), '_11_17_net_plot.pdf'), plot = eleven_seventeen_net_plot, width = 6, height = 4)

# 18-30
create_net_plot(
  data = eighteen_thirty,
  title = "Net Use and Net Ownership in Kano",
  subtitle = "Adults Aged 18–30",
  age_group = "eighteen_thirty"
)
ggsave(filename = paste0(NMEPOutputs, "/net_data/", Sys.Date(), '_18_30_net_plot.pdf'), plot = eighteen_thirty_net_plot, width = 6, height = 4)


# 30+
create_net_plot(
  data = thirtyone_plus,
  title = "Net Use and Net Ownership in Kano",
  subtitle = "Adults Aged 31+",
  age_group = "thirtyone_plus"
)
ggsave(filename = paste0(NMEPOutputs, "/net_data/", Sys.Date(), '_31_plus_net_plot.pdf'), plot = thirtyone_plus_net_plot, width = 6, height = 4)


# save all plots in a word doc
doc <- read_docx()
doc <- doc %>% body_add_gg(value = u5_net_plot, style = "centered", width = 6, height = 4)
doc <- doc %>% body_add_gg(value = six_ten_net_plot, style = "centered", width = 6, height = 4)
doc <- doc %>% body_add_gg(value = eleven_seventeen_net_plot, style = "centered", width = 6, height = 4)
doc <- doc %>% body_add_gg(value = eighteen_thirty_net_plot, style = "centered", width = 6, height = 4)
doc <- doc %>% body_add_gg(value = thirtyone_plus_net_plot, style = "centered", width = 6, height = 4)

# save the document
file_path <- file.path(NMEPOutputs, "/net_data/", "net_data.docx")
print(doc, target = file_path)


## =========================================================================================================================================
### Plots for Proportion of Net Owners who Use Nets
## =========================================================================================================================================

## -----------------------------------------------------------------------------------------------------------------------------------------
### Function to create net use proportion plots for each age group
## -----------------------------------------------------------------------------------------------------------------------------------------

create_net_prop_plot <- function(data, title, subtitle, age_group) {
  
  # filter for only wet data (dry data does not include net data)
  data <- data %>%
    dplyr::filter(season == "Wet")
  
  # calculate net ownership and usage statistics by ward (children under 5 only)
  net_data <- data %>%
    filter(!is.na(net_own)) %>%  # remove households without net ownership data
    filter(net_own != 3) %>%  # remove invalid responses
    group_by(ward) %>%  # group data by ward
    summarise(
      total = n(),  # total number of households
      own_nets = sum(net_own == 1, na.rm = TRUE),  # count of households owning nets
      use_nets = sum(net_use == 1 & net_own == 1, na.rm = TRUE)  # count of households using owned nets
    ) %>%
    mutate(
      proportion_use_nets = use_nets / own_nets  # proportion of households using nets of those that own them
    )
  
  # add label
  net_data <- net_data %>%
    mutate(
      percent = proportion_use_nets * 100,
      label = paste0(round(proportion_use_nets * 100, 1), "%"),
    )
  
  # plot of net use and ownership counts that includes proportions labeled on bars
  net_prop_plot <- ggplot(net_data, aes(x = ward, y = percent)) +
    geom_bar(stat = "identity", position = "dodge", fill = "#6a51a3") +  # side-by-side bar chart
    #scale_fill_manual(name = "", labels = c("Own Nets", "Use Nets"), values = net_colors) +
    geom_text(
      aes(label = label, y = percent + 4),  # place proportion label slightly above the bar
      position = position_dodge(width = 0.9),  # align labels with dodged bars
      size = 3,
      color = "black"
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Ward",
      y = "Proportion",
      fill = ""  # leave legend title blank
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # rotate x-axis labels
    theme_manuscript() +
    theme(plot.subtitle = element_text(hjust = 0.5)) +
    ylim(0, 100)
  
  # save the plot with descriptive name
  assign(paste0(age_group, "_net_prop_plot"), net_prop_plot, envir = .GlobalEnv)
  
  return(net_prop_plot)
}

## -----------------------------------------------------------------------------------------------------------------------------------------
### Apply net function to each age group and save plots
## -----------------------------------------------------------------------------------------------------------------------------------------

# u5
create_net_prop_plot(
  data = u5,
  title = "Proportion of Net Use Among Households That Own Nets in Kano",
  subtitle = "Children Under 5",
  age_group = "u5"
)
ggsave(filename = paste0(NMEPOutputs, "/net_data/", Sys.Date(), '_u5_net_prop_plot.pdf'), plot = u5_net_prop_plot, width = 6, height = 4)

# 6-10
create_net_prop_plot(
  data = six_ten,
  title = "Proportion of Net Use Among Households That Own Nets in Kano",
  subtitle = "Children Aged 6–10",
  age_group = "six_ten"
)
ggsave(filename = paste0(NMEPOutputs, "/net_data/", Sys.Date(), '_6_10_net_prop_plot.pdf'), plot = six_ten_net_prop_plot, width = 6, height = 4)

# 11-17
create_net_prop_plot(
  data = eleven_seventeen,
  title = "Proportion of Net Use Among Households That Own Nets in Kano",
  subtitle = "Children Aged 11–17",
  age_group = "eleven_seventeen"
)
ggsave(filename = paste0(NMEPOutputs, "/net_data/", Sys.Date(), '_11_17_net_prop_plot.pdf'), plot = eleven_seventeen_net_prop_plot, width = 6, height = 4)

# 18-30
create_net_prop_plot(
  data = eighteen_thirty,
  title = "Proportion of Net Use Among Households That Own Nets in Kano",
  subtitle = "Adults Aged 18–30",
  age_group = "eighteen_thirty"
)
ggsave(filename = paste0(NMEPOutputs, "/net_data/", Sys.Date(), '_18_30_net_prop_plot.pdf'), plot = eighteen_thirty_net_prop_plot, width = 6, height = 4)


# 30+
create_net_prop_plot(
  data = thirtyone_plus,
  title = "Proportion of Net Use Among Households That Own Nets in Kano",
  subtitle = "Adults Aged 31+",
  age_group = "thirtyone_plus"
)
ggsave(filename = paste0(NMEPOutputs, "/net_data/", Sys.Date(), '_31_plus_net_prop_plot.pdf'), plot = thirtyone_plus_net_prop_plot, width = 6, height = 4)


# save all plots in a word doc
doc <- read_docx()
doc <- doc %>% body_add_gg(value = u5_net_prop_plot, style = "centered", width = 6, height = 4)
doc <- doc %>% body_add_gg(value = six_ten_net_prop_plot, style = "centered", width = 6, height = 4)
doc <- doc %>% body_add_gg(value = eleven_seventeen_net_prop_plot, style = "centered", width = 6, height = 4)
doc <- doc %>% body_add_gg(value = eighteen_thirty_net_prop_plot, style = "centered", width = 6, height = 4)
doc <- doc %>% body_add_gg(value = thirtyone_plus_net_prop_plot, style = "centered", width = 6, height = 4)

# save the document
file_path <- file.path(NMEPOutputs, "/net_data/", "net_prop_data.docx")
print(doc, target = file_path)


## =========================================================================================================================================
### Explore Relationship Between TPR in Pregnant Women and TPR in Children
## =========================================================================================================================================

## -----------------------------------------------------------------------------------------------------------------------------------------
### Plot: Malaria TPR in Pregnant Women and TPR in Each Age Group
## -----------------------------------------------------------------------------------------------------------------------------------------

# create df that includes only pregnant women
combined_df_preg <- combined_df %>%
  dplyr::filter(pregnant == 1)

# calculate tpr for respondent and for pregnant woman by age group of respondent
preg_tpr_data <- combined_df_preg %>%
  group_by(age_cat) %>%
  summarise(
    respondent_tpr = mean(malaria_positive, na.rm = TRUE) * 100,  # TPR for children
    women_tpr = mean(woman_malaria_positive, na.rm = TRUE) * 100  # TPR for pregnant women
  ) %>%
  pivot_longer(
    cols = c(respondent_tpr, women_tpr),
    names_to = "group",
    values_to = "tpr"
  ) %>%
  mutate(
    group = recode(group, 
                   respondent_tpr = "Respondent",
                   women_tpr = "Pregnant Women in Household")
  )

# remove age group == NA data
preg_tpr_data <- preg_tpr_data %>%
  dplyr::filter(!is.na(age_cat))

# reorder rows in age order
preg_tpr_data <- preg_tpr_data %>%
  mutate(age_cat = factor(age_cat, levels = age_order)) %>%
  arrange(age_cat)

# create the plot
preg_tpr_plot <- ggplot(preg_tpr_data, aes(x = age_cat, y = tpr, fill = group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  scale_fill_manual(
    values = c("Respondent" = "#c6dbef", "Pregnant Women in Household" = "#2171b5"),
    name = ""
  ) +
  labs(
    title = "Malaria TPR for Pregnant Women \nand Respondents by Age Category",
    x = "Age Category of Respondent",
    y = "Malaria TPR (%)"
  ) +
  theme_manuscript() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 12)
  )
preg_tpr_plot

ggsave(filename = paste0(NMEPOutputs, "/pregnant_analysis/", Sys.Date(), '_preg_tpr_plot.pdf'), plot = preg_tpr_plot, width = 6, height = 4)

## -----------------------------------------------------------------------------------------------------------------------------------------
### Chi Square Test for Respondent TPR and Pregnant Woman TPR
## -----------------------------------------------------------------------------------------------------------------------------------------

# create contingency table
ct <- table(combined_df_preg$malaria_positive, combined_df_preg$woman_malaria_positive)

chi_square_test <- chisq.test(ct)
print(chi_square_test)

library(vcd)
assocstats(ct)


## -----------------------------------------------------------------------------------------------------------------------------------------
### Plot: Of pregnant women, bar for counts of them that are positive, stacked bar inside that for respondents in that household
## -----------------------------------------------------------------------------------------------------------------------------------------

# Aggregate data to calculate proportions
plot_data <- combined_df_preg %>%
  group_by(woman_malaria_positive, malaria_positive) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(woman_malaria_positive) %>%
  mutate(proportion = count / sum(count))

# filter to remove NA values in malaria positivity among pregnant women
plot_data <- plot_data %>%
  dplyr::filter(!is.na(woman_malaria_positive))

# create a stacked bar chart
preg_comparison_plot <- ggplot(plot_data, aes(x = factor(woman_malaria_positive, labels = c("Negative", "Positive")), 
                      y = proportion, 
                      fill = factor(malaria_positive, labels = c("Negative", "Positive")))) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(name = "Respondent Malaria Status", values = color_palette) +
  labs(
    title = "Relationship Between Malaria Positivity in Pregnant \nWomen in Household and Survey Respondent",
    x = "Malaria Status of Pregnant Women",
    y = "Proportion",
    fill = "Respondent Malaria Status"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 10),
  ) + 
  theme_manuscript()

ggsave(filename = paste0(NMEPOutputs, "/pregnant_analysis/", Sys.Date(), '_preg_comparison_plot.pdf'), plot = preg_comparison_plot, width = 6, height = 6)

## -----------------------------------------------------------------------------------------------------------------------------------------
### Plot: Malaria TPR in Pregnant Women and TPR in Each Age Group
## -----------------------------------------------------------------------------------------------------------------------------------------

stacked_data <- combined_df_preg %>%
  dplyr::group_by(age_cat) %>%  # group by age group
  dplyr::summarise(
    pregnant_positive = sum(woman_malaria_positive == 1, na.rm = TRUE),
    pregnant_negative = sum(woman_malaria_positive == 0, na.rm = TRUE),
    respondent_positive = sum(malaria_positive == 1, na.rm = TRUE),
    respondent_negative = sum(malaria_positive == 0, na.rm = TRUE)
  ) %>%
  tidyr::pivot_longer(
    cols = c(pregnant_positive, pregnant_negative, respondent_positive, respondent_negative),
    names_to = c("group", "result"),
    names_sep = "_",
    values_to = "count"
  ) %>%
  dplyr::group_by(age_cat, group) %>%
  dplyr::mutate(
    proportion = count / sum(count),
    result = recode(result, 
                    positive = "Positive", 
                    negative = "Negative")
  )

# remove observations where age category is NA
stacked_data <- stacked_data %>%
  dplyr::filter(!is.na(age_cat))

# reorder rows in age order
stacked_data <- stacked_data %>%
  mutate(age_cat = factor(age_cat, levels = age_order)) %>%
  arrange(age_cat)

# create the stacked bar plot
stacked_bar_plot <- ggplot(stacked_data, aes(x = group, y = proportion, fill = result)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ age_cat) +
  scale_fill_manual(values = c("Positive" = "#c55c80", "Negative" = "#f2a5a1")) +
  labs(
    title = "Malaria Test Positivity by Age Group: Pregnant Women vs. Respondents",
    x = "Household Role",
    y = "Proportion",
    fill = "Result"
  ) +
  theme_manuscript() +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(stacked_bar_plot)

ggsave(filename = paste0(NMEPOutputs, "/pregnant_analysis/", Sys.Date(), '_preg_agegroup_plot.pdf'), plot = stacked_bar_plot, width = 8, height = 6)


## =========================================================================================================================================
### WEIGHTED ANALYSIS: Odds Ratio Plots
## =========================================================================================================================================

## -----------------------------------------------------------------------------------------------------------------------------------------
### Get Weighted Values for Each Ward
## -----------------------------------------------------------------------------------------------------------------------------------------

# define survey design for weighted calculations
kano_design <- svydesign(
  id = ~sn + ea,  # specify sampling units
  strata = ~Ward + settlement_type,  # stratify by ward and settlement type
  weights = ~hh_weight,  # apply household weights
  data = combined_df,  # input dataset
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

