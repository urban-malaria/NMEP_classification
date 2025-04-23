
source("load_path.R")

## =========================================================================================================================================
### KATSINA
## =========================================================================================================================================


# Read in Grace's version of the cleaned ITN data and Gift/Yusuf's version
Grace_katsina_itn <- readxl::read_excel(file.path(ITNDir, "cleaned/pbi_distribution_Katsina_clean.xlsx"))
their_katsina_itn <- readxl::read_excel(file.path(ITNDir, "household_member_ward_summaries_Itn_distribution/ITN_distribution_total_ward_katsina_2022.xlsx"))

# Compare ward names between the two cleaned versions
mismatched_df1 <- setdiff(Grace_katsina_itn$Ward, their_katsina_itn$Ward)
mismatched_df2 <- setdiff(their_katsina_itn$Ward, Grace_katsina_itn$Ward)

cat("Values in Grace's cleaned ITN data but not in Gift/Yusuf's cleaned ITN data:\n")
print(mismatched_df1)

cat("\nValues in Gift/Yusuf's cleaned ITN data but not in Grace's cleaned ITN data:\n")
print(mismatched_df2)

# This comparison shows how many ward names differ due to inconsistent spelling/formatting.

# Now load the extracted dataset (the ITN data gets merged with the extracted data for use in the reprioritization)
katsina_extracted_data <- read.csv(file.path(ExtractedDir, "Katsina_wards_variables.csv"))

# Grace's cleaned ITN data matches the extracted dataset, which will minimize merge issues.
mismatched_df1 <- setdiff(Grace_katsina_itn$Ward, katsina_extracted_data$WardName)
mismatched_df2 <- setdiff(katsina_extracted_data$WardName, Grace_katsina_itn$Ward)

cat("Wards in Grace's cleaned ITN data but not in extracted data:\n")
print(mismatched_df1)

cat("\nWards in extracted data but not in Grace's cleaned ITN data:\n")
print(mismatched_df2)

# Now check Gift/Yusuf's cleaned ITN data against the extracted data
mismatched_df1 <- setdiff(their_katsina_itn$Ward, katsina_extracted_data$WardName)
mismatched_df2 <- setdiff(katsina_extracted_data$WardName, their_katsina_itn$Ward)

cat("Wards in Gift/Yusuf's cleaned ITN data but not in extracted data:\n")
print(mismatched_df1)

cat("\nWards in extracted data but not in Gift/Yusuf's cleaned ITN data:\n")
print(mismatched_df2)

# This final comparison demonstrates that Gift/Yusuf's cleaned ITN data has many mismatches with the extracted dataset,
# which could lead to failed joins or missing data during analysis.

# It makes sense that when I ran the package with my version of the cleaned ITN data, I got a higher number of reprioritized wards than the numbers we
# originally sent to the NMEP, because more wards get matched during the merge with the extracted data.