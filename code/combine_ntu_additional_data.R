#### Workflow to join additional data collection to NTU dataset ####
# Zoë Lieb
# 16 July 2025

# Load required libraries
library(dplyr)    
library(readr)    
library(janitor)  
library(tidyverse)

# Step 1: Load both datasets
ntu_data <- read_csv("/Users/zoe/Desktop/CCBP/CCBP_database/data/Carbon_Market_Data_20250408.csv")        # Large NTU dataset
additional_data <- read_csv("/Users/zoe/Desktop/CCBP/CCBP_database/data/All_databases_incl.csv")          # additional dataset

# Step 2: Clean column names to ensure compatibility (e.g., remove spaces, lowercase, underscores)
ntu_data <- janitor::clean_names(ntu_data)
additional_data <- janitor::clean_names(additional_data)

# Step 3: Rename NTU dataset columns to match Additional dataset
# Mapping NTU columns (cleaned) to Additional dataset naming
rename_map <- c(
  registry = "registry",
  buyer = "proponent",
  headquarter_country = "proponent_country_manual",
  uid = "id",
  project_name = "project_name",
  project_sector = "project_sector",
  project_methodologies = "project_methodologies",
  project_country = "host_country_or_area",
  est_price_at_retirement_date = "est_price_at_retirement_date"
)

glimpse(ntu_data)
ntu_renamed <- ntu_data %>%
  rename(!!!setNames(names(rename_map), rename_map))  # Flip name pairs: old_name = new_name

# Step 4: Add origin marker
ntu_renamed <- ntu_renamed %>% mutate(source = "NTU")
additional_data <- additional_data %>% mutate(source = "ZEL_search")


# Step 5: Bind the two datasets together row-wise
# This will keep all unique columns across both datasets
combined_data <- bind_rows(ntu_renamed, additional_data)

# Step 6: Check for duplicates or preview structure
glimpse(combined_data)

duplicated_ids <- combined_data %>% filter(duplicated(id))
head(duplicated_ids)

# Step 7: Export the combined dataset
save(combined_data, file = "Combined_Carbon_Market_Data.RDATA")

# Save it as a .RData file
save(methodologies_by_registry, file = "combineddata_methodology_by_reg.RData")


##### Summarising methodologies per registry ######

# Summarize methodologies by registry
methodologies_by_registry <- combined_data %>%
  filter(!is.na(registry) & !is.na(project_methodologies)) %>%   # Remove NA values
  group_by(registry) %>%
  summarise(
    project_methodologies = paste(unique(project_methodologies), collapse = ", ")
  ) %>%
  arrange(registry)  # Optional: alphabetize the registries

# View or export
print(methodologies_by_registry)

# Save it as a .RData file
save(methodologies_by_registry, file = "Registry_Methodologies_Summary.RData")

# Optional: write to CSV
# write_csv(methodologies_by_registry, "Registry_Methodologies_Summary.csv")

