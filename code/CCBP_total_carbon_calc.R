#### Calculating standard carbon volume for different registries ######

#### first bringing CAR units into "sum of credit volume column originally in NTU #####

# Load the data
load("Combined_Carbon_Market_Data.RDATA")

ls()
class(combined_data)  # or whatever object name you found

# Check structure (optional but useful for debugging)
# str(ntu_df)

### CAR registry ###
# Update 'sum_of_credit_volume' with 'car_total_number_offset_credits_registered'
combined_data$sum_of_credit_volume[!is.na(combined_data$car_total_number_offset_credits_registered)] <- 
  combined_data$car_total_number_offset_credits_registered[!is.na(combined_data$car_total_number_offset_credits_registered)]


#### BCR and Tero together ####

# Update for BCR and Tero registries using 'total_issued_units'
combined_data$sum_of_credit_volume[!is.na(combined_data$total_issued_units)] <- 
  combined_data$total_issued_units[!is.na(combined_data$total_issued_units)]


#### Australia ETS and JCM registries using 't_co2_eq_total' ####

combined_data$sum_of_credit_volume[!is.na(combined_data$t_co2_eq_total)] <- 
  combined_data$t_co2_eq_total[!is.na(combined_data$t_co2_eq_total)]


#### slightly trickier calcs now, pulling in registries that only report annually and we need to count how many years they have been functioning ####

### ICR has c() format in column "projects_estimated_annual_mitigations" - going to try to use this to add up only years until 2025 ####

# Process ICR registry: extract and sum values from 'projects_estimated_annual_mitigations' up to 2025


# clean up messy c() entries in ICR
#### ICR try 1 - doesn't work #####

library(stringr)

combined_data$sum_of_credit_volume[data$registry == "ICR" & !is.na(combined_data$projects_estimated_annual_mitigations)] <- 
  sapply(combined_data$projects_estimated_annual_mitigations[combined_data$registry == "ICR" & !is.na(combined_data$projects_estimated_annual_mitigations)], function(x) {
    
    # Extract all c(...) groups using regex
    c_blocks <- str_extract_all(x, "c\\(([^\\)]*)\\)")[[1]]
    
    # If we don't have at least 3 blocks, return NA
    if (length(c_blocks) < 3) return(NA)
    
    # Extract years (2nd block) and values (3rd block)
    years_raw <- c_blocks[2]
    values_raw <- c_blocks[3]
    
    # Clean years and values: remove 'c(', ')' and extra quotes
    years <- as.numeric(str_extract_all(years_raw, "\\d{4}")[[1]])
    values <- as.numeric(str_extract_all(values_raw, "\\d+\\.*\\d*|NA")[[1]])
    
    # Replace any "NA" as actual NA
    values[values == "NA"] <- NA
    values <- as.numeric(values)
    
    # Sum values for years <= 2025
    sum(values[years <= 2025], na.rm = TRUE)
  })


#### hmmm... that didn't really work 

### trying to extract years another way ####
####ICR try 2 - didn't work #####
library(stringr)

combined_data$sum_of_credit_volume[combined_data$registry == "ICR" & !is.na(combined_data$projects_estimated_annual_mitigations)] <- 
  sapply(combined_data$projects_estimated_annual_mitigations[combined_data$registry == "ICR" & !is.na(combined_data$projects_estimated_annual_mitigations)], function(x) {
    # Extract all c(...) blocks
    c_blocks <- str_extract_all(x, "c\\((.*?)\\)")[[1]]
    
    # Defensive check
    if (length(c_blocks) < 3) return(NA)
    
    # Extract and clean year block
    year_tokens <- unlist(str_split(c_blocks[2], ",\\s*"))
    years <- suppressWarnings(as.numeric(str_extract(year_tokens, "\\d{4}")))
    
    # Extract and clean value block
    value_tokens <- unlist(str_split(c_blocks[3], ",\\s*"))
    values <- suppressWarnings(as.numeric(str_replace_all(value_tokens, "[^0-9\\.]+", "")))
    
    # Defensive check for same length
    if (length(years) != length(values)) return(NA)
    
    # Sum values for years ≤ 2025
    sum(values[years <= 2025], na.rm = TRUE)
  })



#### ICR try 3 ####
library(stringr)
# Load data
combined_data

# Clean 'projects_estimated_annual_mitigations' by removing the first c(...) if more than 2 exist
combined_data$projects_estimated_annual_mitigations <- sapply(combined_data$projects_estimated_annual_mitigations, function(x) {
  if (is.na(x)) return(NA)
  
  # Find all c(...) blocks
  c_blocks <- str_extract_all(x, "c\\((.*?)\\)")[[1]]
  
  # If more than 2 blocks, remove the first one and rejoin the rest
  if (length(c_blocks) > 2) {
    return(paste(c_blocks[-1], collapse = ", "))
  } else {
    return(x)
  }
})

unique(combined_data$projects_estimated_annual_mitigations)


#### ICR try 4 - spliting into new columns ####

library(stringr)
library(dplyr)

# Load the data
combined_data

# Split into parts wherever 'c(' appears
split_parts <- str_split_fixed(combined_data$projects_estimated_annual_mitigations, "c\\(", 4)

# Assign to new columns, re-add the 'c(' to make them valid R expressions if needed
combined_data$icr_timestamp <- ifelse(split_parts[,2] != "", paste0("c(", str_remove(split_parts[,2], "\\)$")), NA)
combined_data$icr_year     <- ifelse(split_parts[,3] != "", paste0("c(", str_remove(split_parts[,3], "\\)$")), NA)
combined_data$icr_value    <- ifelse(split_parts[,4] != "", paste0("c(", str_remove(split_parts[,4], "\\)$")), NA)


#### splitting cells kind of worked. Issue that it dropped the closing parenthese?

#### now to calculate total volume of carbon by adding up the values for each year in a given cell, up to 2025 ####

library(stringr)

# Create an empty vector for results
icr_rows <- which(combined_data$registry == "ICR")
icr_results <- rep(NA, nrow(combined_data))

# Loop only over ICR rows
for (i in icr_rows) {
  year_str <- combined_data$icr_year[i]
  value_str <- combined_data$icr_value[i]
  
  if (is.na(year_str) || is.na(value_str)) next
  
  # Extract years
  years <- str_extract_all(year_str, "\\d{4}")[[1]]
  years <- as.numeric(years)
  
  # Extract values (numbers or "NA")
  raw_vals <- str_extract_all(value_str, "-?\\d*\\.?\\d+|NA")[[1]]
  values <- as.numeric(ifelse(raw_vals == "NA", NA, raw_vals))
  
  # Only proceed if lengths match
  if (length(years) != length(values)) next
  
  # Sum values where year <= 2025
  icr_results[i] <- sum(values[years <= 2025], na.rm = TRUE)
}

# Now assign results into sum_of_credit_volume
combined_data$sum_of_credit_volume <- ifelse(combined_data$registry == "ICR", icr_results, combined_data$sum_of_credit_volume)



##### Clean Development Mechanism #####
## to calculate total estimated carbon for this registry, we needed to annually enter the crediting start and end dates. This information was available in individual project websites, but did not come with the datasheet export option. ##


### calculate number of years from start date to end date, or 2025, whichever is sooner

library(dplyr)
library(lubridate)

class(combined_data$crediting_period_start_date)

combined_data <- combined_data %>%
  mutate(
    parsed_start_date = dmy(crediting_period_start_date),
    parsed_end_date   = dmy(crediting_period_end_date)
  ) %>%
  mutate(
    start_year = case_when(
      registry == "Clean Development Mechanism" & source == "ZEL_search" ~ year(parsed_start_date),
      TRUE ~ NA_integer_
    ),
    end_year = case_when(
      registry == "Clean Development Mechanism" & source == "ZEL_search" ~ year(parsed_end_date),
      TRUE ~ NA_integer_
    ),
    capped_end_year = case_when(
      !is.na(end_year) ~ pmin(end_year, 2025),
      TRUE ~ NA_integer_
    ),
    credited_years = case_when(
      !is.na(start_year) & !is.na(capped_end_year) ~ pmax(capped_end_year - start_year + 1, 0),
      TRUE ~ NA_integer_
    )
  )

### Checking output ####

new_CDM <- combined_data %>%
  filter(registry == "Clean Development Mechanism", source == "ZEL_search") %>%
  select(
    crediting_period_start_date,
    crediting_period_end_date,
    parsed_start_date,
    parsed_end_date,
    start_year,
    end_year,
    capped_end_year,
    credited_years
  ) %>%
  head(70)  # adjust to show more rows if needed







# write csv
# write.csv(data, "Combined_Carbon_Market_Data_Updated.csv", row.names = FALSE)



