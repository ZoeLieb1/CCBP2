#### Calculating standard carbon volume for different registries ######

#### first bringing CAR units into "sum of credit volume column originally in NTU #####

# Load the data
data <- read.csv("Combined_Carbon_Market_Data.csv", stringsAsFactors = FALSE)

# Check structure (optional but useful for debugging)
# str(ntu_df)

### CAR registry ###
# Update 'sum_of_credit_volume' with 'car_total_number_offset_credits_registered'
data$sum_of_credit_volume[!is.na(data$car_total_number_offset_credits_registered)] <- 
  data$car_total_number_offset_credits_registered[!is.na(data$car_total_number_offset_credits_registered)]


#### BCR and Tero together ####

# Update for BCR and Tero registries using 'total_issued_units'
data$sum_of_credit_volume[!is.na(data$total_issued_units)] <- 
  data$total_issued_units[!is.na(data$total_issued_units)]


#### Australia ETS and JCM registries using 't_co2_eq_total' ####

data$sum_of_credit_volume[!is.na(data$t_co2_eq_total)] <- 
  data$t_co2_eq_total[!is.na(data$t_co2_eq_total)]


#### slightly trickier calcs now, pulling in registries that only report annually and we need to count how many years they have been functioning ####

### ICR has c() format in column "projects_estimated_annual_mitigations" - going to try to use this to add up only years until 2025 ####

# Process ICR registry: extract and sum values from 'projects_estimated_annual_mitigations' up to 2025


# clean up messy c() entries in ICR
#### ICR try 1 - doesn't work #####

library(stringr)

data$sum_of_credit_volume[data$registry == "ICR" & !is.na(data$projects_estimated_annual_mitigations)] <- 
  sapply(data$projects_estimated_annual_mitigations[data$registry == "ICR" & !is.na(data$projects_estimated_annual_mitigations)], function(x) {
    
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

data$sum_of_credit_volume[data$registry == "ICR" & !is.na(data$projects_estimated_annual_mitigations)] <- 
  sapply(data$projects_estimated_annual_mitigations[data$registry == "ICR" & !is.na(data$projects_estimated_annual_mitigations)], function(x) {
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
data <- read.csv("Combined_Carbon_Market_Data.csv", stringsAsFactors = FALSE)

# Clean 'projects_estimated_annual_mitigations' by removing the first c(...) if more than 2 exist
data$projects_estimated_annual_mitigations <- sapply(data$projects_estimated_annual_mitigations, function(x) {
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

unique(data$projects_estimated_annual_mitigations)


#### ICR try 4 - spliting into new columns ####

library(stringr)
library(dplyr)

# Load the data
data <- read.csv("Combined_Carbon_Market_Data.csv", stringsAsFactors = FALSE)

# Split into parts wherever 'c(' appears
split_parts <- str_split_fixed(data$projects_estimated_annual_mitigations, "c\\(", 4)

# Assign to new columns, re-add the 'c(' to make them valid R expressions if needed
data$icr_timestamp <- ifelse(split_parts[,2] != "", paste0("c(", str_remove(split_parts[,2], "\\)$")), NA)
data$icr_year     <- ifelse(split_parts[,3] != "", paste0("c(", str_remove(split_parts[,3], "\\)$")), NA)
data$icr_value    <- ifelse(split_parts[,4] != "", paste0("c(", str_remove(split_parts[,4], "\\)$")), NA)


#### splitting cells kind of worked. Issue that it dropped the closing parenthese?

#### now to calculate total volume of carbon by adding up the values for each year in a given cell, up to 2025 ####

library(stringr)

# Create an empty vector for results
icr_rows <- which(data$registry == "ICR")
icr_results <- rep(NA, nrow(data))

# Loop only over ICR rows
for (i in icr_rows) {
  year_str <- data$icr_year[i]
  value_str <- data$icr_value[i]
  
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
data$sum_of_credit_volume <- ifelse(data$registry == "ICR", icr_results, data$sum_of_credit_volume)



##### Clean Development Mechanism #####
## to calculate total estimated carbon for this registry, we needed to annually enter the crediting start and end dates. This information was available in individual project websites, but did not come with the datasheet export option. ##




# Save it as a .RData file
save(data, file = "Combined_Carbon_Market_Data_Updated.RData")



# write csv
# write.csv(data, "Combined_Carbon_Market_Data_Updated.csv", row.names = FALSE)


git rm -r --cached
git push origin --force --all
git reset --soft HEAD~11
git filter-branch --force --index-filter "git rm --cached --ignore-unmatch path/to/largefile" --prune-empty --tag-name-filter cat -- --all
system("git rm -r --cached .")

system("git push origin main --force")
