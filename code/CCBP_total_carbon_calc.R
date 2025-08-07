#### Calculating standard carbon volume for different registries ######

library(stringr)
library(dplyr)
library(lubridate)

#### first bringing CAR units into "sum of credit volume column originally in NTU #####

# Load the data
load("Combined_Carbon_Market_Data.RDATA")

ls()
class(combined_data)

# Check structure
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

#### ICR has c() format in column "projects_estimated_annual_mitigations" - going to try to use this to add up only years until 2025 ####

# Process ICR registry: extract and sum values from 'projects_estimated_annual_mitigations' up to 2025


# clean up messy c() entries in ICR
#### ICR try 1 - doesn't work #####

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



#### Try 5 - ICR ####


# Extract all c(...) groups into a list of strings
all_c_groups <- str_extract_all(combined_data$projects_estimated_annual_mitigations, "c\\([^)]*\\)")

# Create empty vectors for year/value extraction
combined_data$icr_year_raw <- NA_character_
combined_data$icr_value_raw <- NA_character_

# Assign only to ICR rows
icr_rows <- which(combined_data$registry == "ICR" & combined_data$source == "ZEL_search")

for (i in icr_rows) {
  c_blocks <- all_c_groups[[i]]
  
  # Ensure at least 3 blocks exist: timestamp, year, value
  if (length(c_blocks) >= 3) {
    combined_data$icr_year_raw[i] <- c_blocks[2]
    combined_data$icr_value_raw[i] <- c_blocks[3]
  }
}

combined_data <- combined_data %>%
  mutate(
    icr_year_clean = str_extract_all(icr_year_raw, "\\d{4}"),
    icr_value_clean = str_extract_all(icr_value_raw, "-?\\d*\\.?\\d+")
  )

icr_results <- rep(NA, nrow(combined_data))

for (i in icr_rows) {
  years <- combined_data$icr_year_clean[[i]]
  values <- combined_data$icr_value_clean[[i]]
  
  if (length(years) != length(values)) next
  
  years <- as.numeric(years)
  values <- as.numeric(values)
  
  icr_results[i] <- sum(values[years <= 2025], na.rm = TRUE)
}

combined_data$sum_of_credit_volume <- ifelse(
  combined_data$registry == "ICR" & combined_data$source == "ZEL_search",
  icr_results,
  combined_data$sum_of_credit_volume
)


combined_data %>%
  filter(registry == "ICR", source == "ZEL_search") %>%
  select(icr_year_raw, icr_value_raw, sum_of_credit_volume) %>%
  head(43)

##### ICR is still really messed up. It is only giving results for 9 rows, whereas even for the normally formatted rows there should be 10 with values. And none with the extra c() of timestamps works, apparently. I'm not seeing a good fix here. #####

### check ICR ###

# View ICR entries from ZEL_search and their calculated credit volumes
icr_check_table <- combined_data %>%
  filter(registry == "ICR", source == "ZEL_search") %>%
  select(registry, source, sum_of_credit_volume)  # Add other IDs if needed

# View first rows
head(icr_check_table, 20)

icr_check_table %>% filter(!is.na(sum_of_credit_volume)) %>% nrow()

icr_check_table %>% filter(is.na(sum_of_credit_volume))


combined_data %>%
  filter(registry == "ICR", source == "ZEL_search") %>%
  summarise(
    total = n(),
    with_volume = sum(!is.na(sum_of_credit_volume)),
    without_volume = sum(is.na(sum_of_credit_volume))
  )


##### Clean Development Mechanism ##### --- All of this is no longer working!!

## to calculate total estimated carbon for this registry, we needed to mannually enter the crediting start and end dates. This information was available in individual project websites, but did not come with the datasheet export option. ##


### calculate number of years from start date to end date, or 2025, whichever is sooner

class(combined_data$crediting_period_start_date)

# cdm_zel_data <- subset(combined_data,  ##### make subset dataset ##### 
cdm_zel_data <- combined_data[combined_data$registry == "Clean Development Mechanism" & combined_data$source == "ZEL_search",]

cdm_zel_data <- cdm_zel_data %>%
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


### Checking output of CDM year calcs ####

new_CDM <- cdm_zel_data %>%
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

### making emissions numeric

str(cdm_zel_data[, c("estimated_annual_emission_reductions", "credited_years")])

cdm_zel_data$estimated_annual_emission_reductions <- as.numeric(cdm_zel_data$estimated_annual_emission_reductions)

#### multiply CDM annual credit volume by calculated years ####

cdm_zel_data <- cdm_zel_data %>%
  mutate(
    sum_of_credit_volume = case_when(
      registry == "Clean Development Mechanism" & source == "ZEL_search" ~
        estimated_annual_emission_reductions * credited_years,
      TRUE ~ NA_real_
    )
  )

### test it worked ####

CDM_check <- cdm_zel_data %>%
  filter(registry == "Clean Development Mechanism", source == "ZEL_search") %>%
  select(estimated_annual_emission_reductions, credited_years, sum_of_credit_volume) %>%
  head(70)


#### bringing into combined_data ####

## removing outdates CDM ZEL_search rows, so that we don't duplicate

combined_data <- combined_data[!(combined_data$registry == "Clean Development Mechanism" & 
                                   combined_data$source == "ZEL_search"), ]

### checking before Rbind

ncol(cdm_zel_data)
ncol(combined_data)

setdiff(names(cdm_zel_data), names(combined_data))
setdiff(names(combined_data), names(cdm_zel_data))

### could remove the additional columns to make a match, or try bind_rows

combined_data <- bind_rows(cdm_zel_data, combined_data)

### didn't work, lets try removing helper columns:
cdm_zel_data <- cdm_zel_data %>%
  select(names(combined_data))  # Keep only columns that exist in original combined_data


### combine back into combined_data - add back in the updated rows for CDM with new calculation
combined_data <- rbind(cdm_zel_data, combined_data)




##### Social Carbon #####
##  completely not working........ ##


### calculate number of years from start date to end date, or 2025, whichever is sooner

class(combined_data$crediting_period_start_date)


socialcarb_zel_data <- combined_data[combined_data$registry == "Social Carbon" & combined_data$source == "ZEL_search",]

socialcarb_zel_data$estimated_annual_emission_reductions <- as.numeric(socialcarb_zel_data$estimated_annual_emission_reductions)

## this as.numeric conversion isn't working...
socialcarb_zel_data$crediting_period_start_date <- as.numeric(socialcarb_zel_data$crediting_period_start_date)
socialcarb_zel_data$crediting_period_end_date <- as.numeric(socialcarb_zel_data$crediting_period_end_date)

socialcarb_zel_data <- socialcarb_zel_data %>%
  mutate(
    parsed_start_date = mdy(crediting_period_start_date),
    parsed_end_date   = mdy(crediting_period_end_date)
  ) %>%
  mutate(
    start_year = case_when(
      registry == "Social Carbon" & source == "ZEL_search" ~ year(parsed_start_date),
      TRUE ~ NA_integer_
    ),
    end_year = case_when(
      registry == "Social Carbon" & source == "ZEL_search" ~ year(parsed_end_date),
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


### Checking output of CDM year calcs ####

check_socialcarb <- socialcarb_zel_data %>%
  filter(registry == "Social Carbon", source == "ZEL_search") %>%
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
  head(14)


#### multiply CDM annual credit volume by calculated years ####

socialcarb_zel_data <- socialcarb_zel_data %>%
  mutate(
    sum_of_credit_volume = case_when(
      registry == "Social Carbon" & source == "ZEL_search" ~
        estimated_annual_emission_reductions * credited_years,
      TRUE ~ NA_real_
    )
  )

### test it worked ####

socialcarb_check <- socialcarb_zel_data %>%
  filter(registry == "Clean Development Mechanism", source == "ZEL_search") %>%
  select(estimated_annual_emission_reductions, credited_years, sum_of_credit_volume) %>%
  head(70)


#### bringing into combined_data ####

## removing outdates CDM ZEL_search rows, so that we don't duplicate

combined_data <- combined_data[!(combined_data$registry == "Clean Development Mechanism" & 
                                   combined_data$source == "ZEL_search"), ]

### combine back into combined_data - add back in the updated rows for CDM with new calculation
combined_data <- rbind(cdm_zel_data, combined_data)



### save as rdata file

save(combined_data, file = "Combined_Carbon_Market_Data_Updated.RDATA")


# write csv
# write.csv(data, "Combined_Carbon_Market_Data_Updated.csv", row.names = FALSE)



