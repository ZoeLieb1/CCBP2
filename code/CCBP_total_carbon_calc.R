#### Calculating standard carbon volume for different registries ######

library(stringr)
library(dplyr)
library(lubridate)
library(tidyr)

#### first bringing CAR units into "sum of credit volume column originally in NTU #####

# Load the data
load("Combined_Carbon_Market_Data.RDATA")

ls()
class(combined_data)
colnames(combined_data)

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

## checking how may rows should have data in ICR

subset_icr_raw <- combined_data %>%
  filter(registry == "ICR", source == "ZEL_search") %>%
  select(registry, source, projects_estimated_annual_mitigations)

# View the result
View(subset_icr_raw)

### there should be 40 total calculations, even though not all will produce a number because some projects have no value generated yet

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


subset_icr <- combined_data %>%
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

###Brooke's attempt
library(tidyr)
library(purrr)

subset_icr <- combined_data %>%
  filter(registry == "ICR", source == "ZEL_search") %>%
  select(id, icr_year_raw, icr_value_raw, sum_of_credit_volume)

# Example transformation
expanded_data <- subset_icr %>%
  mutate(
    years = map(icr_year_raw, ~ eval(parse(text = .x))),
    values = map(icr_value_raw, ~ eval(parse(text = .x)))
  ) %>%
  mutate(id = row_number()) %>%  # unique ID per row
  unnest(c(years, values)) %>%
  pivot_wider(names_from = years, values_from = values)

View(expanded_data)

#Zoe now you should be able to bring "expanded data" back into the main dataframe using the id

# add ICR data back into combined_data

# add rows up to 2025, plant that into total cabron volumne column


##### @ BROOKE - I think I have resolved the c()'s actually splitting out properly. Now when I go onto your step to the expanded_data, it only shows years starting in 2024, but there should be many other years (early as 2013 has years with corresponding values)


#### @ BROOKE AGAIN - a ha! Ok I see now that the year values come out of order, so 2013 etc. are in there, just not in numerical order. So I think things are going normally now.

### ZEL attempting code clean up ####

#################################

library(stringr)
library(dplyr)
library(purrr)
library(tidyr)

# Step 1: Extract c(...) groups
all_c_groups <- str_extract_all(combined_data$projects_estimated_annual_mitigations, "c\\([^)]*\\)")

# Step 2: Create new columns to store raw year/value strings
combined_data$icr_year_raw <- NA_character_
combined_data$icr_value_raw <- NA_character_

# Step 3: Fill in those columns correctly for ICR/ZEL_search rows
icr_rows <- which(combined_data$registry == "ICR" & combined_data$source == "ZEL_search")

for (i in icr_rows) {
  c_blocks <- all_c_groups[[i]]
  if (length(c_blocks) < 2) next
  
  if (length(c_blocks) >= 3 && str_detect(c_blocks[1], "GMT|\\d{4}")) {
    combined_data$icr_year_raw[i] <- c_blocks[2]
    combined_data$icr_value_raw[i] <- c_blocks[3]
  } else if (length(c_blocks) == 2) {
    combined_data$icr_year_raw[i] <- c_blocks[1]
    combined_data$icr_value_raw[i] <- c_blocks[2]
  }
}

# Step 4: Build subset and expand c(...) vectors
subset_icr <- combined_data %>%
  filter(registry == "ICR", source == "ZEL_search") %>%
  select(id, icr_year_raw, icr_value_raw)

expanded_data <- subset_icr %>%
  mutate(
    year_vec = map(icr_year_raw, ~ tryCatch(eval(parse(text = .x)), error = function(e) NA)),
    value_vec = map(icr_value_raw, ~ tryCatch(eval(parse(text = .x)), error = function(e) NA))
  ) %>%
  filter(!map_lgl(year_vec, is.null), !map_lgl(value_vec, is.null)) %>%
  unnest(c(year_vec, value_vec)) %>%
  rename(year = year_vec, value = value_vec)

# Step 5: Pivot wider by year
expanded_data <- expanded_data %>%
  pivot_wider(
    id_cols = id,  # Use actual id from original dataset!
    names_from = year,
    values_from = value
  )

# Step 6: Identify year columns <= 2025
year_cols_to_sum <- names(expanded_data)[
  grepl("^\\d{4}$", names(expanded_data)) & as.numeric(names(expanded_data)) <= 2025
]

# Step 7: Sum the values for each project
expanded_data <- expanded_data %>%
  rowwise() %>%
  mutate(sum_of_credit_volume = sum(c_across(all_of(year_cols_to_sum)), na.rm = TRUE)) %>%
  ungroup()

# Step 8: Join back to `combined_data` using the `id` column
combined_data <- combined_data %>%
  left_join(
    expanded_data %>% select(id, sum_of_credit_volume),
    by = "id",
    suffix = c("", "_from_expanded")
  ) %>%
  mutate(
    sum_of_credit_volume = coalesce(sum_of_credit_volume_from_expanded, sum_of_credit_volume)
  ) %>%
  select(-sum_of_credit_volume_from_expanded)


### ICR done ####




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
##  works now... no idea what changed! ##


### calculate number of years from start date to end date, or 2025, whichever is sooner

socialcarb_zel_data <- combined_data[combined_data$registry == "Social Carbon" & combined_data$source == "ZEL_search",]


class(socialcarb_zel_data$crediting_period_start_date)
class(socialcarb_zel_data$crediting_period_end_date)
class(socialcarb_zel_data$estimated_annual_emission_reductions)

socialcarb_zel_data <- socialcarb_zel_data %>%
  mutate(
    parsed_start_date = dmy(crediting_period_start_date),
    parsed_end_date   = dmy(crediting_period_end_date)
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


### Checking output of year calcs ####

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
  filter(registry == "Social Carbon", source == "ZEL_search") %>%
  select(estimated_annual_emission_reductions, credited_years, sum_of_credit_volume) %>%
  head(70)


#### bringing into combined_data ####

## removing outdates Social Carbon ZEL_search rows, so that we don't duplicate

combined_data <- combined_data[!(combined_data$registry == "Social Carbon" & 
                                   combined_data$source == "ZEL_search"), ]

### checking before Rbind

ncol(socialcarb_zel_data)
ncol(combined_data)

setdiff(names(socialcarb_zel_data), names(combined_data))
setdiff(names(combined_data), names(socialcarb_zel_data))

### didn't work, lets try removing helper columns:
cdm_zel_data <- cdm_zel_data %>%
  select(names(combined_data))  # Keep only columns that exist in original combined_data


### combine back into combined_data - add back in the updated rows for CDM with new calculation
combined_data <- rbind(cdm_zel_data, combined_data)





######### Verra calc ########################################

### calculate number of years from start date to end date, or 2025, whichever is sooner


verra_zel_data <- combined_data[combined_data$registry == "Verra" & combined_data$source == "ZEL_search",]

class(verra_zel_data$crediting_period_start_date)
class(verra_zel_data$crediting_period_end_date)
class(verra_zel_data$estimated_annual_emission_reductions)

# doing this as.numeric turns the whole column into NAs
## verra_zel_data$crediting_period_start_date <- as.numeric(verra_zel_data$crediting_period_start_date)

verra_zel_data <- verra_zel_data %>%
  mutate(
    parsed_start_date = mdy(crediting_period_start_date),
    parsed_end_date   = mdy(crediting_period_end_date)
  ) %>%
  mutate(
    start_year = case_when(
      registry == "Verra" & source == "ZEL_search" ~ year(parsed_start_date),
      TRUE ~ NA_integer_
    ),
    end_year = case_when(
      registry == "Verra" & source == "ZEL_search" ~ year(parsed_end_date),
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


### Checking output of year calcs ####

check_verra <- verra_zel_data %>%
  filter(registry == "Verra", source == "ZEL_search") %>%
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

verra_zel_data <- verra_zel_data %>%
  mutate(
    sum_of_credit_volume = case_when(
      registry == "Verra" & source == "ZEL_search" ~
        estimated_annual_emission_reductions * credited_years,
      TRUE ~ NA_real_
    )
  )

### test it worked ####

verra_check <- verra_zel_data %>%
  filter(registry == "Verra", source == "ZEL_search") %>%
  select(estimated_annual_emission_reductions, credited_years, sum_of_credit_volume) %>%
  head(30)


#### bringing into combined_data ####

## removing outdates CDM ZEL_search rows, so that we don't duplicate

combined_data <- combined_data[!(combined_data$registry == "Verra" & 
                                   combined_data$source == "ZEL_search"), ]

### combine back into combined_data - add back in the updated rows for CDM with new calculation
combined_data <- rbind(verra_zel_data, combined_data)


###########################################


### save as rdata file

save(combined_data, file = "Combined_Carbon_Market_Data_Updated.RDATA")

# checking columns
# load the file
load("Combined_Carbon_Market_Data_Updated.RDATA")

# see what objects were loaded
ls()

colnames(combined_data)

# making a data subset so that I can check for forest-based keywords

# create a new dataframe with just the desired columns
df_subset_orig_combined <- combined_data[, c("sum_of_credit_volume",
                               "project_name",
                               "proponent_country_manual",
                               "buyer_continent",
                               "host_country_or_area",
                               "project_continent",
                               "project_description",
                               "project_methodologies",
                               "source",
                               "project_sector",
                               "project_sub_sector")]

# quick check
head(df_subset)

unique(df_subset_orig_combined$proponent_country_manual)
unique(df_subset_orig_combined$host_country_or_area)



# write csv
# write.csv(data, "Combined_Carbon_Market_Data_Updated.csv", row.names = FALSE)



##### fixing missing continents in ZEL_search entries #####

# need to auto-fill continent from proponent_country_manual into buyer_continent and host_country_or_area into project_continent

### what should we do about multi-country entries? A handful of projects have more than one country as proponent

# first thing - this is how we are separating out multi-country proponents entries, then onto auto-filling continent

#### Spliting out mult-country entries in proponent_country_manual and dividing up sum_of_credit_volume amount ###

# -------------------------------------------------------------------
# STEP 1 — Identify and isolate only multi-country rows
# -------------------------------------------------------------------
multi_country_rows <- combined_data %>%
  filter(str_detect(proponent_country_manual, "[/,]"))

# -------------------------------------------------------------------
# STEP 2 — Split country names into separate rows
#           (keeping all other columns the same)
# -------------------------------------------------------------------
multi_country_expanded <- multi_country_rows %>%
  # Split on commas or slashes, trim spaces
  separate_rows(proponent_country_manual, sep = "[/,]") %>%
  mutate(proponent_country_manual = str_trim(proponent_country_manual))

# -------------------------------------------------------------------
# STEP 3 — Divide `sum_of_credit_volume` evenly among split rows
# -------------------------------------------------------------------
multi_country_expanded <- multi_country_expanded %>%
  group_by(across(-c(proponent_country_manual, sum_of_credit_volume))) %>% # group by all other cols
  mutate(
    n_countries = n(),                                    # count how many countries the row was split into
    sum_of_credit_volume = sum_of_credit_volume / n_countries  # divide credit volume evenly
  ) %>%
  ungroup() %>%
  select(-n_countries)   # remove helper column

# -------------------------------------------------------------------
# STEP 4 — Verify result
# -------------------------------------------------------------------
multi_country_expanded %>%
  select(proponent_country_manual, sum_of_credit_volume) %>%
  head()

# -----------
# Step 5 - recombine
# -----------
# re-combine back into combined_data dataset #

combined_data_expanded <- combined_data %>%
  # Remove the original multi-country rows
  filter(!str_detect(proponent_country_manual, "[/,]")) %>%
  # Add the new expanded rows
  bind_rows(multi_country_expanded)


##### Countries to continents columns auto-fill #####

library(dplyr)
install.packages("countrycode")
library(countrycode)
library(stringr)

## 1. proponent_country_manual to buyer_continent ##

# only looking at ZEL_search rows (because all of these are missing continent)

combined_data_expanded <- combined_data_expanded %>%
  mutate(
    buyer_continent = case_when(
      # Only apply to ZEL_search rows
      source == "ZEL_search" & str_detect(proponent_country_manual, "[/,]") ~ NA_character_,
      # Some entries need manual entry
      source == "ZEL_search" & proponent_country_manual %in% c("Hong Kong") ~ "Asia",
      source == "ZEL_search" & proponent_country_manual %in% c("Jersey") ~ "Europe",
      source == "ZEL_search" & proponent_country_manual %in% c("Bermuda", "Cayman Islands", "Bahamas") ~ "North America",
      source == "ZEL_search" & proponent_country_manual %in% c("Republic of Mauritius", "Mauritius") ~ "Africa",
      source == "ZEL_search" & proponent_country_manual %in% c("Republic of Korea (South Korea)", "South Korea") ~ "Asia",
      source == "ZEL_search" & proponent_country_manual %in% c("United Kindgom", "United Kingdom of Great Britain and Northern Ireland", "United Kingdom") ~ "Europe",
      
      # Automatic lookup for other ZEL_search rows
      source == "ZEL_search" ~ countrycode(proponent_country_manual, origin = "country.name", destination = "continent"),
      
      # Leave all others unchanged
      TRUE ~ buyer_continent
    )
  )


# checking what came out for continent with ZEL_search

combined_data_expanded %>%
  filter(source == "ZEL_search") %>%
  count(buyer_continent, sort = TRUE)


# checking for unfixed rows - no continent added, could be due to typos, missed a manual entry, or multi-entries

unmapped_countries <- combined_data_expanded %>%
  filter(source == "ZEL_search", is.na(buyer_continent)) %>%
  distinct(proponent_country_manual) %>%
  arrange(proponent_country_manual)

# View in console
unmapped_countries



### 2. doing all the same, but for host country/continent ###

# --- HOST COUNTRY → PROJECT CONTINENT MAPPING ---


# Trim whitespace before matching
combined_data_expanded <- combined_data_expanded %>%
  mutate(host_country_or_area = trimws(host_country_or_area)) %>%
  mutate(
    project_continent = case_when(
      # --- Skip multi-country or "International" entries ---
      str_detect(host_country_or_area, "[/,]") ~ NA_character_,
      host_country_or_area %in% c("International") ~ NA_character_,
      
      # --- Manual corrections for known cases and special codes ---
      host_country_or_area %in% c("MX", "US") ~ "North America",
      host_country_or_area %in% c("Viet Nam", "Vietnam") ~ "Asia",
      host_country_or_area %in% c("Republic of Korea", "South Korea") ~ "Asia",
      host_country_or_area %in% c("Lao People's Democratic Republic", "Laos") ~ "Asia",
      host_country_or_area %in% c("Timor-Leste") ~ "Asia",
      host_country_or_area %in% c("Taiwan") ~ "Asia",
      host_country_or_area %in% c("Papua New Guinea", "Fiji", "Vanuatu", "Solomon Islands", "New Caledonia") ~ "Oceania",
      host_country_or_area %in% c("Mauritius") ~ "Africa",
      host_country_or_area %in% c("Côte d'Ivoire") ~ "Africa",
      host_country_or_area %in% c("Congo, Dem. Rep", "Congo, Rep.") ~ "Africa",
      host_country_or_area %in% c("Kosovo") ~ "Europe",
      host_country_or_area %in% c("Guyana") ~ "South America",
      host_country_or_area %in% c("Aruba") ~ "North America",

      
      # --- Flexible matching for Congo variants ---
      str_detect(host_country_or_area, "^Congo") ~ "Africa",
      str_detect(host_country_or_area, "Dem\\. Rep") ~ "Africa",
      str_detect(host_country_or_area, "Rep\\.") ~ "Africa",
      
      # --- Automatic lookup for all other single-country entries ---
      TRUE ~ countrycode(host_country_or_area, origin = "country.name", destination = "continent")
    )
  )

### check for unmatched hosts

unmapped_hosts <- combined_data_expanded %>%
  filter(is.na(project_continent)) %>%
  distinct(host_country_or_area) %>%
  arrange(host_country_or_area)

unmapped_hosts

### hmm... can't seem to fix Congo issue... did space trim, added flexible matching, still not working.

# checking NAs for new continent entries

# Check which sources now have continent values
combined_data_expanded %>%
  count(source, !is.na(buyer_continent), !is.na(project_continent))




