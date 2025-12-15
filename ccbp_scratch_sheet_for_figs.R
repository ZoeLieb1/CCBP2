#### Step 1 - Clean and agrigate data ####

library(tidyverse)
library(countrycode)
library(readxl)
library(scales)
library(ggrepel)

host_carbon_totals <- Data_complete %>%
  mutate(
    host_country_or_area = case_when(
      host_country_or_area %in% c("US", "United States") ~ "United States of America",
      host_country_or_area %in% c("MX", "Mexico")        ~ "Mexico",
      TRUE ~ host_country_or_area
    )
  ) %>%
  group_by(host_country_or_area) %>%
  summarise(
    total_credit_volume = sum(sum_of_credit_volume, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    iso3c = countrycode(
      host_country_or_area,
      origin = "country.name",
      destination = "iso3c"
    ),
    iso3c = case_when(
      iso3c == "COD" ~ "ZAR",  # DR Congo
      iso3c == "ROU" ~ "ROM",  # Romania
      TRUE ~ iso3c
    )
  )



#### Step 2 — Clean WGI data (all indicators together) ####

wgi_path <- "/Users/zoe/Desktop/CCBP/CCBP_database/data/gov_indicators_wgi/wgidataset.xlsx"

wgi <- read_excel(wgi_path)

wgi_clean <- wgi %>%
  filter(
    indicator %in% c("cc", "ge", "pv", "rl", "rq", "va"),
    year == max(year, na.rm = TRUE)
  ) %>%
  mutate(
    estimate = as.numeric(estimate)
  ) %>%
  select(
    iso3c = code,
    countryname,
    indicator,
    estimate
  )


#### Step 3 - pivot wider for indicators ####

wgi_wide <- wgi_clean %>%
  pivot_wider(
    names_from  = indicator,
    values_from = estimate
  )


#### Step 4 - join carbon and governance indicators ####

host_carbon_wgi <- host_carbon_totals %>%
  left_join(wgi_wide, by = "iso3c")

# check

any(duplicated(wgi_wide$iso3c))
any(duplicated(host_carbon_totals$iso3c))



