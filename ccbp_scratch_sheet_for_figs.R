#### Step 1 - Clean and aggrigate data ####

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
      host_country_or_area %in% c("Republic of Moldova", "Moldova") ~ "Moldova",
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


host_carbon_totals %>%
  count(iso3c, sort = TRUE) %>%
  filter(n > 1)



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

#### Step 5 - pivoting longer again so that I can facet_wrap the indicators ####

host_carbon_wgi_long <- host_carbon_wgi %>%
  pivot_longer(
    cols = c(cc, ge, pv, rl, rq, va),
    names_to = "indicator",
    values_to = "estimate"
  )

# indicator labels

indicator_labels <- c(
  cc = "Control of Corruption",
  ge = "Government Effectiveness",
  pv = "Political Stability",
  rl = "Rule of Law",
  rq = "Regulatory Quality",
  va = "Voice & Accountability"
)

host_carbon_wgi_long <- host_carbon_wgi_long %>%
  mutate(
    indicator_label = indicator_labels[indicator]
  )

# country labels for the plots

label_countries_facets <- host_carbon_wgi_long %>%
  filter(
    total_credit_volume > 0,
    !is.na(estimate)
  ) %>%
  group_by(indicator, indicator_label) %>%
  summarise(
    data = list(
      bind_rows(
        slice_max(pick(everything()), total_credit_volume, n = 3),
        slice_min(pick(everything()), total_credit_volume, n = 3),
        slice_max(pick(everything()), estimate, n = 3),
        slice_min(pick(everything()), estimate, n = 3)
      ) %>%
        distinct(iso3c, .keep_all = TRUE)
    ),
    .groups = "drop"
  ) %>%
  unnest(data)


## plotting

gov_host_plot_faceted_labeled <- ggplot(
  host_carbon_wgi_long %>%
    filter(total_credit_volume > 0, !is.na(estimate)),
  aes(x = total_credit_volume, y = estimate)
) +
  geom_point(alpha = 0.6, size = 1.8) +
  
  geom_text_repel(
    data = label_countries_facets,
    inherit.aes = FALSE,
    aes(
      x = total_credit_volume,
      y = estimate,
      label = countryname
    ),
    size = 2.8,
    box.padding = 0.4,
    point.padding = 0.3,
    segment.color = "grey60",
    max.overlaps = Inf
  ) +
  
  scale_x_log10(
    labels = label_number(
      scale_cut = cut_si(""),
      accuracy = 1
    )
  ) +
  
  scale_y_continuous(n.breaks = 5) +
  
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    colour = "grey60"
  ) +
  
  facet_wrap(~ indicator_label, scales = "free_y") +
  
  labs(
    x = "Total forest-based carbon credit volume (log scale)",
    y = "Governance score (WGI)",
    title = "Forest-based carbon project volume vs governance",
    subtitle = "Host countries; World Governance Indicators (2023)"
  ) +
  
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),
    panel.spacing = unit(1, "lines")
  )

print(gov_host_plot_faceted_labeled)


## Adding GDP per capita to make graduated size symbols

gdp_path <- "/Users/zoe/Desktop/CCBP/CCBP_database/data/GDP_world_data/gdp_world_data_country.csv"

library(readr)

gdp <- read_csv(
  "/Users/zoe/Desktop/CCBP/CCBP_database/data/GDP_world_data/gdp_world_data_country.csv",
  show_col_types = FALSE
)


str(gdp)
head(gdp)

# clean and simplifty data

gdp_2024 <- gdp %>%
  transmute(
    iso3c = country_code,
    gdp_pc_2024.y = as.numeric(`2024`)
  )



## joining on ISO3

host_carbon_wgi_long <- host_carbon_wgi_long %>%
  left_join(gdp_2024, by = "iso3c")


summary(host_carbon_wgi_long$gdp_pc_2024.y)

host_carbon_wgi_long %>%
  filter(is.na(gdp_pc_2024.y)) %>%
  distinct(countryname, iso3c)


head(gdp_2024$iso3c)
unique(nchar(gdp_2024$iso3c))

host_carbon_wgi_long <- host_carbon_wgi_long %>%
  rename(gdp_pc_2024 = gdp_pc_2024.y)

##### adding GDP into facet wrap #####

library(scales)
library(ggrepel)

gov_host_plot_faceted_gdp <- ggplot(
  host_carbon_wgi_long %>%
    filter(
      total_credit_volume > 0,
      !is.na(estimate),
      !is.na(gdp_pc_2024)
    ),
  aes(
    x = total_credit_volume,
    y = estimate,
    size = gdp_pc_2024
  )
) +
  geom_point(alpha = 0.6) +
  
  geom_text_repel(
    data = label_countries_facets,
    inherit.aes = FALSE,
    aes(
      x = total_credit_volume,
      y = estimate,
      label = countryname
    ),
    size = 2.8,
    box.padding = 0.4,
    point.padding = 0.3,
    segment.color = "grey60",
    max.overlaps = Inf
  ) +
  
  scale_x_log10(
    labels = label_number(
      scale_cut = cut_si(""),
      accuracy = 1
    )
  ) +
  
  scale_size_continuous(
    name = "GDP per capita (USD, 2024)",
    trans = "log10",
    range = c(1.5, 6),
    labels = label_dollar(accuracy = 1)
  ) +
  
  scale_y_continuous(n.breaks = 5) +
  
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    colour = "grey60"
  ) +
  
  facet_wrap(~ indicator_label, scales = "free_y") +
  
  labs(
    x = "Total forest-based carbon credit volume (log scale)",
    y = "Governance score (WGI)",
    title = "Forest-based carbon project volume vs governance",
    subtitle = "Point size scaled by GDP per capita (2024)"
  ) +
  
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "right"
  )

print(gov_host_plot_faceted_gdp)


#### trying to plot with GDP logged ####

gov_host_plot_faceted_gdp <- ggplot(
  host_carbon_wgi_long %>%
    filter(
      total_credit_volume > 0,
      !is.na(estimate),
      !is.na(gdp_pc_2024.y),
      gdp_pc_2024.y > 0
    ),
  aes(
    x = total_credit_volume,
    y = estimate,
    size = gdp_pc_2024.y
  )
) +
  geom_point(alpha = 0.6) +
  
  geom_text_repel(
    data = label_countries_facets,
    inherit.aes = FALSE,
    aes(
      x = total_credit_volume,
      y = estimate,
      label = countryname
    ),
    size = 2.8,
    box.padding = 0.4,
    point.padding = 0.3,
    segment.color = "grey60",
    max.overlaps = Inf
  ) +
  
  scale_x_log10(
    labels = scales::label_number(
      scale_cut = scales::cut_si(""),
      accuracy = 1
    )
  ) +
  
  scale_size_continuous(
    name = "GDP per capita (USD, 2024)",
    trans = "log10",
    range = c(1.5, 6),
    labels = scales::label_dollar(accuracy = 1)
  ) +
  
  scale_y_continuous(n.breaks = 5) +
  
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
  
  facet_wrap(~ indicator_label, scales = "free_y") +
  
  labs(
    x = "Total forest-based carbon credit volume (log scale)",
    y = "Governance score (WGI)",
    title = "Forest-based carbon project volume vs governance",
    subtitle = "Point size scaled by log GDP per capita (2024)"
  ) +
  
  theme_minimal()

print(gov_host_plot_faceted_gdp)


#### Linear GDP size, unlogged 


gov_host_plot_faceted_gdp <- ggplot(
  host_carbon_wgi_long %>%
    filter(
      total_credit_volume > 0,
      !is.na(estimate),
      !is.na(gdp_pc_2024),
      gdp_pc_2024.y > 0
    ),
  aes(
    x = total_credit_volume,
    y = estimate,
    size = gdp_pc_2024
  )
) +
  geom_point(alpha = 0.6) +
  
  geom_text_repel(
    data = label_countries_facets,
    inherit.aes = FALSE,
    aes(
      x = total_credit_volume,
      y = estimate,
      label = countryname
    ),
    size = 2.8,
    box.padding = 0.4,
    point.padding = 0.3,
    segment.color = "grey60",
    max.overlaps = Inf
  ) +
  
  scale_x_log10(
    labels = scales::label_number(
      scale_cut = scales::cut_si(""),
      accuracy = 1
    )
  ) +
  
  scale_size_continuous(
    name = "GDP per capita (USD, 2024)",
    range = c(1.5, 6),
    labels = scales::label_dollar(accuracy = 1)
  ) +
  
  scale_y_continuous(n.breaks = 5) +
  
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    colour = "grey60"
  ) +
  
  facet_wrap(~ indicator_label, scales = "free_y") +
  
  labs(
    x = "Total forest-based carbon credit volume (log scale)",
    y = "Governance score (WGI)",
    title = "Forest-based carbon project volume vs governance",
    subtitle = "Point size scaled by GDP per capita (2024)"
  ) +
  
  theme_minimal()

print(gov_host_plot_faceted_gdp)


#### bringing in continent from Data_complete #####

# Step 1 - extract continent at the country ISO3 level

host_country_continent <- Data_complete %>%
  mutate(
    host_country_or_area = case_when(
      host_country_or_area %in% c("US", "United States") ~ "United States of America",
      host_country_or_area %in% c("MX", "Mexico")        ~ "Mexico",
      host_country_or_area %in% c("Republic of Moldova", "Moldova") ~ "Moldova",
      TRUE ~ host_country_or_area
    )
  ) %>%
  distinct(host_country_or_area, project_continent) %>%
  mutate(
    iso3c = countrycode(
      host_country_or_area,
      origin = "country.name",
      destination = "iso3c"
    ),
    iso3c = case_when(
      iso3c == "COD" ~ "ZAR",
      iso3c == "ROU" ~ "ROM",
      TRUE ~ iso3c
    )
  )

# Step 2 — Collapse to one continent per ISO3

host_country_continent_iso <- host_country_continent %>%
  group_by(iso3c) %>%
  summarise(
    project_continent = first(project_continent),
    .groups = "drop"
  )

host_country_continent_iso %>%
  count(project_continent)

## fixing South America into "Americas"

host_country_continent_iso <- host_country_continent_iso %>%
  mutate(
    project_continent = case_when(
      project_continent %in% c("South America", "North America") ~ "Americas",
      TRUE ~ project_continent
    )
  )



# Step 3 — Join continent onto your master plotting dataset

host_carbon_wgi_long <- host_carbon_wgi_long %>%
  left_join(host_country_continent_iso, by = "iso3c")

# check

unique(host_carbon_wgi_long$project_continent)


# Step 4 - Update the plot with continents

continent_colors <- c(
  "Americas" = "#90C2E7",
  "Africa"   = "#4F4D80",
  "Asia" = "#A37466",
  "Europe" = "#98A886",
  "Oceania" = "#415662"
  
)


gov_host_plot_faceted_gdp <- ggplot(
  host_carbon_wgi_long %>%
    filter(
      total_credit_volume > 0,
      !is.na(estimate),
      !is.na(gdp_pc_2024),
      !is.na(project_continent)
    ),
  aes(
    x = total_credit_volume,
    y = estimate,
    size = gdp_pc_2024,
    colour = project_continent
  )
) +
  geom_point(alpha = 0.7) +
  
  geom_text_repel(
    data = label_countries_facets,
    inherit.aes = FALSE,
    aes(
      x = total_credit_volume,
      y = estimate,
      label = countryname
    ),
    size = 2.8,
    colour = "black",   # keep labels readable
    box.padding = 0.4,
    point.padding = 0.3,
    segment.color = "grey60",
    max.overlaps = Inf
  ) +
  
  scale_x_log10(
    labels = scales::label_number(
      scale_cut = scales::cut_si(""),
      accuracy = 1
    )
  ) +
  scale_colour_manual(
    name = "Continent",
    values = continent_colors
  ) +

  scale_size_continuous(
    name = "GDP per capita (USD, 2024)",
    range = c(1.5, 6),
    labels = scales::label_dollar(accuracy = 1)
  ) +
  
  scale_y_continuous(n.breaks = 5) +
  
  facet_wrap(~ indicator_label, scales = "free_y") +
  
  labs(
    x = "Total forest-based carbon credit volume (log scale)",
    y = "Governance score (WGI)",
    colour = "Continent",
    title = "Forest-based carbon project volume vs governance",
    subtitle = "Point colour = continent; point size = GDP per capita (2024)"
  ) +
  
  theme_minimal() +
  theme(
    legend.position = "right"
  )

print(gov_host_plot_faceted_gdp)
