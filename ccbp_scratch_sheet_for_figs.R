### scratch sheet -- proponent country/governance indicator/gdp/continent plot with newly cleaned Data_complete_iso dataframe

library(tidyverse)
library(countrycode)
library(readxl)
library(scales)
library(ggrepel)
library(readr)

#### Step 1 - PROPONENTS Sorting for only international traded projects ####

# starting with Data_complete_iso because it has already been cleaned, continents and ISO assigned, names cleaned in the above cleaning steps

Data_diff <- Data_complete_iso %>%
  filter(
    !is.na(ISO3_host_modern),
    !is.na(ISO3_proponent_modern),
    ISO3_host_modern != ISO3_proponent_modern
  )

# now we have reduced the dataset to only projects that have different proponent/hosts


#### Step 2 - Aggregate carbon volume by proponent country (ISO level) ####

proponent_carbon_totals <- Data_diff %>%
  group_by(ISO3_proponent_modern) %>%
  summarise(
    total_credit_volume = sum(sum_of_credit_volume, na.rm = TRUE),
    proponent_continent_iso = first(proponent_continent_iso),
    .groups = "drop"
  ) %>%
  rename(
    iso3c = ISO3_proponent_modern
  )


# check this

any(duplicated(proponent_carbon_totals$iso3c))

# should be FALSE

#### Step 3 - clean WGI data (governance metrics) ####

wgi_path <- "/Users/zoe/Desktop/CCBP/CCBP_database/data/gov_indicators_wgi/wgidataset.xlsx"

wgi_clean <- read_excel(wgi_path) %>%
  filter(
    indicator %in% c("cc", "ge", "pv", "rl", "rq", "va"),
    year == max(year, na.rm = TRUE)
  ) %>%
  mutate(
    estimate = parse_number(estimate)
  ) %>%
  select(
    iso3c = code,
    indicator,
    estimate
  )


#### Step 4 - pivot wider #####

wgi_wide <- wgi_clean %>%
  pivot_wider(
    names_from = indicator,
    values_from = estimate
  )



#### Step 5 - Join carbon data + governance metrics ####

proponent_carbon_wgi <- proponent_carbon_totals %>%
  left_join(wgi_wide, by = "iso3c")

# check this 

any(duplicated(wgi_wide$iso3c))
any(duplicated(proponent_carbon_totals$iso3c))

# should be FALSE


#### Step 6 - pivot longer for faceting ####

proponent_carbon_wgi_long <- proponent_carbon_wgi %>%
  pivot_longer(
    cols = c(cc, ge, pv, rl, rq, va),
    names_to = "indicator",
    values_to = "estimate"
  )


#### Step 7 - make indicator labels for gov. metrics ####

indicator_labels <- c(
  cc = "Control of Corruption",
  ge = "Government Effectiveness",
  pv = "Political Stability",
  rl = "Rule of Law",
  rq = "Regulatory Quality",
  va = "Voice & Accountability"
)

proponent_carbon_wgi_long <- proponent_carbon_wgi_long %>%
  mutate(
    indicator_label = indicator_labels[indicator]
  )


#### Step 8 - make country labels for each facet ####

label_countries_facets_proponents <- proponent_carbon_wgi_long %>%
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

#### Step 9 - join GDP to carbon/WGI data for making graduated symbols ####

gdp <- read_csv(
  "/Users/zoe/Desktop/CCBP/CCBP_database/data/GDP_world_data/gdp_world_data_country.csv",
  show_col_types = FALSE
)

gdp_2024 <- gdp %>%
  transmute(
    iso3c = country_code,
    gdp_pc_2024 = as.numeric(`2024`)
  )

proponent_carbon_wgi_long <- proponent_carbon_wgi_long %>%
  left_join(gdp_2024, by = "iso3c")


#### Step 10 - Plot with GDP/continents/country labels ####

continent_colors <- c(
  "North America" = "#90C2E7",
  "Central America & Caribbean" = "#7088B4",
  "South America" = "#6D7F74",
  "Africa"   = "#4F4D80",
  "Asia" = "#A37466",
  "Europe" = "#98A886",
  "Oceania" = "#415662"
)

gov_proponent_plot_faceted_gdp_logged <- ggplot(
  proponent_carbon_wgi_long %>%
    filter(
      total_credit_volume > 0,
      !is.na(estimate),
      !is.na(gdp_pc_2024),
      !is.na(proponent_continent_iso)
    ),
  aes(
    x = total_credit_volume,
    y = estimate,
    size = gdp_pc_2024,
    colour = proponent_continent_iso
  )
) +
  geom_point(alpha = 0.7) +
  geom_text_repel(
    data = label_countries_facets_proponents,
    inherit.aes = FALSE,
    aes(
      x = total_credit_volume,
      y = estimate,
      label = iso3c
    ),
    size = 2.8,
    colour = "black",
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
    range = c(1.5, 6),
    labels = label_dollar(accuracy = 1)
  ) +
  scale_colour_manual(
    name = "Continent",
    values = continent_colors
  ) +
  scale_y_continuous(n.breaks = 5) +
  facet_wrap(~ indicator_label, scales = "free_y") +
  theme_minimal() +
  labs(
    x = "Total forest-based carbon credit volume (log scale)",
    y = "Governance score (WGI)",
    title = "Forest-based carbon project volume vs governance, proponent countries",
    subtitle = "International projects only; point size = GDP per capita (2024)"
  ) +
  
  guides(
    colour = guide_legend(
      override.aes = list(size = 5)
    )
  )

gov_proponent_plot_faceted_gdp_logged

#### Step 11 — Unlogged version (linear x-axis) ####

gov_proponent_plot_faceted_gdp_linear <- ggplot(
  proponent_carbon_wgi_long %>%
    filter(
      total_credit_volume > 0,
      !is.na(estimate),
      !is.na(gdp_pc_2024),
      !is.na(proponent_continent_iso)
    ),
  aes(
    x = total_credit_volume,
    y = estimate,
    size = gdp_pc_2024,
    colour = proponent_continent_iso
  )
) +
  geom_point(alpha = 0.7) +
  
  geom_text_repel(
    data = label_countries_facets_proponents,
    inherit.aes = FALSE,
    aes(
      x = total_credit_volume,
      y = estimate,
      label = iso3c
    ),
    size = 2.8,
    colour = "black",
    box.padding = 0.4,
    point.padding = 0.3,
    segment.color = "grey60",
    max.overlaps = Inf
  ) +
  
  scale_x_continuous(
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
  
  scale_colour_manual(
    name = "Continent",
    values = continent_colors
  ) +
  
  scale_y_continuous(n.breaks = 5) +
  
  facet_wrap(~ indicator_label, scales = "free_y") +
  
  guides(
    colour = guide_legend(
      override.aes = list(size = 5)
    )
  ) +
  
  labs(
    x = "Total forest-based carbon credit volume",
    y = "Governance score (WGI)",
    title = "Forest-based carbon project volume vs governance, proponent countries",
    subtitle = "International projects only; linear x-axis; point size = GDP per capita (2024)"
  ) +
  
  theme_minimal() +
  theme(
    legend.position = "right"
  )

print(gov_proponent_plot_faceted_gdp_linear)




##### HOST AND PROPONENT PLOT - with RQ gov metric only ####

#### Step 1 - prepare RQ for hosts and props ####

host_rq <- host_carbon_wgi_long %>%
filter(
  indicator == "rq",
  total_credit_volume > 0,
  !is.na(estimate)
) %>%
  mutate(role = "Host")

proponent_rq <- proponent_carbon_wgi_long %>%
  filter(
    indicator == "rq",
    total_credit_volume > 0,
    !is.na(estimate)
  ) %>%
  mutate(role = "Proponent")


#### Step 2 - combine into dataframe #####

rq_combined <- bind_rows(host_rq, proponent_rq)

#check
table(rq_combined$role)

#### Step 3 - plot ####

rq_overlay_plot <- ggplot(
  rq_combined,
  aes(
    x = total_credit_volume,
    y = estimate,
    colour = role
  )
) +
  geom_point(
    alpha = 0.6,
    size = 2
  ) +
  
  scale_colour_manual(
    values = c(
      "Host" = "#8ECae6",      # light blue
      "Proponent" = "#CDB4DB"  # light purple
    ),
    name = "Country role"
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
  
  labs(
    x = "Total forest-based carbon credit volume (log scale)",
    y = "Regulatory Quality (WGI)",
    title = "Regulatory quality vs forest-based carbon credit volume",
    subtitle = "Host countries (blue) vs proponent countries (purple)"
  ) +
  
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold")
  )

print(rq_overlay_plot)

#### UNLOGGED #####

rq_overlay_plot_linear <- ggplot(
  rq_combined,
  aes(
    x = total_credit_volume,
    y = estimate,
    colour = role
  )
) +
  geom_point(
    alpha = 0.6,
    size = 2
  ) +
  
  scale_colour_manual(
    values = c(
      "Host" = "#17bebb",      # light blue
      "Proponent" = "#cd5334"  # light purple
    ),
    name = "Country role"
  ) +
  
  scale_x_continuous(
    labels = scales::label_number(
      scale_cut = scales::cut_si(""),
      accuracy = 1
    )
  ) +
  
  scale_y_continuous(n.breaks = 5) +
  
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    colour = "grey60"
  ) +
  
  labs(
    x = "Total forest-based carbon credit volume",
    y = "Regulatory Quality (WGI)",
    title = "Regulatory quality vs forest-based carbon credit volume",
    subtitle = "Host countries (blue) vs proponent countries (purple); linear scale"
  ) +
  
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold")
  )

print(rq_overlay_plot_linear)












###### REWRITTEN CODE WITH CONSISTENT LABELS AND SAVED FILES #######


#### ============================================================
#### Governance vs carbon-credit volume plots (HOST + PROPONENT)
#### - Faceted WGI indicators
#### - Logged + unlogged versions
#### - Overlay plot comparing Host vs Proponent (RQ + optionally all metrics)
#### - Saves PNGs with consistent names
#### ============================================================

# Packages (load once)
library(tidyverse)
library(countrycode)
library(readxl)
library(scales)
library(ggrepel)
library(readr)

#### ---------------------------
#### User paths (edit as needed)
#### ---------------------------
wgi_path <- "/Users/zoe/Desktop/CCBP/CCBP_database/data/gov_indicators_wgi/wgidataset.xlsx"
gdp_path <- "/Users/zoe/Desktop/CCBP/CCBP_database/data/GDP_world_data/gdp_world_data_country.csv"

# Output folder (creates if missing)
plot_dir <- "plots_governance"
dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)

#### ---------------------------
#### Assumptions / sanity checks
#### ---------------------------
stopifnot(exists("Data_complete_iso"))

required_cols <- c(
  "ISO3_host_modern", "ISO3_proponent_modern",
  "host_continent_iso", "proponent_continent_iso",
  "sum_of_credit_volume"
)
missing_cols <- setdiff(required_cols, names(Data_complete_iso))
if (length(missing_cols) > 0) {
  stop("Data_complete_iso is missing required columns: ", paste(missing_cols, collapse = ", "))
}

#### ---------------------------
#### Common settings
#### ---------------------------

# WGI indicator labels (facet titles)
indicator_labels <- c(
  cc = "Control of Corruption",
  ge = "Government Effectiveness",
  pv = "Political Stability",
  rl = "Rule of Law",
  rq = "Regulatory Quality",
  va = "Voice & Accountability"
)

# Continent palette (your existing palette)

# new colors #
continent_colors <- c(
"Africa" = "#C99D9B", 
"Europe" = "#1CFEBA",  
"Asia" = "#16ACCA",  
"North America" = "#C62E65",  
"Central America & Caribbean" = "#9D8DF1",  
"South America" = "#694264",  
"Oceania" = "#F4AC45")



# old colors continent_colors <- c(
#  "North America" = "#90C2E7",
#  "Central America & Caribbean" = "#7088B4",
#  "South America" = "#6D7F74",
#  "Africa"   = "#4F4D80",
#  "Asia"     = "#A37466",
#  "Europe"   = "#98A886",
#  "Oceania"  = "#415662"
# )

# Helper: save plot with consistent size + dpi
save_plot_png <- function(plot_obj, filename, width = 13, height = 8, dpi = 320) {
  ggsave(
    filename = file.path(plot_dir, paste0(filename, ".png")),
    plot = plot_obj,
    width = width,
    height = height,
    units = "in",
    dpi = dpi
  )
}

# Helper: make label set per facet (top/bottom credit + top/bottom estimate)
make_facet_labels <- function(df_long, credit_col = total_credit_volume, n_each = 3) {
  df_long %>%
    filter(!!rlang::enquo(credit_col) > 0, !is.na(estimate)) %>%
    group_by(indicator, indicator_label) %>%
    summarise(
      data = list(
        bind_rows(
          slice_max(pick(everything()), !!rlang::enquo(credit_col), n = n_each),
          slice_min(pick(everything()), !!rlang::enquo(credit_col), n = n_each),
          slice_max(pick(everything()), estimate, n = n_each),
          slice_min(pick(everything()), estimate, n = n_each)
        ) %>%
          distinct(iso3c, .keep_all = TRUE)
      ),
      .groups = "drop"
    ) %>%
    unnest(data)
}

# Helper: build faceted plot (logged/unlogged)
build_faceted_plot <- function(df_long, label_df, role_title, continent_col, logged = TRUE) {
  base <- ggplot(
    df_long %>%
      filter(
        total_credit_volume > 0,
        !is.na(estimate),
        !is.na(gdp_pc_2024),
        !is.na(.data[[continent_col]])
      ),
    aes(
      x = total_credit_volume,
      y = estimate,
      size = gdp_pc_2024,
      colour = .data[[continent_col]]
    )
  ) +
    geom_point(alpha = 0.7) +
    geom_text_repel(
      data = label_df,
      inherit.aes = FALSE,
      aes(
        x = total_credit_volume,
        y = estimate,
        label = iso3c
      ),
      size = 2.8,
      colour = "black",
      max.overlaps = Inf
    ) +
    scale_size_continuous(
      name = "GDP per capita (USD, 2024)",
      range = c(1.5, 6),
      labels = label_dollar(accuracy = 1)
    ) +
    scale_colour_manual(
      name = "Continent",
      values = continent_colors
    ) +
    scale_y_continuous(n.breaks = 5) +
    facet_wrap(~ indicator_label, scales = "free_y") +
    guides(
      colour = guide_legend(override.aes = list(size = 5))
    ) +
    theme_minimal() +
    theme(legend.position = "right") +
    labs(
      y = "Governance score (WGI)",
      title = paste0("Forest-based carbon project volume vs governance, ", role_title),
      subtitle = "International projects only; point size = GDP per capita (2024)"
    )
  
  if (logged) {
    base +
      scale_x_log10(
        labels = label_number(scale_cut = cut_si(""), accuracy = 1)
      ) +
      labs(x = "Total forest-based carbon credit volume (log scale)")
  } else {
    base +
      scale_x_continuous(
        labels = label_number(scale_cut = cut_si(""), accuracy = 1)
      ) +
      labs(x = "Total forest-based carbon credit volume")
  }
}

#### ---------------------------
#### Step 1 - Restrict to international traded projects
#### ---------------------------
Data_diff <- Data_complete_iso %>%
  filter(
    !is.na(ISO3_host_modern),
    !is.na(ISO3_proponent_modern),
    ISO3_host_modern != ISO3_proponent_modern
  )

#### ---------------------------
#### Step 2 - Read + prep WGI (latest year in file)
#### ---------------------------
wgi_clean <- read_excel(wgi_path) %>%
  filter(
    indicator %in% names(indicator_labels)
  ) %>%
  mutate(
    year = as.integer(year),
    estimate = parse_number(as.character(estimate))
  )

latest_wgi_year <- max(wgi_clean$year, na.rm = TRUE)

wgi_wide <- wgi_clean %>%
  filter(year == latest_wgi_year) %>%
  select(
    iso3c = code,
    indicator,
    estimate
  ) %>%
  pivot_wider(names_from = indicator, values_from = estimate)

# Checks (optional)
stopifnot(!anyDuplicated(wgi_wide$iso3c))

#### ---------------------------
#### Step 3 - Read + prep GDP (2024)
#### ---------------------------
gdp_2024 <- read_csv(gdp_path, show_col_types = FALSE) %>%
  transmute(
    iso3c = country_code,
    gdp_pc_2024 = suppressWarnings(as.numeric(`2024`))
  )

#### ============================================================
#### PART A — HOST plots
#### ============================================================

# Aggregate carbon volume by host
host_carbon_totals <- Data_diff %>%
  group_by(ISO3_host_modern) %>%
  summarise(
    total_credit_volume = sum(sum_of_credit_volume, na.rm = TRUE),
    host_continent_iso = first(host_continent_iso),
    .groups = "drop"
  ) %>%
  rename(iso3c = ISO3_host_modern)

stopifnot(!anyDuplicated(host_carbon_totals$iso3c))

# Join WGI + GDP, then long
host_carbon_wgi_long <- host_carbon_totals %>%
  left_join(wgi_wide, by = "iso3c") %>%
  left_join(gdp_2024, by = "iso3c") %>%
  pivot_longer(
    cols = all_of(names(indicator_labels)),
    names_to = "indicator",
    values_to = "estimate"
  ) %>%
  mutate(
    indicator_label = unname(indicator_labels[indicator])
  )

# Labels per facet
label_countries_facets_host <- make_facet_labels(host_carbon_wgi_long)

# Plots: logged + unlogged
host_govmetrics_logged <- build_faceted_plot(
  df_long = host_carbon_wgi_long,
  label_df = label_countries_facets_host,
  role_title = "host countries",
  continent_col = "host_continent_iso",
  logged = TRUE
)

host_govmetrics_unlogged <- build_faceted_plot(
  df_long = host_carbon_wgi_long,
  label_df = label_countries_facets_host,
  role_title = "host countries",
  continent_col = "host_continent_iso",
  logged = FALSE
)

# Print + save
print(host_govmetrics_logged)
save_plot_png(host_govmetrics_logged, "host_govmetrics_logged")

print(host_govmetrics_unlogged)
save_plot_png(host_govmetrics_unlogged, "host_govmetrics_unlogged")

#### ============================================================
#### PART B — PROPONENT plots
#### ============================================================

# Aggregate carbon volume by proponent
proponent_carbon_totals <- Data_diff %>%
  group_by(ISO3_proponent_modern) %>%
  summarise(
    total_credit_volume = sum(sum_of_credit_volume, na.rm = TRUE),
    proponent_continent_iso = first(proponent_continent_iso),
    .groups = "drop"
  ) %>%
  rename(iso3c = ISO3_proponent_modern)

stopifnot(!anyDuplicated(proponent_carbon_totals$iso3c))

# Join WGI + GDP, then long
proponent_carbon_wgi_long <- proponent_carbon_totals %>%
  left_join(wgi_wide, by = "iso3c") %>%
  left_join(gdp_2024, by = "iso3c") %>%
  pivot_longer(
    cols = all_of(names(indicator_labels)),
    names_to = "indicator",
    values_to = "estimate"
  ) %>%
  mutate(
    indicator_label = unname(indicator_labels[indicator])
  )

# Labels per facet
label_countries_facets_proponent <- make_facet_labels(proponent_carbon_wgi_long)

# Plots: logged + unlogged
proponent_govmetrics_logged <- build_faceted_plot(
  df_long = proponent_carbon_wgi_long,
  label_df = label_countries_facets_proponent,
  role_title = "proponent countries",
  continent_col = "proponent_continent_iso",
  logged = TRUE
)

proponent_govmetrics_unlogged <- build_faceted_plot(
  df_long = proponent_carbon_wgi_long,
  label_df = label_countries_facets_proponent,
  role_title = "proponent countries",
  continent_col = "proponent_continent_iso",
  logged = FALSE
)

# Print + save
print(proponent_govmetrics_logged)
save_plot_png(proponent_govmetrics_logged, "proponent_govmetrics_logged")

print(proponent_govmetrics_unlogged)
save_plot_png(proponent_govmetrics_unlogged, "proponent_govmetrics_unlogged")

#### ============================================================
#### PART C — Host vs Proponent comparison plots
#### ============================================================

# --- C1: RQ only (overlay) ---
host_rq <- host_carbon_wgi_long %>%
  filter(indicator == "rq", total_credit_volume > 0, !is.na(estimate)) %>%
  mutate(role = "Host")

proponent_rq <- proponent_carbon_wgi_long %>%
  filter(indicator == "rq", total_credit_volume > 0, !is.na(estimate)) %>%
  mutate(role = "Proponent")

rq_combined <- bind_rows(host_rq, proponent_rq)

# Logged overlay (recommended for volume)
host_proponents_rq_logged <- ggplot(
  rq_combined,
  aes(x = total_credit_volume, y = estimate, colour = role)
) +
  geom_point(alpha = 0.6, size = 2) +
  scale_colour_manual(
    values = c("Host" = "#8ECae6", "Proponent" = "#CDB4DB"),
    name = "Country role"
  ) +
  scale_x_log10(labels = label_number(scale_cut = cut_si(""), accuracy = 1)) +
  scale_y_continuous(n.breaks = 5) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
  labs(
    x = "Total forest-based carbon credit volume (log scale)",
    y = "Regulatory Quality (WGI)",
    title = "Regulatory quality vs forest-based carbon credit volume",
    subtitle = "Hosts (blue) vs proponents (purple)"
  ) +
  theme_minimal() +
  theme(legend.position = "right", legend.title = element_text(face = "bold"))

# Unlogged overlay
host_proponents_rq_unlogged <- host_proponents_rq_logged +
  scale_x_continuous(labels = label_number(scale_cut = cut_si(""), accuracy = 1)) +
  labs(
    x = "Total forest-based carbon credit volume",
    subtitle = "Hosts (blue) vs proponents (purple); linear scale"
  )

print(host_proponents_rq_logged)
save_plot_png(host_proponents_rq_logged, "host_proponents_rq_logged")

print(host_proponents_rq_unlogged)
save_plot_png(host_proponents_rq_unlogged, "host_proponents_rq_unlogged")

# --- C2: ALL governance metrics, Host vs Proponent (faceted, no GDP size) ---
# (This answers your “faceted or single plot comparing hosts and proponents across metrics” request.)
host_all <- host_carbon_wgi_long %>%
  filter(total_credit_volume > 0, !is.na(estimate)) %>%
  mutate(role = "Host") %>%
  select(role, iso3c, indicator, indicator_label, total_credit_volume, estimate)

proponent_all <- proponent_carbon_wgi_long %>%
  filter(total_credit_volume > 0, !is.na(estimate)) %>%
  mutate(role = "Proponent") %>%
  select(role, iso3c, indicator, indicator_label, total_credit_volume, estimate)

host_proponents_gov_metrics_all <- bind_rows(host_all, proponent_all)

host_proponents_gov_metrics_all_logged <- ggplot(
  host_proponents_gov_metrics_all,
  aes(x = total_credit_volume, y = estimate, colour = role)
) +
  geom_point(alpha = 0.55, size = 1.8) +
  scale_colour_manual(
    values = c("Host" = "#8ECae6", "Proponent" = "#CDB4DB"),
    name = "Country role"
  ) +
  scale_x_log10(labels = label_number(scale_cut = cut_si(""), accuracy = 1)) +
  scale_y_continuous(n.breaks = 5) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
  facet_wrap(~ indicator_label, scales = "free_y") +
  labs(
    x = "Total forest-based carbon credit volume (log scale)",
    y = "Governance score (WGI)",
    title = "Governance vs forest-based carbon credit volume",
    subtitle = "Host vs proponent countries; faceted across WGI metrics"
  ) +
  theme_minimal() +
  theme(legend.position = "right", legend.title = element_text(face = "bold"))

host_proponents_gov_metrics_all_unlogged <- host_proponents_gov_metrics_all_logged +
  scale_x_continuous(labels = label_number(scale_cut = cut_si(""), accuracy = 1)) +
  labs(
    x = "Total forest-based carbon credit volume",
    subtitle = "Host vs proponent countries; linear scale; faceted across WGI metrics"
  )

print(host_proponents_gov_metrics_all_logged)
save_plot_png(host_proponents_gov_metrics_all_logged, "host_proponents_gov_metrics_all_logged")

print(host_proponents_gov_metrics_all_unlogged)
save_plot_png(host_proponents_gov_metrics_all_unlogged, "host_proponents_gov_metrics_all_unlogged")

#### ============================================================
#### Notes / suggestions (embedded as code comments):
#### - WGI year used: latest year found in the file (latest_wgi_year object).
#### - GDP rows with NA in 2024 are filtered out in the faceted GDP plots.
#### - Overlay comparison plots omit GDP sizing on purpose (cleaner host vs proponent contrast).
#### ============================================================


