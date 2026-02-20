#### ============================================================
#### PART D — Host vs Proponent bubble matrix (cc only)
#### - X: host countries ordered by host cc
#### - Y: proponent countries ordered by proponent cc
#### - Size: total credit volume (tonnes) for each host–proponent pair
#### ============================================================

# #### ---------------------------
# #### Step D1 — Build host/proponent cc lookup tables
# #### ---------------------------
# 
# # Pull cc only from your WGI wide table
# wgi_cc <- wgi_wide %>%
#   select(iso3c, cc)
# 
# # Host cc
# host_cc <- wgi_cc %>%
#   rename(ISO3_host_modern = iso3c, host_cc = cc)
# 
# # Proponent cc
# proponent_cc <- wgi_cc %>%
#   rename(ISO3_proponent_modern = iso3c, proponent_cc = cc)
# 
# #### ---------------------------
# #### Step D2 — Aggregate volume by host–proponent pair
# #### ---------------------------
# 
# pair_volume <- Data_diff %>%
#   group_by(ISO3_host_modern, ISO3_proponent_modern) %>%
#   summarise(
#     pair_credit_volume = sum(sum_of_credit_volume, na.rm = TRUE),
#     host_continent_iso = first(host_continent_iso),
#     proponent_continent_iso = first(proponent_continent_iso),
#     .groups = "drop"
#   ) %>%
#   filter(pair_credit_volume > 0)
# 
# #### ---------------------------
# #### Step D3 — Join cc metrics onto each side (host + proponent)
# #### ---------------------------
# 
# pair_cc <- pair_volume %>%
#   left_join(host_cc, by = "ISO3_host_modern") %>%
#   left_join(proponent_cc, by = "ISO3_proponent_modern") %>%
#   filter(!is.na(host_cc), !is.na(proponent_cc))
# 
# #### ---------------------------
# #### Step D4 — Order axes by cc score
# #### ---------------------------
# 
# host_order <- pair_cc %>%
#   distinct(ISO3_host_modern, host_cc) %>%
#   arrange(host_cc) %>%
#   pull(ISO3_host_modern)
# 
# proponent_order <- pair_cc %>%
#   distinct(ISO3_proponent_modern, proponent_cc) %>%
#   arrange(proponent_cc) %>%
#   pull(ISO3_proponent_modern)
# 
# pair_cc <- pair_cc %>%
#   mutate(
#     host_f = factor(ISO3_host_modern, levels = host_order),
#     proponent_f = factor(ISO3_proponent_modern, levels = proponent_order)
#   )
# 
# #### ---------------------------
# #### Step D5 — Plot (linear size)
# #### ---------------------------
# 
# host_vs_proponent_cc_bubble <- ggplot(
#   pair_cc,
#   aes(
#     x = host_f,
#     y = proponent_f,
#     size = pair_credit_volume
#   )
# ) +
#   geom_point(alpha = 0.6) +
#   scale_size_continuous(
#     name = "Total credit volume (tonnes)",
#     labels = label_number(scale_cut = cut_si(""), accuracy = 1),
#     range = c(0.5, 10)
#   ) +
#   labs(
#     x = "Host country (ordered by Control of Corruption, cc)",
#     y = "Proponent country (ordered by Control of Corruption, cc)",
#     title = "Host vs proponent countries ordered by Control of Corruption (WGI cc)",
#     subtitle = "Each bubble is a host–proponent pair; bubble size = total traded credit volume (tonnes)"
#   ) +
#   theme_minimal() +
#   theme(
#     axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
#     panel.grid.major = element_line(linewidth = 0.2),
#     panel.grid.minor = element_blank()
#   )
# 
# print(host_vs_proponent_cc_bubble)
# save_plot_png(host_vs_proponent_cc_bubble, "host_vs_proponent_cc_bubble_matrix")
# 
# #### ---------------------------
# #### Step D6 — option to filter low vol pairs
# #### ---------------------------
# 
# # Keep only pairs at/above the 75th percentile of pair volume (adjust as needed)
# pair_cc_top <- pair_cc %>%
#   filter(pair_credit_volume >= quantile(pair_credit_volume, 0.75, na.rm = TRUE))
# 
# host_vs_proponent_cc_bubble_top <- ggplot(
#   pair_cc_top,
#   aes(x = host_f, y = proponent_f, size = pair_credit_volume)
# ) +
#   geom_point(alpha = 0.7) +
#   scale_size_continuous(
#     name = "Total credit volume (tonnes)",
#     labels = label_number(scale_cut = cut_si(""), accuracy = 1),
#     range = c(0.8, 12)
#   ) +
#   labs(
#     x = "Host country (ordered by cc)",
#     y = "Proponent country (ordered by cc)",
#     title = "Host vs proponent (WGI cc), high-volume pairs",
#     subtitle = "Filtered to top quartile of host–proponent pair volumes"
#   ) +
#   theme_minimal() +
#   theme(
#     axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
#     panel.grid.major = element_line(linewidth = 0.2),
#     panel.grid.minor = element_blank()
#   )
# 
# print(host_vs_proponent_cc_bubble_top)
# save_plot_png(host_vs_proponent_cc_bubble_top, "host_vs_proponent_cc_bubble_matrix_top_quartile")
# 


#### ============================================================ ####
#### PART E — Host vs Proponent scatter (control of corruption) ####
#### - X: host cc (Control of Corruption) ####
#### - Y: proponent cc ####
#### - Size: total credit volume for host–proponent pair ####
#### ============================================================

#### --------------------------- ####
#### Step E1 — cc lookup tables ####
#### --------------------------- ####
wgi_cc <- wgi_wide %>%
  select(iso3c, cc)

host_cc <- wgi_cc %>%
  rename(ISO3_host_modern = iso3c, host_cc = cc)

proponent_cc <- wgi_cc %>%
  rename(ISO3_proponent_modern = iso3c, proponent_cc = cc)

#### --------------------------- ####
#### Step E2 — Aggregate tonnes by host–proponent pair ####
#### --------------------------- ####
pair_volume <- Data_diff %>%
  group_by(ISO3_host_modern, ISO3_proponent_modern) %>%
  summarise(
    pair_credit_volume = sum(sum_of_credit_volume, na.rm = TRUE),
    host_continent_iso = first(host_continent_iso),
    proponent_continent_iso = first(proponent_continent_iso),
    .groups = "drop"
  ) %>%
  filter(pair_credit_volume > 0)

#### ---------------------------
#### Step E3 — Join cc values ####
#### ---------------------------
pair_cc <- pair_volume %>%
  left_join(host_cc, by = "ISO3_host_modern") %>%
  left_join(proponent_cc, by = "ISO3_proponent_modern") %>%
  filter(!is.na(host_cc), !is.na(proponent_cc))

#### ---------------------------
#### Step E4 — Plot ####
#### ---------------------------
host_vs_proponent_cc_scatter <- ggplot(
  pair_cc,
  aes(x = host_cc, y = proponent_cc, size = pair_credit_volume)
) +
  geom_point(alpha = 0.55) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey55") +
  scale_size_continuous(
    name = "Total credit volume (tonnes)\n(host–proponent pair)",
    labels = label_number(scale_cut = cut_si(""), accuracy = 1),
    range = c(1.5, 10)
  ) +
  scale_x_continuous(n.breaks = 6) +
  scale_y_continuous(n.breaks = 6) +
  labs(
    x = "Host country: Control of Corruption (WGI cc)",
    y = "Proponent country: Control of Corruption (WGI cc)",
    title = "Host vs proponent governance (Control of Corruption, WGI cc)",
    subtitle = "Each point is a host–proponent pair; dashed line is y = x"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

print(host_vs_proponent_cc_scatter)
save_plot_png(host_vs_proponent_cc_scatter, "host_vs_proponent_cc_scatter")


#### ============================================================
#### Host vs Proponent scatter (cc only) — color by region ####
#### ============================================================

host_vs_proponent_cc_scatter_hostcolor <- ggplot(
  pair_cc %>% filter(!is.na(host_continent_iso)),
  aes(
    x = host_cc,
    y = proponent_cc,
    size = pair_credit_volume,
    colour = host_continent_iso
  )
) +
  geom_point(alpha = 0.60) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey55") +
  scale_colour_manual(values = continent_colors, name = "Host continent") +
  scale_size_continuous(
    name = "Total credit volume (tonnes)\n(host–proponent pair)",
    labels = label_number(scale_cut = cut_si(""), accuracy = 1),
    range = c(1.5, 10)
  ) +
  scale_x_continuous(n.breaks = 6) +
  scale_y_continuous(n.breaks = 6) +
  labs(
    x = "Host country: Control of Corruption (WGI cc)",
    y = "Proponent country: Control of Corruption (WGI cc)",
    title = "Host vs proponent governance (WGI cc)",
    subtitle = "Each point is a host–proponent pair; color = host continent; dashed line is y = x"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold")
  ) +
  guides(
    colour = guide_legend(override.aes = list(size = 5))
  )

print(host_vs_proponent_cc_scatter_hostcolor)
save_plot_png(host_vs_proponent_cc_scatter_hostcolor, "host_vs_proponent_cc_scatter_hostcolor")



#### ============================================================
#### PART F — Project-level host vs proponent governance (cc)
#### - Each point = individual project / transaction
#### - X: host country cc
#### - Y: proponent country cc
#### ============================================================

#### ---------------------------
#### Step F1 — Prepare cc lookup
#### ---------------------------
wgi_cc <- wgi_wide %>%
  select(iso3c, cc)

host_cc <- wgi_cc %>%
  rename(ISO3_host_modern = iso3c, host_cc = cc)

proponent_cc <- wgi_cc %>%
  rename(ISO3_proponent_modern = iso3c, proponent_cc = cc)

#### ---------------------------
#### Step F2 — Join cc to project-level data
#### ---------------------------
project_cc <- Data_diff %>%
  left_join(host_cc, by = "ISO3_host_modern") %>%
  left_join(proponent_cc, by = "ISO3_proponent_modern") %>%
  filter(
    !is.na(host_cc),
    !is.na(proponent_cc),
    sum_of_credit_volume > 0
  )

#### ---------------------------
#### Step F3 — Plot (project-level)
#### ---------------------------
project_level_cc_plot <- ggplot(
  project_cc,
  aes(
    x = host_cc,
    y = proponent_cc,
    size = sum_of_credit_volume
  )
) +
  geom_point(alpha = 0.35) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed",
    colour = "grey55"
  ) +
  scale_size_continuous(
    name = "Project credit volume (tonnes)",
    labels = label_number(scale_cut = cut_si(""), accuracy = 1),
    range = c(0.5, 8)
  ) +
  scale_x_continuous(n.breaks = 6) +
  scale_y_continuous(n.breaks = 6) +
  labs(
    x = "Host country: Control of Corruption (WGI cc)",
    y = "Proponent country: Control of Corruption (WGI cc)",
    title = "Project-level governance mismatch in cross-border carbon markets",
    subtitle = "Each point is a project; dashed line is y = x"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right"
  )

print(project_level_cc_plot)
save_plot_png(project_level_cc_plot, "project_level_host_vs_proponent_cc")


#### ============================================================
#### Project-level host vs proponent governance (cc) — HOST continent colors
#### ============================================================

project_level_cc_plot_hostcolor <- ggplot(
  project_cc %>% filter(!is.na(host_continent_iso)),
  aes(
    x = host_cc,
    y = proponent_cc,
    size = sum_of_credit_volume,
    colour = host_continent_iso
  )
) +
  geom_point(alpha = 0.40) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey55") +
  scale_colour_manual(values = continent_colors, name = "Host continent") +
  scale_size_continuous(
    name = "Project credit volume (tonnes)",
    labels = label_number(scale_cut = cut_si(""), accuracy = 1),
    range = c(0.6, 7.5)
  ) +
  scale_x_continuous(n.breaks = 6) +
  scale_y_continuous(n.breaks = 6) +
  labs(
    x = "Host country: Control of Corruption (WGI cc)",
    y = "Proponent country: Control of Corruption (WGI cc)",
    title = "Project-level governance mismatch (WGI cc)",
    subtitle = "Each point is a project; color = host continent; dashed line is y = x"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold")
  ) +
  guides(
    colour = guide_legend(override.aes = list(size = 5))
  )

print(project_level_cc_plot_hostcolor)
save_plot_png(project_level_cc_plot_hostcolor, "project_level_host_vs_proponent_cc_hostcolor")



### checking number of rows plotting by ggplot ####

built <- ggplot_build(project_level_cc_plot)
n_points_drawn <- nrow(built$data[[1]])
n_points_drawn

## how many overlapping coords? ##

project_cc %>%
  count(host_cc, proponent_cc, name = "n_projects") %>%
  arrange(desc(n_projects)) %>%
  slice(1:20)

n_total  <- nrow(project_cc)
n_unique <- project_cc %>% distinct(host_cc, proponent_cc) %>% nrow()
c(total_rows = n_total, unique_xy = n_unique)



#### Adding all WGI metrics ####

#### ============================================================
#### PART G — Host vs Proponent (project level and paired aggregate) governance scatterplots (ALL WGI metrics) ####
#### - Two plot types for each metric:
####   (1) Pair-aggregated (host–proponent pair totals)
####   (2) Project-level (each row / project / transaction)
#### - Color = host continent (your palette)
#### - Saves PNGs with metric key in filename
#### ============================================================

#### ---------------------------
#### Step G1 — Sanity checks ####
#### ---------------------------
stopifnot(exists("Data_diff"))
stopifnot(exists("wgi_wide"))
stopifnot(exists("indicator_labels"))

metrics <- names(indicator_labels)

missing_metrics <- setdiff(metrics, names(wgi_wide))
if (length(missing_metrics) > 0) {
  stop("wgi_wide is missing these WGI columns: ", paste(missing_metrics, collapse = ", "))
}

#### ---------------------------
#### Step G2 — Helper: build a host vs proponent scatter plot ####
#### ---------------------------
build_host_prop_scatter <- function(df, x_col, y_col, size_col, colour_col,
                                    metric_key, metric_label,
                                    title_prefix,
                                    size_legend_title,
                                    alpha = 0.45,
                                    size_range = c(0.6, 8)) {
  
  ggplot(
    df %>% filter(!is.na(.data[[colour_col]])),
    aes(
      x = .data[[x_col]],
      y = .data[[y_col]],
      size = .data[[size_col]],
      colour = .data[[colour_col]]
    )
  ) +
    geom_point(alpha = alpha) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey55") +
    scale_colour_manual(values = continent_colors, name = "Host continent") +
    scale_size_continuous(
      name = size_legend_title,
      labels = label_number(scale_cut = cut_si(""), accuracy = 1),
      range = size_range
    ) +
    scale_x_continuous(n.breaks = 6) +
    scale_y_continuous(n.breaks = 6) +
    labs(
      x = paste0("Host country: ", metric_label, " (WGI ", metric_key, ")"),
      y = paste0("Proponent country: ", metric_label, " (WGI ", metric_key, ")"),
      title = paste0(title_prefix, " — ", metric_label),
      subtitle = "Color = host continent; dashed line is y = x"
    ) +
    theme_minimal() +
    theme(
      legend.position = "right",
      legend.title = element_text(face = "bold")
    ) +
    guides(
      colour = guide_legend(override.aes = list(size = 5))
    )
}

#### ---------------------------
#### Step G3 — Pair-aggregated data base ####
#### ---------------------------
pair_volume_base <- Data_diff %>%
  group_by(ISO3_host_modern, ISO3_proponent_modern) %>%
  summarise(
    pair_credit_volume = sum(sum_of_credit_volume, na.rm = TRUE),
    host_continent_iso = first(host_continent_iso),
    .groups = "drop"
  ) %>%
  filter(pair_credit_volume > 0)

#### ---------------------------
#### Step G4 — Project-level base ####
#### ---------------------------
project_base <- Data_diff %>%
  filter(sum_of_credit_volume > 0)

#### ---------------------------
#### Step G5 — Loop through metrics: build, print, and save plots
#### ---------------------------
for (m in metrics) {
  
  metric_label <- unname(indicator_labels[m])
  
  # Lookup table for metric
  wgi_m <- wgi_wide %>%
    select(iso3c, metric_value = all_of(m))
  
  host_m <- wgi_m %>%
    rename(ISO3_host_modern = iso3c, host_metric = metric_value)
  
  prop_m <- wgi_m %>%
    rename(ISO3_proponent_modern = iso3c, proponent_metric = metric_value)
  
  ## ---- (1) Pair-aggregated plot ----
  pair_m <- pair_volume_base %>%
    left_join(host_m, by = "ISO3_host_modern") %>%
    left_join(prop_m, by = "ISO3_proponent_modern") %>%
    filter(!is.na(host_metric), !is.na(proponent_metric))
  
  p_pair <- build_host_prop_scatter(
    df = pair_m,
    x_col = "host_metric",
    y_col = "proponent_metric",
    size_col = "pair_credit_volume",
    colour_col = "host_continent_iso",
    metric_key = m,
    metric_label = metric_label,
    title_prefix = "Host vs proponent governance (pair-aggregated)",
    size_legend_title = "Total credit volume (tonnes)\n(host–proponent pair)",
    alpha = 0.60,
    size_range = c(1.5, 10)
  )
  
  print(p_pair)
  save_plot_png(p_pair, paste0("pair_host_vs_proponent_", m, "_hostcolor"))
  
  ## ---- (2) Project-level plot ----
  project_m <- project_base %>%
    left_join(host_m, by = "ISO3_host_modern") %>%
    left_join(prop_m, by = "ISO3_proponent_modern") %>%
    filter(!is.na(host_metric), !is.na(proponent_metric))
  
  p_project <- build_host_prop_scatter(
    df = project_m,
    x_col = "host_metric",
    y_col = "proponent_metric",
    size_col = "sum_of_credit_volume",
    colour_col = "host_continent_iso",
    metric_key = m,
    metric_label = metric_label,
    title_prefix = "Host vs proponent governance (project-level)",
    size_legend_title = "Project credit volume (tonnes)",
    alpha = 0.40,
    size_range = c(0.6, 7.5)
  )
  
  # check???
  # after project_m is created, and after p_project is defined:
  built <- ggplot_build(p_project)
  n_points_drawn <- nrow(built$data[[1]])
  
  overlap_top <- project_m %>%
    count(host_metric, proponent_metric, name = "n_projects") %>%
    arrange(desc(n_projects)) %>%
    slice(1:10)
  
  n_total  <- nrow(project_m)
  n_unique <- project_m %>% distinct(host_metric, proponent_metric) %>% nrow()
  
  message("Metric = ", m,
          " | points_drawn = ", n_points_drawn,
          " | total_rows = ", n_total,
          " | unique_xy = ", n_unique)
  
  print(overlap_top)
  
  print(p_project)
  save_plot_png(p_project, paste0("project_host_vs_proponent_", m, "_hostcolor"))
}


###############
#### Network diagram of hosts-proponents ####
###############
library(dplyr)
library(tidyr)
library(stringr)
install.packages("igraph")
library(igraph)
install.packages("ggraph")
library(ggraph)
library(ggplot2)
library(scales)

continent_pal <- c(
    "Africa" = "#C99D9B", 
    "Europe" = "#1CFEBA",  
    "Asia" = "#16ACCA",  
    "North America" = "#C62E65",  
    "Central America & Caribbean" = "#9D8DF1",  
    "South America" = "#694264",  
    "Oceania" = "#F4AC45")


#### edge list ####
edges <- Data_complete_iso %>%
  filter(
    !is.na(host_country_clean),
    !is.na(proponent_country_clean),
    !is.na(sum_of_credit_volume)
  ) %>%
  group_by(host_country_clean, proponent_country_clean) %>%
  summarise(
    credit_volume = sum(sum_of_credit_volume, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(
    from = host_country_clean,
    to   = proponent_country_clean
  )


#### nodes list ####

nodes_host <- Data_complete_iso %>%
  distinct(country = host_country_clean, continent = host_continent_iso) %>%
  filter(!is.na(country))

nodes_prop <- Data_complete_iso %>%
  distinct(country = proponent_country_clean, continent = proponent_continent_iso) %>%
  filter(!is.na(country))

nodes <- bind_rows(nodes_host, nodes_prop) %>%
  group_by(country) %>%
  summarise(
    continent = continent[which(!is.na(continent))[1]],
    .groups = "drop"
  ) %>%
  mutate(
    continent = if_else(is.na(continent), "Unknown", continent)
  )

# keep top nodes only ####

top_n_edges <- 50  # tune this
edges_plot <- edges %>% slice_max(credit_volume, n = top_n_edges)


#### plot #####

g <- graph_from_data_frame(
  d = edges_plot,
  vertices = nodes %>% rename(name = country),
  directed = TRUE
)

# node sizes: total volume in+out
node_strength <- strength(g, vids = V(g), weights = E(g)$credit_volume)
V(g)$node_size <- rescale(node_strength, to = c(2, 12))

# edge widths: credit volume
E(g)$edge_width <- rescale(E(g)$credit_volume, to = c(0.2, 3.5))

# ensure palette covers all continents present
missing_cols <- setdiff(unique(V(g)$continent), names(continent_pal))
if (length(missing_cols) > 0) {
  message("Missing colours for: ", paste(missing_cols, collapse = ", "))
}

set.seed(1)

p_net <- ggraph(g, layout = "fr") +  # try "kk" as alternative
  geom_edge_fan(aes(width = edge_width),
                alpha = 0.25,
                arrow = arrow(length = unit(2.5, "mm"), type = "closed"),
                end_cap = circle(2.5, "mm")) +
  geom_node_point(aes(size = node_size, colour = continent), alpha = 0.95) +
  scale_colour_manual(values = continent_pal, na.value = "grey70") +
  scale_size_identity() +
  scale_edge_width_identity() +
  guides(edge_width = "none", size = "none") +
  theme_void(base_size = 12) +
  theme(
    legend.position = "right",
    legend.title = element_blank()
  )

p_net


#### two column flow layout ####

# edges_plot already exists from earlier code
g <- graph_from_data_frame(
  d = edges_plot,
  vertices = nodes %>% rename(name = country),
  directed = TRUE
)

# Identify which nodes appear as host and/or proponent
hosts <- unique(edges_plot$from)
props <- unique(edges_plot$to)

V(g)$is_host <- V(g)$name %in% hosts
V(g)$is_prop <- V(g)$name %in% props

# If some countries appear on BOTH sides, bipartite layout breaks conceptually.
# Option A (recommended): keep only strictly-bipartite nodes for this plot.
g_bi <- induced_subgraph(g, vids = V(g)[xor(is_host, is_prop)])

# Now define the required bipartite 'type' attribute:
# convention: type == TRUE for the "top" partition (I'll use proponents)
V(g_bi)$type <- V(g_bi)$is_prop

# (optional) sizes/weights again, using this subgraph’s edges
node_strength <- strength(g_bi, vids = V(g_bi), weights = E(g_bi)$credit_volume)
V(g_bi)$node_size <- scales::rescale(node_strength, to = c(2, 12))
E(g_bi)$edge_width <- scales::rescale(E(g_bi)$credit_volume, to = c(0.2, 3.5))

p_bipartite <- ggraph(g_bi, layout = "bipartite") +
  geom_edge_fan(aes(width = edge_width), alpha = 0.25) +
  geom_node_point(aes(colour = continent, size = node_size), alpha = 0.95) +
  scale_colour_manual(values = continent_pal, na.value = "grey70") +
  scale_size_identity() +
  scale_edge_width_identity() +
  theme_void(base_size = 12) +
  theme(legend.position = "right", legend.title = element_blank())

p_bipartite


#### the way above eliminates hosts that are also proponents. Will try this way....####

# classify nodes (host / proponent / both)
V(g)$group <- dplyr::case_when(
  V(g)$name %in% hosts & V(g)$name %in% props ~ "Both",
  V(g)$name %in% hosts ~ "Host",
  V(g)$name %in% props ~ "Proponent",
  TRUE ~ "Other"
)

# Make a manual layout with x positions
set.seed(1)
lay <- create_layout(g, layout = "fr")  # just for y spread

lay$x <- dplyr::case_when(
  lay$name %in% hosts & !(lay$name %in% props) ~ 0,
  lay$name %in% props & !(lay$name %in% hosts) ~ 1,
  lay$name %in% hosts &  (lay$name %in% props) ~ 0.5,
  TRUE ~ 0.5
)

# EDGE widths
E(g_bi)$edge_width <- scales::rescale(
  E(g_bi)$credit_volume,
  to = c(0.2, 3.5)
)

# NODE sizes
V(g_bi)$node_size <- scales::rescale(
  strength(g_bi, weights = E(g_bi)$credit_volume),
  to = c(2, 12)
)


p_two_col <- ggraph(g_bi, layout = "bipartite") +
  geom_edge_fan(aes(width = edge_width), alpha = 0.25) +
  geom_node_point(aes(size = node_size, colour = continent), alpha = 0.95) +
  scale_edge_width_identity() +
  scale_size_identity() +
  scale_colour_manual(values = continent_pal, na.value = "grey70") +
  theme_void()


p_two_col



# 1. Compute attributes on the graph
E(g)$edge_width <- scales::rescale(
  E(g)$credit_volume,
  to = c(0.2, 3.5)
)

V(g)$node_size <- scales::rescale(
  strength(g, weights = E(g)$credit_volume),
  to = c(2, 12)
)

# 2. THEN create the layout
lay <- create_layout(g, layout = "fr")

# 3. Override x positions
lay$x <- dplyr::case_when(
  lay$name %in% hosts & !(lay$name %in% props) ~ 0,
  lay$name %in% props & !(lay$name %in% hosts) ~ 1,
  lay$name %in% hosts &  (lay$name %in% props) ~ 0.5,
  TRUE ~ 0.5
)


ggraph(lay) +
  geom_edge_fan(aes(width = edge_width), alpha = 0.25,
                arrow = arrow(length = unit(2.5, "mm"), type = "closed")) +
  geom_node_point(aes(size = node_size, colour = continent), alpha = 0.95) +
  scale_edge_width_identity() +
  scale_size_identity() +
  scale_colour_manual(values = continent_pal, na.value = "grey70") +
  theme_void()


