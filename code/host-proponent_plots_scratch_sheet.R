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

# #### --------------------------- ####
# #### Step E1 — cc lookup tables ####
# #### --------------------------- ####
# wgi_cc <- wgi_wide %>%
#   select(iso3c, cc)
# 
# host_cc <- wgi_cc %>%
#   rename(ISO3_host_modern = iso3c, host_cc = cc)
# 
# proponent_cc <- wgi_cc %>%
#   rename(ISO3_proponent_modern = iso3c, proponent_cc = cc)
# 
# #### --------------------------- ####
# #### Step E2 — Aggregate tonnes by host–proponent pair ####
# #### --------------------------- ####
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
# #### Step E3 — Join cc values ####
# #### ---------------------------
# pair_cc <- pair_volume %>%
#   left_join(host_cc, by = "ISO3_host_modern") %>%
#   left_join(proponent_cc, by = "ISO3_proponent_modern") %>%
#   filter(!is.na(host_cc), !is.na(proponent_cc))
# 
# #### ---------------------------
# #### Step E4 — Plot ####
# #### ---------------------------
# host_vs_proponent_cc_scatter <- ggplot(
#   pair_cc,
#   aes(x = host_cc, y = proponent_cc, size = pair_credit_volume)
# ) +
#   geom_point(alpha = 0.55) +
#   geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey55") +
#   scale_size_continuous(
#     name = "Total credit volume (tonnes)\n(host–proponent pair)",
#     labels = label_number(scale_cut = cut_si(""), accuracy = 1),
#     range = c(1.5, 10)
#   ) +
#   scale_x_continuous(n.breaks = 6) +
#   scale_y_continuous(n.breaks = 6) +
#   labs(
#     x = "Host country: Control of Corruption (WGI cc)",
#     y = "Proponent country: Control of Corruption (WGI cc)",
#     title = "Host vs proponent governance (Control of Corruption, WGI cc)",
#     subtitle = "Each point is a host–proponent pair; dashed line is y = x"
#   ) +
#   theme_minimal() +
#   theme(legend.position = "right")
# 
# print(host_vs_proponent_cc_scatter)
# save_plot_png(host_vs_proponent_cc_scatter, "host_vs_proponent_cc_scatter")
# 
# 
# #### ============================================================
# #### Host vs Proponent scatter (cc only) — color by region ####
# #### ============================================================
# 
# host_vs_proponent_cc_scatter_hostcolor <- ggplot(
#   pair_cc %>% filter(!is.na(host_continent_iso)),
#   aes(
#     x = host_cc,
#     y = proponent_cc,
#     size = pair_credit_volume,
#     colour = host_continent_iso
#   )
# ) +
#   geom_point(alpha = 0.60) +
#   geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey55") +
#   scale_colour_manual(values = continent_colors, name = "Host continent") +
#   scale_size_continuous(
#     name = "Total credit volume (tonnes)\n(host–proponent pair)",
#     labels = label_number(scale_cut = cut_si(""), accuracy = 1),
#     range = c(1.5, 10)
#   ) +
#   scale_x_continuous(n.breaks = 6) +
#   scale_y_continuous(n.breaks = 6) +
#   labs(
#     x = "Host country: Control of Corruption (WGI cc)",
#     y = "Proponent country: Control of Corruption (WGI cc)",
#     title = "Host vs proponent governance (WGI cc)",
#     subtitle = "Each point is a host–proponent pair; color = host continent; dashed line is y = x"
#   ) +
#   theme_minimal() +
#   theme(
#     legend.position = "right",
#     legend.title = element_text(face = "bold")
#   ) +
#   guides(
#     colour = guide_legend(override.aes = list(size = 5))
#   )
# 
# print(host_vs_proponent_cc_scatter_hostcolor)
# save_plot_png(host_vs_proponent_cc_scatter_hostcolor, "host_vs_proponent_cc_scatter_hostcolor")
# 
# 
# 
# #### ============================================================
# #### PART F — Project-level host vs proponent governance (cc)
# #### - Each point = individual project / transaction
# #### - X: host country cc
# #### - Y: proponent country cc
# #### ============================================================
# 
# #### ---------------------------
# #### Step F1 — Prepare cc lookup
# #### ---------------------------
# wgi_cc <- wgi_wide %>%
#   select(iso3c, cc)
# 
# host_cc <- wgi_cc %>%
#   rename(ISO3_host_modern = iso3c, host_cc = cc)
# 
# proponent_cc <- wgi_cc %>%
#   rename(ISO3_proponent_modern = iso3c, proponent_cc = cc)
# 
# #### ---------------------------
# #### Step F2 — Join cc to project-level data
# #### ---------------------------
# project_cc <- Data_diff %>%
#   left_join(host_cc, by = "ISO3_host_modern") %>%
#   left_join(proponent_cc, by = "ISO3_proponent_modern") %>%
#   filter(
#     !is.na(host_cc),
#     !is.na(proponent_cc),
#     sum_of_credit_volume > 0
#   )
# 
# #### ---------------------------
# #### Step F3 — Plot (project-level)
# #### ---------------------------
# project_level_cc_plot <- ggplot(
#   project_cc,
#   aes(
#     x = host_cc,
#     y = proponent_cc,
#     size = sum_of_credit_volume
#   )
# ) +
#   geom_point(alpha = 0.35) +
#   geom_abline(
#     slope = 1,
#     intercept = 0,
#     linetype = "dashed",
#     colour = "grey55"
#   ) +
#   scale_size_continuous(
#     name = "Project credit volume (tonnes)",
#     labels = label_number(scale_cut = cut_si(""), accuracy = 1),
#     range = c(0.5, 8)
#   ) +
#   scale_x_continuous(n.breaks = 6) +
#   scale_y_continuous(n.breaks = 6) +
#   labs(
#     x = "Host country: Control of Corruption (WGI cc)",
#     y = "Proponent country: Control of Corruption (WGI cc)",
#     title = "Project-level governance mismatch in cross-border carbon markets",
#     subtitle = "Each point is a project; dashed line is y = x"
#   ) +
#   theme_minimal() +
#   theme(
#     legend.position = "right"
#   )
# 
# print(project_level_cc_plot)
# save_plot_png(project_level_cc_plot, "project_level_host_vs_proponent_cc")
# 
# 
# #### ============================================================
# #### Project-level host vs proponent governance (cc) — HOST continent colors
# #### ============================================================
# 
# project_level_cc_plot_hostcolor <- ggplot(
#   project_cc %>% filter(!is.na(host_continent_iso)),
#   aes(
#     x = host_cc,
#     y = proponent_cc,
#     size = sum_of_credit_volume,
#     colour = host_continent_iso
#   )
# ) +
#   geom_point(alpha = 0.40) +
#   geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey55") +
#   scale_colour_manual(values = continent_colors, name = "Host continent") +
#   scale_size_continuous(
#     name = "Project credit volume (tonnes)",
#     labels = label_number(scale_cut = cut_si(""), accuracy = 1),
#     range = c(0.6, 7.5)
#   ) +
#   scale_x_continuous(n.breaks = 6) +
#   scale_y_continuous(n.breaks = 6) +
#   labs(
#     x = "Host country: Control of Corruption (WGI cc)",
#     y = "Proponent country: Control of Corruption (WGI cc)",
#     title = "Project-level governance mismatch (WGI cc)",
#     subtitle = "Each point is a project; color = host continent; dashed line is y = x"
#   ) +
#   theme_minimal() +
#   theme(
#     legend.position = "right",
#     legend.title = element_text(face = "bold")
#   ) +
#   guides(
#     colour = guide_legend(override.aes = list(size = 5))
#   )
# 
# print(project_level_cc_plot_hostcolor)
# save_plot_png(project_level_cc_plot_hostcolor, "project_level_host_vs_proponent_cc_hostcolor")
# 
# 
# 
# ### checking number of rows plotting by ggplot ####
# 
# built <- ggplot_build(project_level_cc_plot)
# n_points_drawn <- nrow(built$data[[1]])
# n_points_drawn
# 
# ## how many overlapping coords? ##
# 
# project_cc %>%
#   count(host_cc, proponent_cc, name = "n_projects") %>%
#   arrange(desc(n_projects)) %>%
#   slice(1:20)
# 
# n_total  <- nrow(project_cc)
# n_unique <- project_cc %>% distinct(host_cc, proponent_cc) %>% nrow()
# c(total_rows = n_total, unique_xy = n_unique)
# 
# 
# 
# #### Adding all WGI metrics ####
# 
# #### ============================================================
# #### PART G — Host vs Proponent (project level and paired aggregate) governance scatterplots (ALL WGI metrics) ####
# #### - Two plot types for each metric:
# ####   (1) Pair-aggregated (host–proponent pair totals)
# ####   (2) Project-level (each row / project / transaction)
# #### - Color = host continent (your palette)
# #### - Saves PNGs with metric key in filename
# #### ============================================================
# 
# #### ---------------------------
# #### Step G1 — Sanity checks ####
# #### ---------------------------
# stopifnot(exists("Data_diff"))
# stopifnot(exists("wgi_wide"))
# stopifnot(exists("indicator_labels"))
# 
# metrics <- names(indicator_labels)
# 
# missing_metrics <- setdiff(metrics, names(wgi_wide))
# if (length(missing_metrics) > 0) {
#   stop("wgi_wide is missing these WGI columns: ", paste(missing_metrics, collapse = ", "))
# }
# 
# #### ---------------------------
# #### Step G2 — Helper: build a host vs proponent scatter plot ####
# #### ---------------------------
# build_host_prop_scatter <- function(df, x_col, y_col, size_col, colour_col,
#                                     metric_key, metric_label,
#                                     title_prefix,
#                                     size_legend_title,
#                                     alpha = 0.45,
#                                     size_range = c(0.6, 8)) {
#   
#   ggplot(
#     df %>% filter(!is.na(.data[[colour_col]])),
#     aes(
#       x = .data[[x_col]],
#       y = .data[[y_col]],
#       size = .data[[size_col]],
#       colour = .data[[colour_col]]
#     )
#   ) +
#     geom_point(alpha = alpha) +
#     geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey55") +
#     scale_colour_manual(values = continent_colors, name = "Host continent") +
#     scale_size_continuous(
#       name = size_legend_title,
#       labels = label_number(scale_cut = cut_si(""), accuracy = 1),
#       range = size_range
#     ) +
#     scale_x_continuous(n.breaks = 6) +
#     scale_y_continuous(n.breaks = 6) +
#     labs(
#       x = paste0("Host country: ", metric_label, " (WGI ", metric_key, ")"),
#       y = paste0("Proponent country: ", metric_label, " (WGI ", metric_key, ")"),
#       title = paste0(title_prefix, " — ", metric_label),
#       subtitle = "Color = host continent; dashed line is y = x"
#     ) +
#     theme_minimal() +
#     theme(
#       legend.position = "right",
#       legend.title = element_text(face = "bold")
#     ) +
#     guides(
#       colour = guide_legend(override.aes = list(size = 5))
#     )
# }
# 
# #### ---------------------------
# #### Step G3 — Pair-aggregated data base ####
# #### ---------------------------
# pair_volume_base <- Data_diff %>%
#   group_by(ISO3_host_modern, ISO3_proponent_modern) %>%
#   summarise(
#     pair_credit_volume = sum(sum_of_credit_volume, na.rm = TRUE),
#     host_continent_iso = first(host_continent_iso),
#     .groups = "drop"
#   ) %>%
#   filter(pair_credit_volume > 0)
# 
# #### ---------------------------
# #### Step G4 — Project-level base ####
# #### ---------------------------
# project_base <- Data_diff %>%
#   filter(sum_of_credit_volume > 0)
# 
# #### ---------------------------
# #### Step G5 — Loop through metrics: build, print, and save plots
# #### ---------------------------
# for (m in metrics) {
#   
#   metric_label <- unname(indicator_labels[m])
#   
#   # Lookup table for metric
#   wgi_m <- wgi_wide %>%
#     select(iso3c, metric_value = all_of(m))
#   
#   host_m <- wgi_m %>%
#     rename(ISO3_host_modern = iso3c, host_metric = metric_value)
#   
#   prop_m <- wgi_m %>%
#     rename(ISO3_proponent_modern = iso3c, proponent_metric = metric_value)
#   
#   ## ---- (1) Pair-aggregated plot ----
#   pair_m <- pair_volume_base %>%
#     left_join(host_m, by = "ISO3_host_modern") %>%
#     left_join(prop_m, by = "ISO3_proponent_modern") %>%
#     filter(!is.na(host_metric), !is.na(proponent_metric))
#   
#   p_pair <- build_host_prop_scatter(
#     df = pair_m,
#     x_col = "host_metric",
#     y_col = "proponent_metric",
#     size_col = "pair_credit_volume",
#     colour_col = "host_continent_iso",
#     metric_key = m,
#     metric_label = metric_label,
#     title_prefix = "Host vs proponent governance (pair-aggregated)",
#     size_legend_title = "Total credit volume (tonnes)\n(host–proponent pair)",
#     alpha = 0.60,
#     size_range = c(1.5, 10)
#   )
#   
#   print(p_pair)
#   save_plot_png(p_pair, paste0("pair_host_vs_proponent_", m, "_hostcolor"))
#   
#   ## ---- (2) Project-level plot ----
#   project_m <- project_base %>%
#     left_join(host_m, by = "ISO3_host_modern") %>%
#     left_join(prop_m, by = "ISO3_proponent_modern") %>%
#     filter(!is.na(host_metric), !is.na(proponent_metric))
#   
#   p_project <- build_host_prop_scatter(
#     df = project_m,
#     x_col = "host_metric",
#     y_col = "proponent_metric",
#     size_col = "sum_of_credit_volume",
#     colour_col = "host_continent_iso",
#     metric_key = m,
#     metric_label = metric_label,
#     title_prefix = "Host vs proponent governance (project-level)",
#     size_legend_title = "Project credit volume (tonnes)",
#     alpha = 0.40,
#     size_range = c(0.6, 7.5)
#   )
#   
#   # check???
#   # after project_m is created, and after p_project is defined:
#   built <- ggplot_build(p_project)
#   n_points_drawn <- nrow(built$data[[1]])
#   
#   overlap_top <- project_m %>%
#     count(host_metric, proponent_metric, name = "n_projects") %>%
#     arrange(desc(n_projects)) %>%
#     slice(1:10)
#   
#   n_total  <- nrow(project_m)
#   n_unique <- project_m %>% distinct(host_metric, proponent_metric) %>% nrow()
#   
#   message("Metric = ", m,
#           " | points_drawn = ", n_points_drawn,
#           " | total_rows = ", n_total,
#           " | unique_xy = ", n_unique)
#   
#   print(overlap_top)
#   
#   print(p_project)
#   save_plot_png(p_project, paste0("project_host_vs_proponent_", m, "_hostcolor"))
# }
# 


####### remaking host-proponent-WGI comparison plots with only top 20 participating countries in carbon trading by volume, and standardising for both GDP and GDP per capita ####

# #### ============================================================
# #### Top-20 participants (host+proponent) + GDP/GDPpc standardised WGI plots
# #### Uses:
# ####   - gdp_2024: iso3c, gdp_pc_2024   (GDP per capita)
# #### Optional:
# ####   - gdp_total_2024: iso3c, gdp_2024 (TOTAL GDP, or rename below)
# #### ============================================================
# 
# stopifnot(exists("Data_diff"))
# stopifnot(exists("wgi_wide"))
# stopifnot(exists("indicator_labels"))
# stopifnot(exists("continent_colors"))
# stopifnot(exists("save_plot_png"))
# stopifnot(exists("gdp_2024"))
# stopifnot(all(c("iso3c", "gdp_pc_2024") %in% names(gdp_2024)))
# 
# metrics <- names(indicator_labels)
# 
# missing_metrics <- setdiff(metrics, names(wgi_wide))
# if (length(missing_metrics) > 0) {
#   stop("wgi_wide is missing these WGI columns: ", paste(missing_metrics, collapse = ", "))
# }
# 
# #### ---------------------------
# #### Helper: scatter plot
# #### ---------------------------
# build_host_prop_scatter <- function(df, x_col, y_col, size_col, colour_col,
#                                     metric_key, metric_label,
#                                     title_prefix,
#                                     size_legend_title,
#                                     alpha = 0.45,
#                                     size_range = c(0.6, 8)) {
# 
#   ggplot(
#     df %>% dplyr::filter(!is.na(.data[[colour_col]])),
#     aes(
#       x = .data[[x_col]],
#       y = .data[[y_col]],
#       size = .data[[size_col]],
#       colour = .data[[colour_col]]
#     )
#   ) +
#     geom_point(alpha = alpha) +
#     geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey55") +
#     scale_colour_manual(values = continent_colors, name = "Host continent") +
#     scale_size_continuous(
#       name = size_legend_title,
#       labels = scales::label_number(scale_cut = scales::cut_si(""), accuracy = 1),
#       range = size_range
#     ) +
#     scale_x_continuous(n.breaks = 6) +
#     scale_y_continuous(n.breaks = 6) +
#     labs(
#       x = paste0("Host country: ", metric_label, " (WGI ", metric_key, ")"),
#       y = paste0("Proponent country: ", metric_label, " (WGI ", metric_key, ")"),
#       title = paste0(title_prefix, " — ", metric_label),
#       subtitle = "Color = host continent; dashed line is y = x"
#     ) +
#     theme_minimal() +
#     theme(
#       legend.position = "right",
#       legend.title = element_text(face = "bold")
#     ) +
#     guides(colour = guide_legend(override.aes = list(size = 5)))
# }
# 
# #### ---------------------------
# #### Step 1: Top-20 participants (host + proponent traded volume)
# #### ---------------------------
# participation <- Data_diff %>%
#   dplyr::filter(sum_of_credit_volume > 0) %>%
#   dplyr::select(ISO3_host_modern, ISO3_proponent_modern, sum_of_credit_volume) %>%
#   dplyr::summarise(host_volume = sum(sum_of_credit_volume, na.rm = TRUE), .by = ISO3_host_modern) %>%
#   dplyr::rename(iso3c = ISO3_host_modern) %>%
#   dplyr::full_join(
#     Data_diff %>%
#       dplyr::filter(sum_of_credit_volume > 0) %>%
#       dplyr::summarise(prop_volume = sum(sum_of_credit_volume, na.rm = TRUE), .by = ISO3_proponent_modern) %>%
#       dplyr::rename(iso3c = ISO3_proponent_modern),
#     by = "iso3c"
#   ) %>%
#   dplyr::mutate(
#     host_volume = dplyr::coalesce(host_volume, 0),
#     prop_volume = dplyr::coalesce(prop_volume, 0),
#     total_participation = host_volume + prop_volume
#   ) %>%
#   dplyr::arrange(dplyr::desc(total_participation))
# 
# top20_iso3 <- participation %>%
#   dplyr::slice_head(n = 20) %>%
#   dplyr::pull(iso3c)
# 
# message("Top-20 participant ISO3s (by host+prop volume): ", paste(top20_iso3, collapse = ", "))
# 
# #### ---------------------------
# #### Step 2: Filter to top-20 only (BOTH host and proponent in top20)
# #### ---------------------------
# Data_top20 <- Data_diff %>%
#   dplyr::filter(
#     sum_of_credit_volume > 0,
#     ISO3_host_modern %in% top20_iso3,
#     ISO3_proponent_modern %in% top20_iso3
#   )
# 
# #### ---------------------------
# #### Step 3: Pair + project bases
# #### ---------------------------
# pair_volume_base <- Data_top20 %>%
#   dplyr::group_by(ISO3_host_modern, ISO3_proponent_modern) %>%
#   dplyr::summarise(
#     pair_credit_volume = sum(sum_of_credit_volume, na.rm = TRUE),
#     host_continent_iso = dplyr::first(host_continent_iso),
#     .groups = "drop"
#   ) %>%
#   dplyr::filter(pair_credit_volume > 0)
# 
# project_base <- Data_top20
# 
# #### ---------------------------
# #### Step 4.1: Read nominal GDP (Current US$) #####
# #### ---------------------------
# 
# gdp_total_path <- "/Users/zoe/Desktop/CCBP/CCBP_database/data/GDP_nominal/gdp_nom_en.csv"
# 
# gdp_nom_raw <- readr::read_csv(gdp_total_path, show_col_types = FALSE)
# 
# # Detect ISO column
# iso_candidates <- c("iso3c", "country_code", "iso3", "code", "Country Code", "ISO3")
# iso_col <- names(gdp_nom_raw)[tolower(names(gdp_nom_raw)) %in% tolower(iso_candidates)][1]
# 
# # Detect year column (prefer 2024)
# year_cols <- names(gdp_nom_raw)[stringr::str_detect(names(gdp_nom_raw), "^\\d{4}$")]
# target_year <- if ("2024" %in% year_cols) "2024" else as.character(max(as.integer(year_cols)))
# 
# message("Using nominal GDP year column: ", target_year)
# 
# gdp_total_2024 <- gdp_nom_raw %>%
#   dplyr::transmute(
#     iso3c = .data[[iso_col]],
#     gdp_total_2024 = suppressWarnings(as.numeric(.data[[target_year]]))
#   ) %>%
#   dplyr::filter(!is.na(iso3c)) %>%
#   dplyr::distinct(iso3c, .keep_all = TRUE)
# 
# stopifnot(!anyDuplicated(gdp_total_2024$iso3c))
# 
# #### ---------------------------
# #### Step 4: Macro joins ####
# ####   - GDP per capita is available (gdp_2024)
# ####   - total GDP is optional (gdp_total_2024)
# #### ---------------------------
# gdp_pc_m <- gdp_2024 %>%
#   dplyr::select(iso3c, gdp_pc_2024)
# 
# host_gdp_pc <- gdp_pc_m %>%
#   dplyr::rename(ISO3_host_modern = iso3c, host_gdp_pc = gdp_pc_2024)
# 
# prop_gdp_pc <- gdp_pc_m %>%
#   dplyr::rename(ISO3_proponent_modern = iso3c, prop_gdp_pc = gdp_pc_2024)
# 
# has_gdp_total <- exists("gdp_total_2024")
# 
# if (has_gdp_total) {
#   # EXPECTED columns: iso3c and some total GDP column
#   # If your total GDP column name differs, change `gdp_total_col` below.
#   gdp_total_col <- setdiff(names(gdp_total_2024), "iso3c")[1]
#   if (is.na(gdp_total_col)) stop("gdp_total_2024 exists but has no total GDP column besides iso3c.")
# 
#   gdp_m <- gdp_total_2024 %>%
#     dplyr::select(iso3c, gdp_total = dplyr::all_of(gdp_total_col))
# 
#   host_gdp <- gdp_m %>%
#     dplyr::rename(ISO3_host_modern = iso3c, host_gdp = gdp_total)
# 
#   prop_gdp <- gdp_m %>%
#     dplyr::rename(ISO3_proponent_modern = iso3c, prop_gdp = gdp_total)
# } else {
#   message("NOTE: No `gdp_total_2024` found, so GDP-standardised plots will be skipped (GDPpc plots will run).")
# }
# 
# add_standardised_sizes <- function(df, vol_col) {
#   # Use mean(host, prop) denominators so the size is symmetric for pairs.
#   out <- df %>%
#     dplyr::mutate(
#       gdp_pc_mean = (host_gdp_pc + prop_gdp_pc) / 2,
#       vol_per_gdp_pc = dplyr::if_else(!is.na(gdp_pc_mean) & gdp_pc_mean > 0, .data[[vol_col]] / gdp_pc_mean, NA_real_)
#     )
# 
#   if ("host_gdp" %in% names(out) && "prop_gdp" %in% names(out)) {
#     out <- out %>%
#       dplyr::mutate(
#         gdp_mean = (host_gdp + prop_gdp) / 2,
#         vol_per_gdp = dplyr::if_else(!is.na(gdp_mean) & gdp_mean > 0, .data[[vol_col]] / gdp_mean, NA_real_)
#       )
#   }
# 
#   out
# }
# 
# #### ---------------------------
# #### Step 5: Standardisation specs
# #### ---------------------------
# std_specs <- list(
#   gdp_pc = list(
#     size_col = "vol_per_gdp_pc",
#     suffix = "stdGDPpc",
#     size_title_pair = "Credit volume / mean GDP per capita\n(host–proponent pair)",
#     size_title_project = "Credit volume / mean GDP per capita"
#   )
# )
# 
# if (has_gdp_total) {
#   std_specs <- c(
#     list(
#       gdp = list(
#         size_col = "vol_per_gdp",
#         suffix = "stdGDP",
#         size_title_pair = "Credit volume / mean GDP\n(host–proponent pair)",
#         size_title_project = "Credit volume / mean GDP"
#       )
#     ),
#     std_specs
#   )
# }
# 
# #### ---------------------------
# #### Step 6: Loop metrics x standardisation x plot type
# #### ---------------------------
# for (m in metrics) {
# 
#   metric_label <- unname(indicator_labels[m])
# 
#   wgi_m <- wgi_wide %>%
#     dplyr::select(iso3c, metric_value = dplyr::all_of(m))
# 
#   host_m <- wgi_m %>%
#     dplyr::rename(ISO3_host_modern = iso3c, host_metric = metric_value)
# 
#   prop_m <- wgi_m %>%
#     dplyr::rename(ISO3_proponent_modern = iso3c, proponent_metric = metric_value)
# 
#   # ---- Pair join (WGI + GDPpc (+GDP total if present)) ----
#   pair_m0 <- pair_volume_base %>%
#     dplyr::left_join(host_m, by = "ISO3_host_modern") %>%
#     dplyr::left_join(prop_m, by = "ISO3_proponent_modern") %>%
#     dplyr::left_join(host_gdp_pc, by = "ISO3_host_modern") %>%
#     dplyr::left_join(prop_gdp_pc, by = "ISO3_proponent_modern")
# 
#   if (has_gdp_total) {
#     pair_m0 <- pair_m0 %>%
#       dplyr::left_join(host_gdp, by = "ISO3_host_modern") %>%
#       dplyr::left_join(prop_gdp, by = "ISO3_proponent_modern")
#   }
# 
#   pair_m0 <- pair_m0 %>%
#     dplyr::filter(!is.na(host_metric), !is.na(proponent_metric)) %>%
#     add_standardised_sizes(vol_col = "pair_credit_volume")
# 
#   # ---- Project join ----
#   project_m0 <- project_base %>%
#     dplyr::left_join(host_m, by = "ISO3_host_modern") %>%
#     dplyr::left_join(prop_m, by = "ISO3_proponent_modern") %>%
#     dplyr::left_join(host_gdp_pc, by = "ISO3_host_modern") %>%
#     dplyr::left_join(prop_gdp_pc, by = "ISO3_proponent_modern")
# 
#   if (has_gdp_total) {
#     project_m0 <- project_m0 %>%
#       dplyr::left_join(host_gdp, by = "ISO3_host_modern") %>%
#       dplyr::left_join(prop_gdp, by = "ISO3_proponent_modern")
#   }
# 
#   project_m0 <- project_m0 %>%
#     dplyr::filter(!is.na(host_metric), !is.na(proponent_metric)) %>%
#     add_standardised_sizes(vol_col = "sum_of_credit_volume")
# 
#   for (std_name in names(std_specs)) {
# 
#     spec <- std_specs[[std_name]]
# 
#     pair_m <- pair_m0 %>% dplyr::filter(!is.na(.data[[spec$size_col]]))
#     project_m <- project_m0 %>% dplyr::filter(!is.na(.data[[spec$size_col]]))
# 
#     ## ---- (1) Pair-aggregated ----
#     p_pair <- build_host_prop_scatter(
#       df = pair_m,
#       x_col = "host_metric",
#       y_col = "proponent_metric",
#       size_col = spec$size_col,
#       colour_col = "host_continent_iso",
#       metric_key = m,
#       metric_label = metric_label,
#       title_prefix = paste0("Host vs proponent governance (pair-aggregated, top-20, ", spec$suffix, ")"),
#       size_legend_title = spec$size_title_pair,
#       alpha = 0.60,
#       size_range = c(1.5, 10)
#     )
# 
#     print(p_pair)
#     save_plot_png(p_pair, paste0("pair_host_vs_proponent_top20_", m, "_", spec$suffix, "_hostcolor"))
# 
#     ## ---- (2) Project-level (+ diagnostics) ----
#     p_project <- build_host_prop_scatter(
#       df = project_m,
#       x_col = "host_metric",
#       y_col = "proponent_metric",
#       size_col = spec$size_col,
#       colour_col = "host_continent_iso",
#       metric_key = m,
#       metric_label = metric_label,
#       title_prefix = paste0("Host vs proponent governance (project-level, top-20, ", spec$suffix, ")"),
#       size_legend_title = spec$size_title_project,
#       alpha = 0.40,
#       size_range = c(0.6, 7.5)
#     )
# 
#     built <- ggplot2::ggplot_build(p_project)
#     n_points_drawn <- nrow(built$data[[1]])
# 
#     overlap_top <- project_m %>%
#       dplyr::count(host_metric, proponent_metric, name = "n_projects") %>%
#       dplyr::arrange(dplyr::desc(n_projects)) %>%
#       dplyr::slice(1:10)
# 
#     n_total  <- nrow(project_m)
#     n_unique <- project_m %>% dplyr::distinct(host_metric, proponent_metric) %>% nrow()
# 
#     message(
#       "Metric = ", m,
#       " | std = ", std_name,
#       " | points_drawn = ", n_points_drawn,
#       " | total_rows = ", n_total,
#       " | unique_xy = ", n_unique
#     )
#     print(overlap_top)
# 
#     print(p_project)
#     save_plot_png(p_project, paste0("project_host_vs_proponent_top20_", m, "_", spec$suffix, "_hostcolor"))
#   }
# }



# ################# WITH LABELS host/proponents, WGI, with standardised GDP and labels ###############
# 
# #### ============================================================
# #### Top-20 participants + HOST-only GDP/GDPpc standardisation
# #### + label biggest host–proponent points
# #### ============================================================
# 
# 
# 
# 
# stopifnot(exists("Data_diff"))
# stopifnot(exists("wgi_wide"))
# stopifnot(exists("indicator_labels"))
# stopifnot(exists("continent_colors"))
# stopifnot(exists("save_plot_png"))
# stopifnot(exists("gdp_2024"))
# stopifnot(all(c("iso3c", "gdp_pc_2024") %in% names(gdp_2024)))
# 
# metrics <- names(indicator_labels)
# 
# missing_metrics <- setdiff(metrics, names(wgi_wide))
# if (length(missing_metrics) > 0) {
#   stop("wgi_wide is missing these WGI columns: ", paste(missing_metrics, collapse = ", "))
# }
# 
# has_ggrepel <- requireNamespace("ggrepel", quietly = TRUE)
# 
# #### ---------------------------
# #### Helper: add labels for biggest trades
# #### ---------------------------
# add_blob_labels <- function(p, df, x_col, y_col, label_col, label_value_col,
#                             n_labels = 10, min_value = NULL) {
#   
#   lab_df <- df %>%
#     dplyr::filter(
#       !is.na(.data[[x_col]]),
#       !is.na(.data[[y_col]]),
#       !is.na(.data[[label_col]]),
#       !is.na(.data[[label_value_col]])
#     )
#   
#   if (!is.null(min_value)) {
#     lab_df <- lab_df %>% dplyr::filter(.data[[label_value_col]] >= min_value)
#   }
#   
#   lab_df <- lab_df %>%
#     dplyr::arrange(dplyr::desc(.data[[label_value_col]])) %>%
#     dplyr::slice_head(n = n_labels)
#   
#   if (nrow(lab_df) == 0) return(p)
#   
#   if (requireNamespace("ggrepel", quietly = TRUE)) {
#     
#     p + ggrepel::geom_text_repel(
#       data = lab_df,
#       aes(
#         x = .data[[x_col]],
#         y = .data[[y_col]],
#         label = .data[[label_col]]
#       ),
#       size = 3.6,                     # slightly larger text
#       fontface = "bold",              # <-- bold labels
#       box.padding = 0.6,              # push label further from text box
#       point.padding = 0.4,            # push away from point
#       force = 1.2,                    # stronger repulsion
#       force_pull = 0.6, 
#       bg.color = "white",
#       bg.r = 0.1,
#       
#   # keep reasonable tether to point
#       min.segment.length = 0,
#       segment.size = 0.4,
#       segment.alpha = 0.6,
#       max.overlaps = Inf
#     )
#     
#   } else {
#     
#     p + geom_text(
#       data = lab_df,
#       aes(
#         x = .data[[x_col]],
#         y = .data[[y_col]],
#         label = .data[[label_col]]
#       ),
#       size = 3.5,
#       fontface = "bold",
#       vjust = -1
#     )
#   }
# }
# 
# #### ---------------------------
# #### Helper: scatter plot builder
# #### ---------------------------
# build_host_prop_scatter <- function(df, x_col, y_col, size_col, colour_col,
#                                     metric_key, metric_label,
#                                     title_prefix,
#                                     size_legend_title,
#                                     alpha = 0.45,
#                                     size_range = c(0.6, 8)) {
#   
#   ggplot(
#     df %>% dplyr::filter(!is.na(.data[[colour_col]])),
#     aes(
#       x = .data[[x_col]],
#       y = .data[[y_col]],
#       size = .data[[size_col]],
#       colour = .data[[colour_col]]
#     )
#   ) +
#     geom_point(alpha = alpha) +
#     geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey55") +
#     scale_colour_manual(values = continent_colors, name = "Host continent") +
#     scale_size_continuous(
#       name = size_legend_title,
#       labels = scales::label_number(scale_cut = scales::cut_si(""), accuracy = 1),
#       range = size_range
#     ) +
#     scale_x_continuous(n.breaks = 6) +
#     scale_y_continuous(n.breaks = 6) +
#     labs(
#       x = paste0("Host country: ", metric_label, " (WGI ", metric_key, ")"),
#       y = paste0("Proponent country: ", metric_label, " (WGI ", metric_key, ")"),
#       title = paste0(title_prefix, " — ", metric_label),
#       subtitle = "Color = host continent; dashed line is y = x"
#     ) +
#     theme_minimal() +
#     theme(
#       legend.position = "right",
#       legend.title = element_text(face = "bold")
#     ) +
#     guides(colour = guide_legend(override.aes = list(size = 5)))
# }
# 
# #### ---------------------------
# #### Step 1: Top-20 participants (host+prop total participation)
# #### ---------------------------
# participation <- Data_diff %>%
#   dplyr::filter(sum_of_credit_volume > 0) %>%
#   dplyr::select(ISO3_host_modern, ISO3_proponent_modern, sum_of_credit_volume) %>%
#   dplyr::summarise(host_volume = sum(sum_of_credit_volume, na.rm = TRUE), .by = ISO3_host_modern) %>%
#   dplyr::rename(iso3c = ISO3_host_modern) %>%
#   dplyr::full_join(
#     Data_diff %>%
#       dplyr::filter(sum_of_credit_volume > 0) %>%
#       dplyr::summarise(prop_volume = sum(sum_of_credit_volume, na.rm = TRUE), .by = ISO3_proponent_modern) %>%
#       dplyr::rename(iso3c = ISO3_proponent_modern),
#     by = "iso3c"
#   ) %>%
#   dplyr::mutate(
#     host_volume = dplyr::coalesce(host_volume, 0),
#     prop_volume = dplyr::coalesce(prop_volume, 0),
#     total_participation = host_volume + prop_volume
#   ) %>%
#   dplyr::arrange(dplyr::desc(total_participation))
# 
# top20_iso3 <- participation %>%
#   dplyr::slice_head(n = 20) %>%
#   dplyr::pull(iso3c)
# 
# message("Top-20 participant ISO3s (by host+prop volume): ", paste(top20_iso3, collapse = ", "))
# 
# #### ---------------------------
# #### Step 2: Filter to top-20 only (BOTH host and proponent in top20)
# #### ---------------------------
# Data_top20 <- Data_diff %>%
#   dplyr::filter(
#     sum_of_credit_volume > 0,
#     ISO3_host_modern %in% top20_iso3,
#     ISO3_proponent_modern %in% top20_iso3
#   )
# 
# #### ---------------------------
# #### Step 3: Pair + project bases
# #### ---------------------------
# pair_volume_base <- Data_top20 %>%
#   dplyr::group_by(ISO3_host_modern, ISO3_proponent_modern) %>%
#   dplyr::summarise(
#     pair_credit_volume = sum(sum_of_credit_volume, na.rm = TRUE),
#     host_continent_iso = dplyr::first(host_continent_iso),
#     .groups = "drop"
#   ) %>%
#   dplyr::filter(pair_credit_volume > 0) %>%
#   dplyr::mutate(pair_label = paste0(ISO3_host_modern, "–", ISO3_proponent_modern))
# 
# project_base <- Data_top20 %>%
#   dplyr::mutate(pair_label = paste0(ISO3_host_modern, "–", ISO3_proponent_modern))
# 
# #### ---------------------------
# #### Step 4: Host GDPpc and optional host total GDP
# #### ---------------------------
# host_gdp_pc <- gdp_2024 %>%
#   dplyr::select(iso3c, gdp_pc_2024) %>%
#   dplyr::rename(ISO3_host_modern = iso3c, host_gdp_pc = gdp_pc_2024)
# 
# has_gdp_total <- exists("gdp_total_2024")
# 
# if (has_gdp_total) {
#   # EXPECTED: iso3c + one total GDP column (or set it explicitly below)
#   gdp_total_col <- setdiff(names(gdp_total_2024), "iso3c")[1]
#   if (is.na(gdp_total_col)) stop("gdp_total_2024 exists but has no total GDP column besides iso3c.")
#   
#   host_gdp <- gdp_total_2024 %>%
#     dplyr::select(iso3c, gdp_total = dplyr::all_of(gdp_total_col)) %>%
#     dplyr::rename(ISO3_host_modern = iso3c, host_gdp = gdp_total)
# } else {
#   message("NOTE: No `gdp_total_2024` found; GDP-standardised plots will be skipped (GDPpc plots will run).")
# }
# 
# add_host_standardised_sizes <- function(df, vol_col) {
#   out <- df %>%
#     dplyr::mutate(
#       vol_per_host_gdp_pc = dplyr::if_else(!is.na(host_gdp_pc) & host_gdp_pc > 0,
#                                            .data[[vol_col]] / host_gdp_pc,
#                                            NA_real_)
#     )
#   
#   if ("host_gdp" %in% names(out)) {
#     out <- out %>%
#       dplyr::mutate(
#         vol_per_host_gdp = dplyr::if_else(!is.na(host_gdp) & host_gdp > 0,
#                                           .data[[vol_col]] / host_gdp,
#                                           NA_real_)
#       )
#   }
#   
#   out
# }
# 
# #### ---------------------------
# #### Step 5: Standardisation specs (host-only)
# #### ---------------------------
# std_specs <- list(
#   gdp_pc = list(
#     size_col = "vol_per_host_gdp_pc",
#     suffix = "stdHostGDPpc",
#     size_title_pair = "Credit volume / host GDP per capita\n(host–proponent pair)",
#     size_title_project = "Credit volume / host GDP per capita"
#   )
# )
# 
# if (has_gdp_total) {
#   std_specs <- c(
#     list(
#       gdp = list(
#         size_col = "vol_per_host_gdp",
#         suffix = "stdHostGDP",
#         size_title_pair = "Credit volume / host GDP\n(host–proponent pair)",
#         size_title_project = "Credit volume / host GDP"
#       )
#     ),
#     std_specs
#   )
# }
# 
# #### ---------------------------
# #### Step 6: Loop metrics x standardisation x plot type (+labels)
# #### ---------------------------
# for (m in metrics) {
#   
#   metric_label <- unname(indicator_labels[m])
#   
#   wgi_m <- wgi_wide %>%
#     dplyr::select(iso3c, metric_value = dplyr::all_of(m))
#   
#   host_m <- wgi_m %>%
#     dplyr::rename(ISO3_host_modern = iso3c, host_metric = metric_value)
#   
#   prop_m <- wgi_m %>%
#     dplyr::rename(ISO3_proponent_modern = iso3c, proponent_metric = metric_value)
#   
#   # ---- Pair base with WGI + host GDPpc (+ host GDP total if present) ----
#   pair_m0 <- pair_volume_base %>%
#     dplyr::left_join(host_m, by = "ISO3_host_modern") %>%
#     dplyr::left_join(prop_m, by = "ISO3_proponent_modern") %>%
#     dplyr::left_join(host_gdp_pc, by = "ISO3_host_modern")
#   
#   if (has_gdp_total) {
#     pair_m0 <- pair_m0 %>% dplyr::left_join(host_gdp, by = "ISO3_host_modern")
#   }
#   
#   pair_m0 <- pair_m0 %>%
#     dplyr::filter(!is.na(host_metric), !is.na(proponent_metric)) %>%
#     add_host_standardised_sizes(vol_col = "pair_credit_volume")
#   
#   # ---- Project base with WGI + host GDPpc (+ host GDP total if present) ----
#   project_m0 <- project_base %>%
#     dplyr::left_join(host_m, by = "ISO3_host_modern") %>%
#     dplyr::left_join(prop_m, by = "ISO3_proponent_modern") %>%
#     dplyr::left_join(host_gdp_pc, by = "ISO3_host_modern")
#   
#   if (has_gdp_total) {
#     project_m0 <- project_m0 %>% dplyr::left_join(host_gdp, by = "ISO3_host_modern")
#   }
#   
#   project_m0 <- project_m0 %>%
#     dplyr::filter(!is.na(host_metric), !is.na(proponent_metric)) %>%
#     add_host_standardised_sizes(vol_col = "sum_of_credit_volume")
#   
#   for (std_name in names(std_specs)) {
#     
#     spec <- std_specs[[std_name]]
#     
#     pair_m <- pair_m0 %>% dplyr::filter(!is.na(.data[[spec$size_col]]))
#     project_m <- project_m0 %>% dplyr::filter(!is.na(.data[[spec$size_col]]))
#     
#     ## ---- (1) Pair-aggregated ----
#     p_pair <- build_host_prop_scatter(
#       df = pair_m,
#       x_col = "host_metric",
#       y_col = "proponent_metric",
#       size_col = spec$size_col,
#       colour_col = "host_continent_iso",
#       metric_key = m,
#       metric_label = metric_label,
#       title_prefix = paste0("Host vs proponent governance (pair-aggregated, top-20, ", spec$suffix, ")"),
#       size_legend_title = spec$size_title_pair,
#       alpha = 0.60,
#       size_range = c(1.5, 10)
#     )
#     
#     # label biggest pair blobs (by the plotted size variable)
#     p_pair <- add_blob_labels(
#       p = p_pair,
#       df = pair_m,
#       x_col = "host_metric",
#       y_col = "proponent_metric",
#       label_col = "pair_label",
#       label_value_col = spec$size_col,
#       n_labels = 12
#     )
#     
#     print(p_pair)
#     save_plot_png(p_pair, paste0("pair_host_vs_proponent_top20_", m, "_", spec$suffix, "_hostcolor"))
#     
#     ## ---- (2) Project-level (+ your diagnostics) ----
#     p_project <- build_host_prop_scatter(
#       df = project_m,
#       x_col = "host_metric",
#       y_col = "proponent_metric",
#       size_col = spec$size_col,
#       colour_col = "host_continent_iso",
#       metric_key = m,
#       metric_label = metric_label,
#       title_prefix = paste0("Host vs proponent governance (project-level, top-20, ", spec$suffix, ")"),
#       size_legend_title = spec$size_title_project,
#       alpha = 0.40,
#       size_range = c(0.6, 7.5)
#     )
#     
#     # label biggest project blobs (by the plotted size variable)
#     # (use fewer labels because there are many more points)
#     p_project <- add_blob_labels(
#       p = p_project,
#       df = project_m,
#       x_col = "host_metric",
#       y_col = "proponent_metric",
#       label_col = "pair_label",
#       label_value_col = spec$size_col,
#       n_labels = 10
#     )
#     
#     built <- ggplot2::ggplot_build(p_project)
#     n_points_drawn <- nrow(built$data[[1]])
#     
#     overlap_top <- project_m %>%
#       dplyr::count(host_metric, proponent_metric, name = "n_projects") %>%
#       dplyr::arrange(dplyr::desc(n_projects)) %>%
#       dplyr::slice(1:10)
#     
#     n_total  <- nrow(project_m)
#     n_unique <- project_m %>% dplyr::distinct(host_metric, proponent_metric) %>% nrow()
#     
#     message(
#       "Metric = ", m,
#       " | std = ", std_name,
#       " | points_drawn = ", n_points_drawn,
#       " | total_rows = ", n_total,
#       " | unique_xy = ", n_unique
#     )
#     print(overlap_top)
#     
#     print(p_project)
#     save_plot_png(p_project, paste0("project_host_vs_proponent_top20_", m, "_", spec$suffix, "_hostcolor"))
#   }
# }

################################################


#### ============================================================ ####
####  Pipeline for Host vs Proponent WGI scatterplots ####
#### Outputs for each WGI metric:
####   A) Raw volume (no GDP correction)
####   B) Standardised by HOST GDP per capita
####   C) Standardised by HOST nominal GDP (Current US$)
#### For each dataset:
####   1) ALL international trades
####   2) Top-20 participants only (both host & proponent in top20)
#### For each plot type:
####   i) Pair-aggregated
####   ii) Project-level (more points and busier, but keeps volumes separated by project)
#### ============================================================ ####

library(tidyverse)
library(readr)
library(stringr)
library(scales)

#### --------------------------- ####
#### Required objects from earlier script ####
#### --------------------------- ####
stopifnot(exists("Data_diff"))            # international only - no in-country trades
stopifnot(exists("wgi_wide"))             # iso3c + metrics columns
stopifnot(exists("indicator_labels"))     # named vector: cc/ge/pv/rl/rq/va
stopifnot(exists("continent_colors"))     # your palette
stopifnot(exists("save_plot_png"))        # your ggsave wrapper
stopifnot(exists("gdp_2024"))             # iso3c + gdp_pc_2024

stopifnot(all(c("iso3c", "gdp_pc_2024") %in% names(gdp_2024)))

metrics <- names(indicator_labels)
missing_metrics <- setdiff(metrics, names(wgi_wide))
if (length(missing_metrics) > 0) {
  stop("wgi_wide is missing these WGI columns: ", paste(missing_metrics, collapse = ", "))
}

#### --------------------------- ####
#### Step 0 — Import nominal GDP (Current US$) -> gdp_total_2024 ####
#### --------------------------- ####
gdp_total_path <- "/Users/zoe/Desktop/CCBP/CCBP_database/data/GDP_nominal/gdp_nom_en.csv"

gdp_nom_raw <- readr::read_csv(
  gdp_total_path,
  col_types = readr::cols(.default = readr::col_character()),
  show_col_types = FALSE
)

iso_candidates <- c("iso3c", "country_code", "iso3", "code", "Country Code", "ISO3")
iso_col <- names(gdp_nom_raw)[tolower(names(gdp_nom_raw)) %in% tolower(iso_candidates)][1]
if (is.na(iso_col)) {
  stop("Couldn't detect ISO3 column in nominal GDP file. Columns are: ",
       paste(names(gdp_nom_raw), collapse = ", "))
}

year_cols <- names(gdp_nom_raw)[stringr::str_detect(names(gdp_nom_raw), "^\\d{4}$")]
if (length(year_cols) == 0) {
  stop("Couldn't detect any 4-digit year columns in nominal GDP file. Columns are: ",
       paste(names(gdp_nom_raw), collapse = ", "))
}
target_year <- if ("2024" %in% year_cols) "2024" else as.character(max(as.integer(year_cols)))
message("Nominal GDP: using year column = ", target_year)


gdp_total_2024 <- gdp_nom_raw %>%
  transmute(
    iso3c = .data[[iso_col]],
    gdp_total_2024 = readr::parse_number(.data[[target_year]])
  ) %>%
  filter(!is.na(iso3c)) %>%
  distinct(iso3c, .keep_all = TRUE)

stopifnot(!anyDuplicated(gdp_total_2024$iso3c))

#### ---------------------------
#### Step 1 — Build macro lookup tables (HOST-only) ####
#### ---------------------------
host_gdp_pc <- gdp_2024 %>%
  select(iso3c, gdp_pc_2024) %>%
  rename(ISO3_host_modern = iso3c, host_gdp_pc = gdp_pc_2024)

host_gdp <- gdp_total_2024 %>%
  select(iso3c, gdp_total_2024) %>%
  rename(ISO3_host_modern = iso3c, host_gdp = gdp_total_2024)

#### ---------------------------
#### Step 2 — Helper: compute sizes (RAW + host-standardised) ####
#### ---------------------------
add_size_modes <- function(df, vol_col) {
  df %>%
    mutate(
      vol_raw = .data[[vol_col]],
      vol_per_host_gdp_pc = if_else(!is.na(host_gdp_pc) & host_gdp_pc > 0,
                                    .data[[vol_col]] / host_gdp_pc, NA_real_),
      vol_per_host_gdp = if_else(!is.na(host_gdp) & host_gdp > 0,
                                 .data[[vol_col]] / host_gdp, NA_real_)
    )
}

#### ---------------------------
#### Step 3 — Helper: plot builder ####
#### ---------------------------
build_host_prop_scatter <- function(df, metric_key, metric_label,
                                    size_col, size_legend_title,
                                    title_prefix,
                                    alpha = 0.45,
                                    size_range = c(0.6, 8)) {
  
  ggplot(
    df %>% filter(!is.na(host_continent_iso),
                  !is.na(host_metric),
                  !is.na(proponent_metric),
                  !is.na(.data[[size_col]]),
                  .data[[size_col]] > 0),
    aes(
      x = host_metric,
      y = proponent_metric,
      size = .data[[size_col]],
      colour = host_continent_iso
    )
  ) +
    geom_point(alpha = alpha) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey55") +
    scale_colour_manual(values = continent_colors, name = "Host continent") +
    scale_size_continuous(
      name = size_legend_title,
      labels = scales::label_number(scale_cut = scales::cut_si(""), accuracy = 1),
      range = size_range
    ) +
    scale_x_continuous(n.breaks = 6) +
    scale_y_continuous(n.breaks = 6) +
    labs(
      x = paste0("Host: ", metric_label, " (WGI ", metric_key, ")"),
      y = paste0("Proponent: ", metric_label, " (WGI ", metric_key, ")"),
      title = paste0(title_prefix, " — ", metric_label),
      subtitle = "Color = host continent; dashed line is y = x"
    ) +
    theme_minimal() +
    theme(
      legend.position = "right",
      legend.title = element_text(face = "bold")
    ) +
    guides(colour = guide_legend(override.aes = list(size = 5)))
}

#### ---------------------------
#### Step 4 — Build dataset variants: ALL vs Top-20 ####
#### ---------------------------
make_top20_subset <- function(df) {
  participation <- df %>%
    filter(sum_of_credit_volume > 0) %>%
    summarise(host_volume = sum(sum_of_credit_volume, na.rm = TRUE), .by = ISO3_host_modern) %>%
    rename(iso3c = ISO3_host_modern) %>%
    full_join(
      df %>%
        filter(sum_of_credit_volume > 0) %>%
        summarise(prop_volume = sum(sum_of_credit_volume, na.rm = TRUE), .by = ISO3_proponent_modern) %>%
        rename(iso3c = ISO3_proponent_modern),
      by = "iso3c"
    ) %>%
    mutate(
      host_volume = coalesce(host_volume, 0),
      prop_volume = coalesce(prop_volume, 0),
      total_participation = host_volume + prop_volume
    ) %>%
    arrange(desc(total_participation))
  
  top20_iso3 <- participation %>% slice_head(n = 20) %>% pull(iso3c)
  message("Top-20 ISO3s: ", paste(top20_iso3, collapse = ", "))
  
  df %>%
    filter(
      sum_of_credit_volume > 0,
      ISO3_host_modern %in% top20_iso3,
      ISO3_proponent_modern %in% top20_iso3
    )
}

Data_all  <- Data_diff %>% filter(sum_of_credit_volume > 0)
Data_top20 <- make_top20_subset(Data_diff)

datasets <- list(
  all = Data_all,
  top20 = Data_top20
)

#### ---------------------------
#### Step 5 — Define size modes to run (raw + GDPpc + GDP) ####
#### ---------------------------
size_modes <- list(
  raw = list(
    size_col_pair = "vol_raw",
    size_col_project = "vol_raw",
    suffix = "rawVolume",
    legend_pair = "Total credit volume (tonnes)\n(host–proponent pair)",
    legend_project = "Project credit volume (tonnes)"
  ),
  gdp_pc = list(
    size_col_pair = "vol_per_host_gdp_pc",
    size_col_project = "vol_per_host_gdp_pc",
    suffix = "stdHostGDPpc",
    legend_pair = "Credit volume / host GDP per capita\n(host–proponent pair)",
    legend_project = "Credit volume / host GDP per capita"
  ),
  gdp = list(
    size_col_pair = "vol_per_host_gdp",
    size_col_project = "vol_per_host_gdp",
    suffix = "stdHostGDP",
    legend_pair = "Credit volume / host GDP (Current US$)\n(host–proponent pair)",
    legend_project = "Credit volume / host GDP (Current US$)"
  )
)

#### ---------------------------
#### Step 6 — Main loop: dataset x metric x size_mode x plot_type ####
#### ---------------------------
for (ds_name in names(datasets)) {
  
  df0 <- datasets[[ds_name]]
  
  # pair base + project base for this dataset variant
  pair_base <- df0 %>%
    group_by(ISO3_host_modern, ISO3_proponent_modern) %>%
    summarise(
      pair_credit_volume = sum(sum_of_credit_volume, na.rm = TRUE),
      host_continent_iso = first(host_continent_iso),
      .groups = "drop"
    ) %>%
    filter(pair_credit_volume > 0)
  
  project_base <- df0
  
  for (m in metrics) {
    
    metric_label <- unname(indicator_labels[m])
    
    # WGI lookups
    wgi_m <- wgi_wide %>% select(iso3c, metric_value = all_of(m))
    host_m <- wgi_m %>% rename(ISO3_host_modern = iso3c, host_metric = metric_value)
    prop_m <- wgi_m %>% rename(ISO3_proponent_modern = iso3c, proponent_metric = metric_value)
    
    # ---- Pair join (WGI + HOST GDPpc + HOST GDP total) ----
    pair_m0 <- pair_base %>%
      left_join(host_m, by = "ISO3_host_modern") %>%
      left_join(prop_m, by = "ISO3_proponent_modern") %>%
      left_join(host_gdp_pc, by = "ISO3_host_modern") %>%
      left_join(host_gdp, by = "ISO3_host_modern") %>%
      filter(!is.na(host_metric), !is.na(proponent_metric)) %>%
      add_size_modes(vol_col = "pair_credit_volume")
    
    # ---- Project join ----
    project_m0 <- project_base %>%
      left_join(host_m, by = "ISO3_host_modern") %>%
      left_join(prop_m, by = "ISO3_proponent_modern") %>%
      left_join(host_gdp_pc, by = "ISO3_host_modern") %>%
      left_join(host_gdp, by = "ISO3_host_modern") %>%
      filter(!is.na(host_metric), !is.na(proponent_metric)) %>%
      add_size_modes(vol_col = "sum_of_credit_volume")
    
    for (mode_name in names(size_modes)) {
      
      mode <- size_modes[[mode_name]]
      
      ## ---- Pair plot ----
      p_pair <- build_host_prop_scatter(
        df = pair_m0,
        metric_key = m,
        metric_label = metric_label,
        size_col = mode$size_col_pair,
        size_legend_title = mode$legend_pair,
        title_prefix = paste0("Host vs proponent governance (pair-aggregated, ", ds_name, ", ", mode$suffix, ")"),
        alpha = 0.60,
        size_range = c(1.5, 10)
      )
      
      print(p_pair)
      save_plot_png(p_pair, paste0("pair_host_vs_proponent_", ds_name, "_", m, "_", mode$suffix, "_hostcolor"))
      
      ## ---- Project plot ----
      p_project <- build_host_prop_scatter(
        df = project_m0,
        metric_key = m,
        metric_label = metric_label,
        size_col = mode$size_col_project,
        size_legend_title = mode$legend_project,
        title_prefix = paste0("Host vs proponent governance (project-level, ", ds_name, ", ", mode$suffix, ")"),
        alpha = 0.40,
        size_range = c(0.6, 7.5)
      )
      
      print(p_project)
      save_plot_png(p_project, paste0("project_host_vs_proponent_", ds_name, "_", m, "_", mode$suffix, "_hostcolor"))
    }
  }
}

message("DONE: generated plots for all datasets x metrics x size modes x plot types.")





################################################


# ###############
# #### Network diagram of hosts-proponents ####
# ###############


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


