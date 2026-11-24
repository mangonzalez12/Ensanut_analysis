library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
library(patchwork)

df <- read.csv(here::here("data", "Output", "clean_data_for_statistical_analysis.csv"),
                   header = TRUE, row.names = 1)

# --- Figure size and text scale -------------------------------------------
# All text sizes derive from `base_pt`, so changing it (or the figure size)
# keeps titles, axes, legend and bar labels in proportion to each other.
fig_width  <- 16   # inches
fig_height <- 10   # inches
fig_dpi    <- 400
base_pt    <- 22   # base font size in points

# geom_text() sizes are in mm; convert from points so bar labels match the theme
label_size <- (base_pt * 0.85) / .pt

nova_colors <- c("#2C7BB6", "#ABD9E9", "#FDAE61", "#D7191C")

nova_labels <- c(NOVA1_pct = "NOVA1 (Unprocessed/minimally processed)",
                 NOVA2_pct = "NOVA2 (Processed culinary ingredients)",
                 NOVA3_pct = "NOVA3 (Processed foods)",
                 NOVA4_pct = "NOVA4 (Ultra-processed foods)")

nature_theme <- theme_minimal(base_size = base_pt) +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.line          = element_line(color = "black", linewidth = 0.4),
    axis.ticks         = element_line(color = "black", linewidth = 0.4),
    axis.title         = element_text(size = rel(1.1)),
    axis.text          = element_text(size = rel(0.9), color = "black"),
    legend.position    = "bottom",
    legend.text        = element_text(size = rel(0.85)),
    strip.background   = element_blank(),
    strip.text         = element_text(face = "bold"),
    plot.title         = element_text(face = "bold", size = rel(1.25), hjust = 0.5)
  )

# --- Helper: stacked bar of mean NOVA composition per category ---
# Segments too thin for an inside label (< small_cut %) get their label placed
# just to the right of the bar in dark text, so nothing overlaps.
small_cut <- 5

plot_nova_stack <- function(data, group_var, x_lab, title) {
  df_sum <- data %>%
    group_by({{ group_var }}, NOVA_group) %>%
    summarise(mean_pct = mean(pct, na.rm = TRUE), .groups = "drop") %>%
    arrange({{ group_var }}, NOVA_group) %>%
    group_by({{ group_var }}) %>%
    # ggplot stacks the first factor level on top, so compute centres from the top down
    mutate(ypos  = sum(mean_pct) - (cumsum(mean_pct) - mean_pct / 2),
           small = mean_pct < small_cut,
           xnum  = as.numeric({{ group_var }})) %>%
    ungroup()

  bar_width <- 0.6

  ggplot(df_sum, aes(x = {{ group_var }}, y = mean_pct, fill = NOVA_group)) +
    geom_col(width = bar_width, color = "white", linewidth = 0.3) +
    geom_text(data = filter(df_sum, !small),
              aes(y = ypos, label = sprintf("%.1f%%", mean_pct)),
              size = label_size, color = "white") +
    geom_text(data = filter(df_sum, small),
              aes(x = xnum + bar_width / 2 + 0.03, y = ypos,
                  label = sprintf("%.1f%%", mean_pct)),
              size = label_size, color = "grey20", hjust = 0) +
    scale_fill_manual(values = nova_colors) +
    guides(fill = guide_legend(nrow = 2)) +
    labs(x = x_lab, y = "Mean % of total kcal", fill = NULL, title = title) +
    nature_theme
}

# --- reshape to long format ---
df_long <- df %>%
  select(AGEs, Groups, NOVA1_pct, NOVA2_pct, NOVA3_pct, NOVA4_pct) %>%
  pivot_longer(cols = starts_with("NOVA"),
               names_to = "NOVA_group",
               values_to = "pct") %>%
  mutate(NOVA_group = factor(recode(NOVA_group, !!!nova_labels), levels = nova_labels),
         AGEs  = factor(AGEs, levels = c("Low (<8000 KU/Day)", "Moderate (8000-15000 KU/Day)", "High (>15000 KU/Day)")),
         Group = factor(Groups, levels = c("Group_1", "Group_2", "Group_3")))

# --- Plot 1: by AGEs consumption level ---
p1 <- plot_nova_stack(df_long, AGEs, "dAGEs consumption level",
                      "Dietary NOVA % Kcal by dAGEs consumption levels")

ggsave(filename = here::here("visualization", "ages", "NOVA_composition_by_AGEs_new.png"),
       plot = p1, width = fig_width, height = fig_height, dpi = fig_dpi)

# --- Plot 1b: by cluster group ---
p1_group <- plot_nova_stack(df_long, Group, "Cluster group",
                            "Dietary NOVA % Kcal by Groups")

ggsave(filename = here::here("visualization", "ages", "NOVA_composition_by_Groups.png"),
       plot = p1_group, width = fig_width, height = fig_height, dpi = fig_dpi)
