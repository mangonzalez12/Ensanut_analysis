library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
library(patchwork)

df <- read.csv(here::here("data", "Output", "clean_data_for_statistical_analysis.csv"),
                   header = TRUE, row.names = 1)


# --- reshape to long format ---
df_long <- df %>%
  select(AGEs, NOVA1_pct, NOVA2_pct, NOVA3_pct, NOVA4_pct) %>%
  pivot_longer(cols = starts_with("NOVA"),
               names_to = "NOVA_group",
               values_to = "pct") %>%
  mutate(NOVA_group = recode(NOVA_group,
                             NOVA1_pct = "NOVA1 (Unprocessed/minimally processed)",
                             NOVA2_pct = "NOVA2 (Processed culinary ingredients)",
                             NOVA3_pct = "NOVA3 (Processed foods)",
                             NOVA4_pct = "NOVA4 (Ultra-processed foods)"),
         AGEs = factor(AGEs, levels = c("Low (<8000 KU/Day)", "Moderate (8000-15000 KU/Day)", "High (>15000 KU/Day)")))

nature_theme <- theme_minimal(base_size = 30) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.4),
    axis.ticks = element_line(color = "black", linewidth = 0.4),
    legend.position = "bottom",
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 20)
  )

# --- Plot 1: Stacked bar of MEAN composition per AGEs level ---
df_summary <- df_long %>%
  group_by(AGEs, NOVA_group) %>%
  summarise(mean_pct = mean(pct, na.rm = TRUE), .groups = "drop")

p1 <- ggplot(df_summary, aes(x = AGEs, y = mean_pct, fill = NOVA_group)) +
  geom_col(width = 0.6, color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.1f%%", mean_pct)),
            position = position_stack(vjust = 0.5), size = 10, color = "white") +
  scale_fill_manual(values = c("#2C7BB6", "#ABD9E9", "#FDAE61", "#D7191C")) +
  labs(x = "AGEs consumption level", y = "Mean % of total kcal", size = 8,
       fill = NULL, title = "Dietary energy composition by NOVA group") +
  nature_theme


# --- Plot 1: Stacked bar of MEAN composition per AGEs level ---
df_summary <- df_long %>%
  group_by(AGEs, NOVA_group) %>%
  summarise(mean_pct = mean(pct, na.rm = TRUE), .groups = "drop")
p1 <- ggplot(df_summary, aes(x = AGEs, y = mean_pct, fill = NOVA_group)) +
  geom_col(width = 0.6, color = "white", linewidth = 0.3) +
  geom_text(aes(label = sprintf("%.1f%%", mean_pct)),
            position = position_stack(vjust = 0.5),
            size = 12, color = "white") +
  scale_fill_manual(values = c("#2C7BB6", "#ABD9E9", "#FDAE61", "#D7191C")) +
  labs(x = "AGEs consumption level", y = "Mean % of total kcal",
       fill = NULL,
       title = "Dietary NOVA % Kcal by dAGEs consumption levels") +
  nature_theme +
  theme(
    plot.title = element_text(size = 34, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 26),
    axis.title.y = element_text(size = 26),
    axis.text.x = element_text(size = 22),
    axis.text.y = element_text(size = 22),
    legend.text = element_text(size = 22)
  )

ggsave(filename = here::here("visualization", "ages", "NOVA_composition_by_AGEs_new.png"),
       plot = p1, width = 25, height = 14, dpi = 400)




# --- reshape to long format, keyed by Group instead of AGEs ---
df_long_group <- df %>%
  select(Groups, NOVA1_pct, NOVA2_pct, NOVA3_pct, NOVA4_pct) %>%
  pivot_longer(cols = starts_with("NOVA"),
               names_to = "NOVA_group",
               values_to = "pct") %>%
  mutate(NOVA_group = recode(NOVA_group,
                             NOVA1_pct = "NOVA1 (Unprocessed/minimally processed)",
                             NOVA2_pct = "NOVA2 (Processed culinary ingredients)",
                             NOVA3_pct = "NOVA3 (Processed foods)",
                             NOVA4_pct = "NOVA4 (Ultra-processed foods)"),
         Group = factor(Groups, levels = c("Group_1", "Group_2", "Group_3")))

# --- Plot 1b: Stacked bar of MEAN composition per Group ---
df_summary_group <- df_long_group %>%
  group_by(Group, NOVA_group) %>%
  summarise(mean_pct = mean(pct, na.rm = TRUE), .groups = "drop")

p1_group <- ggplot(df_summary_group, aes(x = Group, y = mean_pct, fill = NOVA_group)) +
  geom_col(width = 0.6, color = "white", linewidth = 0.3) +
  geom_text(aes(label = sprintf("%.1f%%", mean_pct)),
            position = position_stack(vjust = 0.5), size = 3, color = "white") +
  scale_fill_manual(values = c("#2C7BB6", "#ABD9E9", "#FDAE61", "#D7191C")) +
  labs(x = "Cluster group", y = "Mean % of total kcal",
       fill = NULL, title = "Dietary NOVA % Kcal by Groups") +
  nature_theme

ggsave(filename = here::here("visualization", "ages", "NOVA_composition_by_Groups.png"),
       plot = p1_group, width = 30, height = 14, dpi = 400)


