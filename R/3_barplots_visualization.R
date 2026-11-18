library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
library(patchwork)


df <- read.csv("../Output/clean_data_for statistical analysis.csv", row.names = 1, header = TRUE)



# --- reshape to long format ---
df_long <- df %>%
  select(AGES, NOVA1_pct, NOVA2_pct, NOVA3_pct, NOVA4_pct) %>%
  pivot_longer(cols = starts_with("NOVA"),
               names_to = "NOVA_group",
               values_to = "pct") %>%
  mutate(NOVA_group = recode(NOVA_group,
                             NOVA1_pct = "NOVA1 (Unprocessed/minimally processed)",
                             NOVA2_pct = "NOVA2 (Processed culinary ingredients)",
                             NOVA3_pct = "NOVA3 (Processed foods)",
                             NOVA4_pct = "NOVA4 (Ultra-processed foods)"),
         AGES = factor(AGES, levels = c("Low", "Moderate", "High")))

nature_theme <- theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.4),
    axis.ticks = element_line(color = "black", linewidth = 0.4),
    legend.position = "bottom",
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 18)
  )

# --- Plot 1: Stacked bar of MEAN composition per AGES level ---
df_summary <- df_long %>%
  group_by(AGES, NOVA_group) %>%
  summarise(mean_pct = mean(pct, na.rm = TRUE), .groups = "drop")

p1 <- ggplot(df_summary, aes(x = AGES, y = mean_pct, fill = NOVA_group)) +
  geom_col(width = 0.6, color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.1f%%", mean_pct)),
            position = position_stack(vjust = 0.5), size = 10, color = "white") +
  scale_fill_manual(values = c("#2C7BB6", "#ABD9E9", "#FDAE61", "#D7191C")) +
  labs(x = "AGES consumption level", y = "Mean % of total kcal", size = 8,
       fill = NULL, title = "Dietary energy composition by NOVA group") +
  nature_theme


# --- Plot 1: Stacked bar of MEAN composition per AGES level ---
df_summary <- df_long %>%
  group_by(AGES, NOVA_group) %>%
  summarise(mean_pct = mean(pct, na.rm = TRUE), .groups = "drop")

p1 <- ggplot(df_summary, aes(x = AGES, y = mean_pct, fill = NOVA_group)) +
  geom_col(width = 0.6, color = "white", linewidth = 0.3) +
  geom_text(aes(label = sprintf("%.1f%%", mean_pct)),
            position = position_stack(vjust = 0.5), size = 8, color = "white") +
  scale_fill_manual(values = c("#2C7BB6", "#ABD9E9", "#FDAE61", "#D7191C")) +
  labs(x = "AGES consumption level", y = "Mean % of total kcal",
       fill = NULL, title = "Dietary NOVA % Kcal by AGES consumption levels") +
  nature_theme


ggsave("../Visualizacion/ages/NOVA_composition_by_AGES_new.png", p1, width = 12, height = 10, dpi = 300)






# --- reshape to long format, keyed by Group instead of AGES ---
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


ggsave("../Visualizacion/ages/NOVA_composition_by_Groups.png", p1_group, width = 12, height = 10, dpi = 300)




