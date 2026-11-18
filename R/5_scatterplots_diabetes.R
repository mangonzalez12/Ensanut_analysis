library(ggplot2)
library(ggpubr)  # for stat_cor()



data <- read.csv(here::here("data", "Output", "clean_data_for_statistical_analysis.csv"),
                   row.names = 1, header = TRUE)
#data <- subset(data, Diagnosis == "Diabetes_questionnaire")

 
ggplot(data, aes(x = dAGES_total, y = NOVA4_pct)) + 
  geom_point(size = 2.5, color = "#2c3e50", alpha = 0.7) +
  geom_smooth(method = "lm", color = "#c0392b", fill = "#c0392b", alpha = 0.15) +
  stat_cor(method = "pearson", label.x.npc = "left", label.y.npc = "top", size = 4.5) +
  labs(
    x = "AGES (total)",
    y = "NOVA4",
    title = "Correlation between total AGEs and NOVA4 Consumption in total population"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold")
  )

ggsave(here::here("visualization", "scatter", "AGES_vs_nova4_correlation in total population_new.png"),
       width = 12, height = 6, dpi = 300)




#ggplot(data, aes(x = Glucose, y = NOVA4_pct)) + 
#  geom_point(size = 2.5, color = "#2c3e50", alpha = 0.7) +
#  geom_smooth(method = "lm", color = "#c0392b", fill = "#c0392b", alpha = 0.15) +
#  stat_cor(method = "pearson", label.x.npc = "left", label.y.npc = "top", size = 4.5) +
#  labs(
#    x = "Glucose",
#    y = "NOVA 4 (%)",
#    title = "Correlation between Glucose and NOVA4 Consumption in Diabetes population"
#  ) +
#  theme_minimal(base_size = 13) +
#  theme(
#    plot.title = element_text(face = "bold", hjust = 0.5),
#    panel.grid.minor = element_blank(),
#    axis.title = element_text(face = "bold")
#  )

#ggsave("../Visualizacion/scatter/Glucose_vs_nova4_correlationin in Diabetes population.png", width = 12, height = 6, dpi = 300)





