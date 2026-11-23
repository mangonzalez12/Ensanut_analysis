library(ggplot2)
library(ggpubr)  # for stat_cor()



data <- read.csv(here::here("data", "Output", "clean_data_for_statistical_analysis.csv"),
                   row.names = 1, header = TRUE)
#data <- subset(data, Diagnosis == "Diabetes_questionnaire")

 
fmt_p <- function(p) ifelse(p < 0.00001, "italic(p)<0.00001",
                            paste0("italic(p)==", formatC(p, format = "f", digits = 5)))

ggplot(data, aes(x = dAGES_total, y = NOVA4_pct)) + 
  geom_point(size = 2, color = "#2c3e50", alpha = 0.7) +
  geom_smooth(method = "lm", color = "#c0392b", fill = "#c0392b", alpha = 0.15) +
  stat_cor(aes(label = paste0("italic(R)==", round(after_stat(r), 2), "~`,`~", fmt_p(after_stat(p)))),
           method = "pearson", label.x.npc = "left", label.y.npc = "top", size = 4.5) +
  labs(
    x = "dAGEs (total)",
    y = "NOVA 4-%Kcal",
    title = "Correlation between total dAGEs and NOVA 4-%Kcal in total population"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold")
  )


ggsave(here::here("visualization", "scatter", "dAGEs_vs_nova4_correlation in total population_new.png"),
       width = 12, height = 6, dpi = 300) 





