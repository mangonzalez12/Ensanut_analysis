library(ggplot2)
library(ggpubr)  # for stat_cor()
library(patchwork)



data <- read.csv(here::here("data", "Output", "clean_data_for_statistical_analysis.csv"),
                   row.names = 1, header = TRUE)
#data <- subset(data, Diagnosis == "Diabetes_questionnaire")

 
fmt_p <- function(p) ifelse(p < 0.00001, "italic(p)<0.00001",
                            paste0("italic(p)==", formatC(p, format = "f", digits = 5)))

p1 <- ggplot(data, aes(x = dAGES_total, y = NOVA1_pct)) + 
  geom_point(size = 2, color = "#2c3e50", alpha = 0.7) +
  geom_smooth(method = "lm", color = "#c0392b", fill = "#c0392b", alpha = 0.15) +
  stat_cor(aes(label = paste0("italic(R)==", round(after_stat(r), 2), "~`,`~", fmt_p(after_stat(p)))),
           method = "pearson", label.x.npc = "left", label.y.npc = "top", size = 4.5) +
  labs(
    x = "dAGEs (total)",
    y = "NOVA 1-%Kcal",
    #title = "Correlation between total dAGEs and NOVA 1-%Kcal in total population"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold")
  )

ggsave(here::here("visualization", "scatter", "dAGEs_vs_nova1_correlation in total population_new.png"),
       width = 12, height = 6, dpi = 300) 


p2<- ggplot(data, aes(x = dAGES_total, y = NOVA2_pct)) + 
  geom_point(size = 2, color = "#2c3e50", alpha = 0.7) +
  geom_smooth(method = "lm", color = "#c0392b", fill = "#c0392b", alpha = 0.15) +
  stat_cor(aes(label = paste0("italic(R)==", round(after_stat(r), 2), "~`,`~", fmt_p(after_stat(p)))),
           method = "pearson", label.x.npc = "left", label.y.npc = "top", size = 4.5) +
  labs(
    x = "dAGEs (total)",
    y = "NOVA 2-%Kcal",
    #title = "Correlation between total dAGEs and NOVA 2-%Kcal in total population"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold")
  )

ggsave(here::here("visualization", "scatter", "dAGEs_vs_nova2_correlation in total population_new.png"),
       width = 12, height = 6, dpi = 300) 


p3<- ggplot(data, aes(x = dAGES_total, y = NOVA3_pct)) + 
  geom_point(size = 2, color = "#2c3e50", alpha = 0.7) +
  geom_smooth(method = "lm", color = "#c0392b", fill = "#c0392b", alpha = 0.15) +
  stat_cor(aes(label = paste0("italic(R)==", round(after_stat(r), 2), "~`,`~", fmt_p(after_stat(p)))),
           method = "pearson", label.x.npc = "left", label.y.npc = "top", size = 4.5) +
  labs(
    x = "dAGEs (total)",
    y = "NOVA 3-%Kcal",
    #title = "Correlation between total dAGEs and NOVA 3-%Kcal in total population"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold")
  )

ggsave(here::here("visualization", "scatter", "dAGEs_vs_nova3_correlation in total population_new.png"),
       width = 12, height = 6, dpi = 300) 



p4 <- ggplot(data, aes(x = dAGES_total, y = NOVA4_pct)) + 
  geom_point(size = 2, color = "#2c3e50", alpha = 0.7) +
  geom_smooth(method = "lm", color = "#c0392b", fill = "#c0392b", alpha = 0.15) +
  stat_cor(aes(label = paste0("italic(R)==", round(after_stat(r), 2), "~`,`~", fmt_p(after_stat(p)))),
           method = "pearson", label.x.npc = "left", label.y.npc = "top", size = 4.5) +
  labs(
    x = "dAGEs (total)",
    y = "NOVA 4-%Kcal",
    #title = "Correlation between total dAGEs and NOVA 4-%Kcal in total population"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold")
  )

ggsave(here::here("visualization", "scatter", "dAGEs_vs_nova4_correlation in total population_new.png"),
       width = 12, height = 6, dpi = 300) 



# 1. Combine the four plots into a 2x2 grid
combined_plot <- (p1 | p2) / (p3 | p4) +
  plot_annotation(
    title = "Dietary AGEs vs NOVA 1-4% correlations in the total population",
    theme = theme(
      plot.title = element_text(
        size = 18,
        face = "bold",
        hjust = 0.5
      )
    )
  )
# 2. Save the combined plot
ggsave(
  filename = here::here("visualization", "scatter", "dAGEs_vs_nova_correlation in total population_new.png"),
  plot = combined_plot,
  width = 12,
  height = 6,
  dpi = 300
)





