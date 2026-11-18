library(dplyr)
library(Rtsne)
library(ggplot2)
library(plotly)


#Read data
data <- read.csv("../Output/clean_data_for_clustering.csv", row.names = 1, header = TRUE)
labels <- read.csv("../Output/clean_data_55 variables and 3 clusters.csv", row.names = 1, header = TRUE)

#Fit Rtsne
set.seed(42)
tsne_results <- Rtsne::Rtsne(data,
                             dims=3,
                             perplexity=25,
                             verbose=TRUE,
                             max_iter=1500)
#Organize in a dataframe
tsne_df <- data.frame(tsne_results$Y)
rownames(tsne_df) <- rownames(data) #Set rownames
colnames(tsne_df) <- c("TSNE1", "TSNE2", "TSNE3")

#Match with labels
all(rownames(tsne_df)==rownames(labels))
tsne_df <- cbind(tsne_df, labels)
head(tsne_df)


#Add metadata to each datapoint
ann_colors <- list(
  Groups            = c(Group_1              = "#FADDC3",
                        Group_2              = "#8E063B",
                        Group_3              = "#023FA5"),
  
  Stratum           = c(Metropolitan       = "#0D652C",
                        Rural                = "#BEA037",
                        Urban                = "#068FAB",
                        No_data              = "#DCD4D0"),
  
  Region            = c(Center             = "#E4E5E6",
                        North                = "#835581",
                        South                = "#AC4522"),
  
  Sex               = c(Male               = "#023FA5",
                        Female               = "#FF80FF"),
  
  Diagnosis         = c(Normal             = "#290AD8",
                        Metabolic_Syndrome   = "#72D9FF",
                        Pre_Diabetes         = "#FFE099",
                        Diabetes             = "#A50021"),
  
  PhysicalActivity  = c(Low                = "#33E4FF",
                        Moderate            = "#00AACC",
                        High                = "#007A99",
                        No_data             = "#DCD4D0"),
  
  Age_catg          = c(Quartile1          = "#FFCA99",
                        Quartile2          = "#FFAD65",
                        Quartile3          = "#FF8E33",
                        Quartile4          = "#993D00")
)


# ── Pull colors directly from ann_colors ──────────────────────────────────────
age_colors  <- ann_colors$Age_catg        # Quartile1–4
grp_colors  <- ann_colors$Groups          # Group_1–3
sex_colors  <- ann_colors$Sex             # Male/Female
diag_colors <- ann_colors$Diagnosis       # Normal/MetS/PreDM/DM
strat_colors<- ann_colors$Stratum         # Metropolitan/Rural/Urban/No_data
reg_colors  <- ann_colors$Region          # Center/North/South
pa_colors   <- ann_colors$PhysicalActivity# Low/Moderate/High/No_data

# ── Choose which variable to color by (swap as needed) ───────────────────────
color_var    <- "PhysicalActivity"   # ← change to "Groups", "Sex", "Diagnosis", etc.
color_values <- pa_colors   # ← match to the variable above

p_tsne <- ggplot(tsne_df, aes(x = TSNE1, y = TSNE2,
                              color = .data[[color_var]])) +
  geom_point(alpha = 0.75, size = 1.8, stroke = 0.1) +
  
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "grey80", linewidth = 0.4) +
  geom_vline(xintercept = 0, linetype = "dashed",
             color = "grey80", linewidth = 0.4) +
  
  scale_color_manual(
    values = color_values,
    name   = gsub("_", " ", color_var)   # clean legend title
  ) +
  
  labs(
    title    = "t-SNE projection",
    subtitle = paste0("Colored by ", gsub("_", " ", color_var)),
    x        = "t-SNE 1",
    y        = "t-SNE 2"
  ) +
  
  theme_classic(base_size = 12) +
  theme(
    plot.title        = element_text(face = "bold", size = 13, hjust = 0),
    plot.subtitle     = element_text(size = 10, color = "grey40", hjust = 0),
    axis.title        = element_text(size = 11, face = "bold"),
    axis.text         = element_text(size = 9,  color = "grey30"),
    axis.line         = element_line(color = "grey30", linewidth = 0.5),
    axis.ticks        = element_line(color = "grey30", linewidth = 0.4),
    legend.title      = element_text(size = 10, face = "bold"),
    legend.text       = element_text(size = 9),
    legend.key.size   = unit(0.5, "lines"),
    legend.background = element_blank(),
    legend.position   = "right",
    panel.grid        = element_blank(),
    plot.background   = element_rect(fill = "white", color = NA),
    plot.margin       = margin(10, 15, 10, 10)
  ) +
  
  guides(color = guide_legend(override.aes = list(size = 3, alpha = 1)))

ggsave("../Visualizacion/tsne/TSNE_PhysicalActivity.png",
       plot   = p_tsne,
       width  = 180,
       height = 140,
       units  = "mm",
       dpi    = 300)





#Choose variable to plot
plot_ly(
  data = tsne_df,
  x = ~TSNE1,
  y = ~TSNE2,
  z = ~TSNE3,
  color = ~Groups, #CHOOSE VARIABLE
  type = "scatter3d",
  mode = "markers",
  marker = list(size = 2)
) %>%
  layout(
    legend = list(
      itemsizing = "constant",
      itemwidth = 30
    )
  )
