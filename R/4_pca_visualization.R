
library(ggplot2)
library(tidyr)
library(ggrepel)
library(corrplot)
library(plotly)

data <- read.csv("../Output/clean_data_for_clustering.csv", row.names = 1, header = TRUE)
labels <- read.csv("../Output/clean_data_for statistical analysis.csv", row.names = 1, header = TRUE)


# Run PCA
pca_result <- prcomp(df, center = TRUE)

# Summary
summary(pca_result)

# Loadings (rotation matrix)
pca_result$rotation

# Scree plot
plot(pca_result, type = "l", main = "Scree Plot")

# Biplot (scores + loadings)
biplot(pca_result, scale = 0)

# --- Loadings plot per component ---
loadings <- as.data.frame(pca_result$rotation)
loadings$Variable <- rownames(loadings)

# PC1 loadings
barplot(loadings$PC1[order(loadings$PC1)],
        names.arg = loadings$Variable[order(loadings$PC1)],
        las = 2, main = "PC1 Loadings", col = "steelblue",
        horiz = TRUE)

# PC2 loadings
barplot(loadings$PC2[order(loadings$PC2)],
        names.arg = loadings$Variable[order(loadings$PC2)],
        las = 2, main = "PC2 Loadings", col = "tomato",
        horiz = TRUE)

loadings_long <- pivot_longer(loadings, -Variable,
                               names_to = "PC", values_to = "Loading")

# Filter to first 2 (or more) PCs
loadings_long <- subset(loadings_long, PC %in% c("PC1", "PC2"))

p <-  ggplot(loadings_long, aes(x = reorder(Variable, Loading), y = Loading, fill = Loading > 0)) +
  geom_col() +
  facet_wrap(~PC) +
  coord_flip() +
  scale_fill_manual(values = c("tomato", "steelblue"), guide = "none") +
  labs(x = NULL, y = "Loading", title = "PCA Loadings") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 11),   # tick labels (variable names)
    axis.title = element_text(size = 13),
    strip.text = element_text(size = 12)   # facet ("PC1", "PC2"...) labels
  )
  
ggsave("../Visualizacion/pca/pca_loadingsPC1-PC2.png", plot = p, width = 14, height = 10, dpi = 300)



# Extract scores and loadings
scores   <- as.data.frame(pca_result$x)
loadings <- as.data.frame(pca_result$rotation)
loadings$Variable <- rownames(loadings)

# Scale factor to make arrows visible alongside scores
scale_factor <- max(abs(scores[, c("PC1","PC2")])) / 
                max(abs(loadings[, c("PC1","PC2")])) * 0.7

# Variance explained for axis labels
var_exp <- summary(pca_result)$importance[2, ] * 100

ggplot() +
  # Scores (observations)
  geom_point(data = scores, aes(x = PC1, y = PC2),
             color = "steelblue", alpha = 0.6, size = 2) +
  # Loading arrows
  geom_segment(data = loadings,
               aes(x = 0, y = 0,
                   xend = PC1 * scale_factor,
                   yend = PC2 * scale_factor),
               arrow = arrow(length = unit(0.3, "cm")),
               color = "tomato", linewidth = 0.7) +
  # Loading labels
  geom_text_repel(data = loadings,
                  aes(x = PC1 * scale_factor,
                      y = PC2 * scale_factor,
                      label = Variable),
                  color = "tomato", size = 3.5) +
  # Axes
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
  labs(
    title = "PCA Biplot",
    x = paste0("PC1 (", round(var_exp[1], 1), "%)"),
    y = paste0("PC2 (", round(var_exp[2], 1), "%)")
  ) +
  theme_minimal(base_size = 8)



#rownames(scores)==rownames(labels)




# Create a display-label column, only remapping the NOVA_pct vars
nova_labels <- c(
  NOVA1_pct = "NOVA 1-%Kcal",
  NOVA2_pct = "NOVA 2-%Kcal",
  NOVA3_pct = "NOVA 3-%Kcal",
  NOVA4_pct = "NOVA 4-%Kcal"
)

loadings$Variable_label <- ifelse(
  loadings$Variable %in% names(nova_labels),
  nova_labels[loadings$Variable],
  loadings$Variable
)

#Añadir la columna de labels
pca_df <- cbind(scores, labels)
#pca_df$DIABETES <- as.character(pca_df$DIABETES)
#pca_df$PREDIABETES <- as.character(pca_df$PREDIABETES)




p <- ggplot() +
  geom_point(data = pca_df, aes(x = PC1, y = PC2, color = Age_catg),
             alpha = 0.8, size = 2) +
  geom_segment(data = loadings,
               aes(x = 0, y = 0,
                   xend = PC1 * scale_factor,
                   yend = PC2 * scale_factor),
               arrow = arrow(length = unit(0.3, "cm")),
               color = "black", linewidth = 0.7) +
  geom_text_repel(data = loadings,
                  aes(x = PC1 * scale_factor,
                      y = PC2 * scale_factor,
                      label = Variable_label),          # <- changed
                  color = "black", size = 3.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
  labs(title = "PCA Biplot",
       x = paste0("PC1 (", round(var_exp[1], 1), "%)"),
       y = paste0("PC2 (", round(var_exp[2], 1), "%)")) +
  theme_minimal(base_size = 16)

ggsave("../Visualizacion/pca/pca_biplot_Age_Quartile_2.png", plot = p, width = 14, height = 10, dpi = 300)





head(pca_df)


p <- ggplot() +
  geom_point(data = pca_df, aes(x = PC1, y = PC2, color = AGES),
             alpha = 0.8, size = 2) +
  geom_segment(data = loadings,
               aes(x = 0, y = 0,
                   xend = PC1 * scale_factor,
                   yend = PC2 * scale_factor),
               arrow = arrow(length = unit(0.3, "cm")),
               color = "black", linewidth = 0.7) +
  geom_text_repel(data = loadings,
                  aes(x = PC1 * scale_factor,
                      y = PC2 * scale_factor,
                      label = Variable),
                  color = "black", size = 3.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
  scale_color_manual(values = ann_colors$AGES, name = "AGES", breaks = c("High", "Moderate", "Low")) +   # ← added here
  labs(title = "PCA Biplot",
       x = paste0("PC1 (", round(var_exp[1], 1), "%)"),
       y = paste0("PC2 (", round(var_exp[2], 1), "%)")) +
  theme_minimal(base_size = 16)                    # bumped up from 13

ggsave("../Visualizacion/pca/pca_biplot_AGES_FINAL.png", plot = p, width = 14, height = 10, dpi = 300)






plot_ly(
  data = pca_df,
  x = ~PC1,
  y = ~PC2,
  z = ~PC3,
  color = ~Diagnosis,
  symbol = ~AGES,
  symbols = c("circle", "square", "diamond"),
  type = "scatter3d",
  mode = "markers",
  marker = list(size = 2)
) %>%
  plotly::layout(
    legend = list(
      itemsizing = "constant",
      itemwidth = 30
    )
  )
