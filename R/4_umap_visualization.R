library(umap)
library(plotly)
library(psych)
library(VIM)
library(tidyverse)
library(dplyr)

data <- read.csv("../Output/clean_data_for_clustering.csv", row.names = 1, header = TRUE)
labels <- read.csv("../Output/clean_data_for statistical analysis.csv", row.names = 1, header = TRUE)

#Remove last column
data = data[, -ncol(data), drop = FALSE]

# Scaled data previously
data

# UMAP
umap_config <- umap.defaults
umap_config$n_components <- 3
umap_result <- umap(data, config = umap_config)
umap_df <- as.data.frame(umap_result$layout)
colnames(umap_df) <- c("UMAP1", "UMAP2", "UMAP3")
rownames(umap_df) <- rownames(data)


#Check rownames order
rownames(umap_df)==rownames(labels)


#Añadir labels para mapear
umap_df <- cbind(umap_df, labels)
head(umap_df)

names(umap_df)


plot_ly(
  data = umap_df,
  x = ~UMAP1,
  y = ~UMAP2,
  z = ~UMAP3,
  color = ~Groups,
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


plot_ly(
  data = umap_df,
  x = ~UMAP1,
  y = ~UMAP2,
  z = ~UMAP3,
  color = ~AGES,  #
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