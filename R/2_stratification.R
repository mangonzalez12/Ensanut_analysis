library(pheatmap)
library(ggplot2)
library(cluster)

#Read data
data = read.csv("../Output/clean_data_for_clustering.csv", header = TRUE, row.names = 1)

#Data
df = data


#Identify 3 subgroups of patients 
ht <- pheatmap::pheatmap(
  df,
  scale                    = "column",
  cluster_rows             = TRUE,
  cluster_cols             = TRUE,
  clustering_method        = "ward.D2",
  clustering_distance_cols = "euclidean",
  clustering_distance_rows = "euclidean",
  border_color             = "grey60",
  show_rownames            = FALSE,
  show_colnames            = TRUE,
  cutree_rows              = 3,
  fontsize                 = 11,
  fontsize_col             = 10,
  treeheight_row           = 30,
  treeheight_col           = 30,
  color                    = colorRampPalette(rev(RColorBrewer::brewer.pal(9, "RdBu")))(100),
  breaks                   = seq(-3, 3, length.out = 101)  # ← clamp to ±3 SD
)
# Extract cluster labels from the pheatmap object
cluster_labels <- cutree(ht$tree_row, k = 3)

# Add to your original data
data$pheatmap_cluster <- paste0("Group_", cluster_labels[rownames(data)])

# Check distribution
table(data$pheatmap_cluster)

# Write file to combine 
data2 = read.csv("../Output/clean_data_63 variables.csv", header = TRUE, row.names = 1)
data2$Groups <- data$pheatmap_cluster
write.csv(data2, "../Output/clean_data_for statistical analysis.csv")
