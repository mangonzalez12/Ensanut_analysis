library(pheatmap)
library(ggplot2)
library(cluster)

#Read data
data <- read.csv(here::here("data", "Output", "clean_data_for_clustering.csv"), row.names = 1,
                 header = TRUE)
labels <- read.csv(here::here("data", "Output", "clean_data_for_statistical_analysis.csv"),
                   header = TRUE, row.names = 1)


labels_group1 <- labels[labels$Groups == "Group_1", , drop = FALSE]
labels_group2 <- labels[labels$Groups == "Group_2", , drop = FALSE]
labels_group3 <- labels[labels$Groups == "Group_3", , drop = FALSE]

table(labels_group1$Sex)
table(labels_group2$Sex)
table(labels_group3$Sex)

table(labels_group1$Diagnosis)
table(labels_group2$Diagnosis)
table(labels_group3$Diagnosis)


#Filter labels
keep <- c("Groups", "Stratum", "Region", "Sex","AGES")#, "Diagnosis")
meta <- subset(labels, select = keep)


#data to df
df = data



ann_colors <- list(
  Groups            = c(Group_1                 = "#FADDC3",
                        Group_2                 = "#8E063B",
                        Group_3                 = "#023FA5"),
  
  Stratum           = c(Metropolitan            = "#0D652C",
                        Rural                   = "#BEA037",
                        Urban                   = "#068FAB",
                        No_data                 = "#DCD4D0"),
  
  Region            = c(Center                  = "#E4E5E6",
                        North                   = "#835581",
                        South                   = "#AC4522"),
  
  Sex               = c(Male                    = "#023FA5",
                        Female                  = "#FF80FF"),
  
  #Diagnosis         = c(Normal                 = "#290AD8",
  #                      Metabolic_Syndrome     = "#72D9FF",
  #                      Pre_Diabetes           = "#FFE099",
  #                      Diabetes               = "#A50021"),
  
  #Diagnosis          = c(Metabolic_Syndromelow  = "#72D9FF",
  #                       Metabolic_Syndromehigh = "#290AD8",
  #                       Diabetes               = "#A50021"),
  
  AGES              = c(Low                     = "#FFDFCE",
                        Moderate                = "#F37B3B",
                        High                    = "#7B3014")
)


# Ensure all annotation variables are factors with levels that exactly match ann_colors
meta$AGES <- factor(meta$AGES, levels = names(ann_colors$AGES))
#meta$Diagnosis   <- factor(meta$Diagnosis,   levels = names(ann_colors$Diagnosis))
meta$Sex         <- factor(meta$Sex,         levels = names(ann_colors$Sex))
meta$Region      <- factor(meta$Region,      levels = names(ann_colors$Region))
meta$Stratum     <- factor(meta$Stratum,     levels = names(ann_colors$Stratum))
meta$Groups      <- factor(meta$Groups,      levels = names(ann_colors$Groups))

nova_labels <- c(
  NOVA1_pct = "NOVA 1-%Kcal",
  NOVA2_pct = "NOVA 2-%Kcal",
  NOVA3_pct = "NOVA 3-%Kcal",
  NOVA4_pct = "NOVA 4-%Kcal"
)
relabel_vars <- function(x, lookup = nova_labels) {
  ifelse(x %in% names(lookup), lookup[x], x)
}

#Identify 3 subgroups of patients 
ht <- pheatmap::pheatmap(
  df,
  scale                    = "column",
  cluster_rows             = TRUE,
  cluster_cols             = TRUE,
  main                     = "Patient Stratification into three clusters using Hierarchical Clustering",
  clustering_method        = "ward.D2",
  clustering_distance_cols = "euclidean",
  clustering_distance_rows = "euclidean",
  annotation_row           = meta, 
  annotation_colors        = ann_colors,
  annotation_names_row     = TRUE, 
  border_color             = "grey60",
  show_rownames            = FALSE,
  show_colnames            = TRUE,
  cutree_rows              = 3,
  labels_col               = relabel_vars(colnames(df)),
  labels_row               = relabel_vars(rownames(df)),
  fontsize                 = 11,
  fontsize_col             = 10,
  treeheight_row           = 30,
  treeheight_col           = 30,
  color                    = colorRampPalette(rev(RColorBrewer::brewer.pal(9, "RdBu")))(100),
  breaks                   = seq(-3, 3, length.out = 101),
  silent                   = TRUE
)

ggsave(filename = here::here("visualization", "heatmap", "Heatmap_3_clusters_small.png"),
       plot = ht$gtable, width = 20, height = 12, dpi = 400)


# Heatmap — Figure 2
ht <- pheatmap::pheatmap(
  df,
  scale                    = "column",
  cluster_rows             = TRUE,
  cluster_cols             = TRUE,
  main                     = "Patient Stratification into three clusters using Hierarchical Clustering",
  clustering_method        = "ward.D2",
  clustering_distance_cols = "euclidean",
  clustering_distance_rows = "euclidean",
  annotation_row           = meta, 
  annotation_colors        = ann_colors,
  annotation_names_row     = TRUE, 
  border_color             = "grey60",
  show_rownames            = FALSE,
  show_colnames            = TRUE,
  cutree_rows              = 3,
  labels_col               = relabel_vars(colnames(df)),
  labels_row               = relabel_vars(rownames(df)),
  fontsize                 = 11,
  fontsize_col             = 10,
  treeheight_row           = 30,
  treeheight_col           = 30,
  color                    = colorRampPalette(rev(RColorBrewer::brewer.pal(9, "RdBu")))(100),
  breaks                   = seq(-3, 3, length.out = 101)  # ← clamp to ±3 SD
)

# Save with enough height to fit all 6 legends
dir.create(here::here("visualizacion", "heatmap"), recursive = TRUE, showWarnings = FALSE)

png(here::here("visualization", "heatmap", "heatmap_figura2.png"),
    width = 20, height = 10, units = "in", res = 300)
grid::grid.newpage()
grid::grid.draw(ht$gtable)
dev.off()





# Save as PDF (vector — publication quality)
dir.create(here::here("visualization", "heatmap"), recursive = TRUE, showWarnings = FALSE)

pdf(here::here("visualization", "heatmap", "Figure2_Heatmap_3_clusters.pdf"),
    width = 22, height = 12)
grid::grid.newpage()
grid::grid.draw(ht$gtable)
dev.off()


# Write file to use later in UMAP/PCA visualization
write.csv(data,
          here::here("data", "Output", "p_heatmap3_clusters_clean_data_for_ML.csv"),
          row.names = TRUE)

sil <- silhouette(cluster_labels, dist(df, method = "euclidean"))
cat("Average silhouette:", mean(sil[, 3]), "\n")






####

library(NbClust)

res<-NbClust(df, distance = "euclidean", min.nc=2, max.nc=8, 
             method = "complete", index = "ch")

res$All.index
res$Best.nc
res$Best.partition

table(res$Best.partition)

print("Best partition is 6 clusters")
print("This is the silhouette score for 6 clusters")
cluster_labels_6 = res$Best.partition
sil <- silhouette(cluster_labels_6, dist(df, method = "euclidean"))
cat("Average silhouette:", mean(sil[, 3]), "\n")



#




