library(ggplot2)
library(corrplot)
library(grDevices)




df <- read.csv("../Output/clean_data_55 variables and 3 clusters.csv", header = TRUE, row.names = 1)

#Set variable to numeric
df$BMI <- as.numeric(gsub(",", ".", df$BMI))# IMC.Prom.mmHg
df$Glucose <- as.numeric(gsub(",", ".", df$Glucose))# GLUCOSA.mg.dL
df$HDL<- as.numeric(gsub(",", ".", df$HDL)) #HDL.mg.dL
df$LDL <- as.numeric(gsub(",", ".", df$LDL)) #LDL.mg.dL
df$TotalCholesterol <- as.numeric(gsub(",", ".", df$TotalCholesterol)) #COLESTEROL.TOTAL.mg.dL
df$Triglycerides <- as.numeric(gsub(",", ".", df$Triglycerides)) #TRIGLICÉRIDOS.mg.dL

df_dbl <- df[sapply(df, is.double)]


plot_histograms_gg <- function(df, output_dir = "histograms", bins = 30,
                               fill = "steelblue", color = "white", dpi = 150) {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  numeric_cols <- names(df)[sapply(df, is.numeric)]
  
  if (length(numeric_cols) == 0) {
    message("No numeric columns found in dataframe.")
    return(invisible(NULL))
  }
  
  for (col in numeric_cols) {
    p <- ggplot(df, aes(x = .data[[col]])) +
      geom_histogram(bins = bins, fill = fill, color = color, na.rm = TRUE) +
      labs(title = paste("Histogram of", col), x = col, y = "Frequency") +
      theme_classic(base_size = 13)
    
    filepath <- file.path(output_dir, paste0("hist_", col, ".png"))
    ggsave(filepath, plot = p, width = 7, height = 5, dpi = dpi)
    message("Saved: ", filepath)
  }
  
  message("Done. ", length(numeric_cols), " histogram(s) saved to '", output_dir, "'.")
}



# Save plots
plot_histograms_gg(df_dbl, output_dir = "../Visualizacion/histogramas", bins = 100, dpi = 300)




cor_matrix <- cor(df_dbl, use = "pairwise.complete.obs")

corrplot(cor_matrix,
         method      = "color",
         type        = "upper",
         tl.col      = "black",
         tl.cex      = 0.75,
         tl.srt      = 45,
         addCoef.col = "black",
         number.cex  = 0.6,
         col         = COL2("RdBu", 200),
         diag        = FALSE,
         cl.cex      = 0.75,
         mar         = c(0, 0, 1, 0))

dev.off()

tiff("../Visualizacion/corrplot_all_numeric.tiff", width = 10, height = 9,
     units = "in", res = 600)



final_selected_vars <- c("BMI",
                         "WaistCirc",
                         "SBP",
                         "DBP",
                         "Glucose",
                         "Insulin",
                         "HbA1c",
                         "HDL",
                         "LDL",
                         "TotalCholesterol",
                         "Triglycerides",
                         "Kcal_day",
                         "Carbs_day",
                         "Lipids_day",
                         "Protein_day",
                         "dAGEs_day",
                         "NOVA1_pct",
                         "NOVA2_pct",
                         "NOVA3_pct",
                         "NOVA4_pct")

df_downstream_analysis <- subset(df_dbl, select = final_selected_vars)
df_downstream_analysis






cor_matrix2 <- cor(df_downstream_analysis, use = "pairwise.complete.obs")

corrplot(cor_matrix2,
         method      = "color",
         type        = "upper",
         tl.col      = "black",
         tl.cex      = 0.75,
         tl.srt      = 45,
         addCoef.col = "black",
         number.cex  = 0.6,
         col         = COL2("RdBu", 200),
         diag        = FALSE,
         cl.cex      = 0.75,
         mar         = c(0, 0, 1, 0))

dev.off()

tiff("../Visualizacion/corrplot_selected_variables.tiff", width = 10, height = 9,
     units = "in", res = 600)







#######.   Descriptive stats

library(dplyr)
library(tidyr)
library(moments)

describe_df <- function(df) {
  numeric_cols <- df |> select(where(is.numeric))
  cat_cols     <- df |> select(where(~ is.character(.) | is.factor(.)))
  
  # --- Numeric summary ---
  num_summary <- numeric_cols |>
    pivot_longer(everything(), names_to = "Variable", values_to = "Value") |>
    group_by(Variable) |>
    summarise(
      N         = sum(!is.na(Value)),
      Missing   = sum(is.na(Value)),
      Pct_Miss  = round(mean(is.na(Value)) * 100, 1),
      Mean      = round(mean(Value, na.rm = TRUE), 2),
      SD        = round(sd(Value, na.rm = TRUE), 2),
      CV_pct    = round(sd(Value, na.rm = TRUE) / mean(Value, na.rm = TRUE) * 100, 1),
      Median    = round(median(Value, na.rm = TRUE), 2),
      IQR       = round(IQR(Value, na.rm = TRUE), 2),
      Min       = round(min(Value, na.rm = TRUE), 2),
      Max       = round(max(Value, na.rm = TRUE), 2),
      Skewness  = round(moments::skewness(Value, na.rm = TRUE), 2),
      Kurtosis  = round(moments::kurtosis(Value, na.rm = TRUE), 2),
      .groups = "drop"
    )
  
  # --- Categorical summary ---
  cat_summary <- if (ncol(cat_cols) > 0) {
    cat_cols |>
      pivot_longer(everything(), names_to = "Variable", values_to = "Value") |>
      group_by(Variable) |>
      summarise(
        N          = sum(!is.na(Value)),
        Missing    = sum(is.na(Value)),
        Pct_Miss   = round(mean(is.na(Value)) * 100, 1),
        N_Levels   = n_distinct(Value, na.rm = TRUE),
        Most_Freq  = names(sort(table(Value), decreasing = TRUE))[1],
        Freq_Count = max(table(Value)),
        Freq_Pct   = round(max(table(Value)) / sum(!is.na(Value)) * 100, 1),
        .groups = "drop"
      )
  } else NULL
  
  # --- Dataset-level overview ---
  overview <- data.frame(
    Metric = c("Rows", "Columns", "Numeric vars", "Categorical vars",
               "Complete cases", "Pct complete cases"),
    Value  = c(
      nrow(df),
      ncol(df),
      ncol(numeric_cols),
      ncol(cat_cols),
      sum(complete.cases(df)),
      round(mean(complete.cases(df)) * 100, 1)
    )
  )
  
  list(overview    = overview,
       numeric     = num_summary,
       categorical = cat_summary)
}



stats <- describe_df(df)

stats$overview      # dataset-level snapshot
stats$numeric       # full numeric table
stats$categorical   # frequency summary for factors/characters




library(openxlsx)
library(gt)

wb <- createWorkbook()
addWorksheet(wb, "Overview");    writeData(wb, "Overview",     stats$overview)
addWorksheet(wb, "Numeric");     writeData(wb, "Numeric",      stats$numeric)
addWorksheet(wb, "Categorical"); writeData(wb, "Categorical",  stats$categorical)
saveWorkbook(wb, "../Visualizacion/descriptive_stats.xlsx", overwrite = TRUE)




# Read the sheet back.  "Numeric"
num_summary <- read.xlsx("../Visualizacion/descriptive_stats.xlsx", sheet = "Numeric")

gt_numeric <- num_summary |>
  gt() |>
  tab_header(
    title    = "Table 1. Descriptive Statistics — Numeric Variables",
    subtitle = "CV%: coefficient of variation; IQR: interquartile range"
  ) |>
  tab_spanner(
    label   = "Missing",
    id      = "sp_missing",
    columns = c(Missing, Pct_Miss)
  ) |>
  tab_spanner(
    label   = "Central Tendency",
    id      = "sp_central",
    columns = c(Mean, SD, CV_pct, Median, IQR)
  ) |>
  tab_spanner(
    label   = "Range",
    id      = "sp_range",
    columns = c(Min, Max)
  ) |>
  tab_spanner(
    label   = "Distribution",
    id      = "sp_dist",
    columns = c(Skewness, Kurtosis)
  ) |>
  cols_label(
    Pct_Miss = "Miss%",
    CV_pct   = "CV%"
  ) |>
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) |>
  tab_style(
    style     = cell_fill(color = "#f0f4f8"),
    locations = cells_column_labels()
  ) |>
  tab_style(
    style     = cell_fill(color = "#dce8f5"),
    locations = cells_column_spanners()
  ) |>
  tab_style(
    style     = cell_fill(color = "#ffe0e0"),
    locations = cells_body(columns = Pct_Miss, rows = Pct_Miss > 10)
  ) |>
  tab_style(
    style     = cell_fill(color = "#fff3cd"),
    locations = cells_body(columns = Skewness, rows = abs(Skewness) > 1)
  ) |>
  tab_options(
    table.font.size                   = 11,
    heading.title.font.size           = 13,
    heading.subtitle.font.size        = 10,
    column_labels.border.top.width    = px(2),
    column_labels.border.bottom.width = px(2),
    table_body.border.bottom.width    = px(2),
    table.border.top.color            = "black",
    table.border.top.width            = px(2)
  ) |>
  opt_row_striping()

gtsave(gt_numeric, "../Visualizacion/table_numeric_stats.png", vwidth = 1400)

