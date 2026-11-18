install.packages("renv")
renv::init() #use bioconductor = TRUE in case of bioinformatics packages
library(ggplot2)
renv::status()
renv::snapshot()
