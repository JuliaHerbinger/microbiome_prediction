# Ensure required package managers are installed
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")

# Standard CRAN packages
cran_packages <- c(
  "adespatial", "ape", "betapart", "data.table", "devtools", "dplyr", "factoextra", "ggdendro", "ggplot2",
  "ggpubr", "ggrepel", "ggthemes", "gridExtra", "kableExtra", "kernlab", "knitr", "lme4", "magrittr",
  "mlr3", "mlr3extralearners", "mlr3learners", "mlr3tuning", "openssl", "pander", "paradox", "patchwork",
  "phangorn", "philentropy", "phyloseq", "plotly", "plyr", "png", "R6", "RColorBrewer", "tidyr",
  "tidyverse", "vegan"
)

# Bioconductor packages
bioc_packages <- c(
  "BiocManager", "Biostrings", "DECIPHER", "DESeq2", "GO.db", "impute", "microbiome", "msa", "muscle",
  "metagenomeSeq", "preprocessCore"
)

# GitHub packages (in the format list("package" = "repo"))
github_packages <- list(
  "ranacapa" = "gauravsk/ranacapa",
  "qiime2R" = "jbisanz/qiime2R",
  "MicrobeR" = "jbisanz/MicrobeR",
  "microbiomeutilities" = "microsud/microbiomeutilities",
  "metagMisc" = "vmikk/metagMisc",
  "pairwiseAdonis" = "pmartinezarbizu/pairwiseAdonis/pairwiseAdonis"
)

# Install missing CRAN packages
install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}
invisible(sapply(unique(cran_packages), install_if_missing))

# Install missing Bioconductor packages
install_bioc_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    BiocManager::install(pkg, ask = FALSE)
  }
}
invisible(sapply(unique(bioc_packages), install_bioc_if_missing))

# Install missing GitHub packages
install_github_if_missing <- function(pkg, repo) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    remotes::install_github(repo)
  }
}
invisible(mapply(install_github_if_missing, names(github_packages), github_packages))

# Combine all packages to load
all_packages <- unique(c(cran_packages, bioc_packages, names(github_packages)))

# Load all packages
invisible(lapply(all_packages, function(pkg) {
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}))
