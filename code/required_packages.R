# INSTALL PACKAGES

# for data preparation
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install(c("DECIPHER","DESeq2", "philr", "phyloseq"))
BiocManager::install("preprocessCore")
BiocManager::install("GO.db")
BiocManager::install("impute")
install.packages(c("adespatial","ape","devtools","ggdendro","gridExtra","knitr","MicrobeR","pander","plotly","png","tidyverse","vegan"))
library(devtools)
devtools::install_github("gauravsk/ranacapa")
devtools::install_github("umerijaz/microbiomeSeq") 
install.packages("remotes")
remotes::install_github("jbisanz/qiime2R")
remotes::install_github("jbisanz/MicrobeR")
remotes::install_github("microsud/microbiomeutilities")
BiocManager::install("microbiome")
install.packages(c("ggpubr", "RColorBrewer"))
remotes::install_github("vmikk/metagMisc")
BiocManager::install("muscle")
install.packages("betapart")
install.packages("ggthemes")
install.packages("factoextra")
BiocManager::install("msa")
BiocManager::install("metagenomeSeq")

library("devtools")
install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")


# for modeling

install.packages("mlr3verse")
install.packages("mlr3learners")
install.packages("mlr3extralearners")
install.packages("R6")
install.packages("mlr3")
install.packages("mlr3tuning")
install.packages("paradox")
install.packages("data.table")


# for plotting
install.packages("ggplot2")



