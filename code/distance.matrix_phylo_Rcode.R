
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

###load packages
library(phyloseq)
library(phangorn)
library(muscle)
library(Biostrings)
library(magrittr)
library(betapart)
library(metagMisc)
library(ggplot2)
library(ggthemes)
library(ggpubr)
library(qiime2R)
library(vegan)
library(plyr)
library(lme4)
library(tidyverse)
library(openssl)
library(plotly)
library(grid)
library(microbiome)
library(BiocManager)
library(pairwiseAdonis)
library(ggrepel)
library(factoextra)
library(metagenomeSeq)
library(ape)
library(Biostrings)
library(msa)
library(phangorn)



# Importing metadata 
metadata <- read.table("data/Metadata_16S_V4.txt", header = TRUE, sep = "\t", row.names = 1)
metadata <- sample_data(metadata)

# Importing taxonomy 
taxonomy <- read.table("data/taxa_all.txt", header = TRUE, sep = "\t", row.names = "SV")
taxonomy <- as.matrix(taxonomy)  # Transpose the matrix if needed
taxonomy <- tax_table(taxonomy)

# Importing OTU table 
otu_table <- read.table("data/Subset1-All_studies_merged_16S_table-FINAL-V4-MiSeq-filtered.txt", header = TRUE, sep = "\t", row.names="SV")
otu_table <- otu_table (otu_table, taxa_are_rows = TRUE)



# Create a phyloseq object
seed_physeq <- merge_phyloseq(otu_table, taxonomy, metadata)

# Check the summary of your phyloseq object
summary(seed_physeq)


#data normalization
seed_16s_css <-phyloseq_transform_css(seed_physeq, norm = TRUE, log = TRUE)

#hierarchical clustering call, specifying average-linkage clustering
Species <- merge_samples(seed_16s_css, "Plant_species", fun = mean)
d.seed_16s_css <- phyloseq::distance(Species, method="bray", type="samples")
hell.hclust.16s.Species   <- hclust(d.seed_16s_css, method="average")
fviz_dend(hell.hclust.16s.Species,type = "rectangle",
          sub = "",
          main = "Fig.1",
          ylab = "",
          show_labels = TRUE,
          cex = 0.5, 
          repel = TRUE,
          lwd= 0.5,
          color_labels_by_k = FALSE,
          horiz = TRUE, 
          rect_fill = FALSE)
ggsave("microbiome.jpg", width = 10, height = 6, dpi = 300)



#Metastudy phylogeny
file <- "data/Plant_seqs_arranged.fasta"
dna <- read.dna(file, format="fasta") 
mySeqs <- readDNAStringSet(file)
mySeqs_df = as.data.frame(mySeqs)
tab = as.data.frame(Species@otu_table@.Data)
data = merge(mySeqs_df, tab, by = "row.names", all = FALSE)
data_sequence = mySeqs_df
data_sequence$species = row.names(data_sequence)
#data_test = data[,c("x", 'cc6f72d10b4b0bfb1860f98964429752')]
colnames(data_sequence)[1] = c("sequence")
write.table(data_sequence, "data_sequence.txt", row.names = FALSE)

# Align sequences with muscle
align_msa <- muscle(mySeqs) 
align <- as.phyDat(align_msa)
align_df <- as.data.frame(do.call("rbind", align))
align_df$species = row.names(align_df)
write.table(align_df, "data_aligned.txt", row.names = FALSE, sep = ";")

target = as.data.frame(t(seed_16s_css@otu_table@.Data))
target$grouping = seed_16s_css@sam_data$Plant_species
target_grouped = target %>%
  group_by(grouping) %>%
  dplyr::summarise(across(everything(), mean))
target_grouped = as.data.frame(target_grouped)
row.names(target_grouped) = target_grouped$grouping
target_grouped = target_grouped[, -1]
data_align = merge(align_df, target_grouped[,'497a26126c9e726c768bdec79e8c4e9b', drop = FALSE], by = "row.names", all = FALSE)
data_align = data_align[,-1]
write.table(data_align, "data_ml_test.txt", row.names = FALSE)


#Calculate a distance matrix of species alignment
Malus.ITS.dm <- dist.hamming(align) 

#write.csv(as.matrix(Malus.ITS.dm), file = "50_seed_dm.csv") # write distance matrix for phylogeny
#write.csv(as.matrix(d.seed_16s_css), file = "50_seed_css_dm.csv") # write distance matrix for microbiome 

table(Malus.ITS.dm)
#Hierarchical clustering with average algorithm of species distances
Malus.ITS.hclust  <- hclust(Malus.ITS.dm, method="average")
#plot  Hierarchical clustering in a dendrogram 
fviz_dend(Malus.ITS.hclust,type = "rectangle",
          sub = "",
          main = "Fig.1",
          ylab = "",
          show_labels = TRUE,
          cex = 0.5, 
          repel = TRUE,
          lwd= 0.5,
          color_labels_by_k = FALSE,
          horiz = TRUE, 
          rect_fill = FALSE)

ggsave("phylogeny_tree.jpg", width = 10, height = 6, dpi = 300)

















