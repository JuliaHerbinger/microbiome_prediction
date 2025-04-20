
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



# prepare targets

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
target = as.data.frame(t(seed_16s_css@otu_table@.Data))
target$grouping = seed_16s_css@sam_data$Plant_species

# Preprocess feature space
file <- "data/Plant_seqs_arranged.fasta"
dna <- read.dna(file, format="fasta") 
mySeqs <- readDNAStringSet(file)
mySeqs_df = as.data.frame(mySeqs)
tab = as.data.frame(Species@otu_table@.Data)
data = merge(mySeqs_df, tab, by = "row.names", all = FALSE)
data_sequence = mySeqs_df
data_sequence$species = row.names(data_sequence)
colnames(data_sequence)[1] = c("sequence")

# Align sequences with muscle
align_msa <- muscle(mySeqs) 
align <- as.phyDat(align_msa)
align_df <- as.data.frame(do.call("rbind", align))
X = align_df
var_strat = row.names(align_df)
align_df$species = row.names(align_df)


target = target[which(target$grouping %in% unique(align_df$species)),]
target_grouped = target %>%
  group_by(grouping) %>%
  dplyr::summarise(across(everything(), mean))
target_grouped = as.data.frame(target_grouped)
row.names(target_grouped) = target_grouped$grouping

# filter targets

# 1. filter by zero abundance
# target_filter1 = lapply(target[,-ncol(target)],
#        function(x){
#          temp = lapply(unique(target$grouping),
#                 function(specie){
#                   target_temp = x[which(target$grouping==specie)]
#                   if(quantile(target_temp, 1) == 0){
#                     return(0)
#                   }
#                   else{
#                     return(1)
#                   }
#                 })
#          if(all(unlist(temp) == 0)){
#            return(0)
#          }
#          else{
#            return(1)
#          }
#        })

#target_filtered = target[,which(unlist(target_filter1) == TRUE)]
# 1. Filter step nicht notwendig -- variation auf 0.9 erhöht, später dann für 
# verschiedene Werte filtern und averagen.
target_filtered = target[, -ncol(target)]

# 2. filter according to coefficient of variation 
target_filter2 = lapply(target_filtered, 
                        function(x){
                          temp = lapply(unique(target$grouping),
                                        function(specie){
                                          #browser()
                                          target_temp = x[which(target$grouping==specie)]
                                          if(mean(target_temp, na.rm = TRUE) == 0){
                                            return(0)
                                          }
                                          else if(is.na(sd(target_temp, na.rm = TRUE))){
                                            return(0)
                                          }
                                          else if((sd(target_temp, na.rm = TRUE) / mean(target_temp, na.rm = TRUE)) < 0.5){
                                            return(1)
                                          }
                                          else{
                                            return(0)
                                          }  
                                        })
                          if(any(unlist(temp) == 1)){
                            return(1)
                          }
                          else{
                            return(0)
                          }
                        })


target_filtered = target_filtered[,which(unlist(target_filter2) == TRUE)]
target_grouped_final = target_grouped[,colnames(target_filtered)]



# Final input and output space
Y = target_grouped_final
X = X[order(row.names(X)),]


