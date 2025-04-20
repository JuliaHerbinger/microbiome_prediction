
load("results/bmr_svm_23032025.Rdata")
bmr_svm = bmr
load("results/bmr_comp_21032025.Rdata")
library(ape)
library(ggplot2)

# Figure decreasing distance

# extract results from benchmark for all learners
#data_response_gp = extract_result_data(bmr, 1, "response")
#data_truth_gp = extract_result_data(bmr, 1, "truth")
data_response_gp_tuned = extract_result_data(bmr, 2, "response")
data_truth_gp_tuned = extract_result_data(bmr, 2, "truth")
data_response_knn = extract_result_data(bmr, 1, "response")
data_truth_knn = extract_result_data(bmr, 1, "truth")
#data_response_knn = extract_result_data(bmr, 3, "response")
#data_truth_knn = extract_result_data(bmr, 3, "truth")
data_response_svm = extract_result_data(bmr, 3, "response")
data_truth_svm = extract_result_data(bmr, 3, "truth")

# calculate jsd distances between ground truth and predictions of all learners
jsd_knn = compute_jsd_distance(data_response_knn, data_truth_knn)
#jsd_gp = compute_jsd_distance(data_response_gp, data_truth_gp)
jsd_gp_tuned = compute_jsd_distance(data_response_gp_tuned, data_truth_gp_tuned)
jsd_svm = compute_jsd_distance(data_response_svm, data_truth_svm)

# combine data and prepare for plotting
data_jsd = data.frame("Specie" = row.names(X), "KNN" = jsd_knn, "GP" = jsd_gp_tuned, "SVM" = jsd_svm)
data_plot_jsd = create_data_jsd_plot(data_jsd, "KNN")

# JSD plot
p1 = ggplot(data = data_plot_jsd, aes(x = Specie, y = jsd)) + geom_line(aes(group = Model, col = Model)) + theme_bw() + ylim(0,0.7) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 5))+ ggtitle("All Species") + ylab("JSD") #+ xlab("Specie")

ggsave("jsd_by_species.png", p1, width = 7, height = 4)
print(paste("mean knn = ", mean(jsd_knn, na.rm = T)))
sd(jsd_knn, na.rm = T)
print(paste("mean gp = ", mean(jsd_gp_tuned, na.rm = T)))
sd(jsd_gp_tuned, na.rm = T)
print(paste("mean svm = ", mean(jsd_svm, na.rm = T)))
sd(jsd_svm, na.rm = T)








# Families
library(patchwork)
Brassica_matches = c("Alliaria", "Arabidopsis", "Barbarea", "Brassica", "Capsella", "Cardamine", "Erophila", "Eruca", "Raphanus", "Sinapis")
Brassica <- unique (grep(paste(Brassica_matches,collapse="|"), row.names(X), value=TRUE))

Poaceae_matches = c("Avena", "Elymus", "Festuca", "Lolium", "Oryza")
Poaceae <- unique (grep(paste(Poaceae_matches,collapse="|"), row.names(X), value=TRUE))

Fabaceae_matches = c("Medicago", "Phaseolus", "Pisum", "Vicia")
Fabaceae <- unique (grep(paste(Fabaceae_matches,collapse="|"), row.names(X), value=TRUE))


# filtered comparison plot
data_plot_jsd_fam = data_plot_jsd[data_plot_jsd$Specie %in% c(Poaceae, Brassica),]

p12 = ggplot(data = data_plot_jsd_fam, aes(x = Specie, y = jsd)) + geom_line(aes(group = Model, col = Model)) + theme_bw() + ylim(0,0.7) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 5)) + ggtitle("Poaceae and Brassica") + ylab("JSD")

p_jsd = p1 + p12 + plot_layout(guides = "collect") &
  theme(legend.position = "right")
ggsave("jsd_by_species.png", p_jsd, width = 9.5, height = 4)
print(paste("mean knn = ", mean(data_plot_jsd_fam[data_plot_jsd_fam$Model == "KNN", "jsd"], na.rm = T)))
sd(data_plot_jsd_fam[data_plot_jsd_fam$Model == "KNN", "jsd"], na.rm = T)
print(paste("mean gp = ", mean(data_plot_jsd_fam[data_plot_jsd_fam$Model == "GP", "jsd"], na.rm = T)))
sd(data_plot_jsd_fam[data_plot_jsd_fam$Model == "GP", "jsd"], na.rm = T)
print(paste("mean svm = ", mean(data_plot_jsd_fam[data_plot_jsd_fam$Model == "SVM", "jsd"], na.rm = T)))
sd(data_plot_jsd_fam[data_plot_jsd_fam$Model == "SVM", "jsd"], na.rm = T)


# only species not in Brassica and Poaceae
data_plot_jsd_rem = data_plot_jsd[!(data_plot_jsd$Specie %in% c(Poaceae, Brassica)),]
print(paste("mean knn = ", mean(data_plot_jsd_rem[data_plot_jsd_rem$Model == "KNN", "jsd"], na.rm = T)))
sd(data_plot_jsd_rem[data_plot_jsd_rem$Model == "KNN", "jsd"], na.rm = T)
print(paste("mean gp = ", mean(data_plot_jsd_rem[data_plot_jsd_rem$Model == "GP", "jsd"], na.rm = T)))
sd(data_plot_jsd_rem[data_plot_jsd_rem$Model == "GP", "jsd"], na.rm = T)
print(paste("mean svm = ", mean(data_plot_jsd_rem[data_plot_jsd_rem$Model == "SVM", "jsd"], na.rm = T)))
sd(data_plot_jsd_rem[data_plot_jsd_rem$Model == "SVM", "jsd"], na.rm = T)

p2 = ggplot(data = data_plot_jsd_rem, aes(x = Specie, y = jsd)) + geom_line(aes(group = Model, col = Model)) + theme_bw() + ylim(0,0.7) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 5))+ ggtitle("Remaining Species") + ylab("JSD") #+ xlab("Specie")
p_jsd = p12 + p2 + plot_layout(guides = "collect", widths = c(3.5,2)) &
  theme(legend.position = "right")
ggsave("jsd_by_species.pdf", p_jsd, width = 9.5, height = 4)

# PCoA plot

# calculate distance matrices for KNN
dist_matrix_response_knn = compute_dist_matrix(bmr, type = "response", learner_id = 1)
dist_matrix_truth_knn = compute_dist_matrix(bmr, type = "truth", learner_id = 1)

colnames(dist_matrix_response_knn) = colnames(dist_matrix_truth_knn) = rownames(dist_matrix_response_knn) = rownames(dist_matrix_truth_knn) = rownames(X)
dist_matrix_response_knn = dist_matrix_response_knn[rownames(dist_matrix_response_knn) %in% c(Brassica, Poaceae), colnames(dist_matrix_response_knn) %in% c(Brassica, Poaceae)]
dist_matrix_truth_knn = dist_matrix_truth_knn[rownames(dist_matrix_truth_knn) %in% c(Brassica, Poaceae), colnames(dist_matrix_truth_knn) %in% c(Brassica, Poaceae)]


# Perform PCoA (with centering) on the original distance matrix
pcoa_result_original <- pcoa(dist_matrix_truth_knn)
projection_response = compute_projection_response(pcoa_result_original, dist_matrix_response_knn)

# combine data for plotting
data_pc = as.data.frame(rbind(projection_response[,1:2], pcoa_result_original$vectors[,1:2]))
data_pc$PredVsTrue = c(rep("Pred", nrow(dist_matrix_response_knn)), rep("True", nrow(dist_matrix_response_knn)))
data_pc$Species = rep(row.names(dist_matrix_response_knn), 2)
data_pc_sub = data_pc[data_pc$Species %in% c(Brassica, Poaceae),]
data_pc_sub$Family = NA
data_pc_sub$Family[data_pc_sub$Species %in% Brassica] = "Brassica"
data_pc_sub$Family[data_pc_sub$Species %in% Poaceae] = "Poaceae"
#data_pc_sub$Family[data_pc_sub$Species %in% Fabaceae] = "Fabaceae"

p2 = ggplot(data_pc_sub, aes(x = Axis.1, y = Axis.2)) + geom_point(aes(col = Family, shape = PredVsTrue)) +geom_line(aes(group = Species), col = "lightgrey", lty = "dotted") +
  ylab(paste("PC2:", round(pcoa_result_original$values$Rel_corr_eig[2]*100,0), "%")) +
  xlab(paste("PC1:", round(pcoa_result_original$values$Rel_corr_eig[1]*100,0), "%")) +
  theme_bw()+ labs(shape = NULL) + ggtitle("KNN") + xlim(-0.5, 0.6) + ylim(-0.6,0.25)


p22 = ggplot(data_pc_sub, aes(x = Axis.1, y = Axis.2, label = Species)) + geom_point(aes(col = Family, shape = PredVsTrue)) +geom_line(aes(group = Species), col = "lightgrey", lty = "dotted") +
  ylab(paste("PC2:", round(pcoa_result_original$values$Rel_corr_eig[2]*100,0), "%")) +
  xlab(paste("PC1:", round(pcoa_result_original$values$Rel_corr_eig[1]*100,0), "%")) +
  theme_bw()+ labs(shape = NULL) + ggtitle("KNN") +geom_text(hjust=0, vjust=0, size = 2, alpha = 0.6)+ xlim(-0.4, 0.6) + ylim(-0.6,0.25)


# GP Model
dist_matrix_response_gp = compute_dist_matrix(bmr, type = "response", learner_id = 2)
dist_matrix_truth_gp = compute_dist_matrix(bmr, type = "truth", learner_id = 2)

colnames(dist_matrix_response_gp) = colnames(dist_matrix_truth_gp) = rownames(dist_matrix_response_gp) = rownames(dist_matrix_truth_gp) = rownames(X)
dist_matrix_response_gp = dist_matrix_response_gp[rownames(dist_matrix_response_gp) %in% c(Brassica, Poaceae), colnames(dist_matrix_response_gp) %in% c(Brassica, Poaceae)]
dist_matrix_truth_gp = dist_matrix_truth_gp[rownames(dist_matrix_truth_gp) %in% c(Brassica, Poaceae), colnames(dist_matrix_truth_gp) %in% c(Brassica, Poaceae)]


# Perform PCoA (with centering) on the original distance matrix
pcoa_result_original_gp <- pcoa(dist_matrix_truth_gp)
projection_response_gp = compute_projection_response(pcoa_result_original_gp, dist_matrix_response_gp)

# combine data for plotting
data_pc_gp = as.data.frame(rbind(projection_response_gp[,1:2], pcoa_result_original_gp$vectors[,1:2]))
data_pc_gp$PredVsTrue = c(rep("Pred", nrow(dist_matrix_response_gp)), rep("True", nrow(dist_matrix_response_gp)))
data_pc_gp$Species = rep(row.names(dist_matrix_response_gp), 2)
data_pc_sub_gp = data_pc_gp[data_pc_gp$Species %in% c(Brassica, Poaceae),]
data_pc_sub_gp$Family = NA
data_pc_sub_gp$Family[data_pc_sub_gp$Species %in% Brassica] = "Brassica"
data_pc_sub_gp$Family[data_pc_sub_gp$Species %in% Poaceae] = "Poaceae"
#data_pc_sub$Family[data_pc_sub$Species %in% Fabaceae] = "Fabaceae"

p3 = ggplot(data_pc_sub_gp, aes(x = Axis.1, y = Axis.2)) + geom_point(aes(col = Family, shape = PredVsTrue)) +geom_line(aes(group = Species), col = "lightgrey", lty = "dotted") +
  ylab(paste("PC2:", round(pcoa_result_original_gp$values$Rel_corr_eig[2]*100,0), "%")) +
  xlab(paste("PC1:", round(pcoa_result_original_gp$values$Rel_corr_eig[1]*100,0), "%")) +
  theme_bw()+ labs(shape = NULL) + ggtitle("GP") + xlim(-0.5, 0.6) + ylim(-0.6,0.25)

p33 = ggplot(data_pc_sub_gp, aes(x = Axis.1, y = Axis.2, label = Species)) + geom_point(aes(col = Family, shape = PredVsTrue)) +geom_line(aes(group = Species), col = "lightgrey", lty = "dotted") +
  ylab(paste("PC2:", round(pcoa_result_original_gp$values$Rel_corr_eig[2]*100,0), "%")) +
  xlab(paste("PC1:", round(pcoa_result_original_gp$values$Rel_corr_eig[1]*100,0), "%")) +
  theme_bw()+ labs(shape = NULL) + ggtitle("GP") +geom_text(hjust=0, vjust=0, size = 2, alpha = 0.6)+ xlim(-0.4, 0.6) + ylim(-0.6,0.25)



# SVM Model
dist_matrix_response_svm = compute_dist_matrix(bmr, type = "response", learner_id = 3)
dist_matrix_truth_svm = compute_dist_matrix(bmr, type = "truth", learner_id = 3)

colnames(dist_matrix_response_svm) = colnames(dist_matrix_truth_svm) = rownames(dist_matrix_response_svm) = rownames(dist_matrix_truth_svm) = rownames(X)
dist_matrix_response_svm = dist_matrix_response_svm[rownames(dist_matrix_response_svm) %in% c(Brassica, Poaceae), colnames(dist_matrix_response_svm) %in% c(Brassica, Poaceae)]
dist_matrix_truth_svm = dist_matrix_truth_svm[rownames(dist_matrix_truth_svm) %in% c(Brassica, Poaceae), colnames(dist_matrix_truth_svm) %in% c(Brassica, Poaceae)]


# Perform PCoA (with centering) on the original distance matrix
pcoa_result_original_svm <- pcoa(dist_matrix_truth_svm)
projection_response_svm = compute_projection_response(pcoa_result_original_svm, dist_matrix_response_svm)

# combine data for plotting
data_pc_svm = as.data.frame(rbind(projection_response_svm[,1:2], pcoa_result_original_svm$vectors[,1:2]))
data_pc_svm$PredVsTrue = c(rep("Pred", nrow(dist_matrix_response_svm)), rep("True", nrow(dist_matrix_response_svm)))
data_pc_svm$Species = rep(row.names(dist_matrix_response_svm), 2)
data_pc_sub_svm = data_pc_svm[data_pc_svm$Species %in% c(Brassica, Poaceae),]
data_pc_sub_svm$Family = NA
data_pc_sub_svm$Family[data_pc_sub_svm$Species %in% Brassica] = "Brassica"
data_pc_sub_svm$Family[data_pc_sub_svm$Species %in% Poaceae] = "Poaceae"
#data_pc_sub$Family[data_pc_sub$Species %in% Fabaceae] = "Fabaceae"

p4 = ggplot(data_pc_sub_svm, aes(x = Axis.1, y = Axis.2)) + geom_point(aes(col = Family, shape = PredVsTrue)) +geom_line(aes(group = Species), col = "lightgrey", lty = "dotted") +
  ylab(paste("PC2:", round(pcoa_result_original_svm$values$Rel_corr_eig[2]*100,0), "%")) +
  xlab(paste("PC1:", round(pcoa_result_original_svm$values$Rel_corr_eig[1]*100,0), "%")) +
  theme_bw()+ labs(shape = NULL) + ggtitle("SVM") + xlim(-0.5, 0.6) + ylim(-0.6,0.25)

p33 = ggplot(data_pc_sub_svm, aes(x = Axis.1, y = Axis.2, label = Species)) + geom_point(aes(col = Family, shape = PredVsTrue)) +geom_line(aes(group = Species), col = "lightgrey", lty = "dotted") +
  ylab(paste("PC2:", round(pcoa_result_original_svm$values$Rel_corr_eig[2]*100,0), "%")) +
  xlab(paste("PC1:", round(pcoa_result_original_svm$values$Rel_corr_eig[1]*100,0), "%")) +
  theme_bw()+ labs(shape = NULL) + ggtitle("GP") +geom_text(hjust=0, vjust=0, size = 2, alpha = 0.6)+ xlim(-0.4, 0.6) + ylim(-0.6,0.25)


p = p2+p3+p4 + plot_layout(guides = "collect") &
  #plot_annotation(tag_levels = "A") & 
  theme(legend.position = "right")
ggsave("pcoa_comp.png", p, width = 9, height = 3)
ggsave("pcoa_knn_vs_gp_label.png", p22+p33, width = 12, height = 5)



# EXP
colnames(knn_pred) = colnames(knn_true)
dat_exp = rbind(knn_true,knn_pred)


dist_dist = matrix(nrow = nrow(dat_exp), ncol = nrow(dat_exp) ) 
for(i in 1:nrow(dat_exp)){
  subset_wo_i = (1:nrow(dat_exp))[-i]
  for(j in subset_wo_i){
    dist_dist[i,j] = JSD(rbind(unlist(dat_exp[i,2:ncol(dat_exp)]), unlist(dat_exp[j,2:ncol(dat_exp)])), unit = "log", est.prob = "empirical", test.na = FALSE)
  }
  dist_dist[i,i] = 0
}

colnames(dist_dist) =  rownames(dist_dist)  = rep(rownames(X),2)
dist_dist = dist_dist[rownames(dist_dist) %in% c(Brassica, Poaceae, Fabaceae), colnames(dist_dist) %in% c(Brassica, Poaceae, Fabaceae)]

pc_exp = pcoa(dist_dist)

data_pc = as.data.frame(pc_exp$vectors[,1:2])
data_pc$PredVsTrue = c(rep("True", nrow(dist)), rep("Pred", nrow(dist)))
data_pc$Species = rep(row.names(dist), 2)
data_pc_sub = data_pc[data_pc$Species %in% c(Brassica, Poaceae, Fabaceae),]
data_pc_sub$Family = NA
data_pc_sub$Family[data_pc_sub$Species %in% Brassica] = "Brassica"
data_pc_sub$Family[data_pc_sub$Species %in% Poaceae] = "Poaceae"
data_pc_sub$Family[data_pc_sub$Species %in% Fabaceae] = "Fabaceae"

p2 = ggplot(data_pc_sub, aes(x = Axis.1, y = Axis.2)) + geom_point(aes(col = Family, shape = PredVsTrue)) +geom_line(aes(group = Species), col = "lightgrey", lty = "dotted") +
  ylab(paste("PC2:", round(pc_exp$values$Rel_corr_eig[2]*100,0), "%")) +
  xlab(paste("PC1:", round(pc_exp$values$Rel_corr_eig[1]*100,0), "%")) +
  theme_bw()+ labs(shape = NULL)

#########



wcmdscale(d = dist, eig = TRUE)
pc_pred = wcmdscale(d = dist, eig = TRUE)
pc_true = wcmdscale(d = dist_true, eig = TRUE)
data_pc = as.data.frame(rbind(pc_pred$points[,1:2], pc_true$points[,1:2]))

ggplot(data_pc_sub, aes(x = Dim1, y = Dim2)) + geom_point(aes(col = Family, shape = PredVsTrue)) +geom_line(aes(group = Species), col = "lightgrey", lty = "dotted")
