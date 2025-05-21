# Analysis of Section 4.2 -- Model comparison

# load resutls after running benchmark.R
load("results/bmr_21042025.Rdata")


# 1. extract results from benchmark for all learners
data_response_gp = extract_result_data(bmr, 2, "response")
data_truth_gp = extract_result_data(bmr, 2, "truth")
data_response_knn = extract_result_data(bmr, 1, "response")
data_truth_knn = extract_result_data(bmr, 1, "truth")
data_response_svm = extract_result_data(bmr, 3, "response")
data_truth_svm = extract_result_data(bmr, 3, "truth")
save(data_response_knn, data_truth_knn, file = "results/data_knn.RData")

# 2. calculate jsd distances between ground truth and predictions of all learners
jsd_knn = compute_jsd_distance(data_response_knn, data_truth_knn)
jsd_gp = compute_jsd_distance(data_response_gp, data_truth_gp)
jsd_svm = compute_jsd_distance(data_response_svm, data_truth_svm)

# 3. combine data and prepare for plotting
data_jsd = data.frame("Specie" = row.names(X), "KNN" = jsd_knn, "GP" = jsd_gp, "SVM" = jsd_svm)
data_plot_jsd = create_data_jsd_plot(data_jsd, "KNN")


#---------------------------------------------------------------------
# Performance summary of Table 2

# all species
print(paste("mean knn = ", mean(jsd_knn, na.rm = T)))
sd(jsd_knn, na.rm = T)
print(paste("mean gp = ", mean(jsd_gp_tuned, na.rm = T)))
sd(jsd_gp_tuned, na.rm = T)
print(paste("mean svm = ", mean(jsd_svm, na.rm = T)))
sd(jsd_svm, na.rm = T)

# Brassica and Poaceae
print(paste("mean knn = ", mean(data_plot_jsd_fam[data_plot_jsd_fam$Model == "KNN", "jsd"], na.rm = T)))
sd(data_plot_jsd_fam[data_plot_jsd_fam$Model == "KNN", "jsd"], na.rm = T)
print(paste("mean gp = ", mean(data_plot_jsd_fam[data_plot_jsd_fam$Model == "GP", "jsd"], na.rm = T)))
sd(data_plot_jsd_fam[data_plot_jsd_fam$Model == "GP", "jsd"], na.rm = T)
print(paste("mean svm = ", mean(data_plot_jsd_fam[data_plot_jsd_fam$Model == "SVM", "jsd"], na.rm = T)))
sd(data_plot_jsd_fam[data_plot_jsd_fam$Model == "SVM", "jsd"], na.rm = T)

# Remaining species
data_plot_jsd_rem = data_plot_jsd[!(data_plot_jsd$Specie %in% c(Poaceae, Brassica)),]
print(paste("mean knn = ", mean(data_plot_jsd_rem[data_plot_jsd_rem$Model == "KNN", "jsd"], na.rm = T)))
sd(data_plot_jsd_rem[data_plot_jsd_rem$Model == "KNN", "jsd"], na.rm = T)
print(paste("mean gp = ", mean(data_plot_jsd_rem[data_plot_jsd_rem$Model == "GP", "jsd"], na.rm = T)))
sd(data_plot_jsd_rem[data_plot_jsd_rem$Model == "GP", "jsd"], na.rm = T)
print(paste("mean svm = ", mean(data_plot_jsd_rem[data_plot_jsd_rem$Model == "SVM", "jsd"], na.rm = T)))
sd(data_plot_jsd_rem[data_plot_jsd_rem$Model == "SVM", "jsd"], na.rm = T)

#---------------------------------------------------------------------
# Figure 3: Performance Comparison
data_plot_jsd_fam = data_plot_jsd[data_plot_jsd$Specie %in% c(Poaceae, Brassica),]

p1 = ggplot(data = data_plot_jsd_fam, aes(x = Specie, y = jsd)) + geom_line(aes(group = Model, col = Model)) + theme_bw() + ylim(0,0.7) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 5)) + ggtitle("Poaceae and Brassica") + ylab("Performance (JSD)")

p2 = ggplot(data = data_plot_jsd_rem, aes(x = Specie, y = jsd)) + geom_line(aes(group = Model, col = Model)) + theme_bw() + ylim(0,0.7) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 5))+ ggtitle("Remaining Species") + ylab("Performance (JSD)") #+ xlab("Specie")
p_jsd = p1 + p2 + plot_layout(guides = "collect", widths = c(3.5,2)) &
  theme(legend.position = "right")
ggsave("figures/jsd_by_species.pdf", p_jsd, width = 9.5, height = 4)


#---------------------------------------------------------------------
# Figure 4: PCoA plot

# 1. PCoA plot for KNN

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

p2 = ggplot(data_pc_sub, aes(x = Axis.1, y = Axis.2)) + geom_point(aes(col = Family, shape = PredVsTrue)) +geom_line(aes(group = Species), col = "lightgrey", lty = "dotted") +
  ylab(paste("PC2:", round(pcoa_result_original$values$Rel_corr_eig[2]*100,0), "%")) +
  xlab(paste("PC1:", round(pcoa_result_original$values$Rel_corr_eig[1]*100,0), "%")) +
  theme_bw()+ labs(shape = NULL) + ggtitle("KNN") + xlim(-0.5, 0.6) + ylim(-0.6,0.25)



# 2. PCoA plot for GP

# calculate distance matrices for KNN
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

p3 = ggplot(data_pc_sub_gp, aes(x = Axis.1, y = Axis.2)) + geom_point(aes(col = Family, shape = PredVsTrue)) +geom_line(aes(group = Species), col = "lightgrey", lty = "dotted") +
  ylab(paste("PC2:", round(pcoa_result_original_gp$values$Rel_corr_eig[2]*100,0), "%")) +
  xlab(paste("PC1:", round(pcoa_result_original_gp$values$Rel_corr_eig[1]*100,0), "%")) +
  theme_bw()+ labs(shape = NULL) + ggtitle("GP") + xlim(-0.5, 0.6) + ylim(-0.6,0.25)


# 3. PCoA plot for SVM

# calculate distance matrices for KNN
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

p4 = ggplot(data_pc_sub_svm, aes(x = Axis.1, y = Axis.2)) + geom_point(aes(col = Family, shape = PredVsTrue)) +geom_line(aes(group = Species), col = "lightgrey", lty = "dotted") +
  ylab(paste("PC2:", round(pcoa_result_original_svm$values$Rel_corr_eig[2]*100,0), "%")) +
  xlab(paste("PC1:", round(pcoa_result_original_svm$values$Rel_corr_eig[1]*100,0), "%")) +
  theme_bw()+ labs(shape = NULL) + ggtitle("SVM") + xlim(-0.5, 0.6) + ylim(-0.6,0.25)


# Create final figure 4 of manuscript
p = p2+p3+p4 + plot_layout(guides = "collect") &
  theme(legend.position = "right")
ggsave("figures/pcoa_comp.pdf", p, width = 9, height = 3)

