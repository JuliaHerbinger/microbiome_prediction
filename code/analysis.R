load("bmr_1701.Rdata")
library(ape)
library(ggplot2)

# Figure decreasing distance

# extract results from benchmark for all learners
data_response_knn = extract_result_data(bmr, 1, "response")
data_truth_knn = extract_result_data(bmr, 1, "truth")
data_response_gp = extract_result_data(bmr, 2, "response")
data_truth_gp = extract_result_data(bmr, 2, "truth")

# calculate jsd distances between ground truth and predictions of all learners
jsd_knn = compute_jsd_distance(data_response_knn, data_truth_knn)
jsd_gp = compute_jsd_distance(data_response_gp, data_truth_gp)

# combine data and prepare for plotting
data_jsd = data.frame("specie" = row.names(X), "knn" = jsd_knn, "gp" = jsd_gp)
data_plot_jsd = create_data_jsd_plot(data_jsd, "gp")

# JSD plot
p1 = ggplot(data = data_plot_jsd, aes(x = specie, y = jsd)) + geom_line(aes(group = model, col = model)) + theme_bw() + ylim(0,0.8) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 5))

ggsave("jsd_by_species.png", p1, width = 7, height = 4)
mean(jsd_knn)
sd(jsd_knn)



# PCoA plot





library(patchwork)
Brassica_matches = c("Alliaria", "Arabidopsis", "Barbarea", "Brassica", "Capsella", "Cardamine", "Erophila", "Eruca", "Raphanus", "Sinapis")
Brassica <- unique (grep(paste(Brassica_matches,collapse="|"), row.names(X), value=TRUE))

Poaceae_matches = c("Avena", "Elymus", "Festuca", "Lolium", "Oryza")
Poaceae <- unique (grep(paste(Poaceae_matches,collapse="|"), row.names(X), value=TRUE))

Fabaceae_matches = c("Medicago", "Phaseolus", "Pisum", "Vicia")
Fabaceae <- unique (grep(paste(Fabaceae_matches,collapse="|"), row.names(X), value=TRUE))



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
  theme_bw()+ labs(shape = NULL) + ggtitle("KNN") + xlim(-0.4, 0.6) + ylim(-0.6,0.25)


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
  theme_bw()+ labs(shape = NULL) + ggtitle("GP") + xlim(-0.4, 0.6) + ylim(-0.6,0.25)

p33 = ggplot(data_pc_sub_gp, aes(x = Axis.1, y = Axis.2, label = Species)) + geom_point(aes(col = Family, shape = PredVsTrue)) +geom_line(aes(group = Species), col = "lightgrey", lty = "dotted") +
  ylab(paste("PC2:", round(pcoa_result_original_gp$values$Rel_corr_eig[2]*100,0), "%")) +
  xlab(paste("PC1:", round(pcoa_result_original_gp$values$Rel_corr_eig[1]*100,0), "%")) +
  theme_bw()+ labs(shape = NULL) + ggtitle("GP") +geom_text(hjust=0, vjust=0, size = 2, alpha = 0.6)+ xlim(-0.4, 0.6) + ylim(-0.6,0.25)


ggsave("pcoa_knn_vs_gp.png", p2+p3, width = 8.2, height = 3)
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
