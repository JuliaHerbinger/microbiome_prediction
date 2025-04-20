
# df = data.frame(specie = character(), microbiome = character(), k = integer(), distance = double(), prediction = double(), y = double())
# idx = 1
# k_max = 60
# n_y = ncol(Y)
# n_x = nrow(X)
# dist_matrix <- apply(X, 1, function(instance_1) {
#   apply(X, 1, function(instance_2) {
#     sum(instance_1 != instance_2)/ncol(X)
#   })
# })
# # 1 on diagonale to exclude
# diag(dist_matrix) = 1
# 
# for(k in 1:k_max){
#   for(i in 1:n_y){
#     for(j in 1:n_x){
#       dist_k_idx = sort(dist_matrix[,j], index.return = T)
#       dist_k_avg = mean(dist_k_idx$x[1:k])
#       #if(k<3) dist_k_sd = NA
#       #else dist_k_sd = sd(dist_k_idx$x[1:k])
#       y_idx = dist_k_idx$ix[1:k]
#       pred_k_avg = mean(Y[y_idx,i])
#       #if(k<3) pred_k_sd = NA
#       #else pred_k_sd = sd(Y[y_idx,i])
#       df[idx,"specie"] = row.names(X)[j]
#       df[idx, "microbiome"] = colnames(Y)[i]
#       df[idx, "k"] = k
#       df[idx, "distance"] = dist_k_avg
#       df[idx, "prediction"] = pred_k_avg
#       df[idx, "y"] = Y[j,i]
#       idx = idx + 1
#       print(idx)
#       print(i)
#       print(j)
#       print(k)
#     }
#   }
# }

# dist_matrix
dist_matrix <- apply(X, 1, function(instance_1) {
  apply(X, 1, function(instance_2) {
    sum(instance_1 != instance_2)/ncol(X)
  })
})

# Create a dataframe of all possible (k, i, j) combinations
grid <- expand.grid(k = 1:k_max, i = 1:n_y, j = 1:n_x)

# Function to process a single (k, i, j) triplet
process_entry <- function(row) {
  k <- row$k
  i <- row$i
  j <- row$j
  
  # Get sorted distances once per column `j`
  dist_k_idx <- sort(dist_matrix[, j], index.return = TRUE)
  
  # Compute distance and indices for top-k closest points
  dist_k_avg <- mean(dist_k_idx$x[1:k])
  y_idx <- dist_k_idx$ix[1:k]
  
  # Compute predictions
  pred_k_avg <- mean(Y[y_idx, i])
  
  # Return as a list
  list(
    species = row.names(X)[j],
    microbiome = colnames(Y)[i],
    k = k,
    distance = dist_k_avg,
    prediction = pred_k_avg,
    y = Y[j, i]
  )
}

# Apply function to all grid rows and combine results
df_new <- do.call(rbind, lapply(1:nrow(grid), function(idx) process_entry(grid[idx, ])))

# Convert to a dataframe
df <- as.data.frame(df_new)

# Print result
print(df)



# Aggregating over microbiomes
grouped_data <- df %>%
  group_by(species, k) %>%
  summarise(
    JSD = mean(JSD(rbind(unlist(prediction), unlist(y)), unit = "log", est.prob = "empirical", test.na = TRUE)),
    MAD = mean(abs(unlist(prediction) - unlist(y))),
    mean_distance = mean(unlist(distance)),
    #sd_distance = sd(unlist(distance)),
    .groups = "drop"
  )

# Print result
print(grouped_data)

# Plot with ggplot
grouped_data = as.data.frame(grouped_data)
grouped_data$k = as.integer(grouped_data$k)
grouped_data$species = as.character(unlist(grouped_data$species))
brassica_subset = subset(grouped_data, species %in% Brassica)
poaceae_subset = subset(grouped_data, species %in% Poaceae)
remaining_subset = subset(grouped_data, !(species %in% c(Brassica, Poaceae)))

grouped_data$Family = NA
grouped_data$Family[which(grouped_data$species %in% Brassica)] = "Brassica"
grouped_data$Family[which(grouped_data$species %in% Poaceae)] = "Poaceae"
grouped_data$Family[which(!(grouped_data$species %in% c(Brassica, Poaceae)))] = "Remaining"

p_poa_jsd = ggplot(poaceae_subset, aes(x = k)) +
  geom_line(aes(y = JSD, color = "JSD"), linetype = 2) +
  geom_line(aes(y = MAD, color = "MAE"), linetype = 4) +
  geom_line(aes(y = mean_distance, color = "Phylogenetic Distance")) + ylim(0,0.7) +
  #geom_ribbon(aes(ymin = mean_distance - sd_distance, ymax = mean_distance + sd_distance, fill = "Mean Distance"), alpha = 0.2) +
  facet_wrap(vars(species), ncol = 5) +
  labs(title = "Poaceae Species", x = "k", y = "Distance", color = "Metric", fill = "Metric") +
  theme_minimal()



p_bra_jsd = ggplot(brassica_subset, aes(x = k)) +
  geom_line(aes(y = JSD, color = "JSD"), linetype = 2) +
  geom_line(aes(y = MAD, color = "MAE"), linetype = 4) +
  geom_line(aes(y = mean_distance, color = "Phylogenetic Distance")) +ylim(0,0.7) +
  #geom_ribbon(aes(ymin = mean_distance - sd_distance, ymax = mean_distance + sd_distance, fill = "Mean Distance"), alpha = 0.2) +
  facet_wrap(vars(species), ncol = 5) +
  labs(title = "Brassica Species", x = "k", y = "Distance", color = "Metric", fill = "Metric") +
  theme_minimal()


library(ggh4x)
p_rem_jsd = ggplot(remaining_subset, aes(x = k)) +
  geom_line(aes(y = JSD, color = "JSD"), linetype = 2) +
  geom_line(aes(y = MAD, color = "MAE"), linetype = 4) +
  geom_line(aes(y = mean_distance, color = "Phylogenetic Distance")) +ylim(0,0.7) +
  #geom_ribbon(aes(ymin = mean_distance - sd_distance, ymax = mean_distance + sd_distance, fill = "Mean Distance"), alpha = 0.2) +
  facet_wrap(vars(species), ncol = 5) +
  #facet_wrap(~ species, nrow = 8)  +
  labs(title = "Remaining Species", x = "k", y = "Distance", color = "Metric", fill = "Metric") +
  theme_minimal()

library(patchwork)
p = p_bra_jsd /p_poa_jsd/p_rem_jsd + plot_layout(guides = "collect", heights = c(1.7,3,3)) &
  theme(legend.position = "bottom")
ggsave("knn_k_jsd_vs_mad.pdf", p, width = 9.5, height = 13)
#ggsave("knn_k_jsd_vs_mad_poa.png", p_poa_jsd+p_poa_mad, width = 10, height = 6)
#ggsave("knn_k_jsd_vs_mad_rem.png", p_rem_jsd+p_rem_mad, width = 13, height = 6)

# plot for families
# Aggregating over microbiomes
grouped_data_fam <- grouped_data %>%
  group_by(Family, k) %>%
  summarise(
    mean_JSD = mean(JSD),
    min_jsd = min(JSD),
    max_jsd = max(JSD),
    mean_distance2 = mean(mean_distance),
    min_distance = min(mean_distance),
    max_distance = max(mean_distance),
    #sd_distance = sd(unlist(distance)),
    .groups = "drop"
  )


p_fam = ggplot(grouped_data_fam, aes(x = k)) +
  geom_line(aes(y = mean_JSD, color = "JSD"), linetype = 2) +
  geom_ribbon(aes(ymin = min_jsd, ymax = max_jsd, fill = "JSD"), alpha = 0.3) +
  geom_line(aes(y = mean_distance2, color = "Phyl. Distance")) +
  geom_ribbon(aes(ymin = min_distance, ymax = max_distance, fill = "Phyl. Distance"), alpha = 0.3) +
  #geom_ribbon(aes(ymin = mean_distance - sd_distance, ymax = mean_distance + sd_distance, fill = "Mean Distance"), alpha = 0.2) +
  facet_wrap(vars(Family), ncol = 5) +
  #facet_wrap(~ species, nrow = 8)  +
  labs(x = "k", y = "Distance", color = "Mean", fill = "Range") +
  theme_minimal()

p = p_fam + plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
ggsave("knn_k_jsd_fam.pdf", p, width = 5.5, height = 3)


# which k values are the best for each specie and which ones the worst?
# table with counts of best k values -- compare for two big families
jsd_min_df = grouped_data %>%
  group_by(species) %>%
  filter(JSD == min(JSD))

jsd_min_df_k = jsd_min_df %>%
  group_by(k)%>%
  summarise(k_count = sum(k==k), mean_jsd = mean(JSD))

library(dplyr)
library(tidyr)
library(knitr)

# Transpose the data
transposed_table <- as.matrix(jsd_min_df_k) %>% t()

# Convert to LaTeX format
latex_table <- kable(transposed_table, format = "latex", booktabs = TRUE, align = "c", col.names = NULL, digits = 3)

# Print LaTeX table
cat(latex_table)

# only Brassica
jsd_min_df_k_bra = jsd_min_df[jsd_min_df$Family == c("Brassica"),] %>%
  group_by(k)%>%
  summarise(k_count = sum(k==k), mean_jsd = mean(JSD))

jsd_min_df_k_poa = jsd_min_df[jsd_min_df$Family == c("Poaceae"),] %>%
  group_by(k)%>%
  summarise(k_count = sum(k==k), mean_jsd = mean(JSD))

jsd_min_df_k_rem = jsd_min_df[jsd_min_df$Family == c("Remaining"),] %>%
  group_by(k)%>%
  summarise(k_count = sum(k==k), mean_jsd = mean(JSD))

# Transpose the data
transposed_table <- as.matrix(jsd_min_df_k_poa) %>% t()

# Convert to LaTeX format
latex_table <- kable(transposed_table, format = "latex", booktabs = TRUE, align = "c", col.names = NULL, digits = 2)

# Print LaTeX table
cat(latex_table)


jsd_df = merge(jsd_min_df_k, jsd_min_df_k_bra, by = "k", all = T)
jsd_df = merge(jsd_df, jsd_min_df_k_poa, by = "k", all = T)
jsd_df = merge(jsd_df, jsd_min_df_k_rem, by = "k", all = T)
# Convert to LaTeX format
latex_table <- kable(jsd_df, format = "latex", booktabs = TRUE, align = "c", col.names = NULL, digits = 2)

# Print LaTeX table
cat(latex_table)


# for each best k:
# which microbiomes have the lowest prediction error over all species? and which ones the highest?
# average mad over all species for 10% best and worst microbiomes (list them)
grouped_data_q <- df %>%
  group_by(species, k) %>%
  summarise(
    MAD_01 = quantile(abs(unlist(prediction) - unlist(y)), 0.1),
    MAD_05 = quantile(abs(unlist(prediction) - unlist(y)), 0.5),
    MAD_09 = quantile(abs(unlist(prediction) - unlist(y)), 0.9),
    #mean_distance = mean(unlist(distance)),
    #sd_distance = sd(unlist(distance)),
    .groups = "drop"
  )

# Plot with ggplot
grouped_data_q = as.data.frame(grouped_data_q)
grouped_data_q$k = as.integer(grouped_data_q$k)
grouped_data_q$species = as.character(unlist(grouped_data_q$species))
brassica_subset = subset(grouped_data_q, species %in% Brassica)
#poaceae_subset = subset(grouped_data, species %in% Poaceae)
#remaining_subset = subset(grouped_data, !(species %in% c(Brassica, Poaceae)))

p_mad = ggplot(grouped_data_q, aes(x = k)) +
  geom_line(aes(y = MAD_05, color = "MAD"), size = 1) +
  #geom_line(aes(y = mean_distance, color = "Distance"), size = 1) + ylim(0,0.7) +
  geom_ribbon(aes(ymin = MAD_01, ymax = MAD_09, fill = "Q Range"), alpha = 0.3) +
  facet_wrap(~ species, nrow = 10)  +
  labs(title = "MAD per Species", x = "k", y = "Value", color = "Metric", fill = "Metric") +
  theme_minimal()

ggsave("knn_k_mad.png", p_mad, width = 9, height = 12)

# which microbiomes have lowest/highest prediction error for the two largest families?
# create subdataset for both families, also calculate average mad for top and low microbiomes and
# compare to full set

# plot which shows JSD for species on x axis and also mean distance for best k