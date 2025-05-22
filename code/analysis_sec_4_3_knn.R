# Section 4.3: Analyses of KNN

#---------------------------------------------------------------------
# 1. Define distances for input and output for each possible k

# a. distance matrix of input space
dist_matrix <- apply(X, 1, function(instance_1) {
  apply(X, 1, function(instance_2) {
    sum(instance_1 != instance_2)/ncol(X)
  })
})
# set diagonales to 1 to avoid them being the closest neighbor (instead set to max)
diag(dist_matrix) = 1

# Create a dataframe of all possible (k, i, j) combinations
grid <- expand.grid(k = 1:60, i = 1:ncol(Y), j = 1:nrow(X))

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


#---------------------------------------------------------------------
# 2. Create species-level plots (Figure A1) for phylogenetic distance (HD), and microbial distance (JSD and MAE)

# Aggregating over microbiomes
grouped_data <- df %>%
  dplyr::group_by(species, k) %>%
  dplyr::summarise(
    JSD = mean(JSD(rbind(unlist(prediction), unlist(y)), unit = "log", est.prob = "empirical", test.na = TRUE)),
    MAD = mean(abs(unlist(prediction) - unlist(y))),
    mean_distance = mean(unlist(distance)),
    .groups = "drop"
  )

# define subsets based on families
grouped_data = as.data.frame(grouped_data)
grouped_data$k = as.integer(grouped_data$k)
grouped_data$species = as.character(unlist(grouped_data$species))
brassica_subset = subset(grouped_data, species %in% Brassica)
poaceae_subset = subset(grouped_data, species %in% Poaceae)
remaining_subset = subset(grouped_data, !(species %in% c(Brassica, Poaceae)))

# Define Family variable for plotting
grouped_data$Family = NA
grouped_data$Family[which(grouped_data$species %in% Brassica)] = "Brassica"
grouped_data$Family[which(grouped_data$species %in% Poaceae)] = "Poaceae"
grouped_data$Family[which(!(grouped_data$species %in% c(Brassica, Poaceae)))] = "Remaining"

# Plots for different families
p_poa_jsd = ggplot(poaceae_subset, aes(x = k)) +
  geom_line(aes(y = JSD, color = "Microbial Distance (JSD)"), linetype = 2) +
  geom_line(aes(y = MAD, color = "Microbial Distance (MAE)"), linetype = 4) +
  geom_line(aes(y = mean_distance, color = "Phylogenetic Distance (HD)")) + ylim(0,0.7) +
  facet_wrap(vars(species), ncol = 5) +
  labs(title = "Poaceae Species", x = "k", y = "Distance", color = "Metric", fill = "Metric") +
  theme_minimal()

p_bra_jsd = ggplot(brassica_subset, aes(x = k)) +
  geom_line(aes(y = JSD, color = "Microbial Distance (JSD)"), linetype = 2) +
  geom_line(aes(y = MAD, color = "Microbial Distance (MAE)"), linetype = 4) +
  geom_line(aes(y = mean_distance, color = "Phylogenetic Distance (HD)")) +ylim(0,0.7) +
  facet_wrap(vars(species), ncol = 5) +
  labs(title = "Brassica Species", x = "k", y = "Distance", color = "Metric", fill = "Metric") +
  theme_minimal()

p_rem_jsd = ggplot(remaining_subset, aes(x = k)) +
  geom_line(aes(y = JSD, color = "Microbial Distance (JSD)"), linetype = 2) +
  geom_line(aes(y = MAD, color = "Microbial Distance (MAE)"), linetype = 4) +
  geom_line(aes(y = mean_distance, color = "Phylogenetic Distance (HD)")) +ylim(0,0.7) +
  facet_wrap(vars(species), ncol = 5) +
  labs(title = "Remaining Species", x = "k", y = "Distance", color = "Metric", fill = "Metric") +
  theme_minimal()

# Final plot for Figure A1 in Appendix
p = p_bra_jsd /p_poa_jsd/p_rem_jsd + plot_layout(guides = "collect", heights = c(1.7,3,3)) &
  theme(legend.position = "bottom")
ggsave("figures/knn_k_jsd_vs_mad.pdf", p, width = 9.5, height = 13)


#---------------------------------------------------------------------
# 3. Create Figure 6: Aggregation over species within family

# Aggregating on Family level
grouped_data_fam <- grouped_data %>%
  dplyr::group_by(Family, k) %>%
  dplyr::summarise(
    mean_JSD = mean(JSD),
    min_jsd = min(JSD),
    max_jsd = max(JSD),
    mean_distance2 = mean(mean_distance),
    min_distance = min(mean_distance),
    max_distance = max(mean_distance),
    .groups = "drop"
  )

# Create plot for Figure 6
p_fam = ggplot(grouped_data_fam, aes(x = k)) +
  geom_line(aes(y = mean_JSD, color = "Microbial"), linetype = 2) +
  geom_ribbon(aes(ymin = min_jsd, ymax = max_jsd, fill = "Microbial"), alpha = 0.3) +
  geom_line(aes(y = mean_distance2, color = "Phylogenetic")) +
  geom_ribbon(aes(ymin = min_distance, ymax = max_distance, fill = "Phylogenetic"), alpha = 0.3) +
  facet_wrap(vars(Family), ncol = 5) +
  labs(x = "k", y = "Distance", color = "Mean", fill = "Range") +
  theme_minimal()

# Save Figure 6
p = p_fam + plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
ggsave("figures/knn_k_jsd_fam.pdf", p, width = 6, height = 3)


#---------------------------------------------------------------------
# Create Table 4: Analysis of best k value

# table with counts of best k values
jsd_min_df = grouped_data %>%
  dplyr::group_by(species) %>%
  dplyr::filter(JSD == min(JSD))

jsd_min_df_k = jsd_min_df %>%
  dplyr::group_by(k)%>%
  dplyr::summarise(k_count = sum(k==k), mean_jsd = mean(JSD))


# Create table for each family
jsd_min_df_k_bra = jsd_min_df[jsd_min_df$Family == c("Brassica"),] %>%
  dplyr::group_by(k)%>%
  dplyr::summarise(k_count = sum(k==k), mean_jsd = mean(JSD))

jsd_min_df_k_poa = jsd_min_df[jsd_min_df$Family == c("Poaceae"),] %>%
  dplyr::group_by(k)%>%
  dplyr::summarise(k_count = sum(k==k), mean_jsd = mean(JSD))

jsd_min_df_k_rem = jsd_min_df[jsd_min_df$Family == c("Remaining"),] %>%
  dplyr::group_by(k)%>%
  dplyr::summarise(k_count = sum(k==k), mean_jsd = mean(JSD))

# merge data frames
jsd_df = merge(jsd_min_df_k, jsd_min_df_k_bra, by = "k", all = T)
jsd_df = merge(jsd_df, jsd_min_df_k_poa, by = "k", all = T)
jsd_df = merge(jsd_df, jsd_min_df_k_rem, by = "k", all = T)

# Convert to LaTeX format
latex_table <- kable(jsd_df, format = "latex", booktabs = TRUE, align = "c", col.names = NULL, digits = 2)

# Print Table 3
cat(latex_table)


#---------------------------------------------------------------------
# Create Figure 7: Analysis of tuned knn on a microbial level

# analysis based on tuned knn
load("results/data_knn.RData")
df_mae <- abs(data_truth_knn - data_response_knn)
df_mae$row_ids = NULL
colnames(df_mae) = paste0("M", 1:ncol(df_mae))
df_mae$Species = rownames(X)
df_mae_plot <- melt(setDT(df_mae), id.vars = c("Species"), variable.name = "Microbiome")


# 1. Create data set for heatmap plot
df_mae_plot$Value <- df_mae_plot$value
remaining_species <- unique(df_mae_plot$Species)[!(unique(df_mae_plot$Species) %in% c(Brassica, Poaceae))]
df_mae_plot$Species <- factor(df_mae_plot$Species, levels = c(Brassica, Poaceae, remaining_species))

# 2. Row means (across microbiomes)
row_means <- df_mae_plot %>%
  dplyr::group_by(Species) %>%
  dplyr::summarise(Value = mean(Value), .groups = "drop") %>%
  dplyr::mutate(Microbiome = "Specie Mean", Value_type = "mean")

# 3. Column means (across species)
col_means <- df_mae_plot %>%
  dplyr::group_by(Microbiome) %>%
  dplyr::summarise(Value = mean(Value), .groups = "drop") %>%
  dplyr::mutate(Species = "Microbiome Mean", Value_type = "mean")

# 4. Main data
df_main <- df_mae_plot %>%
  dplyr::mutate(Value_type = "main")

# 5. Combine all
df_combined <- bind_rows(df_main, row_means, col_means)
df_combined$Species = factor(df_combined$Species, levels = c("Microbiome Mean", Brassica, Poaceae, remaining_species))
df_combined$Microbiome = factor(df_combined$Microbiome, levels = c(paste0("M",1:ncol(Y)),"Specie Mean"))


# 6. Bin original values for manual coloring
df_combined <- df_combined %>%
  mutate(
    Value_factor = cut(
      Value,
      breaks = c(-Inf, 0.10, 0.37, 1.43, Inf),
      labels = c("0.00 - 0.10", "0.10 - 0.37", "0.38 - 1.43", "1.44 - 11.29")
    )
  )

# 7. Define red colors for main plot
light_red <- "#FFCCCC"
med1_red  <- "#FF6666"
med2_red  <- "#CC0000"
dark_red  <- "#660000"

# Add width info
df_combined <- df_combined %>%
  mutate(
    tile_width = ifelse(Microbiome == "Specie Mean", 5, 1),
    tile_height = ifelse(Species == "Microbiome Mean", 1.5, 1)
  )

# color species labels according to families
family_colors <- c(
  "Brassica" = "#08306b",  # Medium blue (matplotlib default)
  "Poaceae"  = "#1f77b4",  # Light blue
  "Remaining"    = "#9ecae1",   # Dark navy blue
  "Microbiome Mean" = "black"
)

# Add family labels in data
df_combined$Family = NA
df_combined$Family[df_combined$Species %in% Brassica] = "Brassica"
df_combined$Family[df_combined$Species %in% Poaceae] = "Poaceae"
df_combined$Family[df_combined$Species %in% remaining_species] = "Remaining"
df_combined$Family[df_combined$Species == "Microbiome Mean"] = "Microbiome Mean"


# Color-code species names by wrapping in <span style="color:...">
species_levels <- unique(df_combined$Species)
species_families_named <- df_combined$Family[match(species_levels, df_combined$Species)]
species_colors <- family_colors[as.character(species_families_named)]

# Construct styled labels
styled_species <- paste0(
  "<span style='color:", species_colors, "'>", species_levels, "</span>"
)

# Apply to axis text
names(styled_species) <- species_levels  # so ggplot knows how to match



# Creat Figure 7: Heatmap with summaries in first row and last column
p = ggplot() +
  # Original MAE values
  geom_tile(
    data = df_combined %>% filter(Value_type == "main"),
    aes(x = Microbiome, y = Species, fill = Value_factor, width = tile_width,height = tile_height)
  ) +
  scale_fill_manual(
    values = c(light_red, med1_red, med2_red, dark_red),
    name = "AE",
    guide = guide_legend(title.position = "top", title.hjust = 0.5)
  ) +
  ggnewscale::new_scale_fill() +
  scale_fill_gradient(
    low = "white", high = "darkgreen",
    name = "Mean AE",
    guide = guide_colorbar(title.position = "top", title.hjust = 0.5)
  )+
  ggnewscale::new_scale_fill() +
  # Mean values (row and column)
  geom_tile(
    data = df_combined %>% filter(Value_type == "mean"),
    aes(x = Microbiome, y = Species, fill = Value, width = tile_width, height = tile_height)
  ) +
  scale_fill_gradient(low = "white", high = "darkgreen", name = "Mean AE") +
  theme_minimal() +
  guides(
    fill = guide_legend(title.position = "top", title.hjust = 0.5)
  ) + scale_y_discrete(labels = styled_species) +
  theme(
    axis.text.x = element_text(size = 3, angle = 90, hjust = 1),
    axis.text.y = ggtext::element_markdown(size = 6),
    legend.position = "top",
    legend.direction = "horizontal",
    legend.box = "horizontal"
  )  


ggsave("figures/mae_specie_micro.pdf", p, width = 12, height = 6)


#---------------------------------------------------------------------
# Create Table 5: 10 Microbiomes that are hardest to predict over species

# 10 microbiomes hardest to predict over all species
sorted_median = sort.int(apply(df_mae[,1:372], 2, quantile, probs = 0.5, na.rm = TRUE),index.return = TRUE, decreasing = TRUE)
names(Y)[sorted_median$ix[1:10]]
med_top_10 = sorted_median$x[1:10]

# 10 microbiomes hardest to predict over Poaceae
sorted_median_poa = sort.int(apply(df_mae[df_mae$Species %in% Poaceae,1:372], 2, quantile, probs = 0.5, na.rm = TRUE),index.return = TRUE, decreasing = TRUE)
names(Y)[sorted_median_poa$ix[1:10]]
med_top_10_poa = sorted_median_poa$x[1:10]

# 10 microbiomes hardest to predict over Brassica
sorted_median_bra = sort.int(apply(df_mae[df_mae$Species %in% Brassica,1:372], 2, quantile, probs = 0.5, na.rm = TRUE),index.return = TRUE, decreasing = TRUE)
names(Y)[sorted_median_bra$ix[1:10]]
med_top_10_bra = sorted_median_bra$x[1:10]

# 10 microbiomes hardest to predict over Remaining species
sorted_median_rem = sort.int(apply(df_mae[!(df_mae$Species %in% c(Brassica, Poaceae)),1:372], 2, quantile, probs = 0.5, na.rm = TRUE),index.return = TRUE, decreasing = TRUE)
names(Y)[sorted_median_rem$ix[1:10]]
med_top_10_rem = sorted_median_rem$x[1:10]

# Create Table 4
table_micro = data.frame(names(med_top_10), med_top_10, names(med_top_10_bra), med_top_10_bra, names(med_top_10_poa), med_top_10_poa, names(med_top_10_rem), med_top_10_rem)

# Convert to LaTeX format
latex_table <- kable(table_micro, format = "latex", booktabs = TRUE, align = "c", col.names = NULL, digits = 3, row.names = FALSE)

# Print LaTeX table
cat(latex_table)


#---------------------------------------------------------------------
# Table A1: Mapping of OTU numbers to names

unique_numbers = sort(unique(c(sorted_median$ix[1:10], sorted_median_bra$ix[1:10], sorted_median_poa$ix[1:10], sorted_median_rem$ix[1:10])))
colnames_Y = colnames(Y)[unique_numbers]

df_mapping = data.frame("OTU number" = paste0("M", unique_numbers), "OTU name" = colnames_Y)
kable(df_mapping, format = "latex", booktabs = TRUE, align = "c", row.names = FALSE)
