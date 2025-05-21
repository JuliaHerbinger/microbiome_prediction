# Analysis for Section 4.1: Statistical Analysis

#---------------------------------------------------------------------
# Calculate JSD for microbiome data
dist_jsd = matrix(nrow = nrow(Y), ncol = nrow(Y) ) 
for(i in 1:nrow(Y)){
  subset_wo_i = (1:nrow(Y))[-i]
  for(j in subset_wo_i){
    dist_jsd[i,j] = JSD(rbind(unlist(Y[i,2:ncol(Y)]), unlist(Y[j,2:ncol(Y)])), unit = "log", est.prob = "empirical", test.na = FALSE)
  }
  dist_jsd[i,i] = 0
}
rownames(dist_jsd) = colnames(dist_jsd) = rownames(Y)


#---------------------------------------------------------------------
# Calculate phylogenetic distance based on Hamming distance

dist_matrix <- apply(X, 1, function(instance_1) {
  apply(X, 1, function(instance_2) {
    sum(instance_1 != instance_2)/ncol(X)
  })
})
rownames(dist_matrix) = colnames(dist_matrix) = rownames(X)

#---------------------------------------------------------------------
# Mantel Test

# For all species
m_pearson_all = mantel(dist_matrix, dist_jsd, method = "pearson", permutations = 1000)
m_spear_all = mantel(dist_matrix, dist_jsd, method = "spear", permutations = 1000)

# Poaceae species subset
idx_poa = which(rownames(X) %in% Poaceae)
m_pearson_poa = mantel(dist_matrix[idx_poa, idx_poa], dist_jsd[idx_poa, idx_poa], method = "pearson", permutations = 1000)
m_spear_poa = mantel(dist_matrix[idx_poa, idx_poa], dist_jsd[idx_poa, idx_poa], method = "spear", permutations = 1000)

# Brassica species subset
idx_bra = which(rownames(X) %in% Brassica)
m_pearson_bra = mantel(dist_matrix[idx_bra, idx_bra], dist_jsd[idx_bra, idx_bra], method = "pearson", permutations = 1000)
m_spear_bra = mantel(dist_matrix[idx_bra, idx_bra], dist_jsd[idx_bra, idx_bra], method = "spear", permutations = 1000)

# Remaining species subset
idx_rem = which(rownames(X) %in% Remaining)
m_pearson_rem = mantel(dist_matrix[idx_rem, idx_rem], dist_jsd[idx_rem, idx_rem], method = "pearson", permutations = 1000)
m_spear_rem = mantel(dist_matrix[idx_rem, idx_rem], dist_jsd[idx_rem, idx_rem], method = "spear", permutations = 1000)

#---------------------------------------------------------------------
# Create data frame for Table 1 and Figure 3 in Manuscript
df_mantel = data.frame("Method" = rep(c("Pearson", "Spearman"), 4), 
                       "Statistic" = c(m_pearson_all$statistic,m_spear_all$statistic, 
                                       m_pearson_bra$statistic,m_spear_bra$statistic, 
                                       m_pearson_poa$statistic, m_spear_poa$statistic, 
                                       m_pearson_rem$statistic, m_spear_rem$statistic ), 
                       "Significance" = c(m_pearson_all$signif, m_spear_all$signif, 
                                          m_pearson_bra$signif, m_spear_bra$signif, 
                                          m_pearson_poa$signif, m_spear_poa$signif, 
                                          m_pearson_rem$signif, m_spear_rem$signif),
                       "Family" = rep(c("All", "Brassica", "Poaceae", "Remaining"),each = 2))


#---------------------------------------------------------------------
# Create Table 1
df_mantel <- df_mantel %>%
  mutate(Result = paste0(round(Statistic, 3), " (p = ", signif(Significance, 2), ")"))

df_wide_bracket <- df_mantel %>%
  dplyr::select(Method, Family, Result) %>%
  pivot_wider(names_from = Family, values_from = Result)


kable(df_wide_bracket, format = "latex", booktabs = TRUE, caption = "Mantel Test Results by Family (Statistic with p-value)") %>%
  kable_styling(latex_options = c("hold_position"))


#---------------------------------------------------------------------
# Create Figure 3


# 1. Extract the lower triangle (excluding diagonal) of each distance matrix as vectors
get_lower_tri <- function(mat) {
  mat[lower.tri(mat)]
}

# all species
phylo_all <- get_lower_tri(as.matrix(dist_matrix))
jsd_all   <- get_lower_tri(as.matrix(dist_jsd))

# Poaceae species
phylo_poa <- get_lower_tri(as.matrix(dist_matrix[idx_poa, idx_poa]))
jsd_poa   <- get_lower_tri(as.matrix(dist_jsd[idx_poa, idx_poa]))

# Brassica species
phylo_bra <- get_lower_tri(as.matrix(dist_matrix[idx_bra, idx_bra]))
jsd_bra   <- get_lower_tri(as.matrix(dist_jsd[idx_bra, idx_bra]))

# Remaining species
phylo_rem <- get_lower_tri(as.matrix(dist_matrix[idx_rem, idx_rem]))
jsd_rem   <- get_lower_tri(as.matrix(dist_jsd[idx_rem, idx_rem]))

# 2. Create a dataframe for plotting
dist_df <- data.frame(Phylogenetic = phylo_all, Microbial_JSD = jsd_all, Family = "all")
dist_df = rbind(dist_df, data.frame(Phylogenetic = phylo_poa, Microbial_JSD = jsd_poa, Family = "Poaceae"))
dist_df = rbind(dist_df, data.frame(Phylogenetic = phylo_bra, Microbial_JSD = jsd_bra, Family = "Brassica"))
dist_df = rbind(dist_df, data.frame(Phylogenetic = phylo_rem, Microbial_JSD = jsd_rem, Family = "Remaining"))

# 3. Scatterplot with trend lines
p_dist_all = ggplot(dist_df[dist_df$Family == "all",], aes(x = Phylogenetic, y = Microbial_JSD)) +
  geom_point(alpha = 0.4) +ylim(0,0.8) +
  geom_smooth(method = "lm", color = "blue", se = FALSE, linetype = "solid") +        # Linear trend (Pearson)
  geom_smooth(method = "loess", color = "red", se = FALSE, linetype = "dashed") +     # Nonlinear trend (Spearman-ish)
  labs(
    title = "All Species",
    x = "Phylogenetic Distance (Hamming)",
    y = "Microbial Distance (JSD)"
  ) +
  theme_minimal()

p_dist_poa = ggplot(dist_df[dist_df$Family == "Poaceae",], aes(x = Phylogenetic, y = Microbial_JSD)) +
  geom_point(alpha = 0.4) +ylim(0,0.8) +
  geom_smooth(method = "lm", color = "blue", se = FALSE, linetype = "solid") +        # Linear trend (Pearson)
  geom_smooth(method = "loess", color = "red", se = FALSE, linetype = "dashed") +     # Nonlinear trend (Spearman-ish)
  labs(
    title = "Poaceae Species",
    x = "Phylogenetic Distance (Hamming)",
    y = "Microbial Distance (JSD)"
  ) +
  theme_minimal()

p_dist_bra = ggplot(dist_df[dist_df$Family == "Brassica",], aes(x = Phylogenetic, y = Microbial_JSD)) +
  geom_point(alpha = 0.4) +ylim(0,0.8) +
  geom_smooth(method = "lm", color = "blue", se = FALSE, linetype = "solid") +        # Linear trend (Pearson)
  geom_smooth(method = "loess", color = "red", se = FALSE, linetype = "dashed") +     # Nonlinear trend (Spearman-ish)
  labs(
    title = "Brassica Species",
    x = "Phylogenetic Distance (Hamming)",
    y = "Microbial Distance (JSD)"
  ) +
  theme_minimal()

p_dist_rem = ggplot(dist_df[dist_df$Family == "Remaining",], aes(x = Phylogenetic, y = Microbial_JSD)) +
  geom_point(alpha = 0.4) + ylim(0,0.8) +
  geom_smooth(method = "lm", color = "blue", se = FALSE, linetype = "solid") +        # Linear trend (Pearson)
  geom_smooth(method = "loess", color = "red", se = FALSE, linetype = "dashed") +     # Nonlinear trend (Spearman-ish)
  labs(
    title = "Remaining Species",
    x = "Phylogenetic Distance (Hamming)",
    y = "Microbial Distance (JSD)"
  ) +
  theme_minimal()

#---------------------------------------------------------------------
# Save Figure 3
p = p_dist_all +p_dist_bra + p_dist_poa + p_dist_rem + plot_layout(guides = "collect") 

ggsave("figures/micr_vs_phylo_distance.pdf", p, width = 9, height = 6)
