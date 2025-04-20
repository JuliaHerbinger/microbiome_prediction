# Load required packages
library(ggplot2)

# Sample data
set.seed(123)
df_mae <- abs(data_truth_knn - data_response_knn)
df_mae$row_ids = NULL
colnames(df_mae) = paste0("M", 1:ncol(df_mae))
df_mae$Species = rownames(X)
library(data.table)
df_mae_plot <- melt(setDT(df_mae), id.vars = c("Species"), variable.name = "Microbiome")

# df_mae_plot$Value_transformed <- log(df_mae_plot$value + 0.001) 
# # Create ggplot
# p = ggplot(df_mae_plot, aes(x = Microbiome, y = Species, fill = value)) +
#   geom_tile() +
#   #scale_fill_viridis_c(option = "inferno") +  # Customize color scale
#   #scale_fill_gradient(low = "white", high = "blue")  +
#   scale_fill_gradient(low = "white", high = "darkred", trans = "pseudo_log") + 
#   theme_minimal() +
#   labs(
#        fill = "MAE") +
#   theme(
#     axis.text.x = element_text(size = 3,angle = 90, hjust = 1),  # Adjust x-axis label size
#     axis.text.y = element_text(size = 6)   # Adjust y-axis label size
#   )
# 

# 
# light_red <- rgb(250, 245, 245, maxColorValue = 255)
# med1_red <- rgb(225, 170, 170, maxColorValue = 255)
# med2_red <- rgb(180, 80, 80, maxColorValue = 255)
# dark_red <- rgb(125, 0, 0, maxColorValue = 255)
# # Create a new factor column to map to the color scale
# df_mae_plot$Value_factor <- cut(df_mae_plot$value, breaks = quantile(df_mae_plot$value, probs = c(0,0.75,0.85,0.95,1)), include.lowest = TRUE, labels = c(light_red, med1_red,med2_red,dark_red))
# 
# p = ggplot(df_mae_plot, aes(x = Microbiome, y = Species, fill = Value_factor)) +
#   geom_tile() +
#   scale_fill_manual(values = c(light_red, med1_red,med2_red,dark_red),
#                     labels = c("0.00 - 0.10", "0.10 - 0.37", "0.38 - 1.43", "1.44 - 11.29")) + 
# 
#   theme_minimal() +
#   labs(
#     fill = "MAE") +
#   theme(
#     axis.text.x = element_text(size = 3,angle = 90, hjust = 1),  # Adjust x-axis label size
#     axis.text.y = element_text(size = 6),   # Adjust y-axis label size
#     legend.position = "top"
#   )


library(ggnewscale)
library(ggplot2)
library(ggnewscale)
library(data.table)
library(dplyr)
library(forcats)

# Step 1: Start with df_mae_plot
df_mae_plot$Value <- df_mae_plot$value  # Already numeric
remaining_species <- unique(df_mae_plot$Species)[!(unique(df_mae_plot$Species) %in% c(Brassica, Poaceae))]
df_mae_plot$Species <- factor(df_mae_plot$Species, levels = c(Brassica, Poaceae, remaining_species))

# Step 2: Row means (across microbiomes)
row_means <- df_mae_plot %>%
  group_by(Species) %>%
  summarise(Value = mean(Value), .groups = "drop") %>%
  mutate(Microbiome = "Specie Mean", Value_type = "mean")

# Step 3: Column means (across species)
col_means <- df_mae_plot %>%
  group_by(Microbiome) %>%
  summarise(Value = mean(Value), .groups = "drop") %>%
  mutate(Species = "Microbiome Mean", Value_type = "mean")

# Step 4: Main data
df_main <- df_mae_plot %>%
  mutate(Value_type = "main")

# Step 5: Combine all
df_combined <- bind_rows(df_main, row_means, col_means)
df_combined$Species = factor(df_combined$Species, levels = c("Microbiome Mean", Brassica, Poaceae, remaining_species))
df_combined$Microbiome = factor(df_combined$Microbiome, levels = c(paste0("M",1:ncol(Y)),"Specie Mean"))

# Step 6: Reorder factor levels
#df_combined$Species <- fct_relevel(df_combined$Species, "Microbiome Mean", after = 0)
#df_combined$Microbiome <- fct_relevel(df_combined$Microbiome, "Specie Mean", after = Inf)

# Step 7: Bin original values for manual coloring
df_combined <- df_combined %>%
  mutate(
    Value_factor = cut(
      Value,
      breaks = c(-Inf, 0.10, 0.37, 1.43, Inf),
      labels = c("0.00 - 0.10", "0.10 - 0.37", "0.38 - 1.43", "1.44 - 11.29")
    )
  )

# Step 8: Define red colors
light_red <- "#FFCCCC"
med1_red  <- "#FF6666"
med2_red  <- "#CC0000"
dark_red  <- "#660000"

# Add width info: make the "Row Mean" column wider
df_combined <- df_combined %>%
  mutate(
    tile_width = ifelse(Microbiome == "Specie Mean", 5, 1),
    tile_height = ifelse(Species == "Microbiome Mean", 1.5, 1)
  )

# color species labels

family_colors <- c(
  "Brassica" = "#08306b",  # Medium blue (matplotlib default)
  "Poaceae"  = "#1f77b4",  # Light blue
  "Remaining"    = "#9ecae1",   # Dark navy blue
  "Microbiome Mean" = "black"
)
family_colors <- c(
  "Brassica" = "#1E90FF",  # Dodger Blue
  "Poaceae"  = "#DA70D6",  # Orchid (a light purple)
  "Remaining"    = "#FFA500",   # Orange
  "Microbiome Mean" = "black"
)

library(ggtext)

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
  
  


ggsave("mae_specie_micro.pdf", p, width = 12, height = 6)


# which microbiomes are hard to predict
sorted_means = sort.int(colMeans(df_mae[,1:372]),index.return = TRUE, decreasing = TRUE)
names(Y)[sorted_means$ix[1:10]]
sorted_means$x[1:10]

# easiest:
names(Y)[sorted_means$ix[(length(sorted_means$x)-9):length(sorted_means$x)]]
sorted_means$x[(length(sorted_means$x)-9):length(sorted_means$x)]


# by median
sorted_median = sort.int(apply(df_mae[,1:372], 2, quantile, probs = 0.5, na.rm = TRUE),index.return = TRUE, decreasing = TRUE)
names(Y)[sorted_median$ix[1:10]]
med_top_10 = sorted_median$x[1:10]

# easiest:
names(Y)[sorted_median$ix[(length(sorted_means$x)-9):length(sorted_means$x)]]
sorted_median$x[(length(sorted_means$x)-9):length(sorted_means$x)]

# which microbiomes are hard to predict only Poa
sorted_means_poa = sort.int(colMeans(df_mae[df_mae$Species %in% Poaceae,1:372]),index.return = TRUE, decreasing = TRUE)
names(Y)[sorted_means_poa$ix[1:10]]
sorted_means_poa$x[1:10]

sorted_median_poa = sort.int(apply(df_mae[df_mae$Species %in% Poaceae,1:372], 2, quantile, probs = 0.5, na.rm = TRUE),index.return = TRUE, decreasing = TRUE)
names(Y)[sorted_median_poa$ix[1:10]]
med_top_10_poa = sorted_median_poa$x[1:10]

sd_poa = apply(df_mae[df_mae$Species %in% Poaceae,1:372], 2, sd, na.rm = TRUE)
sd_poa[sorted_median_poa$ix[1:10]]

# which microbiomes are hard to predict only Brassica
sorted_means_bra = sort.int(colMeans(df_mae[df_mae$Species %in% Brassica,1:372]),index.return = TRUE, decreasing = TRUE)
names(Y)[sorted_means_bra$ix[1:10]]
sorted_means_bra$x[1:10]

sorted_median_bra = sort.int(apply(df_mae[df_mae$Species %in% Brassica,1:372], 2, quantile, probs = 0.5, na.rm = TRUE),index.return = TRUE, decreasing = TRUE)
names(Y)[sorted_median_bra$ix[1:10]]
med_top_10_bra = sorted_median_bra$x[1:10]

sd_bra = apply(df_mae[df_mae$Species %in% Brassica,1:372], 2, sd, na.rm = TRUE)
sd_bra[sorted_median_bra$ix[1:10]]


# which microbiomes are hard to predict remaining species
sorted_means_rem = sort.int(colMeans(df_mae[!(df_mae$Species %in% c(Brassica, Poaceae)),1:372]),index.return = TRUE, decreasing = TRUE)
names(Y)[sorted_means_rem$ix[1:10]]
sorted_means_rem$x[1:10]

sorted_median_rem = sort.int(apply(df_mae[!(df_mae$Species %in% c(Brassica, Poaceae)),1:372], 2, quantile, probs = 0.5, na.rm = TRUE),index.return = TRUE, decreasing = TRUE)
names(Y)[sorted_median_rem$ix[1:10]]
med_top_10_rem = sorted_median_rem$x[1:10]

sd_rem = apply(df_mae[!(df_mae$Species %in% c(Brassica, Poaceae)),1:372], 2, sd, na.rm = TRUE)
sd_rem[sorted_median_rem$ix[1:10]]

table_micro = data.frame(names(med_top_10), med_top_10, names(med_top_10_bra), med_top_10_bra, names(med_top_10_poa), med_top_10_poa, names(med_top_10_rem), med_top_10_rem)
# Convert to LaTeX format
latex_table <- kable(table_micro, format = "latex", booktabs = TRUE, align = "c", col.names = NULL, digits = 3, row.names = FALSE)

# Print LaTeX table
cat(latex_table)
