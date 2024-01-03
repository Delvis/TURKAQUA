###############

# Convert to sf object and coerce to XY dimensions
study_sf <- st_zm(study_areas, drop = TRUE)
study_names <- study_sf$Area

# raster for stats
mask.stats <- lc.ME > maskingThreshold
lc.filt.stats <- mask.stats * lc.BR
lc.filt.stats[lc.filt.stats == 0] <- NA # turn zeros into NAs
raster.stats <- raster(lc.filt.stats, crs = crs(satellite), template = satellite)


# Extract raster separated by all ET areas
study_sp <- as(study_sf, "Spatial")
poly.rasters <- extract(raster.stats, study_sp)
names(poly.rasters) <- study_names

# calculate statistics
stats <- sapply(poly.rasters, function(x) if (!is.null(x)) summary(x) else NA )

Total <- sapply(poly.rasters, function(x) length(x))
fossil_cells <- sapply(poly.rasters, function(x) sum(!is.na(x)))
Density <- round(fossil_cells / Total, 4) * 100

stats_summary <- data.frame(t(rbind(stats, Total, Density)))
stats_summary <- stats_summary[order(-stats_summary$Density),] # ordered by maxent predicted fossil cells (cut-off)
colnames(stats_summary) <- c("Min", "LQ", "Median", "Mean", "UQ", "Max", "Empty", "Total", "Density")
stats_summary[1:6] <- round(stats_summary[1:6],4) * 100 # round decimals, convert to percentage

write.table(stats_summary, file = "stats_summary.txt", sep = ",", quote = FALSE, row.names = T)

library(ggplot2)
library(reshape2)

# Remove NA values from the list of matrices
poly.rasters_filtered <- lapply(poly.rasters, function(x) x[!is.na(x)])
# Convert the filtered list of matrices into a long format data frame
poly.rasters_df <- melt(poly.rasters_filtered, value.name = "value")
# Add a column indicating the matrix index
poly.rasters_df$matrix <- rep(1:length(study_areas$Area), times = sapply(poly.rasters_filtered, length))
# Get the names of the density vector
density_names <- names(Density)
# Add the density values to the data frame
poly.rasters_df$density <- Density[density_names[as.numeric(poly.rasters_df$matrix)]]

# Use the density variable to adjust the relative width of the boxplots
BBaqua <- ggplot(
  poly.rasters_df,
  aes(x = reorder(factor(L1), value, mean), y = value, fill = density)
  ) +
  scale_y_continuous(labels = scales::percent) +
  geom_boxplot(varwidth = TRUE) +
  scale_fill_viridis_c(option = "A") +
  labs(x = "Areas", y = "Aquatic (%)", fill = "Cells (%)") +
  stat_summary(fun = mean, geom = "point", shape = 23, color = "black", size = 4) +
  theme_minimal()

ggsave(filename = "Figure_10_BBaqua.png", plot = BBaqua,
       scale = 2, width = 1920, height = 1080, units = "px", dpi = 360, bg = "white")


############

# Function to calculate percent contribution for each predictor in a betareg model

# Calculate percent contribution using a jackknife procedure
vars <- colnames(betareg_dataset)[-1]  # get variable names
pct_contrib <- numeric(length(vars))  # initialize percent contribution vector

for (i in seq_along(vars)) {
  # fit model without the ith variable
  formula_i <- as.formula(paste0("aquatic_ratio ~", paste(vars[-i], collapse = "+")))
  betareg_i <- betareg(formula_i, data = betareg_dataset, link = "logit")

  # calculate percent contribution
  pct_contrib[i] <- 100 * (logLik(betafit) - logLik(betareg_i)) / logLik(betafit)
}

normalized_contrib <- 100 * pct_contrib / sum(pct_contrib)

# Plot percent contribution
barplot(normalized_contrib, names.arg = vars, xlab = "Variable", ylab = "% Contribution")

BR.pct_contrib <- data.frame(Variable = vars,
                             PercentContribution = normalized_contrib)
BR.pct_contrib$Variable <- factor(BR.pct_contrib$Variable, levels = vars)

                      
# Plot Betareg bar chart
vimpBR1 <- ggplot(BR.pct_contrib, aes(x = Variable, y = PercentContribution, color = Variable)) +
  geom_segment(aes(x = Variable, xend = Variable, y = 0, yend = PercentContribution), size = 6) +
  scale_color_manual(values = clrplts, guide = "none") +
  xlab("Features for paleoenvironmental reconstruction") + ylab("A) Percent contribution (BetaReg)") + coord_flip() +
  theme_minimal()

# Max Ent Percent contribution

plot(maxent.fit)
ME.pct_contrib <- plot(maxent.fit)
ME.pct_contrib <- data.frame(Variable = names(ME.pct_contrib),
                             PercentContribution = ME.pct_contrib)

# Reorder the variables in the data frame based on vars_correct_order list
ME.pct_contrib$Variable <- factor(ME.pct_contrib$Variable, levels = vars)

# Plot Maxent bar chart
vimpME1 <- ggplot(ME.pct_contrib, aes(x = Variable, y = PercentContribution, color = Variable)) +
  geom_segment(aes(x = Variable, xend = Variable, y = 0, yend = PercentContribution), size = 6) +
  scale_color_manual(values = clrplts, guide = "none") +
  xlab("Features predicting fossil distribution") + ylab("B) Percent contribution (MaxEnt)") + coord_flip() +
  theme_minimal()

library(gridExtra)
gridVarImp <- grid.arrange(vimpBR1, vimpME1, ncol = 2)

ggsave(filename = "Figure_6_varImp.png", plot = gridVarImp, scale = 1,
       width = 3200, height = 1400, units = "px", dpi = 360)
