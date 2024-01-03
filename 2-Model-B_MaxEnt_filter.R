### maxEnt filter algorithm to avoid over-representation of non-fossiliferous areas

# model is created for ALL of East Turkana
set.seed(199208)
# maxent.fit.cv <- maxent(satellite, coords, args = c("responsecurves", "replicates=10"))
# maxent.fit.cv
maxent.fit <- maxent(satellite, coords, args = c("responsecurves", "writeplotdata"))
maxent.fit

maxent.pred <- predict(maxent.fit, x)

maskingThreshold <- maxent.fit@results["Equal.training.sensitivity.and.specificity.cumulative.threshold",]
maskingThreshold <- as.numeric(round(maskingThreshold) / 100) # cannot be in percentage.

# GLOBAL MAXENT PLOT

lc.ME <- as.matrix(maxent.pred)

classcover.ME <- raster(lc.ME, crs = crs(raster.df), template = raster.df)
class_spdf.ME <- as(classcover.ME, "SpatialPixelsDataFrame")
class_df.ME <- as.data.frame(class_spdf.ME)
colnames(class_df.ME) <- c("fossils", "longitude", "latitude")

ggME <- ggplot() +
  geom_raster(data = class_df.ME, aes(longitude, latitude, fill = fossils)) +
  scale_fill_viridis_c(option = 'B') + coord_sf() + ggtitle("B) MaxEnt Model") +
  theme_minimal()

ggsave(filename = "Final_MaxEnt_EastTurkana.tiff", plot = ggME, device = grDevices::tiff,
       scale = 2, width = 1200, height = 1920, units = "px", dpi = 360)

mask.eastTurkana <- lc.ME > 0.1 # get values for intersection

# GLOBAL PLOT FILTERED

lc.filterET <- mask.eastTurkana * lc.BR # multiply by logical to get zeros
lc.filterET[lc.filterET == 0] <- NA # turn zeros into NAs

classcoverET <- raster(lc.filterET, crs = crs(satellite), template = satellite)
class_spdf <- as(classcoverET, "SpatialPixelsDataFrame")
class_df <- as.data.frame(class_spdf)
colnames(class_df) <- c("value", "longitude", "latitude")

ggF.ET <- ggplot() + 
  geom_raster(data = class_df, aes(longitude, latitude, fill = value)) + 
  scale_fill_viridis_c(direction = -1, labels = scales::percent) +
  labs(fill = "aquatic") + ggtitle("C) PaleoEnv model") +
  geom_sf(data = all_areas, alpha = 0, colour = 'black', size = 0.3) +
  coord_sf(xlim = c(zone[1:2]), ylim = zone[3:4]) +
  geom_sf_text(data = all_areas, aes(label = Area, size = Shape_Area)) +
  guides(size = "none") +
  theme_minimal()

ggsave(filename = "Final_Filtered_EastTurkana.tiff", plot = ggF.ET, device = grDevices::tiff,
       scale = 2, width = 1920, height = 1080, units = "px", dpi = 360)


library(gridExtra)

gridFigure4 <- grid.arrange(ggBR, ggME, ggF.ET, ncol = 3)

ggsave(filename = "Figure_4_global_results.png", plot = gridFigure4, scale = 1.3,
       bg = "white", width = 12, height = 6, dpi = 360)

# Then the single model we created predicts fossil 3 regions of interest:
maxent.pred.Ileret <- predict(maxent.fit, x.Ileret)

maxent.pred.Karari <- predict(maxent.fit, x.Karari)

maxent.pred.KoobiFora <- predict(maxent.fit, x.KoobiFora)

# Mask Ileret

lc.Ileret <- as.matrix(as.numeric(maxent.pred.Ileret))
classcover <- raster(lc.Ileret, crs = crs(raster.brick), template = raster.Ileret)
class_spdf <- as(classcover, "SpatialPixelsDataFrame")
class_df <- as.data.frame(class_spdf)
colnames(class_df) <- c("fossiliferous", "longitude", "latitude")

xy_points <- as.data.frame(crop(coords, pred.zone1))

ggME.Ileret <- ggplot() + 
  geom_raster(data = class_df, aes(longitude, latitude, fill = fossiliferous)) + 
  scale_fill_viridis_c(option = 'B') +
  geom_sf(data = ILERET_areas, alpha = 0, colour = '#e056fd', size = 1.2) +
  geom_sf_text(data = ILERET_areas, aes(label = Area), nudge_y = 170, 
               nudge_x = -140, size = 5, colour = "black") +
  geom_sf_text(data = ILERET_areas, aes(label = Area), nudge_y = 190, 
               nudge_x = -160, size = 5, colour = "white") +
  theme_minimal()

ggsave(filename = "Figure_8_Ileret_MaxEnt.png", plot = ggME.Ileret, bg = "white", 
       scale = 2, width = 1920, height = 1080, units = "px", dpi = 360)

mask.Ileret <- lc.Ileret > maskingThreshold # get values for intersection

# Mask Karari

lc.Karari <- as.matrix(as.numeric(maxent.pred.Karari))
classcover <- raster(lc.Karari, crs = crs(raster.brick), template = raster.Karari)
class_spdf <- as(classcover, "SpatialPixelsDataFrame")
class_df <- as.data.frame(class_spdf)
colnames(class_df) <- c("fossiliferous", "longitude", "latitude")

xy_points <- as.data.frame(crop(coords, pred.zone2))

ggME.Karari <- ggplot() + 
  geom_raster(data = class_df, aes(longitude, latitude, fill = fossiliferous)) + 
  scale_fill_viridis_c(option = 'B') +
  geom_sf(data = KARARI, alpha = 0, aes(color = Area), size = 1.2) +
  scale_color_manual(values = '#e056fd') +
  theme_minimal()

ggsave(filename = "Figure_12_Karari_MaxEnt.png", plot = ggME.Karari, bg = "white", 
       scale = 2, width = 1920, height = 1080, units = "px", dpi = 360)

mask.Karari <- lc.Karari > maskingThreshold # get values for intersection

# Mask Koobi Fora

lc.KoobiFora <- as.matrix(as.numeric(maxent.pred.KoobiFora))
classcover <- raster(lc.KoobiFora, crs = crs(raster.brick), template = raster.KoobiFora)
class_spdf <- as(classcover, "SpatialPixelsDataFrame")
class_df <- as.data.frame(class_spdf)
colnames(class_df) <- c("fossiliferous", "longitude", "latitude")

xy_points <- as.data.frame(crop(coords, pred.zone3))

ggME.KoobiFora <- ggplot() +
  geom_raster(data = class_df, aes(longitude, latitude, fill = fossiliferous)) +
  scale_fill_viridis_c(option = 'B') +
  geom_sf(data = KOOBIFORA, alpha = 0, aes(color = Area), size = 1.2) +
  scale_color_manual(values = '#e056fd') +
  theme_minimal()

ggsave(filename = "Figure_15_KoobiFora_MaxEnt.png", plot = ggME.KoobiFora, bg = "white", 
       scale = 2, width = 1920, height = 1080, units = "px", dpi = 360)

mask.KoobiFora <- lc.KoobiFora > maskingThreshold # get values for intersection

# The idea of doing a MaxEnt "presence" model is so we can filter the output of the percentage based-models.

# ILERET BETA REG

lc.betaIleret <- as.matrix(as.numeric(betareg.pred.Ileret))


classcover <- raster(lc.betaIleret, crs = crs(raster.brick), template = raster.Ileret)
class_spdf <- as(classcover, "SpatialPixelsDataFrame")
class_df <- as.data.frame(class_spdf)
colnames(class_df) <- c("value", "longitude", "latitude")

ggBR.Ileret <- ggplot() + 
  geom_raster(data = class_df, aes(longitude, latitude, fill = value)) + 
  scale_fill_viridis_c(direction = -1, labels = scales::percent) +
  labs(fill = "aquatic") +
  geom_sf(data = ILERET_areas, alpha = 0, colour = '#e056fd', size = 1.2) +
  geom_sf_text(data = ILERET_areas, aes(label = Area), nudge_y = 170, 
               nudge_x = -140, size = 5, colour = "black") +
  geom_sf_text(data = ILERET_areas, aes(label = Area), nudge_y = 190, 
               nudge_x = -160, size = 5, colour = "white") +
  theme_minimal()

ggsave(filename = "Figure_7_Ileret_BetaReg.png", plot = ggBR.Ileret, bg = "white", 
       scale = 2, width = 1920, height = 1080, units = "px", dpi = 360)

# ILERET FILTERED


lc.filterIleret <- mask.Ileret * lc.betaIleret # multiply by logical to get zeros
lc.filterIleret[lc.filterIleret == 0] <- NA # turn zeros into NAs

classcover <- raster(lc.filterIleret, crs = crs(raster.brick), template = raster.Ileret)
class_spdf <- as(classcover, "SpatialPixelsDataFrame")
class_df <- as.data.frame(class_spdf)
colnames(class_df) <- c("value", "longitude", "latitude")

library(ggnewscale)

ggF.Ileret <- ggplot() +
  geom_raster(data = class_df, aes(longitude, latitude, fill = value)) + 
  scale_fill_viridis_c(direction = -1, labels = scales::percent) +
  labs(fill = "aquatic") +
  geom_sf(data = ILERET_areas, alpha = 0, colour = '#e056fd', size = 1.2) +
  geom_sf_text(data = ILERET_areas, aes(label = Area), nudge_y = 180, 
               nudge_x = -150, size = 5, colour = "white") +
  geom_sf_text(data = ILERET_areas, aes(label = Area), nudge_y = 190, 
               nudge_x = -160, size = 5, colour = "black") +
  theme_minimal()

ggsave(filename = "Figure_9_Ileret_PaleoEnv.tiff", plot = ggF.Ileret, device = grDevices::tiff,
       scale = 2, width = 1920, height = 1080, units = "px", dpi = 360)


# KARARI BETA REG

lc.betaKarari <- as.matrix(as.numeric(betareg.pred.Karari))

classcover <- raster(lc.betaKarari, crs = crs(raster.brick), template = raster.Karari)
class_spdf <- as(classcover, "SpatialPixelsDataFrame")
class_df <- as.data.frame(class_spdf)
colnames(class_df) <- c("value", "longitude", "latitude")

xy_points <- as.data.frame(crop(coords, pred.zone2))

ggBR.Karari <- ggplot() + 
  geom_raster(data = class_df, aes(longitude, latitude, fill = value)) + 
  scale_fill_viridis_c(direction = -1, labels = scales::percent) +
  labs(fill = "aquatic") +
  geom_sf(data = KARARI, alpha = 0, aes(color = Area), size = 1.2) +
  scale_color_manual(values = '#e056fd') +
  theme_minimal()

ggsave(filename = "Figure_11_Karari_BetaReg.png", plot = ggBR.Karari, bg = "white", 
       scale = 2, width = 1920, height = 1080, units = "px", dpi = 360)

# KARARI FILTERED


lc.filterKarari <- mask.Karari * lc.betaKarari
lc.filterKarari[lc.filterKarari == 0] <- NA

classcover <- raster(lc.filterKarari, crs = crs(raster.brick), template = raster.Karari)
class_spdf <- as(classcover, "SpatialPixelsDataFrame")
class_df <- as.data.frame(class_spdf)
colnames(class_df) <- c("value", "longitude", "latitude")

ggF.Karari <- ggplot() + 
  geom_raster(data = class_df, aes(longitude, latitude, fill = value)) +
  scale_fill_viridis_c(direction = -1, labels = scales::percent) +
  labs(fill = "aquatic") +
  geom_sf(data = KARARI, alpha = 0, aes(color = Area), size = 1.2) +
  scale_color_manual(values = '#e056fd') +
  theme_minimal()

ggsave(filename = "Figure_13_Karari_PaleoEnv.png", plot = ggF.Karari, 
       scale = 2, width = 1920, height = 1080, units = "px", dpi = 360)

# KOOBI FORA BETA REG


lc.betaKoobiaFora <- as.matrix(as.numeric(betareg.pred.KoobiFora))


classcover <- raster(lc.betaKoobiaFora, crs = crs(raster.brick), template = raster.KoobiFora)
class_spdf <- as(classcover, "SpatialPixelsDataFrame")
class_df <- as.data.frame(class_spdf)
colnames(class_df) <- c("value", "longitude", "latitude")

xy_points <- as.data.frame(crop(coords, pred.zone3))

ggBR.KoobiFora <- ggplot() + 
  geom_raster(data = class_df, aes(longitude, latitude, fill = value)) + 
  scale_fill_viridis_c(direction = -1, labels = scales::percent) +
  labs(fill = "aquatic") +
  geom_sf(data = KOOBIFORA, alpha = 0, aes(color = Area), size = 1.2) +
  scale_color_manual(values = '#e056fd') +
  theme_minimal()

ggsave(filename = "Figure_14_KoobiFora_BetaReg.png", plot = ggBR.KoobiFora, bg = "white", 
       scale = 2, width = 1920, height = 1080, units = "px", dpi = 360)


# KOOBI FORA FILTERED


lc.filterKoobiaFora<- mask.KoobiFora * lc.betaKoobiaFora
lc.filterKoobiaFora[lc.filterKoobiaFora == 0] <- NA
classcover <- raster(lc.filterKoobiaFora, crs = crs(raster.brick), template = raster.KoobiFora)
class_spdf <- as(classcover, "SpatialPixelsDataFrame")
class_df <- as.data.frame(class_spdf)
colnames(class_df) <- c("value", "longitude", "latitude")

ggF.KoobiFora <- ggplot() + 
  geom_raster(data = class_df, aes(longitude, latitude, fill = value)) + 
  scale_fill_viridis_c(direction = -1, labels = scales::percent) +
  labs(fill = "aquatic") +
  geom_sf(data = KOOBIFORA, alpha = 0, aes(color = Area), size = 1.2) +
  scale_color_manual(values = '#e056fd') +
  theme_minimal()

ggsave(filename = "Figure_16_KoobiFora_PaleoEnv.tiff", plot = ggF.KoobiFora, device = grDevices::tiff,
       scale = 2, width = 1920, height = 1080, units = "px", dpi = 360)





############## MAX ENT VALIDATION ############

# Read in KML file and assign CRS to sf object
PaleoTurkana_db <- st_read('EastTurkanaFossilsCoords.kml')
PaleoTurkana_db <- st_transform(PaleoTurkana_db, crs = crs)

# Extract x and y coordinates and create new Spatialxy_points object
PaleoTurkana_xy <- data.frame(st_coordinates(PaleoTurkana_db)[, 1:2])
PaleoTurkana_sp <- st_as_sf(PaleoTurkana_xy, coords = c("X", "Y"))

presences <- extract(satellite, PaleoTurkana_sp)
noobs <- nrow(presences)

# Generate random xy_points outside the presence xy_points
PaleoTurkana_abs <- dismo::randomPoints(satellite, n = noobs, ext = zone)
# PT_absences <- rasterize(PaleoTurkana_abs, satellite)

absences <- extract(satellite, PaleoTurkana_abs)

# Evaluate MaxEnt model using validation dataset
eval <- evaluate(p = presences, a = absences, model = maxent.fit) # find optimal threshold
optimal.threshold <- eval@t[which.max(eval@TPR + eval@TNR)]

# View evaluation metrics
eval

threshold(eval)

plot(eval, 'ROC')
plot(eval, 'TPR')
boxplot(eval)
density(eval)

# re-run with optimal threshold
final.eval <- evaluate(p = presences, a = absences, model = maxent.fit, tr = optimal.threshold) 

final.eval@confusion

# Extract confusion matrix
conf_matrix <- final.eval@confusion

# General percentages
conf_matrix / (length(final.eval@presence) + length(final.eval@absence)) * 100

# Calculate Sensitivity (TPR)
sensitivity <- conf_matrix[1, "tp"] / sum(conf_matrix[1, c("tp", "fn")])

# Calculate NPV
npv <- conf_matrix[1, "tn"] / sum(conf_matrix[1, c("tn", "fn")])

# Print results
cat("Sensitivity (TPR):", sensitivity, "\n")
cat("NPV:", npv, "\n")

auc_ME <- data.frame(TPR = eval@TPR, FPR = eval@FPR)

aucME <- ggplot(data = auc_ME, aes(x = FPR, y = TPR)) + 
  geom_point(color = "#69b3a2", alpha = 0.1) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(x = "False Positive Rate (1 - Specificity)",
       y = "True Positive Rate (Sensitivity)",
       title = paste("MaxEnt ROC Curve, AUC =", round(eval@auc, 3), "for validation set")) +
  annotate("text", x = 0.5, y = 0.5, label = "Random classifier\n(AUC = 0.5)",
           size = 4, color = "gray50", angle = 45, hjust = 0.5, vjust = 0.5) +
  theme_minimal()

ggsave(filename = "Final_AUC_MaxEnt.tiff", plot = aucME, bg = "white",
       scale = 0.4, width = 12, height = 12, dpi = 360)
