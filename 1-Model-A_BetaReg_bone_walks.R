# DATA IS TOO INTENSIVE ON MEMORY FOR MOST PC
# DO NOT SOURCE, source('LoadData.R')
# READ DIRECTLY FROM LoadData.R instead
# FIGURE 01 / BARPLOT Aquatic vs terrestrial faunal input across bonewalks

gg1 <- ggplot(bonewalks) +
  geom_col(aes(x = POI, y = aquatic, fill = Region)) +
  xlab("Bonewalks by # POI") + scale_fill_viridis_d() +
  ylab("Percentage of aquatic fauna") +
  # ggtitle("Aquatic vs terrestrial faunal input across bonewalks") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()

ggsave(filename = "Figure_3_Bonewalks.tiff", plot = gg1, device = grDevices::tiff,
       scale = 1, width = 2060, height = 900, units = "px", dpi = 360)

# X features predict Y target variable

XVALUES <- raster::extract(satellite, coords) # extract values by coordinate location for modeling
aquatic_ratio <- bonewalks$aquatic
betareg_dataset <- data.frame(cbind(aquatic_ratio, XVALUES))

# beta regression models do not accept values of 100% or 0%. Despite not having
# any in our case, this makes the code reproducible for future data collection:

betareg_dataset <- betareg_dataset[!betareg_dataset$aquatic_ratio == 1,] # remove 100% values
betareg_dataset <- betareg_dataset[!betareg_dataset$aquatic_ratio == 0,] # remove 0% values


### DATA MODELLING: Aquatic ratio with Beta Regression

library(betareg)
betafit <-  betareg(aquatic_ratio ~ ., data = betareg_dataset, link = "logit")

summary(betafit)
str(data.frame(x.Ileret))

betareg.pred.Ileret <- predict(betafit, data.frame(x.Ileret)) 
betareg.pred.Karari <- predict(betafit, data.frame(x.Karari)) 
betareg.pred.KoobiFora <- predict(betafit, data.frame(x.KoobiFora))

betareg.pred <- predict(betafit, data.frame(x))

write.table(round(summary(betafit)$coefficients$mean, 5), "beta_reg_coefs.txt", quote = FALSE, sep = ",")

######

# General Beta Regression output for East Turkana

lc.BR <- as.matrix(as.numeric(betareg.pred))

classcover.BR <- raster(lc.BR, crs = crs(raster.df), template = raster.df)
class_spdf.BR <- as(classcover.BR, "SpatialPixelsDataFrame")
class_df.BR <- as.data.frame(class_spdf.BR)
colnames(class_df.BR) <- c("value", "longitude", "latitude")

ggBR <- ggplot() +
  geom_raster(data = class_df.BR, aes(longitude, latitude, fill = value)) +
  scale_fill_viridis_c(direction = -1, labels = scales::percent) +
  labs(fill = "aquatic") + coord_sf() + ggtitle("A) BetaReg Model") +
  theme_minimal()

ggsave(filename = "Final_BetaReg_EastTurkana2.tiff", plot = ggBR, device = grDevices::tiff,
       scale = 2, width = 1080, height = 1920, units = "px", dpi = 360)

# General stats:


# A leave-one-out cross-validation (LOOCV) procedure for the beta regression model

# Get the number of observations in the betareg_dataset data frame
nobs <- nrow(betareg_dataset)

# Create an empty vector to store the predicted values for each iteration
predicted <- vector()

# Loop over each observation in the betareg_dataset data frame
for (i in 1:nobs){
  # Fit a beta regression model on all observations except the ith observation
  betafit.test <-  betareg(aquatic_ratio ~ ., data = betareg_dataset[-i,], link = "logit")
  # Predict the response value for the ith observation using the fitted model
  predicted[i] <- predict(betafit.test, betareg_dataset[i,])
}

# Combine the betareg_dataset data frame with the vector of predicted values
# and calculate the absolute error for each observation
error.table <- cbind(betareg_dataset, predicted)
error.table$error <- abs(error.table$aquatic_ratio - error.table$predicted)

# Print summary statistics of the absolute error
summary(error.table$error)
brk.loocv <- seq(0, 100, by = 10)

# Create a histogram of the absolute error using ggplot2
ggplot(error.table, aes(x = error * 100)) + 
  geom_histogram(aes(y = ..count..),
                 fill = "#2c3e50", breaks = brk.loocv, alpha = 0.75) +
  geom_density(aes(y = ..density..*nrow(error.table)*(brk.loocv[2]-brk.loocv[1])),
               color = "red", size = 1) +   # adds a density curve
  scale_x_continuous(breaks = brk.loocv[c(TRUE, FALSE)]) + # removes every other idx
  scale_y_continuous(breaks = seq(0, 20, by = 2), limits = c(0, 20)) +
  xlab("Error (%)") + ylab("Count") + theme_minimal()

# The scale of the density curve is proportional to the density of observations at each point along the x-axis. In other words, the density curve shows the relative frequency of observations at different error values.
# The density scale is not directly comparable to the count scale on the y-axis of the histogram. The count scale shows the number of observations in each bin of the histogram, while the density scale shows the relative frequency of observations at each point along the x-axis.
# It's also worth noting that the area under the density curve is equal to 1, since the curve represents the probability density function of the distribution. This means that the height of the density curve at a given point represents the probability density of observing an error value near that point.
ggsave("SF1_BetaReg_LOOCV.png", plot = last_plot(), width = 12, scale = 0.5,
       bg = 'white', dpi = 'retina')

##########


# Create an empty vector called pvalues
pvalues <- vector()

# Loop over 10,000 iterations
for (i in 1:10000){
  # Generate a random sample of 45 observations from a uniform distribution and calculate the t-test p-value
  pvalues[i] <- t.test(runif(45), error.table$error)$p.value
}

# Calculate the mean of the p-values from the t-tests
mean(pvalues)

# This part of the code is generating 10,000 random samples of 45 observations from a uniform distribution and performing a t-test on each sample to compare the mean of the errors (calculated earlier) to a hypothetical mean of zero. The p-values from the t-tests are stored in the pvalues vector. Finally, the code calculates the mean of the p-values. This mean p-value can be used to assess the significance of the difference between the mean error and zero, and thus the accuracy of the beta regression model.