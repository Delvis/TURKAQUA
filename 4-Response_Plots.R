# Generate beautiful response plots with ggplot2

# Set the working directory to the folder containing the .dat files
keepoldwd <- getwd()
setwd("C:/Users/delvi/AppData/Local/Temp/Rtmp4cNYmH/raster/maxent/93481033214/plots/")

# Define the partial names to match
match_strings <- c('ultrablue', 'blue', 'green', 'red', 'NIR', 'SWIR1', 'SWIR2',
                   'vegetation', 'elevation', 'slope', 'northness', 'eastness')


file_list <- list.files(pattern = ".dat$")
# Use list.files() to get a list of all .dat files in the folder
file_list1 <- list.files(pattern = "\\_only.dat$")
# remove elements from file_list2 that are in file_list1
file_list2 <- setdiff(file_list, file_list1)


# Create an empty list to store the ggplots
plot_list1 <- list()

# Use grep to match partial names and extract indices
match_indices <- regmatches(file_list1, regexpr(paste(match_strings, collapse = '|'), file_list1))

# Reorder file_list1 based on match_indices
file_list1 <- file_list1[order(match(match_indices, match_strings))]

# Loop through the list of filenames and read each file into a data frame
for (filename in file_list1) {
  # Use the read.table() function to read the file into a data frame with 3 columns
  df_name <- gsub("species_|\\_only.dat", "", filename) # Remove the ".dat" extension from the filename
  df <- read.table(filename, header = TRUE, sep = ",", col.names = c("layer", "x", "y"), colClasses = c("character", "numeric", "numeric"))
  
  # Calculate the x value at which y is maximum
  max_y <- max(df$y)
  max_x <- df$x[which.max(df$y)]
  
  
  # Create a ggplot for the data frame
  p <- ggplot(df, aes(x = x, y = y)) + 
    geom_line(size = 2, color = "#69b3a2") + 
    labs(title = paste(df_name, "(independent model)"), x = "value", y = "likelihood") + 
    scale_x_continuous(
      breaks = c(min(df$x), as.numeric(summary(df$x)[2]),
                 median(df$x), as.numeric(summary(df$x)[5]), 
                 max(df$x))) +
    scale_y_continuous(labels = scales::percent) +
    theme_minimal()
  
  # Add the ggplot to the plot_list
  plot_list1[[df_name]] <- p
}

# Arrange the plots in a grid of 4x3
response_curves1 <- grid.arrange(grobs = plot_list1, ncol = 3)



# Create an empty list to store the ggplots
plot_list2 <- list()

# Reorder file_list1 based on match_indices
file_list2 <- file_list2[order(match(match_indices, match_strings))]

# Loop through the list of filenames and read each file into a data frame
for (filename in file_list2) {
  # Use the read.table() function to read the file into a data frame with 3 columns
  df_name <- gsub("species_|\\.dat", "", filename) # Remove the ".dat" extension from the filename
  df <- read.table(filename, header = TRUE, sep = ",", col.names = c("layer", "x", "y"), colClasses = c("character", "numeric", "numeric"))
  
  # Create a ggplot for the data frame
  p <- ggplot(df, aes(x = x, y = y)) + 
    geom_line(size = 2, color = "#69b3a2") + 
    labs(title = paste(df_name, "(marginal response curve)"), x = "value", y = "likelihood") + 
    scale_x_continuous(
      breaks = c(min(df$x), as.numeric(summary(df$x)[2]),
                 median(df$x), as.numeric(summary(df$x)[5]), 
                 max(df$x))) +
    scale_y_continuous(labels = scales::percent) +
    theme_minimal()
  
  # Add the ggplot to the plot_list
  plot_list2[[df_name]] <- p
}

# Arrange the plots in a grid of 4x3
response_curves2 <- grid.arrange(grobs = plot_list2, ncol = 3)

# back to original folder
setwd(keepoldwd)



# save response curves with independent models
ggsave("SF2_MaxEnt_Independent_Response_Curves.png", response_curves1, bg = "white",
       width = 1600, height = 900, unit = "px", scale = 3, dpi = "retina")

# save response curves with independent models
ggsave("SF3_Response_Curves_final.png", response_curves2, bg = "white",
       width = 1600, height = 900, unit = "px", scale = 3, dpi = "retina")
