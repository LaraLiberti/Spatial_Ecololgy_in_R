# This is an R project for a spatio-ecological multitemporal analysis focusing on fire disturbance and vegetation recovery in the Vesuvius area (Italy).
# The analysis utilizes Normalized Difference Vegetation Index (NDVI) derived from satellite imagery across different time points relative to a fire event.

#  1. Setup: Install and Load Packages & Set Working Directory

# Download and install necessary packages 
install.packages("imageRy") # Installs imageRy, a custom package for remote sensing in R.
install.packages("terra") # 'terra' package for raster data manipulation, a modern successor to 'raster'.
install.packages("viridis") # 'viridis' for aesthetically pleasing and perceptually uniform color palettes.
install.packages("ggplot2") # 'ggplot2' for creating high-quality data visualizations.


# Load all required packages into the current R session.
library(imageRy)
library(terra)
library(viridis)
library(ggplot2)


# Set the working directory where image files are stored.
# This ensures R can find the input data and save output files.
setwd("C:/Users/utente/Desktop/Vesuvio")


# 2. Data Import and NDVI Calculation 

# Load Red and Near-Infrared bands from satellite imagery
# for different time points relative to the fire event in 2017.
Before <- rast("B2017.jpg") # Imagery from before the fire (e.g., early 2017)
During <- rast("D2017.jpg") # Imagery captured during the fire event (e.g., mid-2017)
After <- rast("A2017.jpg") # Imagery immediately after the fire (e.g., late 2017)
After4y <- rast("A2021.jpg") # Imagery 4 years after the fire (2021), showing early recovery
Now <- rast("RN2025.jpg") # Current imagery (2025), showing ongoing recovery or stabilization


# Calculate the Normalized Difference Vegetation Index (NDVI) for all periods.
# NDVI = (NIR - Red) / (NIR + Red)
# This index ranges from -1 to 1, where higher positive values indicate denser, healthier vegetation.
ndviB <- (Before[[1]] - Before[[2]]) / (Before[[1]] + Before[[2]]) # NDVI Pre-Fire
ndviD <- (During[[1]] - During[[2]]) / (During[[1]] + During[[2]]) # NDVI During Fire
ndviA <- (After[[1]] - After[[2]]) / (After[[1]] + After[[2]]) # NDVI Post-Fire
ndviA4y <- (After4y[[1]] - After4y[[2]]) / (After4y[[1]] + After4y[[2]]) # NDVI 4 Years After Fire
ndviN <- (Now[[1]] - Now[[2]]) / (Now[[1]] + Now[[2]]) # NDVI Current (8 years after fire)


# Define a color palette for visualizing NDVI maps.
# 'viridis' provides a perceptually uniform colormap, good for quantitative data.
cl_ndvi <- viridis(100)


# Visualize all NDVI maps in a multi-frame layout (2 rows, 3 columns).
# This allows for a quick visual comparison of vegetation health over time.
par(mfrow = c(2, 3))
plot(ndviB, col = cl_ndvi, main = "NDVI Pre-Fire (2017)")
plot(ndviD, col = cl_ndvi, main = "NDVI During Fire (2017)") # Expect low NDVI in affected areas
plot(ndviA, col = cl_ndvi, main = "NDVI Post-Fire (2017)") # Expect significant decrease in NDVI in burned areas
plot(ndviA4y, col = cl_ndvi, main = "NDVI 4 Years Later (2021)") # Expect signs of recovery
plot(ndviN, col = cl_ndvi, main = "NDVI Current (2025)") # Expect further recovery, approaching pre-fire levels in some areas
dev.off() # Close the graphics device to revert to single plot layout


# 3. Difference Maps for Change Detection 

# Calculate and visualize difference maps to highlight temporal changes in NDVI.
# Positive values indicate an increase in NDVI (e.g., vegetation recovery).
# Negative values indicate a decrease in NDVI (e.g., burning, deforestation).
# Define a diverging color palette to easily interpret changes:
# 'red' indicates a strong decrease in NDVI (likely burned area).
# 'darkgreen' indicates a strong increase in NDVI (likely vegetation recovery).
cl_diff <- colorRampPalette(c("red", "yellow", "grey", "lightgreen", "darkgreen"))(100)


DIF_Pre_Post <- ndviB - ndviA # Change from before the fire to immediately after.
DIF_Post_4y <- ndviA4y - ndviA # Change from immediately after the fire to 4 years later (shows early recovery).
DIF_4y_Now <- ndviN - ndviA4y # Change from 4 years later to current (shows continued recovery/stabilization).


# Visualize all difference maps in a single row, three-column multi-frame.
# This provides a clear temporal sequence of the fire impact and subsequent recovery.
par(mfrow = c(1, 3))
plot(DIF_Pre_Post, col = cl_diff, main = "NDVI Change: Pre vs Post-Fire")
plot(DIF_Post_4y, col = cl_diff, main = "NDVI Change: Post-Fire vs 4 Years Later")
plot(DIF_4y_Now, col = cl_diff, main = "NDVI Change: 4 Years Later vs Current")
dev.off()


# 4. RGB Visualization of NDVI Maps for Temporal Comparison 

# Create an RGB composite image using different NDVI periods as color channels.
# This technique effectively visualizes temporal dynamics by mapping changes to color.
# The chosen bands are R=Now, G=4YearsAfter, B=Before.
stack_ndvi <- c(ndviB, ndviA, ndviA4y, ndviN) # Stacks NDVI layers in chronological order

# Interpret the colors in the resulting RGB composite:
# - Red areas (high R, low G/B) = High NDVI in 2025 ("Now") but lower in previous periods (strong recovery in recent years).
# - Green areas (high G, low R/B) = High NDVI in 2021 ("4 Years After") but lower in 2017 and 2025 (e.g., peak recovery in 2021, slight decline or stabilization).
# - Blue areas (high B, low R/G) = High NDVI in 2017 ("Before") but lower in 2021 and 2025 (areas impacted by fire and not fully recovered, or non-vegetated areas).
# - Yellow areas (Red + Green) = High NDVI in both 2025 and 2021 (sustained high vegetation since 2021).
# - Cyan areas (Green + Blue) = High NDVI in both 2021 and 2017 (areas that were healthy before fire and recovered by 2021).
# - Magenta areas (Red + Blue) = High NDVI in both 2025 and 2017 (areas that were healthy before fire and recovered by 2025, but possibly had a dip in 2021).
# - White areas (Red + Green + Blue) = High NDVI across all three periods (stable, healthy vegetation zones unaffected by fire or quickly recovered).
# - Dark areas = Low NDVI in all three periods (non-vegetated areas or severely burned areas with no recovery).

im.plotRGB(stack_ndvi, r = 4, g = 3, b = 1) # Maps ndviN to Red, ndviA4y to Green, ndviB to Blue.
title("RGB NDVI Composite: Recovery from 2017 (Blue) to 2025 (Red)")
dev.off()


# 5. Classification for Quantifying Burned and Recovered Areas 

# Perform unsupervised classification on NDVI maps to segment the area into distinct vegetation classes.
# This allows for quantifying the proportion of different land cover types (e.g., vegetation health).
# The 'classify' function reclassifies pixel values into discrete categories based on defined thresholds.
# Thresholds are chosen to represent "Absent", "Stressed", and "Healthy" vegetation.
# Class 1: Absent vegetation (e.g., bare soil, water, severely burned areas)
# Class 2: Stressed vegetation (e.g., sparse vegetation, agricultural fields, initial recovery)
# Class 3: Healthy vegetation (e.g., dense forests, vigorous growth)

# Classification for Pre-Fire NDVI
prefire_classified <- classify(ndviB, rcl = matrix(c(-Inf, 0.2, 1, # Absent
                                                      0.2, 0.5, 2, # Stressed
                                                      0.5, Inf, 3), # Healthy
                                                    ncol = 3, byrow = TRUE))

# Classification for Post-Fire NDVI
postfire_classified <- classify(ndviA, rcl = matrix(c(-Inf, 0.2, 1, # Absent
                                                       0.2, 0.5, 2, # Stressed
                                                       0.5, Inf, 3), # Healthy
                                                     ncol = 3, byrow = TRUE))

# Classification for 4 Years After Fire NDVI
after4y_classified <- classify(ndviA4y, rcl = matrix(c(-Inf, 0.2, 1, # Absent
                                                         0.2, 0.5, 2, # Stressed
                                                         0.5, Inf, 3), # Healthy
                                                       ncol = 3, byrow = TRUE))

# Classification for Current NDVI
now_classified <- classify(ndviN, rcl = matrix(c(-Inf, 0.2, 1, # Absent
                                                   0.2, 0.5, 2, # Stressed
                                                   0.5, Inf, 3), # Healthy
                                                 ncol = 3, byrow = TRUE))


# Visualize the classified maps in a 2x2 multi-frame.
# A consistent color scheme is applied for easy comparison across time periods.
par(mfrow=c(2,2))
plot(prefire_classified, main="Classification: Pre-Fire (2017)", col=c("darkgrey","blue", "darkgreen"))
plot(postfire_classified, main="Classification: Post-Fire (2017)", col=c("darkgrey","blue", "darkgreen"))
plot(after4y_classified, main="Classification: 4 Years Later (2021)", col=c("darkgrey","blue", "darkgreen"))
plot(now_classified, main="Classification: Current (2025)", col=c("darkgrey","blue", "darkgreen"))
dev.off()


# 6. Quantifying Class Proportions and Visualization

# Calculate the proportional area of each classified vegetation class.
# This provides quantitative insights into the extent of burning and recovery.

# For Pre-Fire classification
f_prefire <- freq(prefire_classified) # Calculates frequency of each class
tot_prefire <- ncell(prefire_classified) # Total number of pixels in the raster
p_prefire <- f_prefire$count * 100 / tot_prefire # Percentage of each class
p_prefire

# For Post-Fire classification
f_postfire <- freq(postfire_classified)
tot_postfire <- ncell(postfire_classified)
p_postfire <- f_postfire$count * 100 / tot_postfire
p_postfire

# For 4 Years After classification
f_after4y <- freq(after4y_classified)
tot_after4y <- ncell(after4y_classified)
p_after4y <- f_after4y$count * 100 / tot_after4y
p_after4y

# For Current classification
f_now <- freq(now_classified)
tot_now <- ncell(now_classified)
p_now <- f_now$count * 100 / tot_now
p_now


# Manually define class labels and store calculated percentages in a data frame.
# This data frame will be used for plotting.
class<- c("Absent", "Stressed", "Healthy")
pre <- c(p_prefire[1], p_prefire[2], p_prefire[3]) # Assuming p_prefire is ordered by class value (1, 2, 3)
post <- c(p_postfire[1], p_postfire[2], p_postfire[3])
A4y <- c(p_after4y[1], p_after4y[2], p_after4y[3])
rightnow <- c(p_now[1], p_now[2], p_now[3])


data_p <- data.frame( class, pre, post, A4y, rightnow)
View(data_p) # View the summary table in a new window


# Define class labels for plotting and ensure correct factor order for consistent plotting.
class_labels <- c("Absent", "Stressed", "Healthy")
# Ensure the 'class' column is a factor with the desired order for plotting.
data_p$class <- factor(data_p$class, levels = class_labels)


# Create a bar plot showing the percentage of plant cover for the "Pre-Fire" period.
# This serves as a baseline for comparison with subsequent periods.
ggplot(data_p, aes(x = class, y = pre, fill = class)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d(option = "viridis") + # Use a discrete viridis palette for classes
  labs(title = "Percentage of Plant Cover Pre-Fire (2017)",
       y = "Percentage of Area (%)",
       x = "Vegetation Class") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) # Center the plot title
dev.off() # Close the graphics device


# Re-structure the data into a "long" format for ggplot2, which is more suitable for grouped bar charts.
# This ensures each row represents a percentage of a specific class at a given time point.
# Define class and time labels with their desired chronological order for proper plotting.
class_labels_ordered <- c("Absent", "Stressed", "Healthy")
time_labels_ordered <- c("Pre-Fire", "Post-Fire", "4Y-After", "Now")

data_for_plot <- data.frame(
  Time = factor(rep(time_labels_ordered, each = length(class_labels_ordered)),
                levels = time_labels_ordered), # Set levels for Time factor for chronological order
  Class = factor(rep(class_labels_ordered, times = 4),
                 levels = class_labels_ordered), # Set levels for Class factor for consistent order
  Percentage = c(p_prefire, p_postfire, p_after4y, p_now)) # Combine all percentage vectors

# Print to verify the dataframe structure and factor ordering.
print("Data for Classification Percentage Plot:")
print(data_for_plot)


# Generate a stacked bar plot visualizing the percentage of plant cover over time by class.
# This plot clearly shows the shift in proportions of Absent, Stressed, and Healthy vegetation
# following the fire and during the recovery process.
ggplot(data_for_plot, aes(x = Time, y = Percentage, fill = Class)) +
  geom_bar(stat = "identity", position = "stack", color = "black") + # Add black borders for clarity
  scale_fill_viridis_d(option = "viridis") +
  labs(title = "Percentage of Plant Cover Change Over Time",
       y = "Percentage of Area (%)",
       x = "Time Period") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels for readability
        plot.title = element_text(face = "bold", hjust = 0.5)) # Bold and center title
dev.off() # Close the graphics device


# 7. Moving Window Approach for Heterogeneity (Variability) Assessment 

# Calculate the standard deviation (SD) of NDVI using a moving window.
# SD serves as a proxy for spatial heterogeneity or variability within a local neighborhood.
# A 3x3 matrix (1/9, 3, 3) defines a square window of 3x3 pixels.
# Higher SD values indicate greater variability (e.g., patchy recovery, mixed land cover).

# Variability of post-fire NDVI (immediately after the fire)
sd_ndviA <- focal(ndviA, matrix(1/9, 3, 3), fun = sd)
# Variability of current NDVI (8 years after the fire)
sd_ndviN <- focal(ndviN, matrix(1/9, 3, 3), fun = sd)


# Define a color palette for visualizing variability, 'magma' is often good for highlighting extremes.
cl_sd <- viridis(100, option = "magma")


# Visualize the variability maps for post-fire and current NDVI in a 1x2 multi-frame.
# This helps understand how the spatial heterogeneity of the landscape has evolved since the fire.
par(mfrow=c(1,2))
plot(sd_ndviA, col = cl_sd, main = "Post-Fire NDVI Variability (2017)") # Expect high variability in burned/partially recovered areas
plot(sd_ndviN, col = cl_sd, main = "Current NDVI Variability (2025)") # Observe if variability has decreased (more homogeneous recovery) or persisted
dev.off()


# Calculate the standard deviation on the NDVI difference map (Post-Fire to 4 Years Later).
# This specifically highlights where the *change* in recovery (rate or pattern) is most heterogeneous.
# Areas with high variability in recovery might indicate complex ecological processes or human interventions.
sd_diff_post_now <- focal(DIF_Post_4y, matrix(1/9, 3, 3), fun = sd)
plot(sd_diff_post_now, col = cl_sd, main = "Variability of NDVI Recovery (Post-Fire to 2021)")
dev.off()
