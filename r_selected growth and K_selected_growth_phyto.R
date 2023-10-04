# Load required packages
library(readr)  # For reading CSV files
library(ggplot2)  # For plotting

# Load the dataset
dataset <- read.csv("PIC_timeseries.csv")

# Extract the PIC data for phytoplankton
PIC_data <- dataset$PIC

# Time points for simulation
time <- seq(0, length(PIC_data) - 1, by = 1)  # Adjusted for 0-based indexing

# Parameters for K-selected growth
r_K <- 0.2  # Intrinsic growth rate for K-selected growth (modify as needed)
K_K <- 0.8  # Carrying capacity for K-selected growth (modify as needed)

# Parameters for r-selected growth
r_r <- 0.5  # Intrinsic growth rate for r-selected growth (modify as needed)

# Initial condition
P0 <- PIC_data[1]  # Initial phytoplankton density is the first data point

# Function to simulate K-selected growth of phytoplankton
simulate_K_selected_growth <- function(P0, r, K, time) {
  n <- length(time)
  P <- numeric(n)
  P[1] <- P0
  
  for (i in 2:n) {
    dP <- r * P[i - 1] * (1 - P[i - 1] / K)  # Logistic growth equation
    P[i] <- P[i - 1] + dP
  }
  
  result <- data.frame(Time = time, Phytoplankton = P)
  return(result)
}

# Function to simulate r-selected growth of phytoplankton
simulate_r_selected_growth <- function(P0, r, time) {
  n <- length(time)
  P <- numeric(n)
  P[1] <- P0
  
  for (i in 2:n) {
    dP <- r * P[i - 1]
    P[i] <- P[i - 1] + dP
  }
  
  result <- data.frame(Time = time, Phytoplankton = P)
  return(result)
}

# Simulate K-selected growth of phytoplankton
K_selected_growth_data <- simulate_K_selected_growth(P0, r_K, K_K, time)

# Simulate r-selected growth of phytoplankton
r_selected_growth_data <- simulate_r_selected_growth(P0, r_r, time)

# Plot the results to visualize the population growth
library(gridExtra)

# Plot K-selected growth
plot_K_selected <- ggplot(K_selected_growth_data, aes(x = Time, y = Phytoplankton)) +
  geom_line(color = "blue", size = 1) +
  labs(x = "Time (days)", y = "Phytoplankton Population Density", title = "K-Selected Growth of Phytoplankton") +
  theme_minimal()

# Plot r-selected growth
plot_r_selected <- ggplot(r_selected_growth_data, aes(x = Time, y = Phytoplankton)) +
  geom_line(color = "red", size = 1) +
  labs(x = "Time (days)", y = "Phytoplankton Population Density", title = "R-Selected Growth of Phytoplankton") +
  theme_minimal()

# Arrange and display the plots
grid.arrange(plot_K_selected, plot_r_selected, ncol = 2)
