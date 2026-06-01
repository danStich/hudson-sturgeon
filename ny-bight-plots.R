# Libraries ----
library(tidyverse)
library(reshape)
library(R2jags)
library(snowfall)

# Simulation results ----
# Load the results object (it's named "results")
load("results/adult_sim_results_bight.rda")

# Check it out
glimpse(results)


# Bias ----
# Plot of bias by n_reps by year
bias_plot <- results %>% 
  ggplot(aes(x = factor(n_reps), y = bias, color = factor(n_reps))) +
  geom_boxplot(width = 0.33) +
  geom_hline(yintercept = 0, linetype = 2) +
  xlab(expression(paste("Number of replicates per site (", italic(n), ")"))) +
  ylab(expression(paste("Bias (", italic(hat(N)), " - ", italic(N), ")"))) +
  labs(color = expression(italic("n")))

bias_plot

jpeg(filename = "results/ny-bight-bias.jpg",
     res = 300,
     height = 1800, width = 2400)
bias_plot
dev.off()

# Precision ----
# Plot of precision by n_reps
sd_plot <- results %>% 
  ggplot(aes(x = factor(n_reps), y = n_sd, color = factor(n_reps))) +
  geom_boxplot(width = 0.33) +
  geom_hline(yintercept = 0, linetype = 2) +
  xlab(expression(paste("Number of replicates per site (", italic(n), ")"))) +
  ylab(expression(paste("Standard deviation of (", italic(hat(N)), " )"))) +
  labs(color = expression(italic("n")))

sd_plot

jpeg(filename = "results/ny-bight-sd.jpg",
     res = 300,
     height = 1800, width = 2400)
sd_plot
dev.off()
