# Load packages
library(dplyr)
library(tidyr)

#load posterior samples
posterior_samples

# Get the list of parameters (assumes the same structure for all subjects)
parameters <- names(posterior_samples[[1]])

# How many MCMC samples are there for "K" in a single subject?
n_samples  <- length(posterior_samples[[1]]$K)

# How many subjects do we have?
n_subjects <- length(posterior_samples)

# Create an empty list to store mean values for each parameter
parameter_means <- list()

# Loop over each parameter
for (param in parameters) {
  # Create a vector to store the mean for the current parameter
  param_means <- numeric(n_samples)
  
  # Loop over each sample index
  for (i in seq_len(n_samples)) {
    # Extract the values for the current parameter across all subjects at sample index i
    param_values <- numeric(n_subjects)
    for (s in seq_len(n_subjects)) {
      param_values[s] <- posterior_samples[[s]][[param]][i]
    }
    
    # Compute the mean across all subjects for the current parameter at sample index i
    param_means[i] <- mean(param_values)
  }
  
  # Store the mean vector for this parameter
  parameter_means[[param]] <- param_means
}

# Convert the parameter_means list to a long-format data frame
parameter_means_df <- do.call(cbind, parameter_means) %>%
  as.data.frame() %>%
  pivot_longer(cols = everything(), names_to = "parameter", values_to = "value") %>%
  mutate(index = rep(1:length(parameter_means[[1]]), times = length(parameter_means))) %>%
  arrange(parameter, index) %>%  # Order by parameter first, then by index
  select(index, parameter, value)  # Reorder columns

# Save as CSV without quotation marks
write.csv(parameter_means_df, file = "parameter_means_wetzels_sim_addict_low.csv", row.names = FALSE, quote = FALSE)