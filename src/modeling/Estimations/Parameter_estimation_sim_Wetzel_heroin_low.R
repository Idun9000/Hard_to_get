# Load required libraries
install.packages("pacman")
pacman::p_load(R2jags, parallel, ggplot2, reshape2)

set.seed(2000)

setwd('/work/DC_exam/Hard_to_get/src/modeling/Estimations')

# Function to calculate the maximum of the posterior density
MPD <- function(x) {
  density(x)$x[which(density(x)$y == max(density(x)$y))]
}

#---------- Load and prepare the data
ctr_data <- read.csv('/work/DC_exam/Hard_to_get/out/LLM_simulated_data/deckn_results_wetzels-addict-low_150_42_2024-12-21_14-53-10.txt', header = TRUE)
subIDs <- unique(ctr_data$subjID)
nsubs <- length(subIDs)
ntrials_max <- 150

x_raw <- ctr_data$deckN
X_raw <- ctr_data$gain + ctr_data$loss # Note the sign!

# Empty arrays to fill
ntrials_all <- array(0, c(nsubs))
x_all <- array(0, c(nsubs, ntrials_max))
X_all <- array(0, c(nsubs, ntrials_max))

# Fill arrays for each subject
for (s in 1:nsubs) {
  ntrials_all[s] <- length(x_raw[ctr_data$subjID == subIDs[s]])
  
  x_sub <- x_raw[ctr_data$subjID == subIDs[s]] 
  length(x_sub) <- ntrials_max
  
  X_sub <- X_raw[ctr_data$subjID == subIDs[s]] 
  length(X_sub) <- ntrials_max
  
  x_all[s, ] <- x_sub
  X_all[s, ] <- X_sub
}

# Scaling payoffs
X_all <- X_all / 100

#---------- Run JAGS for all subjects
params <- c("a_rew", "a_pun", "K", "theta", "omega_f", "omega_p")

# Create a list to store posterior samples for each subject
posterior_samples <- vector("list", length = nsubs)

for (s in 1:nsubs) {
  cat("Running JAGS for subject:", s, "of", nsubs, "\n")
  
  # Subject-specific data
  x <- x_all[s, ]
  X <- X_all[s, ]
  ntrials <- ntrials_all[s]
  
  # JAGS model setup
  data <- list("x", "X", "ntrials")
  
  # Run JAGS
  samples <- jags.parallel(data, inits = NULL, params,
                           model.file = "ORL_no_theta.txt",
                           n.chains = 3, n.iter = 5000, n.burnin = 1000, 
                           n.thin = 1, n.cluster = 4)
  
  # Store the posterior samples for the subject
  posterior_samples[[s]] <- samples$BUGSoutput$sims.list
}


# Investigate convergence
pacman::p_load(coda)
samples_list <- mcmc.list(lapply(1:ncol(samples$BUGSoutput$sims.array), 
                                 function(chain) as.mcmc(samples$BUGSoutput$sims.array[, chain, ])))


# Get the parameter names from the first chain
param_names <- colnames(samples_list[[1]])
print(param_names)

# Subset the mcmc.list for the parameters of interest
selected_params <- c("a_rew", "a_pun", "K", "omega_f", "omega_p") # Update with your parameters
filtered_samples <- samples_list[, selected_params, drop = FALSE]

#Open a graphics device to save the combined plot
png("traceplot_sim_Wetzel_heroin_low.png", width = 1200, height = 800)

# Set up a grid layout for 3 rows and 2 columns (adjust as needed)
par(mfrow = c(3, 2))

# Loop through each parameter and plot its trace
selected_params <- c("a_rew", "a_pun", "K", "omega_f", "omega_p") # Update as needed

for (param in selected_params) {
  # Extract traces for the parameter across chains
  traces <- samples_list[, param, drop = FALSE]
  
  # Plot the traceplot for the parameter
  traceplot(traces, main = paste("Traceplot for", param), ask = FALSE)
}

# Close the graphics device to save the plot
dev.off()


# Gelman-Rubin diagnostic
gelman_results <- gelman.diag(samples_list[, c("a_rew", "a_pun", "K", "omega_f", "omega_p")], multivariate = FALSE)
print(gelman_results)

# Save the workspace 
save.image(file = "estimation_sim_Wetzel_heroin_low.RData")