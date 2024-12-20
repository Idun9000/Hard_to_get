# Load required libraries
install.packages("pacman")
pacman::p_load(R2jags, parallel, ggplot2, reshape2)

set.seed(2000)

setwd('/work/DC_exam/Hard_to_get/src/modeling')

# Function to calculate the maximum of the posterior density
MPD <- function(x) {
  density(x)$x[which(density(x)$y == max(density(x)$y))]
}

#---------- Load and prepare the data
ctr_data <- read.table('/work/DC_exam/Hard_to_get/in/Data/raw_data/Ahn_data/Ahn_data_healthy_filtered.txt', header = TRUE)
subIDs <- unique(ctr_data$subjID)
nsubs <- length(subIDs) # Update the subject count
ntrials_max <- 100

x_raw <- ctr_data$deck
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
params <- c("a_rew", "a_pun", "K", "theta", "omega_f", "omega_p", "p")

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

#---------- Combine posteriors and prepare for plotting
# Combine all subjects' samples into data frames
combine_parameter_samples <- function(param_name) {
  do.call(rbind, lapply(1:nsubs, function(s) {
    data.frame(
      subjID = subIDs[s],
      value = posterior_samples[[s]][[param_name]]
    )
  }))
}

# Create combined data for each parameter
params_list <- c("a_rew", "a_pun", "K", "theta", "omega_f", "omega_p")
posterior_data <- lapply(params_list, combine_parameter_samples)
names(posterior_data) <- params_list

#---------- Plot parameter distributions
plot_parameter_distributions <- function(posterior_data) {
  for (param_name in names(posterior_data)) {
    # Create the plot
    p <- ggplot(posterior_data[[param_name]], aes(x = value, group = subjID, fill = as.factor(subjID))) +
      geom_density(alpha = 0.4) +
      labs(title = paste("Posterior Distribution of", param_name),
           x = param_name, y = "Density", fill = "Subject ID") +
      theme_minimal() +
      theme(legend.position = "none")
    
    # Print the plot to check interactively
    print(p)
    
    # Save the plot to a file
    ggsave(filename = paste0("posterior_", param_name, ".png"), plot = p, width = 8, height = 6)
  }
}

#plot_parameter_distributions(posterior_data)