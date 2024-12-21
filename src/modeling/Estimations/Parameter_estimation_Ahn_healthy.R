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
ctr_data <- read.table('/work/DC_exam/Hard_to_get/in/Data/raw_data/Ahn_data/Ahn_data_healthy_filtered.txt', header = TRUE)
subIDs <- unique(ctr_data$subjID)
nsubs <- length(subIDs)
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


# Combine posterior samples into a single data frame
combine_samples_to_df <- function(posterior_samples, subIDs, group_label) {
  combined_data <- data.frame()
  
  for (s in 1:length(posterior_samples)) {
    subject_data <- data.frame(
      subjID = subIDs[s],
      group = group_label,
      a_rew   = posterior_samples[[s]]$a_rew,
      a_pun   = posterior_samples[[s]]$a_pun,
      K       = posterior_samples[[s]]$K,
      theta   = posterior_samples[[s]]$theta,
      omega_f = posterior_samples[[s]]$omega_f,
      omega_p = posterior_samples[[s]]$omega_p
    )
    combined_data <- rbind(combined_data, subject_data)
  }
  
  return(combined_data)
}

# Create a data frame for healthy controls
healthy_data <- combine_samples_to_df(posterior_samples, subIDs, group_label = "healthy")

head(healthy_data)

# Save to CSV
write.csv(healthy_data, "posterior_samples_healthy.csv", row.names = FALSE)

# Look at the distribution of parameters
plot(density(healthy_data$a_rew))
plot(density(healthy_data$a_pun))
plot(density(healthy_data$K))
plot(density(healthy_data$omega_f))
plot(density(healthy_data$omega_p))

# Investigate convergence
pacman::p_load(coda)
samples_list <- mcmc.list(lapply(1:ncol(samples$BUGSoutput$sims.array), 
                                 function(chain) as.mcmc(samples$BUGSoutput$sims.array[, chain, ])))

# Convert samples_list to a matrix
samples_matrix <- as.matrix(samples_list)

# Convert the matrix to a data frame
samples_df <- as.data.frame(samples_matrix)

# Look at the distribution of parameters
plot(density(samples_df$a_rew))
plot(density(samples_df$a_pun))
plot(density(samples_df$K))
plot(density(samples_df$omega_f))
plot(density(samples_df$omega_p))

# trace plot
traceplot(samples, ask=FALSE, mfrow = c(3, 2), varname=params)

# Gelman-Rubin diagnostic
gelman_results <- gelman.diag(samples_list[, c("a_rew", "a_pun", "K", "omega_f", "omega_p")], multivariate = FALSE)
print(gelman_results)

# Save the workspace 
save.image(file = "estimation_ahn_healthy.RData")