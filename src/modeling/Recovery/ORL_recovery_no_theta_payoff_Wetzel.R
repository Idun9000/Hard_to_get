install.packages("pacman")
pacman::p_load(hesim, extraDistr, R2jags, parallel, ggpubr)

set.seed(2000)

setwd("/work/DC_exam/Hard_to_get/src/modeling/Recovery")

# defining a function for calculating the maximum of the posterior density (not exactly the same as the mode)
MPD <- function(x) {
  density(x)$x[which(density(x)$y==max(density(x)$y))]
}


#------ create task environment -------------------
# NB! mod(ntrials, nstruct) (aka. ntrials %% nstruct) must be 0
ntrials <- 150 # total number of trials in our payoff structure
nstruct <- 10 # size of our subdivisions for pseudorandomization
freq <- 0.5 # probability of our frequent losses (we have losses half of the time)
infreq <- 0.1 # probability of our infrequent losses (we have losses 1/10th of the time)
bad_r <- 100 # "bad" winnings
bad_freq_l <- -250 # "bad" frequent loss
bad_infreq_l <- -1250 # "bad" infrequent loss
good_r <- 50 # "good" winnings
good_freq_l <- -50 # "good" frequent loss
good_infreq_l <- -250 # "good" infrequent loss

# Bad frequent
A_R <- rep(bad_r, nstruct) # we win on every trials
A_L <- c(rep(bad_freq_l, nstruct*freq),rep(0,nstruct*(1-freq))) # we have losses half of the time

# Bad infrequent
B_R <- rep(bad_r, nstruct)
B_L <- c(rep(bad_infreq_l, nstruct*infreq),rep(0,nstruct*(1-infreq))) # we have losses 1/10th of the time

# Good frequent
C_R <- rep(good_r, nstruct)
C_L <- c(rep(good_freq_l, nstruct*freq),rep(0,nstruct*(1-freq)))

# Good infrequent
D_R <- rep(good_r, nstruct)
D_L <- c(rep(good_infreq_l, nstruct*infreq),rep(0,nstruct*(1-infreq)))

# create the pseudorandomized full payoff structure
A <- array(NA,ntrials) # setting up and empty array to be filled
B <- array(NA,ntrials)
C <- array(NA,ntrials)
D <- array(NA,ntrials)
for (i in 1:(ntrials/nstruct)) {
  A[(1+(i-1)*nstruct):(i*nstruct)] <- (A_R + sample(A_L)) # randomly shuffling the loss-array for every ten trials (and adding those losses to the winnings)
  B[(1+(i-1)*nstruct):(i*nstruct)] <- (B_R + sample(B_L))
  C[(1+(i-1)*nstruct):(i*nstruct)] <- (C_R + sample(C_L))
  D[(1+(i-1)*nstruct):(i*nstruct)] <- (D_R + sample(D_L))
}


payoff <- cbind(A,B,C,D)/100 # combining all four decks as columns with each 100 trials - dividing our payoffs by 100 to make the numbers a bit easier to work with

# let's look at the payoff
colSums(payoff) # the two bad decks should sum to -25 (i.e. -2500), and the two good ones to 25 (i.e. 2500)
#----------------------------------------------------

source("ORL.R")


###--------------Run full parameter recovery -------------
niterations <- 100 # fewer because it takes too long

true_a_rew <- array(NA,c(niterations))
true_a_pun <- array(NA,c(niterations))
true_K <- array(NA,c(niterations))
true_omega_f <- array(NA,c(niterations))
true_omega_p <- array(NA,c(niterations))

infer_a_rew <- array(NA,c(niterations))
infer_a_pun <- array(NA,c(niterations))
infer_K <- array(NA,c(niterations))
infer_omega_f <- array(NA,c(niterations))
infer_omega_p <- array(NA,c(niterations))

start_time = Sys.time()

for (i in 1:niterations) {
  
  # let's see how robust the model is. Does it recover all sorts of values?
  a_rew <- runif(1,0,1)
  a_pun <- runif(1,0,1)
  K <- runif(1,0,2)
  theta <- 1 #runif(1,.2,3) # could also just be a set value (e.g. 1) to simplify the model a bit
  #omega_f <- runif(1,-2,2)
  #omega_p <- runif(1,-2,2)
  omega_f <- runif(1,-10,10)
  omega_p <- runif(1,-10,10)
  
  ORL_sims <- ORL(payoff,ntrials,a_rew,a_pun,K,theta,omega_f,omega_p)
  
  x <- ORL_sims$x
  X <- ORL_sims$X
  
  # set up jags and run jags model
  data <- list("x","X","ntrials") 
  params<-c("a_rew","a_pun","K","omega_f","omega_p")
  samples <- jags.parallel(data, inits=NULL, params,
                  model.file ="ORL_no_theta.txt", n.chains=3, 
                  n.iter=3000, n.burnin=1000, n.thin=1, n.cluster=3)
  
  
  true_a_rew[i] <- a_rew
  true_a_pun[i] <- a_pun
  true_K[i] <- K
  true_omega_f[i] <- omega_f
  true_omega_p[i] <- omega_p
  
  # find maximum a posteriori
  Y <- samples$BUGSoutput$sims.list
  infer_a_rew[i] <- MPD(Y$a_rew)
  infer_a_pun[i] <- MPD(Y$a_pun)
  infer_K[i] <- MPD(Y$K)
  infer_omega_f[i] <- MPD(Y$omega_f)
  infer_omega_p[i] <- MPD(Y$omega_p)
  
  print(i)
  
}

end_time = Sys.time()
end_time - start_time

# let's look at some scatter plots

par(mfrow=c(3,2))
plot(true_a_rew,infer_a_rew)
plot(true_a_pun,infer_a_pun)
plot(true_K,infer_K)
plot(true_omega_f,infer_omega_f)
plot(true_omega_p,infer_omega_p)

# Define the folder where you want to save the plot
output_folder <- "/work/DC_exam/Hard_to_get/out/plots"

# Set the file path and name
output_file <- file.path(output_folder, "parameter_recovery_plots_Wetzel_structure_lower_range.png")

# Open the graphics device
png(filename = output_file, width = 800, height = 1000)  # Adjust width and height as needed

# Create the composite plot layout
par(mfrow = c(3, 2))  # Set up the 3x2 layout

# Plot each pair of true vs. inferred parameters
plot(true_a_rew, infer_a_rew, 
     main = "a_rew: True vs Inferred", 
     xlab = "True a_rew", ylab = "Inferred a_rew")
abline(0, 1, col = "red", lty = 2)  # Add y=x line for reference

plot(true_a_pun, infer_a_pun, 
     main = "a_pun: True vs Inferred", 
     xlab = "True a_pun", ylab = "Inferred a_pun")
abline(0, 1, col = "red", lty = 2)

plot(true_K, infer_K, 
     main = "K: True vs Inferred", 
     xlab = "True K", ylab = "Inferred K")
abline(0, 1, col = "red", lty = 2)

plot(true_omega_f, infer_omega_f, 
     main = "omega_f: True vs Inferred", 
     xlab = "True omega_f", ylab = "Inferred omega_f")
abline(0, 1, col = "red", lty = 2)

plot(true_omega_p, infer_omega_p, 
     main = "omega_p: True vs Inferred", 
     xlab = "True omega_p", ylab = "Inferred omega_p")
abline(0, 1, col = "red", lty = 2)

# Close the graphics device
dev.off()

# Output confirmation message
cat("Plot saved to:", output_file, "\n")


# for investigating multi-colinearity
# par(mfrow=c(2,2))
# plot(true_a_rew,true_a_pun)
# plot(infer_a_rew,infer_a_pun)
# plot(true_omega_f,true_omega_p)
# plot(infer_omega_f,infer_omega_p)
# 
# par(mfrow=c(2,2))
# plot(true_a_rew,true_omega_f)
# plot(infer_a_rew,infer_omega_f)
# plot(true_a_rew,true_omega_p)
# plot(infer_a_rew,infer_omega_p)









