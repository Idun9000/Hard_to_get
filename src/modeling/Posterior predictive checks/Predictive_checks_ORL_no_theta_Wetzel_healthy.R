install.packages("pacman")
pacman::p_load(R2jags, parallel, ggplot2)

set.seed(2000)

### NB! Don't forget to set your working directory
setwd('/work/DC_exam/Hard_to_get/src/modeling/Posterior predictive checks')

# defining a function for calculating the maximum of the posterior density (not exactly the same as the mode)
MPD <- function(x) {
  density(x)$x[which(density(x)$y==max(density(x)$y))]
}

#----------getting the data

#load control data
Wetzel_healthy_data <- read.table("/work/DC_exam/Wetzel_data_healthy.txt",header=TRUE)

#----------prepare data for jags models - want trial x subject arrays for choice and gain & loss ----
# identify and count unique subject IDs
subIDs <- unique(Wetzel_healthy_data$subjID)
nsubs <- length(subIDs)
ntrials_max <- 150

# all choices (x) and outcomes (X)
x_raw <- Wetzel_healthy_data$deck
X_raw <- Wetzel_healthy_data$wins + Wetzel_healthy_data$losses #note the sign!

#--- assign choices and outcomes in trial x sub matrix

#different number of trials across subjects. We'll need to fix this by padding arrays of < 100
#this is just so we can make the array
#then we'll also need to record number of valid trials for each sub, 
#then run the JAGS model on only valid trials

# empty arrays to fill
ntrials_all <- array(0,c(nsubs))
x_all <- array(0,c(nsubs,ntrials_max))
X_all <- array(0,c(nsubs,ntrials_max))

for (s in 1:nsubs) {
  
  #record n trials for subject s
  ntrials_all[s] <- length(x_raw[Wetzel_healthy_data$subjID==subIDs[s]])
  
  #pad trials with NA if n trials < maximum (i.e. 100)
  x_sub <- x_raw[Wetzel_healthy_data$subjID==subIDs[s]] 
  length(x_sub) <- ntrials_max
  
  X_sub <- X_raw[Wetzel_healthy_data$subjID==subIDs[s]] 
  length(X_sub) <- ntrials_max
  
  # assign arrays
  x_all[s,] <- x_sub
  X_all[s,] <- X_sub
  
}

# Scaling the payoffs (cuz the learning parameter becomes less relevant for very large payoffs/losses)
X_all <- X_all/1000



# let's see how the model goes for more than 1 subject. Let's run this on all subjects
pred_success <- array(nsubs)

start_time = Sys.time()

for (s in 1:nsubs) {
  
  x <- x_all[s, ]
  X <- X_all[s, ]
  
  ntrials <- ntrials_all[s]
  
  # set up jags and run jags model on one subject
  data <- list("x","X","ntrials") 
  params<-c("a_rew","a_pun","K","omega_f","omega_p","p")
  temp_samples <- jags.parallel(data, inits=NULL, params,
                                model.file ="ORL_no_theta.txt",
                                n.chains=3, n.iter=5000, n.burnin=1000, n.thin=1, n.cluster=3)
  
  p_post <- temp_samples$BUGSoutput$sims.list$p
  
  x_predict <- array(ntrials)
  
  for (t in 1:ntrials) {
    p_predict <- c(
      MPD(p_post[,t,1]),
      MPD(p_post[,t,2]),
      MPD(p_post[,t,3]),
      MPD(p_post[,t,4])
    )
    
    x_predict[t] <- which.max(p_predict)
    
  }
  
  pred_success[s] <- sum(x_predict==x[1:ntrials]) # only comparing with trials for which we have choices
  print(s)
  
}

end_time = Sys.time()
end_time - start_time

pred_success_adjust <- pred_success/ntrials_all

avg_pred <- mean(pred_success_adjust)


library(ggplot2)

pred_df <- data.frame(pred_success_adjust)
pred_df$sub <- seq_len(nrow(pred_df))
pred_df$avg <- mean(pred_df$pred_success_adjust, na.rm = TRUE)
pred_df$chance <- 0.25

# Your ggplot object
p <- ggplot(pred_df, aes(sub, pred_success_adjust)) +
  geom_point() +
  geom_line(aes(y=chance), linetype="dashed", color = "black") +
  geom_ribbon(aes(xmin = -Inf, xmax = Inf), fill = "pink", alpha = 0.6) + 
  geom_line(aes(y=avg)) + 
  ylim(0,1)

# Save as PNG
ggsave("predictive_check_Wetzel_healthy.png", plot = p, width = 8, height = 6, dpi = 300)

# Bar plot for each participant, with average and chance lines
prettyplot <- ggplot(pred_df, aes(x = factor(sub), y = pred_success_adjust)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +  # Bars for each participant
  geom_hline(aes(yintercept = avg), color = "red", linetype = "solid", size = 1) +  # Line for avg
  geom_hline(aes(yintercept = chance), color = "black", linetype = "dotted", size = 1) +  # Dotted line for chance
  labs(
    x = "Subject",
    y = "Predicted Choice Accuracy",
    title = "Healthy Participants (Wetzel)"
  ) +
  ylim(0,1)
theme_minimal()

ggsave("predictive_check_Wetzel_healthy_pretty_plot.png", plot = prettyplot, width = 8, height = 6, dpi = 300)


# Save the workspace 
save.image(file = "predictive_checks_Wetzel_healthy.RData") 
# Later, you can reload it with load("my_workspace.RData")