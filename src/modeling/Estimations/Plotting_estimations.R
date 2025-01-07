# Load libraries
pacman::p_load(ggplot2, dplyr, cowplot, ggpubr, tidyverse)

setwd("/work/DC_exam")

# Load datasets
Ahn_healthy_data <- read.csv("posterior_samples_Ahn_healthy.csv")
Ahn_healthy_sim_low <- read.csv("posterior_samples_Ahn_healthy_low.csv")
Ahn_healthy_sim_high <- read.csv("posterior_samples_Ahn_healthy_high.csv")

Ahn_heroin_data <- read.csv("posterior_samples_Ahn_heroin.csv")
Ahn_heroin_sim_low <- read.csv("posterior_samples_Ahn_heroin_low.csv")
Ahn_heroin_sim_high <- read.csv("posterior_samples_Ahn_heroin_high.csv")

Wetzel_healthy_data <- read.csv("posterior_samples_Wetzels_healthy.csv")
Wetzel_healthy_sim_low <- read.csv("posterior_samples_Wetzels_healthy_low.csv")
Wetzel_healthy_sim_high <- read.csv("posterior_samples_Wetzels_healthy_high.csv")

Wetzel_heroin_sim_low <- read.csv("posterior_samples_Wetzels_heroin_low.csv")
Wetzel_heroin_sim_high <- read.csv("posterior_samples_Wetzels_heroin_high.csv")

# Calculate mean and median for each parameter in each dataset

# Assuming the datasets are already loaded
datasets <- list(
  Ahn_healthy_data = Ahn_healthy_data,
  Ahn_healthy_sim_low = Ahn_healthy_sim_low,
  Ahn_healthy_sim_high = Ahn_healthy_sim_high,
  Ahn_heroin_data = Ahn_heroin_data,
  Ahn_heroin_sim_low = Ahn_heroin_sim_low,
  Ahn_heroin_sim_high = Ahn_heroin_sim_high,
  Wetzel_healthy_data = Wetzel_healthy_data,
  Wetzel_healthy_sim_low = Wetzel_healthy_sim_low,
  Wetzel_healthy_sim_high = Wetzel_healthy_sim_high,
  Wetzel_heroin_sim_low = Wetzel_heroin_sim_low,
  Wetzel_heroin_sim_high = Wetzel_heroin_sim_high
)

# Function to calculate mean and median for each parameter in a dataset
calculate_summary_stats <- function(data) {
  data %>%
    summarise(
      a_pun_mean = mean(a_pun),
      a_pun_median = median(a_pun),
      a_rew_mean = mean(a_rew),
      a_rew_median = median(a_rew),
      K_mean = mean(K),
      K_median = median(K),
      omega_f_mean = mean(omega_f),
      omega_f_median = median(omega_f),
      omega_p_mean = mean(omega_p),
      omega_p_median = median(omega_p)
    )
}

# Calculate summary statistics for each dataset
summary_stats <- lapply(datasets, calculate_summary_stats)

# Combine the results into a single data frame for viewing
combined_summary_stats <- bind_rows(summary_stats, .id = "Dataset")


write.csv(combined_summary_stats, "summary_statistics.csv", row.names = FALSE)



# Add dataset identifiers
Ahn_healthy_data <- Ahn_healthy_data %>% mutate(Dataset = "Data")
Ahn_healthy_sim_low <- Ahn_healthy_sim_low %>% mutate(Dataset = "LLM Low Information")
Ahn_healthy_sim_high <- Ahn_healthy_sim_high %>% mutate(Dataset = "LLM High Information")

Ahn_heroin_data <- Ahn_heroin_data %>% mutate(Dataset = "Data")
Ahn_heroin_sim_low <- Ahn_heroin_sim_low %>% mutate(Dataset = "LLM Low Information")
Ahn_heroin_sim_high <- Ahn_heroin_sim_high %>% mutate(Dataset = "LLM High Information")

Wetzel_healthy_data <- Wetzel_healthy_data %>% mutate(Dataset = "Data")
Wetzel_healthy_sim_low <- Wetzel_healthy_sim_low %>% mutate(Dataset = "LLM Low Information")
Wetzel_healthy_sim_high <- Wetzel_healthy_sim_high %>% mutate(Dataset = "LLM High Information")

Wetzel_heroin_sim_low <- Wetzel_heroin_sim_low %>% mutate(Dataset = "LLM Low Information")
Wetzel_heroin_sim_high <- Wetzel_heroin_sim_high %>% mutate(Dataset = "LLM High Information")

# Combine datasets
Ahn_healthy <- bind_rows(Ahn_healthy_data, Ahn_healthy_sim_low, Ahn_healthy_sim_high)
Ahn_heroin <- bind_rows(Ahn_heroin_data, Ahn_heroin_sim_low, Ahn_heroin_sim_high)
Wetzel_healthy <- bind_rows(Wetzel_healthy_data, Wetzel_healthy_sim_low, Wetzel_healthy_sim_high)
Wetzel_heroin <- bind_rows(Wetzel_heroin_sim_low, Wetzel_heroin_sim_high)


# legend theme
legend <- theme(legend.text = element_text(size = 20), 
                 legend.title = element_text(size = 20), 
                 legend.key.size = unit(2, 'cm'))

# A_rew
Ahn_healthy_a_rew <- ggplot(Ahn_healthy, aes(x = a_rew, fill = Dataset)) +
  geom_density(alpha = 0.5) +  
  scale_fill_manual(values = c("Data" = "blue", "LLM Low Information" = "green", "LLM High Information" = "red")) +  # Custom fill colors
  scale_color_manual(values = c("Data" = "darkblue", "LLM Low Information" = "darkgreen", "LLM High Information" = "darkred")) +  # Custom line colors +
  labs(
    title = "a_rew healthy",
    x = "a_rew",
    y = "Density",
    fill = "Dataset"
  ) +
  xlim(0,1)+
  legend +
  theme_minimal()

Ahn_heroin_a_rew <- ggplot(Ahn_heroin, aes(x = a_rew, fill = Dataset)) +
  geom_density(alpha = 0.5) +  
  scale_fill_manual(values = c("Data" = "blue", "LLM Low Information" = "green", "LLM High Information" = "red")) +  # Custom fill colors
  scale_color_manual(values = c("Data" = "darkblue", "LLM Low Information" = "darkgreen", "LLM High Information" = "darkred")) +  # Custom line colors +
  labs(
    title = "a_rew addict",
    x = "a_rew",
    y = "Density",
    fill = "Dataset"
  ) +
  xlim(0,1)+
  legend +
  theme_minimal()

Wetzel_healthy_a_rew <- ggplot(Wetzel_healthy, aes(x = a_rew, fill = Dataset)) +
  geom_density(alpha = 0.5) +  
  scale_fill_manual(values = c("Data" = "blue", "LLM Low Information" = "green", "LLM High Information" = "red")) +  # Custom fill colors
  scale_color_manual(values = c("Data" = "darkblue", "LLM Low Information" = "darkgreen", "LLM High Information" = "darkred")) +  # Custom line colors +
  labs(
    title = "a_rew healthy",
    x = "a_rew",
    y = "Density",
    fill = "Dataset"
  ) +
  xlim(0,1)+
  legend +
  theme_minimal()

Wetzel_heroin_a_rew <- ggplot(Wetzel_heroin, aes(x = a_rew, fill = Dataset)) +
  geom_density(alpha = 0.5) + 
  scale_fill_manual(values = c("LLM Low Information" = "green", "LLM High Information" = "red")) +  # Custom fill colors
  scale_color_manual(values = c("LLM Low Information" = "darkgreen", "LLM High Information" = "darkred")) +  # Custom line colors +
  labs(
    title = "a_rew addict",
    x = "a_rew",
    y = "Density",
    fill = "Dataset"
  ) +
  xlim(0,1)+
  legend +
  theme_minimal()

# A_pun
Ahn_healthy_a_pun <- ggplot(Ahn_healthy, aes(x = a_pun, fill = Dataset)) +
  geom_density(alpha = 0.5) +  
  scale_fill_manual(values = c("Data" = "blue", "LLM Low Information" = "green", "LLM High Information" = "red")) +  # Custom fill colors
  scale_color_manual(values = c("Data" = "darkblue", "LLM Low Information" = "darkgreen", "LLM High Information" = "darkred")) +  # Custom line colors +
  labs(
    title = "a_pun healthy",
    x = "a_pun",
    y = "Density",
    fill = "Dataset"
  ) +
  xlim(0,1)+
  legend +
  theme_minimal()

Ahn_heroin_a_pun <- ggplot(Ahn_heroin, aes(x = a_pun, fill = Dataset)) +
  geom_density(alpha = 0.5) + 
  scale_fill_manual(values = c("Data" = "blue", "LLM Low Information" = "green", "LLM High Information" = "red")) +  # Custom fill colors
  scale_color_manual(values = c("Data" = "darkblue", "LLM Low Information" = "darkgreen", "LLM High Information" = "darkred")) +  # Custom line colors +
  labs(
    title = "a_pun addict",
    x = "a_pun",
    y = "Density",
    fill = "Dataset"
  ) +
  xlim(0,1)+
  legend +
  theme_minimal()

Wetzel_healthy_a_pun <- ggplot(Wetzel_healthy, aes(x = a_pun, fill = Dataset)) +
  geom_density(alpha = 0.5) +  
  scale_fill_manual(values = c("Data" = "blue", "LLM Low Information" = "green", "LLM High Information" = "red")) +  # Custom fill colors
  scale_color_manual(values = c("Data" = "darkblue", "LLM Low Information" = "darkgreen", "LLM High Information" = "darkred")) +  # Custom line colors +
  labs(
    title = "a_pun healthy",
    x = "a_pun",
    y = "Density",
    fill = "Dataset"
  ) +
  xlim(0,1)+
  legend +
  theme_minimal()

Wetzel_heroin_a_pun <- ggplot(Wetzel_heroin, aes(x = a_pun, fill = Dataset)) +
  geom_density(alpha = 0.5) +  
  scale_fill_manual(values = c("LLM Low Information" = "green", "LLM High Information" = "red")) +  # Custom fill colors
  scale_color_manual(values = c("LLM Low Information" = "darkgreen", "LLM High Information" = "darkred")) +  # Custom line colors +
  labs(
    title = "a_pun addict",
    x = "a_pun",
    y = "Density",
    fill = "Dataset"
  ) +
  xlim(0,1)+
  legend +
  theme_minimal()

# K
Ahn_healthy_K <- ggplot(Ahn_healthy, aes(x = K, fill = Dataset)) +
  geom_density(alpha = 0.5) +  
  scale_fill_manual(values = c("Data" = "blue", "LLM Low Information" = "green", "LLM High Information" = "red")) +  # Custom fill colors
  scale_color_manual(values = c("Data" = "darkblue", "LLM Low Information" = "darkgreen", "LLM High Information" = "darkred")) +  # Custom line colors +
  labs(
    title = "K healthy",
    x = "K",
    y = "Density",
    fill = "Dataset"
  ) +
  xlim(0,2)+
  legend +
  theme_minimal()

Ahn_heroin_K <- ggplot(Ahn_heroin, aes(x = K, fill = Dataset)) +
  geom_density(alpha = 0.5) +  
  scale_fill_manual(values = c("Data" = "blue", "LLM Low Information" = "green", "LLM High Information" = "red")) +  # Custom fill colors
  scale_color_manual(values = c("Data" = "darkblue", "LLM Low Information" = "darkgreen", "LLM High Information" = "darkred")) +  # Custom line colors +
  labs(
    title = "K addict",
    x = "K",
    y = "Density",
    fill = "Dataset"
  ) +
  xlim(0,2)+
  legend +
  theme_minimal()

Wetzel_healthy_K <- ggplot(Wetzel_healthy, aes(x = K, fill = Dataset)) +
  geom_density(alpha = 0.5) +  
  scale_fill_manual(values = c("Data" = "blue", "LLM Low Information" = "green", "LLM High Information" = "red")) +  # Custom fill colors
  scale_color_manual(values = c("Data" = "darkblue", "LLM Low Information" = "darkgreen", "LLM High Information" = "darkred")) +  # Custom line colors +
  labs(
    title = "K healthy",
    x = "K",
    y = "Density",
    fill = "Dataset"
  ) +
  xlim(0,2)+
  legend +
  theme_minimal()

Wetzel_heroin_K <- ggplot(Wetzel_heroin, aes(x = K, fill = Dataset)) +
  geom_density(alpha = 0.5) +  
  scale_fill_manual(values = c("LLM Low Information" = "green", "LLM High Information" = "red")) +  # Custom fill colors
  scale_color_manual(values = c("LLM Low Information" = "darkgreen", "LLM High Information" = "darkred")) +  # Custom line colors +
  labs(
    title = "K addict",
    x = "K",
    y = "Density",
    fill = "Dataset"
  ) +
  xlim(0,2)+
  legend +
  theme_minimal()

# Omega_f
Ahn_healthy_omega_f <- ggplot(Ahn_healthy, aes(x = omega_f, fill = Dataset)) +
  geom_density(alpha = 0.5) +  
  scale_fill_manual(values = c("Data" = "blue", "LLM Low Information" = "green", "LLM High Information" = "red")) +  # Custom fill colors
  scale_color_manual(values = c("Data" = "darkblue", "LLM Low Information" = "darkgreen", "LLM High Information" = "darkred")) +  # Custom line colors +
  labs(
    title = "omega_f healthy",
    x = "omega_f",
    y = "Density",
    fill = "Dataset"
  ) +
  legend +
  theme_minimal()

Ahn_heroin_omega_f <- ggplot(Ahn_heroin, aes(x = omega_f, fill = Dataset)) +
  geom_density(alpha = 0.5) +  
  scale_fill_manual(values = c("Data" = "blue", "LLM Low Information" = "green", "LLM High Information" = "red")) +  # Custom fill colors
  scale_color_manual(values = c("Data" = "darkblue", "LLM Low Information" = "darkgreen", "LLM High Information" = "darkred")) +  # Custom line colors +
  labs(
    title = "omega_f addict",
    x = "omega_f",
    y = "Density",
    fill = "Dataset"
  ) +
  legend +
  theme_minimal()

Wetzel_healthy_omega_f <- ggplot(Wetzel_healthy, aes(x = omega_f, fill = Dataset)) +
  geom_density(alpha = 0.5) +  
  scale_fill_manual(values = c("Data" = "blue", "LLM Low Information" = "green", "LLM High Information" = "red")) +  # Custom fill colors
  scale_color_manual(values = c("Data" = "darkblue", "LLM Low Information" = "darkgreen", "LLM High Information" = "darkred")) +  # Custom line colors +
  labs(
    title = "omega_f healthy",
    x = "omega_f",
    y = "Density",
    fill = "Dataset"
  ) +
  legend +
  theme_minimal()

Wetzel_heroin_omega_f <- ggplot(Wetzel_heroin, aes(x = omega_f, fill = Dataset)) +
  geom_density(alpha = 0.5) +  
  scale_fill_manual(values = c("LLM Low Information" = "green", "LLM High Information" = "red")) +  # Custom fill colors
  scale_color_manual(values = c("LLM Low Information" = "darkgreen", "LLM High Information" = "darkred")) +  # Custom line colors +
  labs(
    title = "omega_f addict",
    x = "omega_f",
    y = "Density",
    fill = "Dataset"
  ) +
  legend +
  theme_minimal()

# Omega_p
Ahn_healthy_omega_p <- ggplot(Ahn_healthy, aes(x = omega_p, fill = Dataset)) +
  geom_density(alpha = 0.5) +  
  scale_fill_manual(values = c("Data" = "blue", "LLM Low Information" = "green", "LLM High Information" = "red")) +  # Custom fill colors
  scale_color_manual(values = c("Data" = "darkblue", "LLM Low Information" = "darkgreen", "LLM High Information" = "darkred")) +  # Custom line colors +
  labs(
    title = "omega_p healthy",
    x = "omega_p",
    y = "Density",
    fill = "Dataset"
  ) +
  legend +
  theme_minimal()

Ahn_heroin_omega_p <- ggplot(Ahn_heroin, aes(x = omega_p, fill = Dataset)) +
  geom_density(alpha = 0.5) +  
  scale_fill_manual(values = c("Data" = "blue", "LLM Low Information" = "green", "LLM High Information" = "red")) +  # Custom fill colors
  scale_color_manual(values = c("Data" = "darkblue", "LLM Low Information" = "darkgreen", "LLM High Information" = "darkred")) +  # Custom line colors +
  labs(
    title = "omega_p addict",
    x = "omega_p",
    y = "Density",
    fill = "Dataset"
  ) +
  legend +
  theme_minimal()

Wetzel_healthy_omega_p <- ggplot(Wetzel_healthy, aes(x = omega_p, fill = Dataset)) +
  geom_density(alpha = 0.5) +  
  scale_fill_manual(values = c("Data" = "blue", "LLM Low Information" = "green", "LLM High Information" = "red")) +  # Custom fill colors
  scale_color_manual(values = c("Data" = "darkblue", "LLM Low Information" = "darkgreen", "LLM High Information" = "darkred")) +  # Custom line colors +
  labs(
    title = "omega_p healthy",
    x = "omega_p",
    y = "Density",
    fill = "Dataset"
  ) +
  legend +
  theme_minimal()

Wetzel_heroin_omega_p <- ggplot(Wetzel_heroin, aes(x = omega_p, fill = Dataset)) +
  geom_density(alpha = 0.5) +  
  scale_fill_manual(values = c("LLM Low Information" = "green", "LLM High Information" = "red")) +  # Custom fill colors
  scale_color_manual(values = c("LLM Low Information" = "darkgreen", "LLM High Information" = "darkred")) +  # Custom line colors +
  labs(
    title = "omega_p addict",
    x = "omega_p",
    y = "Density",
    fill = "Dataset"
  ) +
  legend +
  theme_minimal()



# Giga plotting

Ahn_giga <- ggpubr::ggarrange(
  Ahn_healthy_a_rew, Ahn_heroin_a_rew, 
  Ahn_healthy_a_pun, Ahn_heroin_a_pun, 
  Ahn_healthy_K, Ahn_heroin_K, 
  Ahn_healthy_omega_f, Ahn_heroin_omega_f, 
  Ahn_healthy_omega_p, Ahn_heroin_omega_p, # list of plots
  common.legend = TRUE, # COMMON LEGEND
  legend = "bottom", # legend position
  align = "hv", # Align them both, horizontal and vertical
  nrow = 5, # number of rows
  ncol = 2
)

Wetzel_giga <- ggpubr::ggarrange(
  Wetzel_healthy_a_rew, Wetzel_heroin_a_rew, 
  Wetzel_healthy_a_pun, Wetzel_heroin_a_pun, 
  Wetzel_healthy_K, Wetzel_heroin_K, 
  Wetzel_healthy_omega_f, Wetzel_heroin_omega_f, 
  Wetzel_healthy_omega_p, Wetzel_heroin_omega_p, # list of plots
  common.legend = TRUE, # COMMON LEGEND
  legend = "bottom", # legend position
  align = "hv", # Align them both, horizontal and vertical
  nrow = 5, # number of rows
  ncol = 2
)

Ahn_giga <- annotate_figure(Ahn_giga, top = text_grob("Posterior estimations of Ahn Data and LLM Simulations", 
                                      color = "black", face = "bold", size = 20))

Wetzel_giga <- annotate_figure(Wetzel_giga, top = text_grob("Posterior estimations of Wetzel Data and LLM Simulations", 
                                          color = "black", face = "bold", size = 20))

Ahn_giga
Wetzel_giga

ggsave(
  filename = "Wetzel_giga_plot.jpg",  # File name
  plot = Wetzel_giga,     # Plot object
  device = "jpg",                    # File format
  width = 8,                        # Width of the image in inches
  height = 10,                       # Height of the image in inches
  dpi = 300                          # Resolution in dots per inch
)

ggsave(
  filename = "Ahn_giga_plot.jpg",  # File name
  plot = Ahn_giga,     # Plot object
  device = "jpg",                    # File format
  width = 8,                        # Width of the image in inches
  height = 10,                       # Height of the image in inches
  dpi = 300                          # Resolution in dots per inch
)