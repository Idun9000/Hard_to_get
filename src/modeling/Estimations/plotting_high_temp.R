# Load required libraries
library(ggplot2)
library(dplyr)
library(cowplot)
library(ggpubr)
library(tidyverse)

# Load datasets
Ahn_sim_healthy_hightemp_data <- read.csv("parameter_means_ahn_sim_high_hightemp.csv")
Human_data <- read.csv("/work/SabrinaSchrollZakiHansen#5217/Hard_to_get/out/parameter_means/parameter_means_ahn_healthy.csv")
LLM_high_temp_data <- read.csv("/work/SabrinaSchrollZakiHansen#5217/Hard_to_get/out/parameter_means/parameter_means_ahn_sim_healthy_high.csv")

# Add dataset identifiers
Ahn_sim_healthy_hightemp_data <- Ahn_sim_healthy_hightemp_data %>% mutate(Dataset = "LLM High Information, 2.0 Temperature")
Human_data <- Human_data %>% mutate(Dataset = "Human Data")
LLM_high_temp_data <- LLM_high_temp_data %>% mutate(Dataset = "LLM High Information, Default Temperature")

# Combine datasets
combined_data <- bind_rows(
  Ahn_sim_healthy_hightemp_data,
  Human_data,
  LLM_high_temp_data
)

# Legend theme
legend <- theme(legend.text = element_text(size = 20), 
                legend.title = element_text(size = 20), 
                legend.key.size = unit(2, 'cm'))

# Helper function to create density plots
create_density_plot <- function(data, parameter_name, x_limits, title) {
  ggplot(data %>% filter(parameter == parameter_name), aes(x = value, fill = Dataset)) +
    geom_density(alpha = 0.5) +
    scale_fill_manual(values = c(
      "Human Data" = "blue",
      "LLM High Information, Default Temperature" = "red",
      "LLM High Information, 2.0 Temperature" = "yellow"
    )) +
    labs(
      title = title,
      x = parameter_name,
      y = "Density",
      fill = "Dataset"
    ) +
    xlim(x_limits) +
    legend +
    theme_minimal()
}

# Create plots for each parameter
a_rew_plot <- create_density_plot(combined_data, "a_rew", c(0.15, 0.75), "a_rew Healthy")
a_pun_plot <- create_density_plot(combined_data, "a_pun", c(0.05, 0.5), "a_pun Healthy")
k_plot <- create_density_plot(combined_data, "K", c(1.3, 3.2), "K Healthy")
omega_f_plot <- create_density_plot(combined_data, "omega_f", c(0, 4.5), "omega_f Healthy")
omega_p_plot <- create_density_plot(combined_data, "omega_p", c(-1.5, 0.7), "omega_p Healthy")

# Combine all plots into one
combined_plot <- ggpubr::ggarrange(
  a_rew_plot, 
  a_pun_plot, 
  k_plot, 
  omega_f_plot, 
  omega_p_plot, 
  common.legend = TRUE,
  legend = "bottom",
  align = "hv",
  nrow = 5,
  ncol = 1
)

# Add title to the combined plot
final_plot <- ggpubr::annotate_figure(combined_plot, 
                                      top = ggpubr::text_grob("Posterior Estimations of Human Data and LLM Simulations", 
                                                              color = "black", face = "bold", size = 20))

# Save the final plot
ggsave(
  filename = "Ahn_combined_plot.jpg",
  plot = final_plot,
  device = "jpg",
  width = 10,
  height = 10,
  dpi = 300
)
