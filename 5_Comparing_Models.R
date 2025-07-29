
# Load required libraries
library(tidyverse)
library(gridExtra)
library(scales)
library(cowplot)
library(grid)

# stats:

abby_models = read_csv("data/csuflow18_model_performance.csv")
new_sub_models <- list.files("data/model_results_no_burn/", pattern = "all_model_stats", full.names = TRUE) %>%
  map_dfr(~read_csv(.)) %>%
  mutate(flow_stat = str_remove(flow_stat, "^all_"),
         flow_stat = str_remove(flow_stat, "_q_mmd$|_q_mm$")) %>%
  mutate(flow_stat = ifelse(flow_stat == "q_ann_mm", "ann",
                            ifelse(flow_stat == "winter_mm", "winter_monthly_avg",
                                   flow_stat))) %>%
  filter(model_type == "MLR_Train") %>%
  mutate(model_type = "CSUFlow25") #%>%
# filter(!flow_stat %in% c("annual_mean_max", "annual_mean_min", "annual_mean", "mean_flowdate_0.1", "mean_flowdate_0.2",
#                          "mean_flowdate_0.3", "mean_flowdate_0.4", "mean_flowdate_0.5", "mean_flowdate_0.6", "mean_flowdate_0.7", "mean_flowdate_0.8",
#                          "mean_flowdate_0.9", "monsoon_frac", "q5", "q95", "flood_freq_1.5", 
#                          "winter_mm", "winter_total")) 

usgs_models <- read_csv("data/USGSvsFlow25_nse 1.csv") %>%
  filter(flow == "mm") %>%
  select(flow_stat = month, 
         NSE = nse,
         PBIAS = bias,
         usgs_hydro_region = region) %>%
  mutate(model_type = paste0("USGS")) #%>%
  #filter(usgs_hydro_region != "all") 

all_model_stats <- bind_rows(abby_models, new_sub_models) %>%
  bind_rows(usgs_models) %>%
  select(flow_stat, model_type, usgs_hydro_region, NSE, PBIAS, CV, model = vars) %>%
  mutate(flow_stat = tolower(flow_stat))

write_csv(all_model_stats, "data/all_models_stats_no_burn.csv")



# obs. vs. pred
usgs_raw <- read_csv("data/USGSvsFlow25 2.csv") 

usgs_obs <- usgs_raw %>%
  select(gage_used,
         index,
         all_jan_Q_mm_csu = all_jan_Q_mm,
         all_jan_Q_mm_usgs = Q1_mm,
         all_feb_Q_mm_csu = all_feb_Q_mm,
         all_feb_Q_mm_usgs = Q2_mm,
         all_mar_Q_mm_csu = all_mar_Q_mm,
         all_mar_Q_mm_usgs = Q3_mm,
         all_apr_Q_mm_csu = all_apr_Q_mm,
         all_apr_Q_mm_usgs = Q4_mm,
         all_may_Q_mm_csu = all_may_Q_mm,
         all_may_Q_mm_usgs = Q5_mm,
         all_jun_Q_mm_csu = all_jun_Q_mm,
         all_jun_Q_mm_usgs = Q6_mm,
         all_jul_Q_mm_csu = all_jul_Q_mm,
         all_jul_Q_mm_usgs = Q7_mm,
         all_aug_Q_mm_csu = all_aug_Q_mm,
         all_aug_Q_mm_usgs = Q8_mm,
         all_sep_Q_mm_csu = all_sep_Q_mm,
         all_sep_Q_mm_usgs = Q9_mm,
         all_oct_Q_mm_csu = all_oct_Q_mm,
         all_oct_Q_mm_usgs = Q10_mm,
         all_nov_Q_mm_csu = all_nov_Q_mm,
         all_nov_Q_mm_usgs = Q11_mm,
         all_dec_Q_mm_csu = all_dec_Q_mm,
         all_dec_Q_mm_usgs = Q12_mm,
         all_ann_Q_mm_csu = all_ann_Q_mm,
         all_ann_Q_mm_usgs = QA_mm,
         usgs_hydro_region)  %>%
  pivot_longer(cols = -c("gage_used", "index", "usgs_hydro_region"),
    names_to = "stat_source",
    values_to = "value") %>%
  filter(!is.na(value)) %>%
  extract(stat_source, into = c("stat", "source"), regex = "^(all_.*)_Q_mm_(csu|usgs)$") %>%
  filter(!is.na(source)) %>%
  pivot_wider(names_from = source,
    values_from = value) %>%
  select(gage_used, index, flow_stat = stat, obs = csu, pred = usgs, usgs_hydro_region) %>%
  mutate(model_type = "USGS",
    flow_stat = str_remove(flow_stat, "^all_")) 

abby_obs <- read_csv("data/csuflow18_obs_predict.csv") %>%
  mutate(model_type = "CSUFlow18")

flow25_obs <- list.files("data/model_results_no_burn/", pattern = "obs_pred_", full.names = TRUE) %>%
  map_dfr(~read_csv(.)) %>% #read_csv("data/model_results/obs_vs_predict_20260625.csv") %>%
  filter(dataset == "Train") %>%
  mutate(flow_stat = str_remove(flow_stat, "^all_"),
         flow_stat = str_remove(flow_stat, "_q_mmd$|_q_mm$")) %>%  # handles both _Q_mmd and _Q_mm
  rename(obs = observed, 
         pred = predicted) %>%
  mutate(model_type = "CSUFlow25") 

all_obs_pred <- bind_rows(flow25_obs, usgs_obs, abby_obs) %>%
  select(flow_stat, model_type, obs, pred, usgs_hydro_region) %>%
  filter(!is.na(obs)) %>%
  mutate(flow_stat = ifelse(flow_stat == "q_ann_mm", "ann", flow_stat),
         flow_stat = tolower(flow_stat))

write_csv(all_obs_pred, "data/all_models_obs_pred_no_burn.csv")


all_csuflow25_stats <- list.files("data/model_results_no_burn/", pattern = "all_model_stats", full.names = TRUE) %>%
  map_dfr(~read_csv(.)) %>%
  mutate(model_type = ifelse(model_type == "MLR_Train", "CSUFlow25 - Training", "CSUFlow25 - CV"))



















# Read the data
data <- all_obs_pred %>%
  filter(!flow_stat %in% c("annual_mean_max", "annual_mean_min", "annual_mean", "mean_flowdate_0.1", "mean_flowdate_0.2",
                           "mean_flowdate_0.3", "mean_flowdate_0.4", "mean_flowdate_0.5", "mean_flowdate_0.6", "mean_flowdate_0.7", "mean_flowdate_0.8",
                           "mean_flowdate_0.9", "monsoon_frac", "q5", "q95", "flood_freq_1.5",
                           "winter_avg_monthly", "winter_total"))

# Define colors for USGS regions (based on the plot)
region_colors <- c(
  "EastSlopeHeadwaters" = "blue",  # Blue
  "Green" = "green",               # Green
  "RioGrande" = "orange",             # Red
  "SanJuanDolores" = "red"        # Purple
)

region_shapes <- c(
  "EastSlopeHeadwaters" = 16,  # Circle (blue)
  "Green" = 18, # Diamond (orange)
  "RioGrande" = 17,             # Triangle
  "SanJuanDolores" = 15        # Circle (purple)
)

# Define labels with spaces for the legend
region_labels <- c(
  "EastSlopeHeadwaters" = "East Slope Headwaters",
  "Green" = "Green",
  "RioGrande" = "Rio Grande",
  "SanJuanDolores" = "San Juan Dolores"
)

# Function to create plots for each flow statistic
create_flow_plots <- function(flow_stat_name) {
  
  # Filter data for the specific flow statistic
  flow_data <- data %>% filter(flow_stat == flow_stat_name)
  
  # Calculate axis limits for CSU models (CSUFlow25 and CSUFlow18)
  csu_data <- flow_data %>% filter(model_type %in% c("CSUFlow25", "CSUFlow18"))
  csu_min <- min(c(csu_data$obs, csu_data$pred), na.rm = TRUE)
  csu_max <- max(c(csu_data$obs, csu_data$pred), na.rm = TRUE)
  
  # Calculate axis limits for USGS model
  usgs_data <- flow_data %>% filter(model_type == "USGS")
  usgs_min <- min(c(usgs_data$obs, usgs_data$pred), na.rm = TRUE)
  usgs_max <- max(c(usgs_data$obs, usgs_data$pred), na.rm = TRUE)
  
  # Create CSUFlow25 plot
  p1 <- ggplot(flow_data %>% filter(model_type == "CSUFlow25"), 
               aes(x = obs, y = pred)) +
    geom_point(color = "black", size = 3, alpha = 0.7) +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
    xlim(csu_min, csu_max) +
    ylim(csu_min, csu_max) +
    labs(x = "Observed", y = "Predicted", title = "CSUFlow25") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 20),
      axis.title = element_text(size = 18.5),
      axis.text = element_text(size = 17),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, size = 0.5)
    ) +
    coord_fixed()
  
  # Create CSUFlow18 plot
  p2 <- ggplot(flow_data %>% filter(model_type == "CSUFlow18"), 
               aes(x = obs, y = pred)) +
    geom_point(color = "black", size = 3, alpha = 0.7) +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
    xlim(csu_min, csu_max) +
    ylim(csu_min, csu_max) +
    labs(x = "Observed", y = "Predicted", title = "CSUFlow18") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 20),
      axis.title = element_text(size = 18.5),
      axis.text = element_text(size = 17),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, size = 0.5)
    ) +
    coord_fixed()
  
  # Create USGS plot
  p3 <- ggplot(flow_data %>% filter(model_type == "USGS"), 
               aes(x = obs, y = pred, color = usgs_hydro_region, shape = usgs_hydro_region)) +
    geom_point(size = 4, alpha = 0.8) +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
    scale_color_manual(values = region_colors, labels = region_labels) +
    scale_shape_manual(values = region_shapes, labels = region_labels) +
    xlim(usgs_min, usgs_max) +
    ylim(usgs_min, usgs_max) +
    labs(x = "Observed", y = "Predicted", title = "USGS") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 20),
      axis.title = element_text(size = 18.5),
      axis.text = element_text(size = 17),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, size = 0.5),
      legend.position = "none"  # Remove legend for individual plots
    ) +
    coord_fixed()
  
  # Combine plots
  combined_plot <- grid.arrange(p1, p2, p3, ncol = 3, 
                                top = grid::textGrob(
                                  paste0("Q", flow_stat_name),
                                  gp = grid::gpar(fontsize = 24, fontface = "bold")
                                ))
                                
  
  return(list(csuflow25 = p1, csuflow18 = p2, usgs = p3, combined = combined_plot))
}

# Get unique flow statistics
flow_stats <- unique(data$flow_stat)

# Create plots for each flow statistic
plot_list <- list()
for (stat in flow_stats) {
  plot_list[[stat]] <- create_flow_plots(stat)
}

# Create a dummy plot just for the legend
dummy_data <- data.frame(
  x = 1:4,
  y = 1:4,
  region = names(region_colors)
)

ggplot(dummy_data, aes(x = x, y = y, color = region, shape = region)) +
  geom_point(size = 3) +
  scale_color_manual(values = region_colors, labels = region_labels, name = "USGS Hydrologic Region") +
  scale_shape_manual(values = region_shapes, labels = region_labels, name = "USGS Hydrologic Region") +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 18.5),
    legend.text = element_text(size = 17)
  )

for (stat in flow_stats) {
  ggsave(paste0("figs/", stat, "_comparison.png"), plot_list[[stat]]$combined,
         width = 12, height = 4, dpi = 600)
}






flow_data <- list.files("data/model_results_no_burn/", pattern = "obs_pred_", full.names = TRUE) %>%
  map_dfr(~read_csv(.)) %>% 
  mutate(flow_stat = str_remove(flow_stat, "^all_"),
         flow_stat = str_remove(flow_stat, "_q_mmd$|_q_mm$")) %>%
  filter(flow_stat %in% c("annual_mean_max",   "annual_mean_min",   "annual_mean", "mean_flowdate_0.1", "mean_flowdate_0.2",
                          "mean_flowdate_0.3", "mean_flowdate_0.4", "mean_flowdate_0.5", "mean_flowdate_0.6", "mean_flowdate_0.7", "mean_flowdate_0.8",
                          "mean_flowdate_0.9", "monsoon_frac",      "q5",                "q95",              "flood_freq_1.5", 
                          "winter_avg_monthly", "winter_total")) %>%
  write_csv("data/temp.csv")

# Create proper titles for each flow statistic
flow_titles <- c(
  "annual_mean_min" = "Qmin", 
  "annual_mean_max" = "Qmax",
  "monsoon_frac" = "Monsoon",
  "flood_freq_1.5" = "Bankfull",
  "q5" = "Q5",
  "q95" = "Q95",
  "mean_flowdate_0.1" = "Day10",
  "mean_flowdate_0.2" = "Day20",
  "mean_flowdate_0.3" = "Day30",
  "mean_flowdate_0.4" = "Day40",
  "mean_flowdate_0.5" = "Day50",
  "mean_flowdate_0.6" = "Day60",
  "mean_flowdate_0.7" = "Day70",
  "mean_flowdate_0.8" = "Day80",
  "mean_flowdate_0.9" = "Day90",
  "winter_total" = "Winter Total",
  "winter_avg_monthly" = "Winter Avg Monthly"
)


# Filter for CSUFlow25 model type
csu_data <- flow_data 

# Function to create individual plots
create_scatter_plot <- function(data, flow_stat_name) {
  plot_data <- data %>% filter(flow_stat == flow_stat_name)
  
  if(nrow(plot_data) == 0) {
    return(ggplot() + theme_void() + labs(title = flow_titles[flow_stat_name]))
  }
  
  # Calculate axis limits (equal for both axes)
  min_val <- min(c(plot_data$observed, plot_data$predicted), na.rm = TRUE)
  max_val <- max(c(plot_data$observed, plot_data$predicted), na.rm = TRUE)
  
  # Add some padding
  padding <- (max_val - min_val) * 0.05
  axis_min <- min_val - padding
  axis_max <- max_val + padding
  
  ggplot(plot_data, aes(x = observed, y = predicted)) +
    geom_point(color = "black", size = 4, alpha = 0.7) +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", size = 0.8) +
    xlim(axis_min, axis_max) +
    ylim(axis_min, axis_max) +
    labs(x = "Observed", y = "Predicted", title = flow_titles[flow_stat_name]) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 20),
      axis.title = element_text(size = 18.5),
      axis.text = element_text(size = 17),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, size = 0.5)
    ) +
    coord_fixed()
}

main_flow_stats <- c("annual_mean_max", "annual_mean_min", "monsoon_frac", 
                     "q5", "q95", "flood_freq_1.5")
main_plots <- map(main_flow_stats, ~create_scatter_plot(csu_data, .))
main_grid <- do.call(grid.arrange, c(main_plots, ncol = 3))
ggsave(main_grid, filename = "figs/5_misc_stats.png",
       width = 12, height = 9, dpi = 600)


winter_flow_stats <- c("winter_total", "winter_avg_monthly")
winter_plots <- map(winter_flow_stats, ~create_scatter_plot(csu_data, .))
winter_grid <- do.call(grid.arrange, c(winter_plots, ncol = 2))
ggsave(winter_grid, filename = "figs/7_winter_stats.png",
       width = 12, height = 4, dpi = 600)


timing_stats <- c("mean_flowdate_0.1", "mean_flowdate_0.2",
                  "mean_flowdate_0.3", "mean_flowdate_0.4", "mean_flowdate_0.5", "mean_flowdate_0.6", "mean_flowdate_0.7", "mean_flowdate_0.8",
                  "mean_flowdate_0.9")
timing_plots <- map(timing_stats, ~create_scatter_plot(csu_data,.))
timing_grid <- do.call(grid.arrange, c(timing_plots, ncol = 3))
ggsave(timing_grid, filename = "figs/6_flowdate_stats.png",
       width = 12, height = 13.5, dpi = 600)











create_fixed_plots <- function(flow_stat_name) {
  
  # Filter data for the specific flow statistic
  flow_data <- data %>% filter(flow_stat == flow_stat_name)
  
  # Calculate global axis limits across all model types for this flow stat
  global_min <- min(c(flow_data$obs, flow_data$pred), na.rm = TRUE)
  global_max <- max(c(flow_data$obs, flow_data$pred), na.rm = TRUE)
  
  plot_theme <- theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 20),
      axis.title = element_text(size = 18.5),
      axis.text = element_text(size = 17),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, size = 0.5)
    )
  
  # CSUFlow25
  p1 <- ggplot(flow_data %>% filter(model_type == "CSUFlow25"), 
               aes(x = obs, y = pred)) +
    geom_point(color = "black", size = 3, alpha = 0.7) +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
    xlim(global_min, global_max) +
    ylim(global_min, global_max) +
    labs(x = "Observed", y = "Predicted", title = "CSUFlow25") +
    plot_theme +
    coord_fixed()
  
  # CSUFlow18
  p2 <- ggplot(flow_data %>% filter(model_type == "CSUFlow18"), 
               aes(x = obs, y = pred)) +
    geom_point(color = "black", size = 3, alpha = 0.7) +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
    xlim(global_min, global_max) +
    ylim(global_min, global_max) +
    labs(x = "Observed", y = "Predicted", title = "CSUFlow18") +
    plot_theme +
    coord_fixed()
  
  # USGS
  p3 <- ggplot(flow_data %>% filter(model_type == "USGS"), 
               aes(x = obs, y = pred, color = usgs_hydro_region, shape = usgs_hydro_region)) +
    geom_point(size = 4, alpha = 0.8) +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
    scale_color_manual(values = region_colors, labels = region_labels) +
    scale_shape_manual(values = region_shapes, labels = region_labels) +
    xlim(global_min, global_max) +
    ylim(global_min, global_max) +
    labs(x = "Observed", y = "Predicted", title = "USGS") +
    plot_theme +
    coord_fixed() +
    theme(legend.position = "none")
  
  # Combine plots with title
  combined_plot <- grid.arrange(p1, p2, p3, ncol = 3, 
                                top = grid::textGrob(
                                  paste0("Q", flow_stat_name),
                                  gp = grid::gpar(fontsize = 24, fontface = "bold")
                                ))
  
  return(list(csuflow25 = p1, csuflow18 = p2, usgs = p3, combined = combined_plot))
}

# Get unique flow statistics
flow_stats <- unique(data$flow_stat)

# Create plots for each flow statistic
plot_list <- list()
for (stat in flow_stats) {
  plot_list[[stat]] <- create_fixed_plots(stat)
}

# Create a dummy plot just for the legend
dummy_data <- data.frame(
  x = 1:4,
  y = 1:4,
  region = names(region_colors)
)

ggplot(dummy_data, aes(x = x, y = y, color = region, shape = region)) +
  geom_point(size = 3) +
  scale_color_manual(values = region_colors, labels = region_labels, name = "USGS Hydrologic Region") +
  scale_shape_manual(values = region_shapes, labels = region_labels, name = "USGS Hydrologic Region") +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 18.5),
    legend.text = element_text(size = 17)
  )

for (stat in flow_stats) {
  ggsave(paste0("figs/fixed_", stat, "_comparison.png"), plot_list[[stat]]$combined,
         width = 12, height = 4, dpi = 600)
}

