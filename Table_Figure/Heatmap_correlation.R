#############-----------------------------------------------------###############
#############                      correlation                    ############### 
#############-----------------------------------------------------###############
# Description:
# This script explores the relationship between predictor variables and indoor
# temperature/humidity using correlation analysis and a heatmap figure.
#
# Outputs:
# - results/Figure/correlation_temp_hr.png
# - results/Figure/correlation_heat_map.png

library(ggplot2)
library(corrplot)

script_dir <- if (!is.null(sys.frames()[[1]]$ofile)) {
  dirname(normalizePath(sys.frames()[[1]]$ofile))
} else {
  getwd()
}
project_root <- normalizePath(file.path(script_dir, "..", ".."), winslash = "/", mustWork = FALSE)
model_dir <- file.path(project_root, "db", "output", "Model")
results_dir <- file.path(project_root, "results")
figure_dir <- file.path(results_dir, "Figure")
if (!dir.exists(figure_dir)) dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

load(file.path(model_dir, "temp_preidctors_data_model.RData"))
selected_vars <- c("temperature_min_home", "temperature_mean_home", "temperature_max_home", 
                   "humidity_mean_home","hum_mean_lag1_avg","temp_mean_lag1_avg", "indoor_temp", "indoor_hr")

correlation_selected <- temp_preidctors_data_model[, selected_vars]

correlation_selected <- correlation_selected[, sapply(correlation_selected, is.numeric)]
correlation_selected <- cor(correlation_selected, use = "pairwise.complete.obs")

range(correlation_selected, na.rm = TRUE)
png(file.path(figure_dir, "correlation_temp_hr.png"),
    width = 1200, height = 1000, res = 150)

corrplot(correlation_selected,
         method = "color",
         type = "upper",
         col = colorRampPalette(c("blue", "white", "red"))(100),
         tl.col = "black",
         tl.srt = 45,
         addCoef.col = "black",
         tl.cex = 0.8,
         number.cex = 0.6)  # Adjust text size if needed

dev.off()

###################################################################################


ggplot(temp_preidctors_data_model_winter, aes(x = difficult_pay_heating, y = indoor_temp, fill = difficult_pay_heating)) +
  stat_summary(fun = mean, geom = "bar", width = 0.6, color = "black") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  theme_minimal() +
  labs(x = "Category", y = "Mean Indoor Temperature (°C)", title = "Indoor Temperature by Category")




#############-----------------------------------------------------###############
#############                 correlation heat map                ############### 
#############-----------------------------------------------------###############
library(ggplot2)
library(reshape2) 
library(corrplot)  
library(RColorBrewer) 
library(dplyr)


# Convert to long format for ggplot2
cor_df <- read.csv(file.path(results_dir, "correlation.csv"))
cor_df$predictors <- factor(cor_df$predictors, levels = cor_df$predictors[order(cor_df$correlation)])

# Plot using ggplot2
# line
ggplot(cor_df, aes(x = correlation, y = predictors, color = correlation)) +
  geom_segment(aes(x = 0, xend = correlation, yend = predictors), size = 1) +  # Draw line
  geom_point(size = 3) +  # Draw points
  geom_text(aes(label = round(correlation, 2)), color = "black", 
            vjust = ifelse(cor_df$correlation < 0, 1.5, -0.5),  # Add labels with rounded correlation
            size = 3, fontface = "bold") +
  scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  labs(title = "Correlation Between Indoor Temperature and Predictors",
       x = "Correlation", y = "Predictors") +
  theme(axis.text.y = element_text(size = 8))  # Adjust label size for readability

# bar
ggplot(cor_df, aes(x = correlation, y = predictors, fill = correlation)) +
  geom_bar(stat = "identity", width = 0.8) +  # Horizontal bars
  geom_text(aes(label = round(correlation, 2)), color = "black",  # Numbers in black
            hjust = ifelse(cor_df$correlation < 0, -0.2, 1.2),  # Adjust text position
            size = 3, fontface = "bold") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  labs(title = "Correlation Between Indoor Temperature and Predictors",
       x = "Correlation", y = "Predictors") +
  theme(axis.text.y = element_text(size = 8))  # Adjust label size for readability

ggsave(file.path(figure_dir, "correlation_heat_map.png"), 
       width = 15, height = 8, 
       dpi = 600)