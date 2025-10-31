################################################################################
################################################################################
############################ S4 Exploratory Visualization ######################
################################################################################
################################################################################


# Last update - Josh: 02/16/2025

# Libraries
source("scripts/libraries.R")
# functions
source("scripts/functions.R")

# Data prepped for competition indexes
load("data/growth_long/subalpine.RData")

################################################################################

ggplot(subalpine %>% 
         filter(dataset_type == "multi", !is.na(dCI), !is.na(growth_ratio)),
       aes(dCI, growth_ratio, color = hc4)) +
  geom_point(alpha = 0.4, size = 1.2) +
  geom_smooth(method = "gam", formula = y ~ s(x), se = TRUE) +
  scale_y_log10() +
  facet_wrap(~hc4) +
  theme_bw() +
  labs(
    x = "Δ Competition Index (CI₃ - CI₁)",
    y = "Growth Ratio (log-scaled)",
    color = "Height Class"
  )


subalpine %>%
  filter(dataset_type == "multi") %>%
  pivot_longer(cols = c(elevation_m, slope_deg, soil_moisture),
               names_to = "var", values_to = "value") %>%
  ggplot(aes(value, growth_ratio)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "gam", formula = y ~ s(x)) +
  facet_wrap(~var, scales = "free_x") +
  scale_y_log10() +
  theme_bw()


subalpine %>%
  filter(dataset_type == "multi", !is.na(growth_ratio)) %>%
  ggplot(aes(growth_ratio)) +
  geom_histogram(bins = 40, fill = "steelblue", alpha = 0.7) +
  scale_x_log10() +
  theme_minimal() +
  labs(title = "Distribution of Growth Ratios (log scale)")



library(corrplot)

# correlation matrix
env_cor <- subalpine %>%
  st_drop_geometry() %>%
  select(elevation_m, 
         #slope_deg, 
         aspect_deg,
         soil_moisture, 
         soil_temperature,
         #air_temperature, 
         relative_humidity) %>%
  cor(use = "complete.obs")

# rounded view in console
round(env_cor, 2)

corrplot(env_cor,
         method = "square",
         type = "upper",
         tl.col = "black",
         tl.cex = 0.8,
         number.cex = 0.7,
         addCoef.col = "black",
         col = colorRampPalette(c("#2166AC", "white", "#B2182B"))(200),
         mar = c(0, 0, 1, 0))

# install.packages("car") # if not already installed
library(car)

# Drop geometry and keep relevant vars
vif_data <- subalpine %>%
  st_drop_geometry() %>%
  select(elevation_m, aspect_deg, soil_moisture, soil_temperature, relative_humidity) %>%
  na.omit()

# Fit a simple linear model for VIF diagnostics
vif_model <- lm(elevation_m ~ aspect_deg + soil_moisture + soil_temperature + relative_humidity, data = vif_data)

# Calculate VIFs
vif_values <- vif(vif_model)
print(vif_values)

# Optional: nice summary
if (any(vif_values > 5)) {
  cat("\n⚠️  Some predictors show moderate collinearity (>5). Consider checking correlations.\n")
} else if (any(vif_values > 3)) {
  cat("\n⚠️  Mild collinearity detected (>3). Watch for instability in smooth terms.\n")
} else {
  cat("\n✅  All predictors show acceptable collinearity (<3). Good to proceed.\n")
}

# dropping slope and air temp

