################################################################################
################### Model Assumptions, Testing, and Viz ########################
################################################################################

# Last update - Josh: 02/16/2025

# Libraries
source("scripts/libraries.R")
# functions
source("scripts/functions.R")

# Data prepped for competition indexes
load("data/growth_long/subalpine_non_spatial.RData")
df <- subalpine_non_spatial
df <- na.omit(df)
names(df)

################################################################################
### Data Exploration - Basal Area ~ Competition
################################################################################

# RGR ~ soil Moisture by Species
ggplot(df, aes(x = as.factor(moisture_class), y = log(rgr_basal_area), fill = Spec)) +
  geom_boxplot() +
  facet_wrap(~ Spec) +
  theme_minimal() +
  labs(title = "RGR Basal Area by Moisture Class and Species",
       x = "Moisture Class",
       y = "RGR Basal Area")

# RGR ~ Height / Species
ggplot(df, aes(x = height, y = log(rgr_basal_area), fill = Spec)) +
  geom_boxplot() +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal() +
  labs(title = "RGR Basal Area vs. Height",
       x = "Height",
       y = "RGR Basal Area")

# RGR ~ Soil Moisture / Species
ggplot(df, aes(x = soil_moisture, y = log(rgr_basal_area), col = Spec)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  facet_wrap(~ Spec) +
  theme_minimal() +
  labs(title = "RGR Basal Area vs. Air Temperature by Species",
       x = "Soil Moisture",
       y = "RGR Basal Area")

# RGR ~ Air Temperature / Species
ggplot(df, aes(x = air_temperature, y = log(rgr_basal_area), col = Spec)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  facet_wrap(~ Spec) +
  theme_minimal() +
  labs(title = "RGR Basal Area vs. Air Temperature by Species",
       x = "Air Temperature",
       y = "RGR Basal Area")

# RGR ~ Soil temperature / Species
ggplot(df, aes(x = soil_temperature, y = log(rgr_basal_area), col = Spec)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  facet_wrap(~ Spec) +
  theme_minimal() +
  labs(title = "RGR Basal Area vs. Soil Temperature by Species",
       x = "Soil Temperature",
       y = "RGR Basal Area")

# RGR ~ Competition / Species
ggplot(df, aes(x = competition, y = log(rgr_basal_area), col = Spec)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  facet_wrap(~ Spec) +
  theme_minimal() +
  labs(title = "RGR Basal Area vs. Competition by Species",
       x = "Competition",
       y = "RGR Basal Area")


################################################################################






