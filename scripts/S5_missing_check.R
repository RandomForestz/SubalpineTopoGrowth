################################################################################
################################################################################
############################ S5 Catch the Gremlins #############################
################################################################################
################################################################################

# Libraries
source("scripts/libraries.R")
# functions
source("scripts/functions.R")

# Data prepped for competition indexes
load("data/growth_long/subalpine.RData")

# Structure check
str(subalpine)
summary(subalpine)
sapply(subalpine, class)

# Make factors
subalpine <- subalpine %>% 
  mutate(
    Tree_ID = as.factor(Tree_ID),
    Spec = as.factor(Spec),
    hc1 = as.factor(hc1),
    hc3 = as.factor(hc3),
    dead = as.factor(dead),
    dataset_type = as.factor(dataset_type),
    across(c(dCI, elevation_m, aspect_deg, soil_moisture, relative_humidity, growth_ratio),
           as.numeric)
  )

# Assign predictor list
predictor_groups <- list(
  competition = c("dCI"),
  structure = c("hc4"),
  composition = c("Spec"),
  topography = c("elevation_m", "aspect_deg"),
  soil = c("soil_moisture"),
  climate = c("relative_humidity"),
  random = c("Plot")
)

# Check for missing or extreme values
library(naniar)
miss_var_summary(subalpine)
summary(select(subalpine, dCI, elevation_m, aspect_deg, soil_moisture, relative_humidity, growth_ratio))
library(ggplot2)
ggplot(subalpine, aes(y = growth_ratio)) +
  geom_boxplot() +
  facet_wrap(~Spec, scales = "free_y") +
  theme_minimal()

