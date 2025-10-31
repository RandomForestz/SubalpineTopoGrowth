################################################################################
################################################################################
######################### S6 Assumptions / Diagnostics #########################
################################################################################
################################################################################

# Libraries
source("scripts/libraries.R")
# functions
source("scripts/functions.R")

# Data prepped for competition indexes
load("data/growth_long/subalpine_clean.RData")

################################################################################

# Check response distribution = Go with Log transformation

# raw
ggplot(subalpine, aes(x = growth_ratio)) +
  geom_histogram(bins = 40, fill = "steelblue") +
  theme_minimal()

# log-transformed
ggplot(subalpine, aes(x = log(growth_ratio))) +
  geom_histogram(bins = 40, fill = "firebrick") +
  theme_minimal() # Excellent, removes tail, centers


################################################################################

# plotting pairwise relationships, earlier VIF was <3
GGally::ggpairs(subalpine %>%
                  st_drop_geometry() %>%
                  select(dCI, slope_deg, soil_moisture, aspect_deg, relative_humidity))

################################################################################

# Scale and center predictors
subalpine_scaled <- subalpine %>%
  mutate(across(c(dCI, slope_deg, soil_moisture, 
                  soil_temperature,
                  air_temperature,
                  slope_deg,
                  aspect_deg, relative_humidity,
                  elevation_m),
                ~ scale(.)[, 1]))

################################################################################

# Checking linearity and additivity for fun
vars <- c("dCI", "soil_moisture", "aspect_deg", "relative_humidity", "air_temperature",
          "soil_temperature", "slope_deg", "elevation_m")
for (v in vars) {
  print(
    ggplot(subalpine, aes_string(x = v, y = "log(growth_ratio)")) +
      geom_point(alpha = 0.3) +
      geom_smooth(method = "loess") +
      theme_minimal() +
      ggtitle(v)
  )
}


################################################################################

# Check random effect structure

subalpine_scaled %>%
  group_by(Plot) %>%
  summarize(n_trees = n_distinct(Tree_ID)) %>%
  arrange(desc(n_trees))

################################################################################

# PCA

climate_means <- subalpine_scaled %>%
  st_drop_geometry() %>% 
  group_by(Plot) %>%
  summarize(across(c(soil_moisture, soil_temperature, air_temperature, relative_humidity),
                   ~ mean(.x, na.rm = TRUE)))

climate_pca <- prcomp(climate_means %>%
                        select(soil_moisture, soil_temperature, air_temperature, relative_humidity) %>%
                        scale(),
                      center = TRUE, scale. = TRUE)

# Extract variance explained
summary(climate_pca)

pca_scores <- as.data.frame(climate_pca$x) %>%
  bind_cols(climate_means %>% select(Plot))

# Inspect
pca_scores

ggplot(pca_scores, aes(x = PC1, y = PC2, label = Plot)) +
  geom_point(size = 4, color = "steelblue") +
  geom_text(vjust = -0.8, size = 3.5) +
  theme_minimal(base_size = 13) +
  labs(title = "PCA of Plot-level Microclimate",
       x = paste0("PC1 (", round(summary(climate_pca)$importance[2,1] * 100, 1), "%)"),
       y = paste0("PC2 (", round(summary(climate_pca)$importance[2,2] * 100, 1), "%)"))


loadings <- as.data.frame(climate_pca$rotation) %>%
  rownames_to_column("Variable")

ggplot(pca_scores, aes(PC1, PC2)) +
  geom_point(size = 4, color = "steelblue") +
  geom_text(aes(label = Plot), vjust = -0.8, size = 3.5) +
  geom_segment(data = loadings,
               aes(x = 0, y = 0, xend = PC1 * 3, yend = PC2 * 3),
               arrow = arrow(length = unit(0.25, "cm")), color = "firebrick") +
  geom_text(data = loadings,
            aes(x = PC1 * 3.2, y = PC2 * 3.2, label = Variable),
            color = "firebrick", size = 3.5) +
  theme_minimal(base_size = 13) +
  labs(title = "Principal Components Analysis of Plot-level Microclimate Variables",
       x = paste0("PC1 (", round(summary(climate_pca)$importance[2,1] * 100, 1), "%)",
                  "    Drier/Colder ------> Wetter/Humid"),
       y = paste0("PC2 (", round(summary(climate_pca)$importance[2,2] * 100, 1), "%)",
                  "    Colder Air/Soil ------> Warmer Air/Soil")) +
  theme_bw()
climate_pca$rotation
climate_means %>%
  st_drop_geometry() %>%
  left_join(subalpine_scaled %>% st_drop_geometry() %>% distinct(Plot, elevation_m), by = "Plot") %>%
  arrange(desc(elevation_m))
