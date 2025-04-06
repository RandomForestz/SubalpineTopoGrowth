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
df <- df[df$annual_ba_cm_2022 > 0, ]
names(df)

################################################################################
# Question & Hypotheses
################################################################################

# 1.	How does soil moisture availability and competition influence subalpine 
#     tree growth?

# Hypothesis 1: Tree growth will be greater in wetter sites particularly 
#               for species that are less tolerant of drought.
# Hypothesis 2: Tree growth will be limited by not only soil moisture but an 
#               interaction with competition, with trees growth being suppressed 
#               in areas of low soil moisture availability and high competition.


################################################################################
### Data Exploration - Basal Area ~ Competition
################################################################################

# RGR ~ soil Moisture by Species
ggplot(df, aes(x = as.factor(moisture_class), y = annual_ba_cm_2022, fill = Spec)) +
  geom_boxplot() +
  facet_wrap(~ Spec) +
  theme_minimal() +
  labs(title = "Basal Area Increase ~ Moisture Class and Species",
       x = "Moisture Class",
       y = "Basal Area Increase") +
  ylim(c(0,100))

# RGR ~ Height / Species
ggplot(df, aes(x = as.factor(soil_moisture), y = annual_ba_cm_2022, fill = Spec)) +
  geom_boxplot() +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal() +
  labs(title = "Basal Area ~ Soil Moisture",
       x = "Soil Moisture",
       y = "Basal Area Increase") +
  ylim(c(0,100))



# growth ~ Competition / Species
ggplot(df, aes(x = ci_3, y = annual_ba_cm_2022, col = Spec)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  facet_wrap(~ Spec) +
  theme_minimal() +
  labs(title = "Basal Area Increase vs. Competition by Species",
       x = "Competition",
       y = "RGR Basal Area") +
  ylim(c(0,100)) +
  xlim(c(0,100))

df <- df %>% 
  filter(dead == 0)

# growth by dead neighbors
ggplot(df) +
  geom_point(mapping = aes(x = dead_neighbors_6m, y = annual_ba_cm_2022, col = Spec), alpha = .1) +
  geom_smooth(mapping = aes(x = dead_neighbors_6m, y = annual_ba_cm_2022, col = Spec), se = F, method = "lm") +
  facet_wrap(~Plot)+
  ylim(c(0,50)) +
  xlim(c(0,30))

################################################################################

z_scores <- scale(df$ci_3)
filtered_data <- df[abs(z_scores) < 3, ]


simp_lm <- lm(annual_ba_cm_2022 ~ ci_3 + Spec + dbh4 + soil_moisture, data = filtered_data)
summary(simp_lm)


simp_lmer <- lmer(annual_ba_cm_2022 ~ ci_3 + dbh4 + Spec * soil_moisture + (1|Plot), data = filtered_data)
summary(simp_lmer)

spec_lmer <- lmer(annual_ba_cm_2022 ~ ci_3 + dbh4 + soil_moisture + (1|Spec), data = filtered_data)
summary(spec_lmer)


ranef(simp_lmer)
lattice::dotplot(ranef(simp_lmer, condVar = TRUE))
ranef(spec_lmer)
lattice::dotplot(ranef(spec_lmer, condVar = TRUE))



