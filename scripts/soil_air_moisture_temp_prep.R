################################################################################
########################## Plot Moisture & Air #################################
################################################################################

# Last update - Josh: 02/08/2025

# Note: No Brainard Lakes Plots

# Source: https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-nwt.296.1

# Soil - biweekly

# Libraries
source("scripts/libraries.R")
# functions
source("scripts/functions.R")

# load data
soil_moisture <- read.csv("data/soils/soilmoisture_biweekly_micromet.ra.data.csv")
soil_temp <- read.csv("data/soils/soiltemp_micromet.ra.data.csv")
air_temp <- read.csv("data/air_temp/airtemp_rh_micromet.ra.data.csv")

### soil moisture ##############################################################
soil_moisture <- soil_moisture %>% 
  group_by(local_site) %>% 
  summarise(mean_sm = mean(SM_pct, na.rm = T)) 

### soil temp ##################################################################
soil_temperature <- soil_temp %>% 
  group_by(localSite) %>% 
  summarise(mean_temp = mean(TempC, na.rm = T)) 

### air temp ###################################################################
air_temperature <- air_temp %>% 
  group_by(local_site) %>% 
  summarise(mean_temp = mean(TempC, na.rm = T)) 

### air humidity ###############################################################
humidity <- air_temp %>% 
  group_by(local_site) %>% 
  summarise(mean_rh = mean(RH, na.rm = T)) 

################################################################################

sm <- ggplot(data = soil_moisture) +
  geom_col(mapping = aes(x = as.factor(local_site), y = mean_sm, fill = local_site)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Mean Soil Moisture by Plot", x = "Plot", y = "value", fill = "")

st <- ggplot(data = soil_temperature) +
  geom_col(mapping = aes(x = as.factor(localSite), y = mean_temp, fill = localSite)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Mean Soil Temperature by Plot", x = "Plot", y = "C", fill = "")

at <- ggplot(data = air_temperature) +
  geom_col(mapping = aes(x = as.factor(local_site), y = mean_temp, fill = local_site)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Mean Air Temperature by Plot", x = "Plot", y = "C", fill = "")

rh <- ggplot(data = humidity) +
  geom_col(mapping = aes(x = as.factor(local_site), y = mean_rh, fill = local_site)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Mean Relative Humidity by Plot", x = "Plot", y = "Humidity", fill = "")

ggarrange(sm, st, at, rh)
