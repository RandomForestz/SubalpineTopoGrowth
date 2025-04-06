################################################################################
########################## Topographic Variables ###############################
################################################################################

# Last update - Josh: 02/04/2025

# Libraries
source("scripts/libraries.R")
# functions
source("scripts/functions.R")

# Data prepped with competition indexes
load("data/comp/growth_comp.RData")

################################################################################

# Organize dataset
subalpine <- growth_comp
subalpine$id <- paste0(subalpine$Plot, "_", subalpine$Tree) # create ID column

### MRS 1 ######################################################################
mrs1 <- subalpine %>%
  subset(Plot == "MRS1") %>% 
  mutate("elevation" = 2900) %>% 
  mutate("slope" = 5) %>% 
  mutate("aspect" = 90) %>% 
  mutate(hc1 = case_when(                
    hc1 == "1" ~ "Dominant",
    hc1 == "2" ~ "Co-Dominant",
    hc1 == "3" ~ "Suppressed",
    TRUE ~ as.character(hc1))) %>% 
  mutate(hc3 = case_when(                
    hc3 == "1" ~ "Dominant",
    hc3 == "2" ~ "Co-Dominant",
    hc3 == "3" ~ "Suppressed",
    TRUE ~ as.character(hc3))) %>%
  mutate(hc4 = case_when(                
    hc4 == "1" ~ "Dominant",
    hc4 == "2" ~ "Co-Dominant",
    hc4 == "3" ~ "Suppressed",
    TRUE ~ as.character(hc4))) %>%
  mutate(ci_1 = round(Hegyi_dbh1, digits = 2)) %>% 
  mutate(ci_2 = round(Hegyi_dbh3, digits = 2)) %>% 
  mutate(ci_3 = round(Hegyi_dbh4, digits = 2)) %>% 
  mutate("soil_moisture" = 3.25) %>% 
  mutate("soil_temperature" = 4.90) %>% 
  mutate("air_temperature" = 4.30) %>% 
  mutate("relative_humidity" = 56.9) %>% 
  mutate("moisture_class" = "Xeric") %>% 
  mutate("basal_area_1982" = ((dbh1/2.54)^2 * 0.005454)) %>% 
  mutate("basal_area_2016" = ((dbh3/2.54)^2 * 0.005454)) %>%
  mutate("basal_area_2022" = ((dbh4/2.54)^2 * 0.005454)) %>% 
  mutate("ba_growth_2016" = basal_area_2016 - basal_area_1982) %>% 
  mutate("ba_growth_2022" = basal_area_2022 - basal_area_2016) %>% 
  mutate("annualized_ba_2016" = ((ba_growth_2016)/(2016-1982))) %>% 
  mutate("annualized_ba_2022" = ((ba_growth_2016)/(2022-2016))) %>% 
  mutate("annual_ba_cm_2016" = annualized_ba_2016 * 929.03) %>% 
  mutate("annual_ba_cm_2022" = annualized_ba_2022 * 929.03) %>% 
  mutate("dead_census" = ifelse(deadPeriod == 0, "Alive", 
                              ifelse(deadPeriod %in% c(1,2,3,4,5,6,7,8,9), "After Census 1",
                                     ifelse(deadPeriod %in% c(10, 11, 12), "After Census 2", deadPeriod)))) %>% 
  {count_dead(.,dead_col = "dead_census")} %>% 
  .[c(18, 4:5, 8, 38, 24, 12, 40, 19:21, 25:29)]

#mrs1_comp <- mrs1 %>% 
#  pivot_longer(cols = starts_with("ci_"),
#               names_to = "comp_census",
#               values_to = "competition") %>% 
#  mutate(Census = rep(c("1", "2", "3"), length.out = n()))

#mrs1_ba <- mrs1 %>% 
#  pivot_longer(cols = starts_with("basal"),
#               names_to = "basal_area",
#               values_to = "BA")

#mrs1_hc <- mrs1 %>% 
#  pivot_longer(cols = starts_with("hc"),
#               names_to = "height",
#               values_to = "height_class") 

# mrs1_long <- mrs1_comp %>% 
#  mutate(rgr_basal_area = mrs1_rgr$RGR) %>% 
#  mutate(height = mrs1_hc$height_class)  %>% 
#  .[c(1:3, 28:30, 27, 21:22, 4:11, 23:25)] %>% 
#  mutate(
#    id = as.factor(id),  
#    Plot = as.factor(Plot),
#    Spec = as.factor(Spec),
#    Census = as.factor(Census),
#    moisture_class = as.factor(moisture_class),
#    dead_census = as.factor(dead_census),
#    dead = as.numeric(dead),  # binary format
#    competition = as.numeric(competition),
#    rgr_basal_area = as.numeric(rgr_basal_area),
#    aspect = as.numeric(aspect),  
#    height = fct_relevel(as.factor(height), c("Suppressed", "Co-Dominant", "Dominant")) # Ordinal factor
#  ) %>% 
#  filter(Census != 1)

### BW2 ########################################################################
bw2 <- subalpine %>%
  subset(Plot == "BW2") %>% 
  mutate("elevation" = 2980) %>% 
  mutate("slope" = 5) %>% 
  mutate("aspect" = 90) %>% 
  mutate(hc1 = case_when(                
    hc1 == "1" ~ "Dominant",
    hc1 == "2" ~ "Co-Dominant",
    hc1 == "3" ~ "Suppressed",
    TRUE ~ as.character(hc1))) %>% 
  mutate(hc3 = case_when(                
    hc3 == "1" ~ "Dominant",
    hc3 == "2" ~ "Co-Dominant",
    hc3 == "3" ~ "Suppressed",
    TRUE ~ as.character(hc3))) %>%
  mutate(hc4 = case_when(                
    hc4 == "1" ~ "Dominant",
    hc4 == "2" ~ "Co-Dominant",
    hc4 == "3" ~ "Suppressed",
    TRUE ~ as.character(hc4))) %>%
  mutate(ci_1 = round(Hegyi_dbh1, digits = 2)) %>% 
  mutate(ci_2 = round(Hegyi_dbh3, digits = 2)) %>% 
  mutate(ci_3 = round(Hegyi_dbh4, digits = 2)) %>% 
  mutate("soil_moisture" = 3.35) %>% 
  mutate("soil_temperature" = 3.09) %>% 
  mutate("air_temperature" = 3.26) %>% 
  mutate("relative_humidity" = 39.2) %>% 
  mutate("moisture_class" = "Xeric") %>% 
  mutate("basal_area_1982" = ((dbh1/2.54)^2 * 0.005454)) %>% 
  mutate("basal_area_2016" = ((dbh3/2.54)^2 * 0.005454)) %>%
  mutate("basal_area_2022" = ((dbh4/2.54)^2 * 0.005454)) %>% 
  mutate("ba_growth_2016" = basal_area_2016 - basal_area_1982) %>% 
  mutate("ba_growth_2022" = basal_area_2022 - basal_area_2016) %>% 
  mutate("annualized_ba_2016" = ((ba_growth_2016)/(2016-1982))) %>% 
  mutate("annualized_ba_2022" = ((ba_growth_2016)/(2022-2016))) %>% 
  mutate("annual_ba_cm_2016" = annualized_ba_2016 * 929.03) %>% 
  mutate("annual_ba_cm_2022" = annualized_ba_2022 * 929.03) %>% 
  mutate(dead_census = ifelse(deadPeriod == 0, "Alive", 
                              ifelse(deadPeriod %in% c(1,2,3,4,5,6,7,8,9), "After Census 1",
                                     ifelse(deadPeriod %in% c(10, 11, 12), "After Census 2", deadPeriod)))) %>% 
  {count_dead(.,dead_col = "dead_census")} %>%  
  .[c(18, 4:5, 8, 38, 24, 12, 40, 19:21, 25:29)]


### BW3 ########################################################################
bw3 <- subalpine %>%
  subset(Plot == "BW3") %>% 
  mutate("elevation" = 2980) %>% 
  mutate("slope" = 5) %>% 
  mutate("aspect" = 90) %>% 
  mutate(hc1 = case_when(                
    hc1 == "1" ~ "Dominant",
    hc1 == "2" ~ "Co-Dominant",
    hc1 == "3" ~ "Suppressed",
    TRUE ~ as.character(hc1))) %>% 
  mutate(hc3 = case_when(                
    hc3 == "1" ~ "Dominant",
    hc3 == "2" ~ "Co-Dominant",
    hc3 == "3" ~ "Suppressed",
    TRUE ~ as.character(hc3))) %>%
  mutate(hc4 = case_when(                
    hc4 == "1" ~ "Dominant",
    hc4 == "2" ~ "Co-Dominant",
    hc4 == "3" ~ "Suppressed",
    TRUE ~ as.character(hc4))) %>%
  mutate(ci_1 = round(Hegyi_dbh1, digits = 2)) %>% 
  mutate(ci_2 = round(Hegyi_dbh3, digits = 2)) %>% 
  mutate(ci_3 = round(Hegyi_dbh4, digits = 2)) %>% 
  mutate("soil_moisture" = 6.19) %>% 
  mutate("soil_temperature" = 2.27) %>% 
  mutate("air_temperature" = 2.96) %>% 
  mutate("relative_humidity" = 65.6) %>% 
  mutate("moisture_class" = "Mesic") %>%  
  mutate("basal_area_1982" = ((dbh1/2.54)^2 * 0.005454)) %>% 
  mutate("basal_area_2016" = ((dbh3/2.54)^2 * 0.005454)) %>%
  mutate("basal_area_2022" = ((dbh4/2.54)^2 * 0.005454)) %>% 
  mutate("ba_growth_2016" = basal_area_2016 - basal_area_1982) %>% 
  mutate("ba_growth_2022" = basal_area_2022 - basal_area_2016) %>% 
  mutate("annualized_ba_2016" = ((ba_growth_2016)/(2016-1982))) %>% 
  mutate("annualized_ba_2022" = ((ba_growth_2016)/(2022-2016))) %>% 
  mutate("annual_ba_cm_2016" = annualized_ba_2016 * 929.03) %>% 
  mutate("annual_ba_cm_2022" = annualized_ba_2022 * 929.03) %>% 
  mutate(dead_census = ifelse(deadPeriod == 0, "Alive", 
                              ifelse(deadPeriod %in% c(1,2,3,4,5,6,7,8,9), "After Census 1",
                                     ifelse(deadPeriod %in% c(10, 11, 12), "After Census 2", deadPeriod)))) %>% 
  {count_dead(.,dead_col = "dead_census")} %>%  
  .[c(18, 4:5, 8, 38, 24, 12, 40, 19:21, 25:29)]



### MRS4 ########################################################################
mrs4 <- subalpine %>%
  subset(Plot == "MRS4") %>% 
  mutate("elevation" = 3170) %>% 
  mutate("slope" = 10) %>% 
  mutate("aspect" = 90) %>% 
  mutate(hc1 = case_when(                
    hc1 == "1" ~ "Dominant",
    hc1 == "2" ~ "Co-Dominant",
    hc1 == "3" ~ "Suppressed",
    TRUE ~ as.character(hc1))) %>% 
  mutate(hc3 = case_when(                
    hc3 == "1" ~ "Dominant",
    hc3 == "2" ~ "Co-Dominant",
    hc3 == "3" ~ "Suppressed",
    TRUE ~ as.character(hc3))) %>%
  mutate(hc4 = case_when(                
    hc4 == "1" ~ "Dominant",
    hc4 == "2" ~ "Co-Dominant",
    hc4 == "3" ~ "Suppressed",
    TRUE ~ as.character(hc4))) %>%
  mutate(ci_1 = round(Hegyi_dbh1, digits = 2)) %>% 
  mutate(ci_2 = round(Hegyi_dbh3, digits = 2)) %>% 
  mutate(ci_3 = round(Hegyi_dbh4, digits = 2)) %>% 
  mutate("soil_moisture" = 8.02) %>% 
  mutate("soil_temperature" = 2.85) %>% 
  mutate("air_temperature" = 2.30) %>% 
  mutate("relative_humidity" = 64.3) %>% 
  mutate("moisture_class" = "Mesic") %>%
  mutate("basal_area_1982" = ((dbh1/2.54)^2 * 0.005454)) %>% 
  mutate("basal_area_2016" = ((dbh3/2.54)^2 * 0.005454)) %>%
  mutate("basal_area_2022" = ((dbh4/2.54)^2 * 0.005454)) %>% 
  mutate("ba_growth_2016" = basal_area_2016 - basal_area_1982) %>% 
  mutate("ba_growth_2022" = basal_area_2022 - basal_area_2016) %>% 
  mutate("annualized_ba_2016" = ((ba_growth_2016)/(2016-1982))) %>% 
  mutate("annualized_ba_2022" = ((ba_growth_2016)/(2022-2016))) %>% 
  mutate("annual_ba_cm_2016" = annualized_ba_2016 * 929.03) %>% 
  mutate("annual_ba_cm_2022" = annualized_ba_2022 * 929.03) %>% 
  mutate(dead_census = ifelse(deadPeriod == 0, "Alive", 
                              ifelse(deadPeriod %in% c(1,2,3,4,5,6,7,8,9), "After Census 1",
                                     ifelse(deadPeriod %in% c(10, 11, 12), "After Census 2", deadPeriod)))) %>% 
  {count_dead(.,dead_col = "dead_census")} %>%  
  .[c(18, 4:5, 8, 38, 24, 12, 40, 19:21, 25:29)]


### MRS5 ########################################################################
mrs5 <- subalpine %>%
  subset(Plot == "MRS5") %>% 
  mutate("elevation" = 3280) %>% 
  mutate("slope" = 5) %>% 
  mutate("aspect" = 90) %>% 
  mutate(hc1 = case_when(                
    hc1 == "1" ~ "Dominant",
    hc1 == "2" ~ "Co-Dominant",
    hc1 == "3" ~ "Suppressed",
    TRUE ~ as.character(hc1))) %>% 
  mutate(hc3 = case_when(                
    hc3 == "1" ~ "Dominant",
    hc3 == "2" ~ "Co-Dominant",
    hc3 == "3" ~ "Suppressed",
    TRUE ~ as.character(hc3))) %>%
  mutate(hc4 = case_when(                
    hc4 == "1" ~ "Dominant",
    hc4 == "2" ~ "Co-Dominant",
    hc4 == "3" ~ "Suppressed",
    TRUE ~ as.character(hc4))) %>%
  mutate(ci_1 = round(Hegyi_dbh1, digits = 2)) %>% 
  mutate(ci_2 = round(Hegyi_dbh3, digits = 2)) %>% 
  mutate(ci_3 = round(Hegyi_dbh4, digits = 2)) %>% 
  mutate("soil_moisture" = 31.50) %>% 
  mutate("soil_temperature" = 2.19) %>% 
  mutate("air_temperature" = 0.56) %>% 
  mutate("relative_humidity" = 66.6) %>% 
  mutate("moisture_class" = "Hydric") %>% 
  mutate("basal_area_1982" = ((dbh1/2.54)^2 * 0.005454)) %>% 
  mutate("basal_area_2016" = ((dbh3/2.54)^2 * 0.005454)) %>%
  mutate("basal_area_2022" = ((dbh4/2.54)^2 * 0.005454)) %>% 
  mutate("ba_growth_2016" = basal_area_2016 - basal_area_1982) %>% 
  mutate("ba_growth_2022" = basal_area_2022 - basal_area_2016) %>% 
  mutate("annualized_ba_2016" = ((ba_growth_2016)/(2016-1982))) %>% 
  mutate("annualized_ba_2022" = ((ba_growth_2016)/(2022-2016))) %>% 
  mutate("annual_ba_cm_2016" = annualized_ba_2016 * 929.03) %>% 
  mutate("annual_ba_cm_2022" = annualized_ba_2022 * 929.03) %>% 
  mutate(dead_census = ifelse(deadPeriod == 0, "Alive", 
                              ifelse(deadPeriod %in% c(1,2,3,4,5,6,7,8,9), "After Census 1",
                                     ifelse(deadPeriod %in% c(10, 11, 12), "After Census 2", deadPeriod)))) %>% 
  {count_dead(.,dead_col = "dead_census")} %>%  
  .[c(18, 4:5, 8, 38, 24, 12, 40, 19:21, 25:29)] 


### MRS7 ########################################################################
mrs7 <- subalpine %>%
  subset(Plot == "MRS7") %>% 
  mutate("elevation" = 3260) %>% 
  mutate("slope" = 15) %>% 
  mutate("aspect" = 180) %>% 
  mutate(hc1 = case_when(                
    hc1 == "1" ~ "Dominant",
    hc1 == "2" ~ "Co-Dominant",
    hc1 == "3" ~ "Suppressed",
    TRUE ~ as.character(hc1))) %>% 
  mutate(hc3 = case_when(                
    hc3 == "1" ~ "Dominant",
    hc3 == "2" ~ "Co-Dominant",
    hc3 == "3" ~ "Suppressed",
    TRUE ~ as.character(hc3))) %>%
  mutate(hc4 = case_when(                
    hc4 == "1" ~ "Dominant",
    hc4 == "2" ~ "Co-Dominant",
    hc4 == "3" ~ "Suppressed",
    TRUE ~ as.character(hc4))) %>%
  mutate(ci_1 = round(Hegyi_dbh1, digits = 2)) %>% 
  mutate(ci_2 = round(Hegyi_dbh3, digits = 2)) %>% 
  mutate(ci_3 = round(Hegyi_dbh4, digits = 2)) %>% 
  mutate("soil_moisture" = 5.74) %>% 
  mutate("soil_temperature" = 2.97) %>% 
  mutate("air_temperature" = 1.12) %>% 
  mutate("relative_humidity" = 64.6) %>% 
  mutate("moisture_class" = "Xeric") %>% 
  mutate("basal_area_1982" = ((dbh1/2.54)^2 * 0.005454)) %>% 
  mutate("basal_area_2016" = ((dbh3/2.54)^2 * 0.005454)) %>%
  mutate("basal_area_2022" = ((dbh4/2.54)^2 * 0.005454)) %>% 
  mutate("ba_growth_2016" = basal_area_2016 - basal_area_1982) %>% 
  mutate("ba_growth_2022" = basal_area_2022 - basal_area_2016) %>% 
  mutate("annualized_ba_2016" = ((ba_growth_2016)/(2016-1982))) %>% 
  mutate("annualized_ba_2022" = ((ba_growth_2016)/(2022-2016))) %>% 
  mutate("annual_ba_cm_2016" = annualized_ba_2016 * 929.03) %>% 
  mutate("annual_ba_cm_2022" = annualized_ba_2022 * 929.03) %>% 
  mutate(dead_census = ifelse(deadPeriod == 0, "Alive", 
                              ifelse(deadPeriod %in% c(1,2,3,4,5,6,7,8,9), "After Census 1",
                                     ifelse(deadPeriod %in% c(10, 11, 12), "After Census 2", deadPeriod)))) %>% 
  {count_dead(.,dead_col = "dead_census")} %>%  
  .[c(18, 4:5, 8, 38, 24, 12, 40, 19:21, 25:29)] 



### MRS11 ########################################################################
mrs11 <- subalpine %>%
  subset(Plot == "MRS11") %>% 
  mutate("elevation" = 3347) %>% 
  mutate("slope" = 5) %>% 
  mutate("aspect" = 160) %>% 
  mutate(hc1 = case_when(                
    hc1 == "1" ~ "Dominant",
    hc1 == "2" ~ "Co-Dominant",
    hc1 == "3" ~ "Suppressed",
    TRUE ~ as.character(hc1))) %>% 
  mutate(hc3 = case_when(                
    hc3 == "1" ~ "Dominant",
    hc3 == "2" ~ "Co-Dominant",
    hc3 == "3" ~ "Suppressed",
    TRUE ~ as.character(hc3))) %>%
  mutate(hc4 = case_when(                
    hc4 == "1" ~ "Dominant",
    hc4 == "2" ~ "Co-Dominant",
    hc4 == "3" ~ "Suppressed",
    TRUE ~ as.character(hc4))) %>%
  mutate(ci_1 = round(Hegyi_dbh1, digits = 2)) %>% 
  mutate(ci_2 = round(Hegyi_dbh3, digits = 2)) %>% 
  mutate(ci_3 = round(Hegyi_dbh4, digits = 2)) %>% 
  mutate("soil_moisture" = 9.87) %>% 
  mutate("soil_temperature" = 2.61) %>% 
  mutate("air_temperature" = 0.47) %>% 
  mutate("relative_humidity" = 66.6) %>% 
  mutate("moisture_class" = "Mesic") %>%
  mutate("basal_area_1982" = ((dbh1/2.54)^2 * 0.005454)) %>% 
  mutate("basal_area_2016" = ((dbh3/2.54)^2 * 0.005454)) %>%
  mutate("basal_area_2022" = ((dbh4/2.54)^2 * 0.005454)) %>% 
  mutate("ba_growth_2016" = basal_area_2016 - basal_area_1982) %>% 
  mutate("ba_growth_2022" = basal_area_2022 - basal_area_2016) %>% 
  mutate("annualized_ba_2016" = ((ba_growth_2016)/(2016-1982))) %>% 
  mutate("annualized_ba_2022" = ((ba_growth_2016)/(2022-2016))) %>% 
  mutate("annual_ba_cm_2016" = annualized_ba_2016 * 929.03) %>% 
  mutate("annual_ba_cm_2022" = annualized_ba_2022 * 929.03) %>% 
  mutate(dead_census = ifelse(deadPeriod == 0, "Alive", 
                              ifelse(deadPeriod %in% c(1,2,3,4,5,6,7,8,9), "After Census 1",
                                     ifelse(deadPeriod %in% c(10, 11, 12), "After Census 2", deadPeriod)))) %>% 
  {count_dead(.,dead_col = "dead_census")} %>%  
  .[c(18, 4:5, 8, 38, 24, 12, 40, 19:21, 25:29)] 



### MRS12 ########################################################################
mrs12 <- subalpine %>%
  subset(Plot == "MRS12") %>% 
  mutate("elevation" = 3380) %>% 
  mutate("slope" = 8) %>% 
  mutate("aspect" = 200) %>% 
  mutate(hc1 = case_when(                
    hc1 == "1" ~ "Dominant",
    hc1 == "2" ~ "Co-Dominant",
    hc1 == "3" ~ "Suppressed",
    TRUE ~ as.character(hc1))) %>% 
  mutate(hc3 = case_when(                
    hc3 == "1" ~ "Dominant",
    hc3 == "2" ~ "Co-Dominant",
    hc3 == "3" ~ "Suppressed",
    TRUE ~ as.character(hc3))) %>%
  mutate(hc4 = case_when(                
    hc4 == "1" ~ "Dominant",
    hc4 == "2" ~ "Co-Dominant",
    hc4 == "3" ~ "Suppressed",
    TRUE ~ as.character(hc4))) %>%
  mutate(ci_1 = round(Hegyi_dbh1, digits = 2)) %>% 
  mutate(ci_2 = round(Hegyi_dbh3, digits = 2)) %>% 
  mutate(ci_3 = round(Hegyi_dbh4, digits = 2)) %>% 
  mutate("soil_moisture" = 9.16) %>% 
  mutate("soil_temperature" = 3.65) %>% 
  mutate("air_temperature" = 4.35) %>% 
  mutate("relative_humidity" = 66.4) %>% 
  mutate("moisture_class" = "Xeric") %>% 
  mutate("basal_area_1982" = ((dbh1/2.54)^2 * 0.005454)) %>% 
  mutate("basal_area_2016" = ((dbh3/2.54)^2 * 0.005454)) %>%
  mutate("basal_area_2022" = ((dbh4/2.54)^2 * 0.005454)) %>% 
  mutate("ba_growth_2016" = basal_area_2016 - basal_area_1982) %>% 
  mutate("ba_growth_2022" = basal_area_2022 - basal_area_2016) %>% 
  mutate("annualized_ba_2016" = ((ba_growth_2016)/(2016-1982))) %>% 
  mutate("annualized_ba_2022" = ((ba_growth_2016)/(2022-2016))) %>% 
  mutate("annual_ba_cm_2016" = annualized_ba_2016 * 929.03) %>% 
  mutate("annual_ba_cm_2022" = annualized_ba_2022 * 929.03) %>% 
  mutate(dead_census = ifelse(deadPeriod == 0, "Alive", 
                              ifelse(deadPeriod %in% c(1,2,3,4,5,6,7,8,9), "After Census 1",
                                     ifelse(deadPeriod %in% c(10, 11, 12), "After Census 2", deadPeriod)))) %>% 
  {count_dead(.,dead_col = "dead_census")} %>%  
  .[c(18, 4:5, 8, 38, 24, 12, 40, 19:21, 25:29)]  

### MRS13 ########################################################################
mrs13 <- subalpine %>%
  subset(Plot == "MRS13") %>% 
  mutate("elevation" = 3337) %>% 
  mutate("slope" = 10) %>% 
  mutate("aspect" = 225) %>% 
  mutate(hc1 = case_when(                
    hc1 == "1" ~ "Dominant",
    hc1 == "2" ~ "Co-Dominant",
    hc1 == "3" ~ "Suppressed",
    TRUE ~ as.character(hc1))) %>% 
  mutate(hc3 = case_when(                
    hc3 == "1" ~ "Dominant",
    hc3 == "2" ~ "Co-Dominant",
    hc3 == "3" ~ "Suppressed",
    TRUE ~ as.character(hc3))) %>%
  mutate(hc4 = case_when(                
    hc4 == "1" ~ "Dominant",
    hc4 == "2" ~ "Co-Dominant",
    hc4 == "3" ~ "Suppressed",
    TRUE ~ as.character(hc4))) %>%
  mutate(ci_1 = round(Hegyi_dbh1, digits = 2)) %>% 
  mutate(ci_2 = round(Hegyi_dbh3, digits = 2)) %>% 
  mutate(ci_3 = round(Hegyi_dbh4, digits = 2)) %>% 
  mutate("soil_moisture" = 13.05) %>% 
  mutate("soil_temperature" = 3.93) %>% 
  mutate("air_temperature" = 1.02) %>% 
  mutate("relative_humidity" = 67.7) %>% 
  mutate("moisture_class" = "Xeric") %>% 
  mutate("basal_area_1982" = ((dbh1/2.54)^2 * 0.005454)) %>% 
  mutate("basal_area_2016" = ((dbh3/2.54)^2 * 0.005454)) %>%
  mutate("basal_area_2022" = ((dbh4/2.54)^2 * 0.005454)) %>% 
  mutate("ba_growth_2016" = basal_area_2016 - basal_area_1982) %>% 
  mutate("ba_growth_2022" = basal_area_2022 - basal_area_2016) %>% 
  mutate("annualized_ba_2016" = ((ba_growth_2016)/(2016-1982))) %>% 
  mutate("annualized_ba_2022" = ((ba_growth_2016)/(2022-2016))) %>% 
  mutate("annual_ba_cm_2016" = annualized_ba_2016 * 929.03) %>% 
  mutate("annual_ba_cm_2022" = annualized_ba_2022 * 929.03) %>% 
  mutate(dead_census = ifelse(deadPeriod == 0, "Alive", 
                              ifelse(deadPeriod %in% c(1,2,3,4,5,6,7,8,9), "After Census 1",
                                     ifelse(deadPeriod %in% c(10, 11, 12), "After Census 2", deadPeriod)))) %>% 
  {count_dead(.,dead_col = "dead_census")} %>%  
  .[c(18, 4:5, 8, 38, 24, 12, 40, 19:21, 25:29)]  



################################################################################

# Combine all 

sub_grow_2022 <- rbind.data.frame(mrs1, bw2, bw3,
                                  mrs4, mrs5,
                                  mrs7, mrs11, mrs12,
                                  mrs13)

subalpine <- sub_grow_2022 %>% 
  filter(Spec %in% c("PICO", "PIFL", "PIEN", "ABLA"))  # remove any non-target trees
  
subalpine_non_spatial <- sf::st_drop_geometry(subalpine) # create non spatial dataset

save(subalpine, file = "data/growth_long/subalpine.RData") # save
save(subalpine_non_spatial, file = "data/growth_long/subalpine_non_spatial.RData") # save



