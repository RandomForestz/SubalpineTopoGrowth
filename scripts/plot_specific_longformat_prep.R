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
  mutate("elev" = 2900) %>% 
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
  mutate(ci_1 = round(ci_1, digits = 2)) %>% 
  mutate(ci_2 = round(ci_2, digits = 2)) %>% 
  mutate(ci_3 = round(ci_3, digits = 2)) %>% 
  mutate("soil_moisture" = 3.25) %>% 
  mutate("soil_temperature" = 4.90) %>% 
  mutate("air_temperature" = 4.30) %>% 
  mutate("relative_humidity" = 56.9) %>% 
  mutate("moisture_class" = "Xeric") %>% 
  mutate("Basal_Area_1982" = ((dbh1^2) * 0.005454)) %>% 
  mutate("Basal_Area_2016" = ((dbh3^2) * 0.005454)) %>%
  mutate("Basal_Area_2022" = ((dbh4^2) * 0.005454)) %>% 
  mutate(Dead_Census = ifelse(deadPeriod == 0, "Alive", 
                              ifelse(deadPeriod %in% c(1,2,3,4,5,6,7,8,9), "After Census 1",
                                     ifelse(deadPeriod %in% c(10, 11, 12), "After Census 2", deadPeriod)))) %>% 
  .[c(18, 4:5, 19:29, 14:16, 9:12, 30, 1:2)]

mrs1_comp <- mrs1 %>% 
  pivot_longer(cols = starts_with("ci_"),
               names_to = "comp_census",
               values_to = "Competition") %>% 
  mutate(Census = rep(c("1", "2", "3"), length.out = n()))

mrs1_basal <- mrs1 %>% 
  pivot_longer(cols = starts_with("Basal_Area"),
               names_to = "ba_census",
               values_to = "Basal_Area")

mrs1_hc <- mrs1 %>% 
  pivot_longer(cols = starts_with("hc"),
               names_to = "height",
               values_to = "Height_Class") 

mrs1_long <- mrs1_comp %>% 
  mutate(Basal_Area = mrs1_basal$Basal_Area) %>% 
  mutate(Height = mrs1_hc$Height_Class) %>% 
  .[c(1:2, 25, 3:11, 18:19, 26, 24, 27, 20:21)] %>% 
  mutate(
    id = as.factor(id),  
    Plot = as.factor(Plot),
    Spec = as.factor(Spec),
    Census = as.factor(Census),
    moisture_class = as.factor(moisture_class),
    Dead_Census = as.factor(Dead_Census),
    dead = as.numeric(dead),  # binary format
    Competition = as.numeric(Competition),
    Basal_Area = as.numeric(Basal_Area),
    aspect = as.numeric(aspect),  
    Height = fct_relevel(as.factor(Height), c("Suppressed", "Co-Dominant", "Dominant")) # Ordinal factor
  )


### BW2 ########################################################################
bw2 <- subalpine %>%
  subset(Plot == "BW2") %>% 
  mutate("elev" = 2980) %>% 
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
  mutate(ci_1 = round(ci_1, digits = 2)) %>% 
  mutate(ci_2 = round(ci_2, digits = 2)) %>% 
  mutate(ci_3 = round(ci_3, digits = 2)) %>% 
  mutate("soil_moisture" = 3.35) %>% 
  mutate("soil_temperature" = 3.09) %>% 
  mutate("air_temperature" = 3.26) %>% 
  mutate("relative_humidity" = 39.2) %>% 
  mutate("moisture_class" = "Xeric") %>% 
  mutate("Basal_Area_1982" = ((dbh1^2) * 0.005454)) %>% 
  mutate("Basal_Area_2016" = ((dbh3^2) * 0.005454)) %>%
  mutate("Basal_Area_2022" = ((dbh4^2) * 0.005454)) %>% 
  mutate(Dead_Census = ifelse(deadPeriod == 0, "Alive", 
                              ifelse(deadPeriod %in% c(1,2,3,4,5,6,7,8,9), "After Census 1",
                                     ifelse(deadPeriod %in% c(10, 11, 12), "After Census 2", deadPeriod)))) %>% 
  .[c(18, 4:5, 19:29, 14:16, 9:12, 30, 1:2)]

bw2_comp <- bw2 %>% 
  pivot_longer(cols = starts_with("ci_"),
               names_to = "comp_census",
               values_to = "Competition") %>% 
  mutate(Census = rep(c("1", "2", "3"), length.out = n()))

bw2_basal <- bw2 %>% 
  pivot_longer(cols = starts_with("Basal_Area"),
               names_to = "ba_census",
               values_to = "Basal_Area")

bw2_hc <- bw2 %>% 
  pivot_longer(cols = starts_with("hc"),
               names_to = "height",
               values_to = "Height_Class") 

bw2_long <- bw2_comp %>% 
  mutate(Basal_Area = bw2_basal$Basal_Area) %>% 
  mutate(Height = bw2_hc$Height_Class) %>% 
  .[c(1:2, 25, 3:11, 18:19, 26, 24, 27, 20:21)] %>% 
  mutate(
    id = as.factor(id),  
    Plot = as.factor(Plot),
    Spec = as.factor(Spec),
    Census = as.factor(Census),
    moisture_class = as.factor(moisture_class),
    Dead_Census = as.factor(Dead_Census),
    dead = as.numeric(dead),  # binary format
    Competition = as.numeric(Competition),
    Basal_Area = as.numeric(Basal_Area),
    aspect = as.numeric(aspect),  
    Height = fct_relevel(as.factor(Height), c("Suppressed", "Co-Dominant", "Dominant")) # Ordinal factor
  )

### BW3 ########################################################################
bw3 <- subalpine %>%
  subset(Plot == "BW3") %>% 
  mutate("elev" = 2980) %>% 
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
  mutate(ci_1 = round(ci_1, digits = 2)) %>% 
  mutate(ci_2 = round(ci_2, digits = 2)) %>% 
  mutate(ci_3 = round(ci_3, digits = 2)) %>% 
  mutate("soil_moisture" = 6.19) %>% 
  mutate("soil_temperature" = 2.27) %>% 
  mutate("air_temperature" = 2.96) %>% 
  mutate("relative_humidity" = 65.6) %>% 
  mutate("moisture_class" = "Mesic") %>% 
  mutate("Basal_Area_1982" = ((dbh1^2) * 0.005454)) %>% 
  mutate("Basal_Area_2016" = ((dbh3^2) * 0.005454)) %>%
  mutate("Basal_Area_2022" = ((dbh4^2) * 0.005454)) %>% 
  mutate(Dead_Census = ifelse(deadPeriod == 0, "Alive", 
                              ifelse(deadPeriod %in% c(1,2,3,4,5,6,7,8,9), "After Census 1",
                                     ifelse(deadPeriod %in% c(10, 11, 12), "After Census 2", deadPeriod)))) %>% 
  .[c(18, 4:5, 19:29, 14:16, 9:12, 30, 1:2)]

bw3_comp <- bw3 %>% 
  pivot_longer(cols = starts_with("ci_"),
               names_to = "comp_census",
               values_to = "Competition") %>% 
  mutate(Census = rep(c("1", "2", "3"), length.out = n()))

bw3_basal <- bw3 %>% 
  pivot_longer(cols = starts_with("Basal_Area"),
               names_to = "ba_census",
               values_to = "Basal_Area")

bw3_hc <- bw3 %>% 
  pivot_longer(cols = starts_with("hc"),
               names_to = "height",
               values_to = "Height_Class") 

bw3_long <- bw3_comp %>% 
  mutate(Basal_Area = bw3_basal$Basal_Area) %>% 
  mutate(Height = bw3_hc$Height_Class) %>% 
  .[c(1:2, 25, 3:11, 18:19, 26, 24, 27, 20:21)] %>% 
  mutate(
    id = as.factor(id),  
    Plot = as.factor(Plot),
    Spec = as.factor(Spec),
    Census = as.factor(Census),
    moisture_class = as.factor(moisture_class),
    Dead_Census = as.factor(Dead_Census),
    dead = as.numeric(dead),  # binary format
    Competition = as.numeric(Competition),
    Basal_Area = as.numeric(Basal_Area),
    aspect = as.numeric(aspect),  
    Height = fct_relevel(as.factor(Height), c("Suppressed", "Co-Dominant", "Dominant")) # Ordinal factor
  )

### MRS4 ########################################################################
mrs4 <- subalpine %>%
  subset(Plot == "MRS4") %>% 
  mutate("elev" = 3170) %>% 
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
  mutate(ci_1 = round(ci_1, digits = 2)) %>% 
  mutate(ci_2 = round(ci_2, digits = 2)) %>% 
  mutate(ci_3 = round(ci_3, digits = 2)) %>% 
  mutate("soil_moisture" = 8.02) %>% 
  mutate("soil_temperature" = 2.85) %>% 
  mutate("air_temperature" = 2.30) %>% 
  mutate("relative_humidity" = 64.3) %>% 
  mutate("moisture_class" = "Xeric") %>% 
  mutate("Basal_Area_1982" = ((dbh1^2) * 0.005454)) %>% 
  mutate("Basal_Area_2016" = ((dbh3^2) * 0.005454)) %>%
  mutate("Basal_Area_2022" = ((dbh4^2) * 0.005454)) %>% 
  mutate(Dead_Census = ifelse(deadPeriod == 0, "Alive", 
                              ifelse(deadPeriod %in% c(1,2,3,4,5,6,7,8,9), "After Census 1",
                                     ifelse(deadPeriod %in% c(10, 11, 12), "After Census 2", deadPeriod)))) %>% 
  .[c(18, 4:5, 19:29, 14:16, 9:12, 30, 1:2)]

mrs4_comp <- mrs4 %>% 
  pivot_longer(cols = starts_with("ci_"),
               names_to = "comp_census",
               values_to = "Competition") %>% 
  mutate(Census = rep(c("1", "2", "3"), length.out = n()))

mrs4_basal <- mrs4 %>% 
  pivot_longer(cols = starts_with("Basal_Area"),
               names_to = "ba_census",
               values_to = "Basal_Area")

mrs4_hc <- mrs4 %>% 
  pivot_longer(cols = starts_with("hc"),
               names_to = "height",
               values_to = "Height_Class") 

mrs4_long <- mrs4_comp %>% 
  mutate(Basal_Area = mrs4_basal$Basal_Area) %>% 
  mutate(Height = mrs4_hc$Height_Class) %>% 
  .[c(1:2, 25, 3:11, 18:19, 26, 24, 27, 20:21)] %>% 
  mutate(
    id = as.factor(id),  
    Plot = as.factor(Plot),
    Spec = as.factor(Spec),
    Census = as.factor(Census),
    moisture_class = as.factor(moisture_class),
    Dead_Census = as.factor(Dead_Census),
    dead = as.numeric(dead),  # binary format
    Competition = as.numeric(Competition),
    Basal_Area = as.numeric(Basal_Area),
    aspect = as.numeric(aspect),  
    Height = fct_relevel(as.factor(Height), c("Suppressed", "Co-Dominant", "Dominant")) # Ordinal factor
  )

### MRS5 ########################################################################
mrs5 <- subalpine %>%
  subset(Plot == "MRS5") %>% 
  mutate("elev" = 3280) %>% 
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
  mutate(ci_1 = round(ci_1, digits = 2)) %>% 
  mutate(ci_2 = round(ci_2, digits = 2)) %>% 
  mutate(ci_3 = round(ci_3, digits = 2)) %>% 
  mutate("soil_moisture" = 31.50) %>% 
  mutate("soil_temperature" = 2.19) %>% 
  mutate("air_temperature" = 0.56) %>% 
  mutate("relative_humidity" = 66.6) %>% 
  mutate("moisture_class" = "Xeric") %>% 
  mutate("Basal_Area_1982" = ((dbh1^2) * 0.005454)) %>% 
  mutate("Basal_Area_2016" = ((dbh3^2) * 0.005454)) %>%
  mutate("Basal_Area_2022" = ((dbh4^2) * 0.005454)) %>% 
  mutate(Dead_Census = ifelse(deadPeriod == 0, "Alive", 
                              ifelse(deadPeriod %in% c(1,2,3,4,5,6,7,8,9), "After Census 1",
                                     ifelse(deadPeriod %in% c(10, 11, 12), "After Census 2", deadPeriod)))) %>% 
  .[c(18, 4:5, 19:29, 14:16, 9:12, 30, 1:2)]

mrs5_comp <- mrs5 %>% 
  pivot_longer(cols = starts_with("ci_"),
               names_to = "comp_census",
               values_to = "Competition") %>% 
  mutate(Census = rep(c("1", "2", "3"), length.out = n()))

mrs5_basal <- mrs5 %>% 
  pivot_longer(cols = starts_with("Basal_Area"),
               names_to = "ba_census",
               values_to = "Basal_Area")

mrs5_hc <- mrs5 %>% 
  pivot_longer(cols = starts_with("hc"),
               names_to = "height",
               values_to = "Height_Class") 

mrs5_long <- mrs5_comp %>% 
  mutate(Basal_Area = mrs5_basal$Basal_Area) %>% 
  mutate(Height = mrs5_hc$Height_Class) %>% 
  .[c(1:2, 25, 3:11, 18:19, 26, 24, 27, 20:21)] %>% 
  mutate(
    id = as.factor(id),  
    Plot = as.factor(Plot),
    Spec = as.factor(Spec),
    Census = as.factor(Census),
    moisture_class = as.factor(moisture_class),
    Dead_Census = as.factor(Dead_Census),
    dead = as.numeric(dead),  # binary format
    Competition = as.numeric(Competition),
    Basal_Area = as.numeric(Basal_Area),
    aspect = as.numeric(aspect),  
    Height = fct_relevel(as.factor(Height), c("Suppressed", "Co-Dominant", "Dominant")) # Ordinal factor
  )

### MRS7 ########################################################################
mrs7 <- subalpine %>%
  subset(Plot == "MRS7") %>% 
  mutate("elev" = 3260) %>% 
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
  mutate(ci_1 = round(ci_1, digits = 2)) %>% 
  mutate(ci_2 = round(ci_2, digits = 2)) %>% 
  mutate(ci_3 = round(ci_3, digits = 2)) %>% 
  mutate("soil_moisture" = 5.74) %>% 
  mutate("soil_temperature" = 2.97) %>% 
  mutate("air_temperature" = 1.12) %>% 
  mutate("relative_humidity" = 64.6) %>% 
  mutate("moisture_class" = "Xeric") %>% 
  mutate("Basal_Area_1982" = ((dbh1^2) * 0.005454)) %>% 
  mutate("Basal_Area_2016" = ((dbh3^2) * 0.005454)) %>%
  mutate("Basal_Area_2022" = ((dbh4^2) * 0.005454)) %>% 
  mutate(Dead_Census = ifelse(deadPeriod == 0, "Alive", 
                              ifelse(deadPeriod %in% c(1,2,3,4,5,6,7,8,9), "After Census 1",
                                     ifelse(deadPeriod %in% c(10, 11, 12), "After Census 2", deadPeriod)))) %>% 
  .[c(18, 4:5, 19:29, 14:16, 9:12, 30, 1:2)]

mrs7_comp <- mrs7 %>% 
  pivot_longer(cols = starts_with("ci_"),
               names_to = "comp_census",
               values_to = "Competition") %>% 
  mutate(Census = rep(c("1", "2", "3"), length.out = n()))

mrs7_basal <- mrs7 %>% 
  pivot_longer(cols = starts_with("Basal_Area"),
               names_to = "ba_census",
               values_to = "Basal_Area")

mrs7_hc <- mrs7 %>% 
  pivot_longer(cols = starts_with("hc"),
               names_to = "height",
               values_to = "Height_Class") 

mrs7_long <- mrs7_comp %>% 
  mutate(Basal_Area = mrs7_basal$Basal_Area) %>% 
  mutate(Height = mrs7_hc$Height_Class) %>% 
  .[c(1:2, 25, 3:11, 18:19, 26, 24, 27, 20:21)] %>% 
  mutate(
    id = as.factor(id),  
    Plot = as.factor(Plot),
    Spec = as.factor(Spec),
    Census = as.factor(Census),
    moisture_class = as.factor(moisture_class),
    Dead_Census = as.factor(Dead_Census),
    dead = as.numeric(dead),  # binary format
    Competition = as.numeric(Competition),
    Basal_Area = as.numeric(Basal_Area),
    aspect = as.numeric(aspect),  
    Height = fct_relevel(as.factor(Height), c("Suppressed", "Co-Dominant", "Dominant")) # Ordinal factor
  )

### MRS11 ########################################################################
mrs11 <- subalpine %>%
  subset(Plot == "MRS11") %>% 
  mutate("elev" = 3347) %>% 
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
  mutate(ci_1 = round(ci_1, digits = 2)) %>% 
  mutate(ci_2 = round(ci_2, digits = 2)) %>% 
  mutate(ci_3 = round(ci_3, digits = 2)) %>% 
  mutate("soil_moisture" = 9.87) %>% 
  mutate("soil_temperature" = 2.61) %>% 
  mutate("air_temperature" = 0.47) %>% 
  mutate("relative_humidity" = 66.6) %>% 
  mutate("moisture_class" = "Xeric") %>% 
  mutate("Basal_Area_1982" = ((dbh1^2) * 0.005454)) %>% 
  mutate("Basal_Area_2016" = ((dbh3^2) * 0.005454)) %>%
  mutate("Basal_Area_2022" = ((dbh4^2) * 0.005454)) %>% 
  mutate(Dead_Census = ifelse(deadPeriod == 0, "Alive", 
                              ifelse(deadPeriod %in% c(1,2,3,4,5,6,7,8,9), "After Census 1",
                                     ifelse(deadPeriod %in% c(10, 11, 12), "After Census 2", deadPeriod)))) %>% 
  .[c(18, 4:5, 19:29, 14:16, 9:12, 30, 1:2)]

mrs11_comp <- mrs11 %>% 
  pivot_longer(cols = starts_with("ci_"),
               names_to = "comp_census",
               values_to = "Competition") %>% 
  mutate(Census = rep(c("1", "2", "3"), length.out = n()))

mrs11_basal <- mrs11 %>% 
  pivot_longer(cols = starts_with("Basal_Area"),
               names_to = "ba_census",
               values_to = "Basal_Area")

mrs11_hc <- mrs11 %>% 
  pivot_longer(cols = starts_with("hc"),
               names_to = "height",
               values_to = "Height_Class") 

mrs11_long <- mrs11_comp %>% 
  mutate(Basal_Area = mrs11_basal$Basal_Area) %>% 
  mutate(Height = mrs11_hc$Height_Class) %>% 
  .[c(1:2, 25, 3:11, 18:19, 26, 24, 27, 20:21)] %>% 
  mutate(
    id = as.factor(id),  
    Plot = as.factor(Plot),
    Spec = as.factor(Spec),
    Census = as.factor(Census),
    moisture_class = as.factor(moisture_class),
    Dead_Census = as.factor(Dead_Census),
    dead = as.numeric(dead),  # binary format
    Competition = as.numeric(Competition),
    Basal_Area = as.numeric(Basal_Area),
    aspect = as.numeric(aspect),  
    Height = fct_relevel(as.factor(Height), c("Suppressed", "Co-Dominant", "Dominant")) # Ordinal factor
  ) %>% 
  filter(Census != 1)

### MRS12 ########################################################################
mrs12 <- subalpine %>%
  subset(Plot == "MRS12") %>% 
  mutate("elev" = 3380) %>% 
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
  mutate(ci_1 = round(ci_1, digits = 2)) %>% 
  mutate(ci_2 = round(ci_2, digits = 2)) %>% 
  mutate(ci_3 = round(ci_3, digits = 2)) %>% 
  mutate("soil_moisture" = 9.16) %>% 
  mutate("soil_temperature" = 3.65) %>% 
  mutate("air_temperature" = 4.35) %>% 
  mutate("relative_humidity" = 66.4) %>% 
  mutate("moisture_class" = "Xeric") %>% 
  mutate("Basal_Area_1982" = ((dbh1^2) * 0.005454)) %>% 
  mutate("Basal_Area_2016" = ((dbh3^2) * 0.005454)) %>%
  mutate("Basal_Area_2022" = ((dbh4^2) * 0.005454)) %>% 
  mutate(Dead_Census = ifelse(deadPeriod == 0, "Alive", 
                              ifelse(deadPeriod %in% c(1,2,3,4,5,6,7,8,9), "After Census 1",
                                     ifelse(deadPeriod %in% c(10, 11, 12), "After Census 2", deadPeriod)))) %>% 
  .[c(18, 4:5, 19:29, 14:16, 9:12, 30, 1:2)]

mrs12_comp <- mrs12 %>% 
  pivot_longer(cols = starts_with("ci_"),
               names_to = "comp_census",
               values_to = "Competition") %>% 
  mutate(Census = rep(c("1", "2", "3"), length.out = n()))

mrs12_basal <- mrs12 %>% 
  pivot_longer(cols = starts_with("Basal_Area"),
               names_to = "ba_census",
               values_to = "Basal_Area")

mrs12_hc <- mrs12 %>% 
  pivot_longer(cols = starts_with("hc"),
               names_to = "height",
               values_to = "Height_Class") 

mrs12_long <- mrs12_comp %>% 
  mutate(Basal_Area = mrs12_basal$Basal_Area) %>% 
  mutate(Height = mrs12_hc$Height_Class) %>% 
  .[c(1:2, 25, 3:11, 18:19, 26, 24, 27, 20:21)] %>% 
  mutate(
    id = as.factor(id),  
    Plot = as.factor(Plot),
    Spec = as.factor(Spec),
    Census = as.factor(Census),
    moisture_class = as.factor(moisture_class),
    Dead_Census = as.factor(Dead_Census),
    dead = as.numeric(dead),  # binary format
    Competition = as.numeric(Competition),
    Basal_Area = as.numeric(Basal_Area),
    aspect = as.numeric(aspect),  
    Height = fct_relevel(as.factor(Height), c("Suppressed", "Co-Dominant", "Dominant")) # Ordinal factor
  ) %>% 
  filter(Census != 1)

### MRS13 ########################################################################
mrs13 <- subalpine %>%
  subset(Plot == "MRS13") %>% 
  mutate("elev" = 3337) %>% 
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
  mutate(ci_1 = round(ci_1, digits = 2)) %>% 
  mutate(ci_2 = round(ci_2, digits = 2)) %>% 
  mutate(ci_3 = round(ci_3, digits = 2)) %>% 
  mutate("soil_moisture" = 13.05) %>% 
  mutate("soil_temperature" = 3.93) %>% 
  mutate("air_temperature" = 1.02) %>% 
  mutate("relative_humidity" = 67.7) %>% 
  mutate("moisture_class" = "Xeric") %>% 
  mutate("Basal_Area_1982" = ((dbh1^2) * 0.005454)) %>% 
  mutate("Basal_Area_2016" = ((dbh3^2) * 0.005454)) %>%
  mutate("Basal_Area_2022" = ((dbh4^2) * 0.005454)) %>% 
  mutate(Dead_Census = ifelse(deadPeriod == 0, "Alive", 
                              ifelse(deadPeriod %in% c(1,2,3,4,5,6,7,8,9), "After Census 1",
                                     ifelse(deadPeriod %in% c(10, 11, 12), "After Census 2", deadPeriod)))) %>% 
  .[c(18, 4:5, 19:29, 14:16, 9:12, 30, 1:2)]

mrs13_comp <- mrs13 %>% 
  pivot_longer(cols = starts_with("ci_"),
               names_to = "comp_census",
               values_to = "Competition") %>% 
  mutate(Census = rep(c("1", "2", "3"), length.out = n()))

mrs13_basal <- mrs13 %>% 
  pivot_longer(cols = starts_with("Basal_Area"),
               names_to = "ba_census",
               values_to = "Basal_Area")

mrs13_hc <- mrs13 %>% 
  pivot_longer(cols = starts_with("hc"),
               names_to = "height",
               values_to = "Height_Class") 

mrs13_long <- mrs13_comp %>% 
  mutate(Basal_Area = mrs13_basal$Basal_Area) %>% 
  mutate(Height = mrs13_hc$Height_Class) %>% 
  .[c(1:2, 25, 3:11, 18:19, 26, 24, 27, 20:21)] %>% 
  mutate(
    id = as.factor(id),  
    Plot = as.factor(Plot),
    Spec = as.factor(Spec),
    Census = as.factor(Census),
    moisture_class = as.factor(moisture_class),
    Dead_Census = as.factor(Dead_Census),
    dead = as.numeric(dead),  # binary format
    Competition = as.numeric(Competition),
    Basal_Area = as.numeric(Basal_Area),
    aspect = as.numeric(aspect),  
    Height = fct_relevel(as.factor(Height), c("Suppressed", "Co-Dominant", "Dominant")) # Ordinal factor
  ) %>% 
  filter(Census != 1)


################################################################################

# Combine all long format

sub_grow_long <- rbind.data.frame(mrs1_long, bw2_long, bw3_long,
                                  mrs4_long, mrs5_long,
                                  mrs7_long, mrs11_long, mrs12_long,
                                  mrs13_long)

save(sub_grow_long, file = "data/growth_long/growth_long.RData")
