################################################################################
########################## Topographic Variables ###############################
################################################################################

# Last update - Josh: 02/04/2025

# Libraries
source("scripts/libraries.R")
# functions
source("scripts/functions.R")

# Data prepped with compeititon indexes
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
  mutate("basal_area_1982" = ((dbh1^2) * 0.005454)) %>% 
  mutate("basal_area_2016" = ((dbh3^2) * 0.005454)) %>%
  mutate("basal_area_2022" = ((dbh4^2) * 0.005454)) %>% 
  mutate(dead_census = ifelse(deadPeriod == 0, "Alive", 
                              ifelse(deadPeriod %in% c(1,2,3,4,5,6,7,8,9), "After Census 1",
                                     ifelse(deadPeriod %in% c(10, 11, 12), "After Census 2", deadPeriod)))) %>% 
  .[c(18, 4:5, 19:29, 14:16, 9:12, 30, 1:2)]

mrs1_comp <- mrs1 %>% 
  pivot_longer(cols = starts_with("ci_"),
               names_to = "comp_census",
               values_to = "compeititon") %>% 
  mutate(Census = rep(c("1", "2", "3"), length.out = n()))

mrs1_basal <- mrs1 %>% 
  pivot_longer(cols = starts_with("basal_area"),
               names_to = "ba_census",
               values_to = "basal_area")

mrs1_hc <- mrs1 %>% 
  pivot_longer(cols = starts_with("hc"),
               names_to = "height",
               values_to = "height_class") 

mrs1_long <- mrs1_comp %>% 
  mutate(basal_area = mrs1_basal$basal_area) %>% 
  mutate(height = mrs1_hc$height_class) %>% 
  .[c(1:2, 25, 3:11, 18:19, 26, 24, 27, 20:21)] %>% 
  mutate(
    id = as.factor(id),  
    Plot = as.factor(Plot),
    Spec = as.factor(Spec),
    Census = as.factor(Census),
    moisture_class = as.factor(moisture_class),
    dead_census = as.factor(dead_census),
    dead = as.numeric(dead),  # binary format
    compeititon = as.numeric(compeititon),
    basal_area = as.numeric(basal_area),
    aspect = as.numeric(aspect),  
    height = fct_relevel(as.factor(height), c("Suppressed", "Co-Dominant", "Dominant")) # Ordinal factor
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
  mutate("basal_area_1982" = ((dbh1^2) * 0.005454)) %>% 
  mutate("basal_area_2016" = ((dbh3^2) * 0.005454)) %>%
  mutate("basal_area_2022" = ((dbh4^2) * 0.005454)) %>% 
  mutate(dead_census = ifelse(deadPeriod == 0, "Alive", 
                              ifelse(deadPeriod %in% c(1,2,3,4,5,6,7,8,9), "After Census 1",
                                     ifelse(deadPeriod %in% c(10, 11, 12), "After Census 2", deadPeriod)))) %>% 
  .[c(18, 4:5, 19:29, 14:16, 9:12, 30, 1:2)]

bw2_comp <- bw2 %>% 
  pivot_longer(cols = starts_with("ci_"),
               names_to = "comp_census",
               values_to = "compeititon") %>% 
  mutate(Census = rep(c("1", "2", "3"), length.out = n()))

bw2_basal <- bw2 %>% 
  pivot_longer(cols = starts_with("basal_area"),
               names_to = "ba_census",
               values_to = "basal_area")

bw2_hc <- bw2 %>% 
  pivot_longer(cols = starts_with("hc"),
               names_to = "height",
               values_to = "height_class") 

bw2_long <- bw2_comp %>% 
  mutate(basal_area = bw2_basal$basal_area) %>% 
  mutate(height = bw2_hc$height_class) %>% 
  .[c(1:2, 25, 3:11, 18:19, 26, 24, 27, 20:21)] %>% 
  mutate(
    id = as.factor(id),  
    Plot = as.factor(Plot),
    Spec = as.factor(Spec),
    Census = as.factor(Census),
    moisture_class = as.factor(moisture_class),
    dead_census = as.factor(dead_census),
    dead = as.numeric(dead),  # binary format
    compeititon = as.numeric(compeititon),
    basal_area = as.numeric(basal_area),
    aspect = as.numeric(aspect),  
    height = fct_relevel(as.factor(height), c("Suppressed", "Co-Dominant", "Dominant")) # Ordinal factor
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
  mutate("basal_area_1982" = ((dbh1^2) * 0.005454)) %>% 
  mutate("basal_area_2016" = ((dbh3^2) * 0.005454)) %>%
  mutate("basal_area_2022" = ((dbh4^2) * 0.005454)) %>% 
  mutate(dead_census = ifelse(deadPeriod == 0, "Alive", 
                              ifelse(deadPeriod %in% c(1,2,3,4,5,6,7,8,9), "After Census 1",
                                     ifelse(deadPeriod %in% c(10, 11, 12), "After Census 2", deadPeriod)))) %>% 
  .[c(18, 4:5, 19:29, 14:16, 9:12, 30, 1:2)]

bw3_comp <- bw3 %>% 
  pivot_longer(cols = starts_with("ci_"),
               names_to = "comp_census",
               values_to = "compeititon") %>% 
  mutate(Census = rep(c("1", "2", "3"), length.out = n()))

bw3_basal <- bw3 %>% 
  pivot_longer(cols = starts_with("basal_area"),
               names_to = "ba_census",
               values_to = "basal_area")

bw3_hc <- bw3 %>% 
  pivot_longer(cols = starts_with("hc"),
               names_to = "height",
               values_to = "height_class") 

bw3_long <- bw3_comp %>% 
  mutate(basal_area = bw3_basal$basal_area) %>% 
  mutate(height = bw3_hc$height_class) %>% 
  .[c(1:2, 25, 3:11, 18:19, 26, 24, 27, 20:21)] %>% 
  mutate(
    id = as.factor(id),  
    Plot = as.factor(Plot),
    Spec = as.factor(Spec),
    Census = as.factor(Census),
    moisture_class = as.factor(moisture_class),
    dead_census = as.factor(dead_census),
    dead = as.numeric(dead),  # binary format
    compeititon = as.numeric(compeititon),
    basal_area = as.numeric(basal_area),
    aspect = as.numeric(aspect),  
    height = fct_relevel(as.factor(height), c("Suppressed", "Co-Dominant", "Dominant")) # Ordinal factor
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
  mutate("moisture_class" = "Mesic") %>% 
  mutate("basal_area_1982" = ((dbh1^2) * 0.005454)) %>% 
  mutate("basal_area_2016" = ((dbh3^2) * 0.005454)) %>%
  mutate("basal_area_2022" = ((dbh4^2) * 0.005454)) %>% 
  mutate(dead_census = ifelse(deadPeriod == 0, "Alive", 
                              ifelse(deadPeriod %in% c(1,2,3,4,5,6,7,8,9), "After Census 1",
                                     ifelse(deadPeriod %in% c(10, 11, 12), "After Census 2", deadPeriod)))) %>% 
  .[c(18, 4:5, 19:29, 14:16, 9:12, 30, 1:2)]

mrs4_comp <- mrs4 %>% 
  pivot_longer(cols = starts_with("ci_"),
               names_to = "comp_census",
               values_to = "compeititon") %>% 
  mutate(Census = rep(c("1", "2", "3"), length.out = n()))

mrs4_basal <- mrs4 %>% 
  pivot_longer(cols = starts_with("basal_area"),
               names_to = "ba_census",
               values_to = "basal_area")

mrs4_hc <- mrs4 %>% 
  pivot_longer(cols = starts_with("hc"),
               names_to = "height",
               values_to = "height_class") 

mrs4_long <- mrs4_comp %>% 
  mutate(basal_area = mrs4_basal$basal_area) %>% 
  mutate(height = mrs4_hc$height_class) %>% 
  .[c(1:2, 25, 3:11, 18:19, 26, 24, 27, 20:21)] %>% 
  mutate(
    id = as.factor(id),  
    Plot = as.factor(Plot),
    Spec = as.factor(Spec),
    Census = as.factor(Census),
    moisture_class = as.factor(moisture_class),
    dead_census = as.factor(dead_census),
    dead = as.numeric(dead),  # binary format
    compeititon = as.numeric(compeititon),
    basal_area = as.numeric(basal_area),
    aspect = as.numeric(aspect),  
    height = fct_relevel(as.factor(height), c("Suppressed", "Co-Dominant", "Dominant")) # Ordinal factor
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
  mutate("moisture_class" = "Hydric") %>% 
  mutate("basal_area_1982" = ((dbh1^2) * 0.005454)) %>% 
  mutate("basal_area_2016" = ((dbh3^2) * 0.005454)) %>%
  mutate("basal_area_2022" = ((dbh4^2) * 0.005454)) %>% 
  mutate(dead_census = ifelse(deadPeriod == 0, "Alive", 
                              ifelse(deadPeriod %in% c(1,2,3,4,5,6,7,8,9), "After Census 1",
                                     ifelse(deadPeriod %in% c(10, 11, 12), "After Census 2", deadPeriod)))) %>% 
  .[c(18, 4:5, 19:29, 14:16, 9:12, 30, 1:2)]

mrs5_comp <- mrs5 %>% 
  pivot_longer(cols = starts_with("ci_"),
               names_to = "comp_census",
               values_to = "compeititon") %>% 
  mutate(Census = rep(c("1", "2", "3"), length.out = n()))

mrs5_basal <- mrs5 %>% 
  pivot_longer(cols = starts_with("basal_area"),
               names_to = "ba_census",
               values_to = "basal_area")

mrs5_hc <- mrs5 %>% 
  pivot_longer(cols = starts_with("hc"),
               names_to = "height",
               values_to = "height_class") 

mrs5_long <- mrs5_comp %>% 
  mutate(basal_area = mrs5_basal$basal_area) %>% 
  mutate(height = mrs5_hc$height_class) %>% 
  .[c(1:2, 25, 3:11, 18:19, 26, 24, 27, 20:21)] %>% 
  mutate(
    id = as.factor(id),  
    Plot = as.factor(Plot),
    Spec = as.factor(Spec),
    Census = as.factor(Census),
    moisture_class = as.factor(moisture_class),
    dead_census = as.factor(dead_census),
    dead = as.numeric(dead),  # binary format
    compeititon = as.numeric(compeititon),
    basal_area = as.numeric(basal_area),
    aspect = as.numeric(aspect),  
    height = fct_relevel(as.factor(height), c("Suppressed", "Co-Dominant", "Dominant")) # Ordinal factor
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
  mutate("basal_area_1982" = ((dbh1^2) * 0.005454)) %>% 
  mutate("basal_area_2016" = ((dbh3^2) * 0.005454)) %>%
  mutate("basal_area_2022" = ((dbh4^2) * 0.005454)) %>% 
  mutate(dead_census = ifelse(deadPeriod == 0, "Alive", 
                              ifelse(deadPeriod %in% c(1,2,3,4,5,6,7,8,9), "After Census 1",
                                     ifelse(deadPeriod %in% c(10, 11, 12), "After Census 2", deadPeriod)))) %>% 
  .[c(18, 4:5, 19:29, 14:16, 9:12, 30, 1:2)]

mrs7_comp <- mrs7 %>% 
  pivot_longer(cols = starts_with("ci_"),
               names_to = "comp_census",
               values_to = "compeititon") %>% 
  mutate(Census = rep(c("1", "2", "3"), length.out = n()))

mrs7_basal <- mrs7 %>% 
  pivot_longer(cols = starts_with("basal_area"),
               names_to = "ba_census",
               values_to = "basal_area")

mrs7_hc <- mrs7 %>% 
  pivot_longer(cols = starts_with("hc"),
               names_to = "height",
               values_to = "height_class") 

mrs7_long <- mrs7_comp %>% 
  mutate(basal_area = mrs7_basal$basal_area) %>% 
  mutate(height = mrs7_hc$height_class) %>% 
  .[c(1:2, 25, 3:11, 18:19, 26, 24, 27, 20:21)] %>% 
  mutate(
    id = as.factor(id),  
    Plot = as.factor(Plot),
    Spec = as.factor(Spec),
    Census = as.factor(Census),
    moisture_class = as.factor(moisture_class),
    dead_census = as.factor(dead_census),
    dead = as.numeric(dead),  # binary format
    compeititon = as.numeric(compeititon),
    basal_area = as.numeric(basal_area),
    aspect = as.numeric(aspect),  
    height = fct_relevel(as.factor(height), c("Suppressed", "Co-Dominant", "Dominant")) # Ordinal factor
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
  mutate("moisture_class" = "Mesic") %>% 
  mutate("basal_area_1982" = ((dbh1^2) * 0.005454)) %>% 
  mutate("basal_area_2016" = ((dbh3^2) * 0.005454)) %>%
  mutate("basal_area_2022" = ((dbh4^2) * 0.005454)) %>% 
  mutate(dead_census = ifelse(deadPeriod == 0, "Alive", 
                              ifelse(deadPeriod %in% c(1,2,3,4,5,6,7,8,9), "After Census 1",
                                     ifelse(deadPeriod %in% c(10, 11, 12), "After Census 2", deadPeriod)))) %>% 
  .[c(18, 4:5, 19:29, 14:16, 9:12, 30, 1:2)]

mrs11_comp <- mrs11 %>% 
  pivot_longer(cols = starts_with("ci_"),
               names_to = "comp_census",
               values_to = "compeititon") %>% 
  mutate(Census = rep(c("1", "2", "3"), length.out = n()))

mrs11_basal <- mrs11 %>% 
  pivot_longer(cols = starts_with("basal_area"),
               names_to = "ba_census",
               values_to = "basal_area")

mrs11_hc <- mrs11 %>% 
  pivot_longer(cols = starts_with("hc"),
               names_to = "height",
               values_to = "height_class") 

mrs11_long <- mrs11_comp %>% 
  mutate(basal_area = mrs11_basal$basal_area) %>% 
  mutate(height = mrs11_hc$height_class) %>% 
  .[c(1:2, 25, 3:11, 18:19, 26, 24, 27, 20:21)] %>% 
  mutate(
    id = as.factor(id),  
    Plot = as.factor(Plot),
    Spec = as.factor(Spec),
    Census = as.factor(Census),
    moisture_class = as.factor(moisture_class),
    dead_census = as.factor(dead_census),
    dead = as.numeric(dead),  # binary format
    compeititon = as.numeric(compeititon),
    basal_area = as.numeric(basal_area),
    aspect = as.numeric(aspect),  
    height = fct_relevel(as.factor(height), c("Suppressed", "Co-Dominant", "Dominant")) # Ordinal factor
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
  mutate("basal_area_1982" = ((dbh1^2) * 0.005454)) %>% 
  mutate("basal_area_2016" = ((dbh3^2) * 0.005454)) %>%
  mutate("basal_area_2022" = ((dbh4^2) * 0.005454)) %>% 
  mutate(dead_census = ifelse(deadPeriod == 0, "Alive", 
                              ifelse(deadPeriod %in% c(1,2,3,4,5,6,7,8,9), "After Census 1",
                                     ifelse(deadPeriod %in% c(10, 11, 12), "After Census 2", deadPeriod)))) %>% 
  .[c(18, 4:5, 19:29, 14:16, 9:12, 30, 1:2)]

mrs12_comp <- mrs12 %>% 
  pivot_longer(cols = starts_with("ci_"),
               names_to = "comp_census",
               values_to = "compeititon") %>% 
  mutate(Census = rep(c("1", "2", "3"), length.out = n()))

mrs12_basal <- mrs12 %>% 
  pivot_longer(cols = starts_with("basal_area"),
               names_to = "ba_census",
               values_to = "basal_area")

mrs12_hc <- mrs12 %>% 
  pivot_longer(cols = starts_with("hc"),
               names_to = "height",
               values_to = "height_class") 

mrs12_long <- mrs12_comp %>% 
  mutate(basal_area = mrs12_basal$basal_area) %>% 
  mutate(height = mrs12_hc$height_class) %>% 
  .[c(1:2, 25, 3:11, 18:19, 26, 24, 27, 20:21)] %>% 
  mutate(
    id = as.factor(id),  
    Plot = as.factor(Plot),
    Spec = as.factor(Spec),
    Census = as.factor(Census),
    moisture_class = as.factor(moisture_class),
    dead_census = as.factor(dead_census),
    dead = as.numeric(dead),  # binary format
    compeititon = as.numeric(compeititon),
    basal_area = as.numeric(basal_area),
    aspect = as.numeric(aspect),  
    height = fct_relevel(as.factor(height), c("Suppressed", "Co-Dominant", "Dominant")) # Ordinal factor
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
  mutate("basal_area_1982" = ((dbh1^2) * 0.005454)) %>% 
  mutate("basal_area_2016" = ((dbh3^2) * 0.005454)) %>%
  mutate("basal_area_2022" = ((dbh4^2) * 0.005454)) %>% 
  mutate(dead_census = ifelse(deadPeriod == 0, "Alive", 
                              ifelse(deadPeriod %in% c(1,2,3,4,5,6,7,8,9), "After Census 1",
                                     ifelse(deadPeriod %in% c(10, 11, 12), "After Census 2", deadPeriod)))) %>% 
  .[c(18, 4:5, 19:29, 14:16, 9:12, 30, 1:2)]

mrs13_comp <- mrs13 %>% 
  pivot_longer(cols = starts_with("ci_"),
               names_to = "comp_census",
               values_to = "compeititon") %>% 
  mutate(Census = rep(c("1", "2", "3"), length.out = n()))

mrs13_basal <- mrs13 %>% 
  pivot_longer(cols = starts_with("basal_area"),
               names_to = "ba_census",
               values_to = "basal_area")

mrs13_hc <- mrs13 %>% 
  pivot_longer(cols = starts_with("hc"),
               names_to = "height",
               values_to = "height_class") 

mrs13_long <- mrs13_comp %>% 
  mutate(basal_area = mrs13_basal$basal_area) %>% 
  mutate(height = mrs13_hc$height_class) %>% 
  .[c(1:2, 25, 3:11, 18:19, 26, 24, 27, 20:21)] %>% 
  mutate(
    id = as.factor(id),  
    Plot = as.factor(Plot),
    Spec = as.factor(Spec),
    Census = as.factor(Census),
    moisture_class = as.factor(moisture_class),
    dead_census = as.factor(dead_census),
    dead = as.numeric(dead),  # binary format
    compeititon = as.numeric(compeititon),
    basal_area = as.numeric(basal_area),
    aspect = as.numeric(aspect),  
    height = fct_relevel(as.factor(height), c("Suppressed", "Co-Dominant", "Dominant")) # Ordinal factor
  ) %>% 
  filter(Census != 1)


################################################################################

# Combine all long format

sub_grow_long <- rbind.data.frame(mrs1_long, bw2_long, bw3_long,
                                  mrs4_long, mrs5_long,
                                  mrs7_long, mrs11_long, mrs12_long,
                                  mrs13_long)

subalpine <- sub_grow_long %>% 
  filter(Spec %in% c("PICO", "PIFL", "PIEN", "ABLA")) # remove any non-target trees

subalpine_non_spatial <- sf::st_drop_geometry(subalpine)

save(subalpine, file = "data/growth_long/subalpine.RData")
save(subalpine_non_spatial, file = "data/growth_long/subalpine_non_spatial.RData")
