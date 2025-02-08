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
  mutate("moisture" = "Xeric") %>% 
  mutate("Basal_Area_1982" = ((dbh1^2) * 0.005454)) %>% 
  mutate("Basal_Area_2016" = ((dbh3^2) * 0.005454)) %>%
  mutate("Basal_Area_2022" = ((dbh4^2) * 0.005454)) %>% 
  mutate(Dead_Census = ifelse(deadPeriod == 0, "Alive", 
                              ifelse(deadPeriod %in% c(1,2,3,4,5,6,7,8,9), "After Census 1",
                                     ifelse(deadPeriod %in% c(10, 11, 12), "After Census 2", deadPeriod)))) %>% 
  .[c(18, 4:5, 23:25, 9:11, 14:16, 19:22, 12, 26, 17)]

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
  .[c(1:3, 19, 10:15, 18, 20, 21, 16)] %>% 
  mutate(
    id = as.factor(id),  
    Plot = as.factor(Plot),
    Spec = as.factor(Spec),
    Census = as.factor(Census),
    moisture = as.factor(moisture),
    Dead_Census = as.factor(Dead_Census),
    dead = as.numeric(dead),  # binary format
    Competition = as.numeric(Competition),
    Basal_Area = as.numeric(Basal_Area),
    aspect = as.numeric(aspect),  
    Height = fct_relevel(as.factor(Height), c("Suppressed", "Co-Dominant", "Dominant")) # Ordinal factor
  )


### BW2 ########################################################################