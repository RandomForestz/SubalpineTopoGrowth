################################################################################
########################## Competition Indices #################################
################################################################################

# Last update - Josh: 02/09/2025

# Libraries
source("scripts/libraries.R")
# functions
source("scripts/functions.R")

# Data prepped for competition indexes
load("data/growth_initial_prep/data_growth.RData")

################################################################################
# Compute Hegyi's Index 
################################################################################

# MRS1
mrs1_comp <- data_growth %>% 
  filter(Plot == "MRS1") %>% 
  mutate(hegyi(., dbh_col = "dbh1")) %>% 
  mutate(hegyi(., dbh_col = "dbh3")) %>% 
  mutate(hegyi(., dbh_col = "dbh4"))

# BW2
bw2_comp <- data_growth %>% 
  filter(Plot == "BW2") %>% 
  mutate(hegyi(., dbh_col = "dbh1")) %>% 
  mutate(hegyi(., dbh_col = "dbh3")) %>% 
  mutate(hegyi(., dbh_col = "dbh4"))

# BW3
bw3_comp <- data_growth %>% 
  filter(Plot == "BW3") %>% 
  mutate(hegyi(., dbh_col = "dbh1")) %>% 
  mutate(hegyi(., dbh_col = "dbh3")) %>% 
  mutate(hegyi(., dbh_col = "dbh4"))

# MRS4
mrs4_comp <- data_growth %>% 
  filter(Plot == "MRS4") %>% 
  mutate(hegyi(., dbh_col = "dbh1")) %>% 
  mutate(hegyi(., dbh_col = "dbh3")) %>% 
  mutate(hegyi(., dbh_col = "dbh4"))

# MRS5
mrs5_comp <- data_growth %>% 
  filter(Plot == "MRS5") %>% 
  mutate(hegyi(., dbh_col = "dbh1")) %>% 
  mutate(hegyi(., dbh_col = "dbh3")) %>% 
  mutate(hegyi(., dbh_col = "dbh4"))

# MRS7
mrs7_comp <- data_growth %>% 
  filter(Plot == "MRS7") %>% 
  mutate(hegyi(., dbh_col = "dbh1")) %>% 
  mutate(hegyi(., dbh_col = "dbh3")) %>% 
  mutate(hegyi(., dbh_col = "dbh4"))

# MRS11
mrs11_comp <- data_growth %>% 
  filter(Plot == "MRS11") %>% 
  mutate(hegyi(., dbh_col = "dbh1")) %>% 
  mutate(hegyi(., dbh_col = "dbh3")) %>% 
  mutate(hegyi(., dbh_col = "dbh4"))

# MRS12
mrs12_comp <- data_growth %>% 
  filter(Plot == "MRS12") %>% 
  mutate(hegyi(., dbh_col = "dbh1")) %>% 
  mutate(hegyi(., dbh_col = "dbh3")) %>% 
  mutate(hegyi(., dbh_col = "dbh4"))

# MRS13
mrs13_comp <- data_growth %>% 
  filter(Plot == "MRS13") %>% 
  mutate(hegyi(., dbh_col = "dbh1")) %>% 
  mutate(hegyi(., dbh_col = "dbh3")) %>% 
  mutate(hegyi(., dbh_col = "dbh4"))


# Combine
growth_comp <- rbind(mrs1_comp,
                     bw2_comp,
                     bw3_comp,
                     mrs4_comp, 
                     mrs5_comp,
                     mrs7_comp,
                     mrs11_comp,
                     mrs12_comp,
                     mrs13_comp)


# save
save(growth_comp, file = "data/comp/growth_comp.RData")
