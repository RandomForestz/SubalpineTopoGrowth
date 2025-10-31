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

mrs1_boundary <- mrs1_comp %>%
  st_union() %>%
  st_convex_hull() %>% 
  st_buffer(., -6)

mrs1_comp <- sf::st_filter(mrs1_comp, mrs1_boundary)


# BW2
bw2_comp <- data_growth %>% 
  filter(Plot == "BW2") %>% 
  mutate(hegyi(., dbh_col = "dbh1")) %>% 
  mutate(hegyi(., dbh_col = "dbh3")) %>% 
  mutate(hegyi(., dbh_col = "dbh4"))

bw2_boundary <- bw2_comp %>%
  st_union() %>%
  st_convex_hull() %>% 
  st_buffer(., -6)

bw2_comp <- sf::st_filter(bw2_comp, bw2_boundary)
plot(bw2_boundary)
plot(bw2_comp$geometry, add = T)

# BW3
bw3_comp <- data_growth %>% 
  filter(Plot == "BW3") %>% 
  mutate(hegyi(., dbh_col = "dbh1")) %>% 
  mutate(hegyi(., dbh_col = "dbh3")) %>% 
  mutate(hegyi(., dbh_col = "dbh4"))

bw3_boundary <- bw3_comp %>%
  st_union() %>%
  st_convex_hull() %>% 
  st_buffer(., -6)

bw3_comp <- sf::st_filter(bw3_comp, bw3_boundary)
plot(bw3_boundary)
plot(bw3_comp$geometry, add = T)

# MRS4
mrs4_comp <- data_growth %>% 
  filter(Plot == "MRS4") %>% 
  mutate(hegyi(., dbh_col = "dbh1")) %>% 
  mutate(hegyi(., dbh_col = "dbh3")) %>% 
  mutate(hegyi(., dbh_col = "dbh4"))

mrs4_boundary <- mrs4_comp %>%
  st_union() %>%
  st_convex_hull() %>% 
  st_buffer(., -6)

mrs4_comp <- sf::st_filter(mrs4_comp, mrs4_boundary)
plot(mrs4_boundary)
plot(mrs4_comp$geometry, add = T)

# MRS5
mrs5_comp <- data_growth %>% 
  filter(Plot == "MRS5") %>% 
  mutate(hegyi(., dbh_col = "dbh1")) %>% 
  mutate(hegyi(., dbh_col = "dbh3")) %>% 
  mutate(hegyi(., dbh_col = "dbh4"))

mrs5_boundary <- mrs5_comp %>%
  st_union() %>%
  st_convex_hull() %>% 
  st_buffer(., -6)

mrs5_comp <- sf::st_filter(mrs5_comp, mrs5_boundary)
plot(mrs5_boundary)
plot(mrs5_comp$geometry, add = T)

# MRS7
mrs7_comp <- data_growth %>% 
  filter(Plot == "MRS7") %>% 
  mutate(hegyi(., dbh_col = "dbh1")) %>% 
  mutate(hegyi(., dbh_col = "dbh3")) %>% 
  mutate(hegyi(., dbh_col = "dbh4"))

mrs7_boundary <- mrs7_comp %>%
  st_union() %>%
  st_convex_hull() %>% 
  st_buffer(., -6)

mrs7_comp <- sf::st_filter(mrs7_comp, mrs7_boundary)
plot(mrs7_boundary)
plot(mrs7_comp$geometry, add = T)

# MRS11
mrs11_comp <- data_growth %>% 
  filter(Plot == "MRS11") %>% 
  mutate(hegyi(., dbh_col = "dbh1")) %>% 
  mutate(hegyi(., dbh_col = "dbh3")) %>% 
  mutate(hegyi(., dbh_col = "dbh4"))

mrs11_boundary <- mrs11_comp %>%
  st_union() %>%
  st_convex_hull() %>% 
  st_buffer(., -6)

mrs11_comp <- sf::st_filter(mrs11_comp, mrs11_boundary)
plot(mrs11_boundary)
plot(mrs11_comp$geometry, add = T)

# MRS12
mrs12_comp <- data_growth %>% 
  filter(Plot == "MRS12") %>% 
  mutate(hegyi(., dbh_col = "dbh1")) %>% 
  mutate(hegyi(., dbh_col = "dbh3")) %>% 
  mutate(hegyi(., dbh_col = "dbh4"))

mrs12_boundary <- mrs12_comp %>%
  st_union() %>%
  st_convex_hull() %>% 
  st_buffer(., -6)

mrs12_comp <- sf::st_filter(mrs12_comp, mrs12_boundary)
plot(mrs12_boundary)
plot(mrs12_comp$geometry, add = T)

# MRS13
mrs13_comp <- data_growth %>% 
  filter(Plot == "MRS13") %>% 
  mutate(hegyi(., dbh_col = "dbh1")) %>% 
  mutate(hegyi(., dbh_col = "dbh3")) %>% 
  mutate(hegyi(., dbh_col = "dbh4"))

mrs13_boundary <- mrs13_comp %>%
  st_union() %>%
  st_convex_hull() %>% 
  st_buffer(., -6)

mrs13_comp <- sf::st_filter(mrs13_comp, mrs13_boundary)
plot(mrs13_boundary)
plot(mrs13_comp$geometry, add = T)



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
