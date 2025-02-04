################################################################################
########################## Data Manipulation & Prep ############################
################################################################################

# Last update - Josh: 01/31/2025

# Libraries
source("scripts/libraries.R")
# functions
source("scripts/functions.R")

# Data
plots <- read.csv("data/growth_raw/perm_plots_all.csv")
plots_upper <- read.csv("data/growth_raw/plots_upper.csv")

# View
names(plots)
head(plots)
table(plots$Plot)

# Subset to only those with spatial data + columns needed
plots_spatial <- plots %>% 
  subset(Plot %in% c("MRS1", "BW2", "BW3", "MRS4", "MRS5", "BL6",
                     "MRS7")) %>% 
  select(c("Plot", "Tree",         # Descriptors
           "dbh1", "dbh3", "dbh4", # Diameter
           "hc1", "hc3", "hc4",    # Height
           "dead", "deadPeriod"))  # Mortality

# Load shapefiles (shp_list)
load("data/shapefiles_raw/shapefiles_list.RData")

# View shp_list
list <- sapply(seq_along(shp_list), function(i) unique(shp_list[[i]]$source_file)) %>% 
  as.data.frame()
list 

################################################################################
### Combine Tabular and Spatial Data for Each Plot #############################
################################################################################

### MRS 1 ######################################################################
mrs1_growth <- plots_spatial %>% 
  subset(Plot == "MRS1")
mrs1_growth$Tree <- as.numeric(mrs1_growth$Tree)

# Process Shapefile for join
mrs1_shp <- process_shapefile(shp_list, 4)

# join tabular and spatial data
mrs1 <- mrs1_shp %>% 
  left_join(mrs1_growth, by = c("TreeNum" = "Tree")) %>% 
  rename("Tree" = TreeNum)
mrs1; plot(st_geometry(mrs1))


### BW2 ########################################################################
bw2_growth <- plots_spatial %>% 
  subset(Plot == "BW2")
bw2_growth$Tree <- as.numeric(bw2_growth$Tree)

# Process Shapefile for join
bw2_shp <- process_shapefile(shp_list, 2)

# join tabular and spatial data
bw2 <- bw2_shp %>% 
  left_join(bw2_growth, by = c("TreeNum" = "Tree")) %>% 
  rename("Tree" = TreeNum)
bw2; plot(st_geometry(bw2))


### BW3 ########################################################################
bw3_growth <- plots_spatial %>% 
  subset(Plot == "BW3")
bw3_growth$Tree <- as.numeric(bw3_growth$Tree)

# Process Shapefile for join
bw3_shp <- process_shapefile(shp_list, 3)

# join tabular and spatial data
bw3 <- bw3_shp %>% 
  left_join(bw3_growth, by = c("TreeNum" = "Tree")) %>% 
  rename("Tree" = TreeNum)
bw3; plot(st_geometry(bw3))


### MRS 4 ######################################################################
mrs4_growth <- plots_spatial %>% 
  subset(Plot == "MRS4")
mrs4_growth$Tree <- as.numeric(mrs4_growth$Tree)

# Process Shapefile for join
mrs4_shp <- process_shapefile(shp_list, 8)

# join tabular and spatial data
mrs4 <- mrs4_shp %>% 
  left_join(mrs4_growth, by = c("TreeNum" = "Tree")) %>% 
  rename("Tree" = TreeNum)
mrs4; plot(st_geometry(mrs4))


### MRS 5 ######################################################################
mrs5_growth <- plots_spatial %>% 
  subset(Plot == "MRS5")
mrs5_growth$Tree <- as.numeric(mrs5_growth$Tree)

# Process Shapefile for join
mrs5_shp <- process_shapefile(shp_list, 9)

# join tabular and spatial data
mrs5 <- mrs5_shp %>% 
  left_join(mrs5_growth, by = c("TreeNum" = "Tree")) %>% 
  rename("Tree" = TreeNum)
mrs5; plot(st_geometry(mrs5))


### BL6 ########################################################################
bl6_growth <- plots_spatial %>% 
  subset(Plot == "BL6")
bl6_growth$Tree <- as.numeric(bl6_growth$Tree)

# Process Shapefile for join
bl6_shp <- process_shapefile(shp_list, 1)

# join tabular and spatial data
bl6 <- bl6_shp %>% 
  left_join(bl6_growth, by = c("TreeNum" = "Tree")) %>% 
  rename("Tree" = TreeNum)
bl6; plot(st_geometry(bl6))


### MRS 7 ######################################################################
mrs7_growth <- plots_spatial %>% 
  subset(Plot == "MRS7")
mrs7_growth$Tree <- as.numeric(mrs7_growth$Tree)

# Process Shapefile for join
mrs7_shp <- process_shapefile(shp_list, 10)

# join tabular and spatial data
mrs7 <- mrs7_shp %>% 
  left_join(mrs7_growth, by = c("TreeNum" = "Tree")) %>% 
  rename("Tree" = TreeNum)
mrs7; plot(st_geometry(mrs7))

### MRS 11 ######################################################################
mrs11_growth <- plots_upper %>% 
  subset(Plot == "MRS11")
mrs11_growth$Tree <- as.numeric(mrs11_growth$Tree)

# Process Shapefile for join
mrs11_shp <- process_shapefile_alt(shp_list, 5)

# join tabular and spatial data
mrs11 <- mrs11_shp %>% 
  left_join(mrs11_growth, by = c("TreeNumber" = "Tree")) %>% 
  rename("Tree" = TreeNumber)
mrs11; plot(st_geometry(mrs11))

### MRS 12 ######################################################################
mrs12_growth <- plots_upper %>% 
  subset(Plot == "MRS12")
mrs12_growth$Tree <- as.numeric(mrs12_growth$Tree)

# Process Shapefile for join
mrs12_shp <- process_shapefile_id(shp_list, 6)

# join tabular and spatial data
mrs12 <- mrs12_shp %>% 
  left_join(mrs12_growth, by = c("Id" = "Tree")) %>% 
  rename("Tree" = Id)
mrs12; plot(st_geometry(mrs12))

### MRS 13 ######################################################################
mrs13_growth <- plots_upper %>% 
  subset(Plot == "MRS13")
mrs13_growth$Tree <- as.numeric(mrs13_growth$Tree)

# Process Shapefile for join
mrs13_shp <- process_shapefile_id(shp_list, 7)

# join tabular and spatial data
mrs13 <- mrs13_shp %>% 
  left_join(mrs13_growth, by = c("Id" = "Tree")) %>% 
  rename("Tree" = Id)
mrs13; plot(st_geometry(mrs13))

################################################################################
# Save
data_growth_three_census <- rbind(mrs1, bw2, bw3, mrs4, mrs5, bl6, mrs7)
data_growth_two_census <- rbind(mrs11, mrs12, mrs13)  

save(data_growth_three_census, file = "data/growth_initial_prep/three_census_growth.RData")
save(data_growth_two_census, file = "data/growth_initial_prep/two_census_growth.RData")
  
  
  
  
  
  
  