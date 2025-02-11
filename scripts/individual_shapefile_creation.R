################################################################################
########################## Individual Shapefiles ###############################
################################################################################

# Last update - Josh: 02/10/2025

# Libraries
source("scripts/libraries.R")
# functions
source("scripts/functions.R")

# Data 
load("data/growth_long/subalpine.RData")

################################################################################
### filter by plot 
################################################################################

mrs_1 <- subalpine %>% 
  filter(Plot == "MRS1")
bw_2 <- subalpine %>% 
  filter(Plot == "BW2")
bw_3 <- subalpine %>% 
  filter(Plot == "BW3")
mrs_4 <- subalpine %>% 
  filter(Plot == "MRS4")
mrs_5 <- subalpine %>% 
  filter(Plot == "MRS5")
mrs_7 <- subalpine %>% 
  filter(Plot == "MRS7")
mrs_11 <- subalpine %>% 
  filter(Plot == "MRS11")
mrs_12 <- subalpine %>% 
  filter(Plot == "MRS12")
mrs_13 <- subalpine %>% 
  filter(Plot == "MRS13")

sf::write_sf(mrs_1, "data/shapefiles_complete/mrs_1.shp")
sf::write_sf(bw_2, "data/shapefiles_complete/bw_2.shp")
sf::write_sf(bw_3, "data/shapefiles_complete/bw_3.shp")
sf::write_sf(mrs_4, "data/shapefiles_complete/mrs_4.shp")
sf::write_sf(mrs_5, "data/shapefiles_complete/mrs_5.shp")
sf::write_sf(mrs_7, "data/shapefiles_complete/mrs_7.shp")
sf::write_sf(mrs_11, "data/shapefiles_complete/mrs_11.shp")
sf::write_sf(mrs_12, "data/shapefiles_complete/mrs_12.shp")
sf::write_sf(mrs_13, "data/shapefiles_complete/mrs_13.shp")





