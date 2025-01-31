################################################################################
########################## Shapefile Manipulation ##############################
################################################################################

# Last update - Josh: 01/31/2025

# Libraries
source("scripts/libraries.R")

# Upload Shapefiles
shp_path <- "data/shapefiles_raw" # Path
shp_files <- list.files(shp_path, pattern = "\\.shp$", full.names = T) # Specify
shp_list <- lapply(shp_files, st_read) # Read 

# Save as .RData
save(shp_list, file = "data/shapefiles_raw.RData")
