################################################################################
########################## Shapefile Manipulation ##############################
################################################################################

# Last update - Josh: 01/31/2025

# Libraries
source("scripts/libraries.R")

# Upload Shapefiles
shp_path <- "data/shapefiles_raw" # Path

shp_files <- list.files(shp_path, pattern = "\\.shp$", full.names = T) # Specify

read_shp_with_filename <- function(file) {
  st_read(file) %>%
    mutate(source_file = basename(file))  # Add filename as a column
}

shp_list <- map(shp_files, read_shp_with_filename) # Read 

# Save as .RData
save(shp_list, file = "data/shapefiles_raw/shapefiles_list.RData")
