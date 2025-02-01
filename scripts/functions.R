################################################################################
############################## Functions #######################################
################################################################################

# Taking raw shapefiles from list and processing to be seperate and useful

process_shapefile <- function(shp_list, index) {
  shp_list[[index]] %>%
    # Extract coordinates
    mutate(
      X = st_coordinates(geometry)[, 1],  # X coordinate
      Y = st_coordinates(geometry)[, 2]   # Y coordinate
    ) %>%
    # Bind TreeNum and coordinates together
    select(X, Y, TreeNum) %>%
    # Convert to sf object with CRS 26913
    st_as_sf(coords = c("X", "Y"), crs = 26913)
}