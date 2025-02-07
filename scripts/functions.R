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

process_shapefile_alt <- function(shp_list, index) {
  shp_list[[index]] %>%
    # Extract coordinates
    mutate(
      X = st_coordinates(geometry)[, 1],  # X coordinate
      Y = st_coordinates(geometry)[, 2]   # Y coordinate
    ) %>%
    # Bind TreeNum and coordinates together
    select(X, Y, TreeNumber) %>%
    # Convert to sf object with CRS 26913
    st_as_sf(coords = c("X", "Y"), crs = 26913)
}

process_shapefile_id <- function(shp_list, index) {
  shp_list[[index]] %>%
    # Extract coordinates
    mutate(
      X = st_coordinates(geometry)[, 1],  # X coordinate
      Y = st_coordinates(geometry)[, 2]   # Y coordinate
    ) %>%
    # Bind TreeNum and coordinates together
    select(X, Y, Id) %>%
    # Convert to sf object with CRS 26913
    st_as_sf(coords = c("X", "Y"), crs = 26913)
}

################################################################################
# Compute Hegyi's Index
################################################################################

# Function to compute Hegyi's Competition Index with a size condition
compute_hegyi_ci <- function(data, size_threshold = 10) {
  # Ensure required columns exist
  if (!all(c("X", "Y", "dbh") %in% colnames(data))) {
    stop("Data must contain 'X', 'Y', and 'dbh' columns.")
  }
  
  # Compute pairwise distances
  coords <- as.matrix(data[, c("X", "Y")])
  dist_matrix <- spDists(coords, longlat = FALSE)  # Euclidean distances
  
  # Initialize CI vector
  hegyi_index <- numeric(nrow(data))
  
  # Calculate Hegyi’s CI for each tree
  for (i in 1:nrow(data)) {
    di <- data$dbh[i]
    # Neighbors are trees that are within the size threshold and distance > 0
    neighbors <- which(dist_matrix[i, ] > 0 & abs(data$dbh - di) <= size_threshold)
    
    # Ensure there are neighbors before computing sum
    if (length(neighbors) > 0) {
      hegyi_index[i] <- sum(((data$dbh[neighbors] / di) / dist_matrix[i, neighbors]), na.rm = TRUE)
    } else {
      hegyi_index[i] <- NA  # No competition if no neighbors
    }
  }
  
  # Return original data with computed CI
  data$Hegyi_CI <- hegyi_index
  return(data)
}


################################################################################
# Prepare dataset with comp
################################################################################

compute_census_ci <- function(data, plot_name, dbh_column, size_threshold = 10) {
  data %>%
    subset(Plot == plot_name) %>%
    st_drop_geometry() %>%
    select(X, Y, dbh = dbh_column) %>%
    st_drop_geometry() %>%
    compute_hegyi_ci(size_threshold = size_threshold)
}

# Function to create the competition index for multiple censuses
compute_comp_ci <- function(data, plot_name = plot, size_threshold = 6) {
  census1 <- compute_census_ci(data, plot_name, "dbh1", size_threshold)
  census2 <- compute_census_ci(data, plot_name, "dbh3", size_threshold)
  census3 <- compute_census_ci(data, plot_name, "dbh4", size_threshold)
  
  data_growth %>%
    subset(Plot == plot_name) %>%
    mutate(ci_1 = census1$Hegyi_CI) %>%
    mutate(ci_2 = census2$Hegyi_CI) %>%
    mutate(ci_3 = census3$Hegyi_CI)
}


################################################################################
# Edge buffer for competition edge effects
################################################################################

edge_buffer <- function(shp, dist_m) {
  convex <- process_shapefile(shp) %>%  
    st_union() %>% 
    st_convex_hull()
  
  buff <- st_buffer(convex, dist = dist_m)
  return(buff)
}
