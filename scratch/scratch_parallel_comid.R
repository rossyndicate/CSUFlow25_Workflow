library(future)      # For parallel processing
library(furrr)       # For parallel versions of purrr functions
library(dplyr)
library(purrr)

# Set up parallelization - adjust the number of workers based on your system
# Using multisession to create separate R processes
plan(multisession, workers = parallel::detectCores() - 1)

# Function to get comid using nldi
get_comid_nldi <- function(site_id) {
  result <- try(get_nldi_feature(list(featureSource = "nwissite", featureID = site_id))$comid, 
                silent = TRUE)
  if (inherits(result, "try-error") || is.null(result)) NA_character_ else result
}

# Function to get comid using coordinates
get_comid_coords <- function(geom) {
  result <- try(discover_nhdplus_id(geom), silent = TRUE)
  if (inherits(result, "try-error")) NA else result
}

# Create safe versions
safe_get_comid_nldi <- possibly(get_comid_nldi, otherwise = NA_character_, quiet = TRUE)
safe_get_comid_coords <- possibly(get_comid_coords, otherwise = NA, quiet = TRUE)

# Parallelize the API calls
gage_sites_parallel <- gage_sites %>%
  # First process NLDI calls in parallel
  mutate(comid = future_map_chr(
    nldi_compatible_site_id,
    ~safe_get_comid_nldi(.x),
    .options = furrr_options(seed = TRUE)
  )) %>%
  # Then process coordinate-based calls in parallel
  mutate(comid_coords = future_map(
    geometry,
    ~safe_get_comid_coords(.x),
    .options = furrr_options(seed = TRUE)
  ))

# When done with parallel processing
plan(sequential)