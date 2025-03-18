# Original ----
# load in the NHD as a table. This table lists all COMIDs in CONUS and allows you to "navigate" the NHD.
nhd <- read_csv(here("data", "nhd_flow_network.csv"))

watershed_delineator <- function(STUSPS, site_no, station_nm, comid, geometry) {
  # Retrieve upstream COMIDs ----
  upstream <- tryCatch(
    suppressWarnings(nhdplusTools::get_UT(nhd, comid)),
    error = function(e) {
      message("Error retrieving upstream COMIDs for ", site_no, ": ", e$message)
      return(NULL)
    }
  )
  
  ## Handle cases where no upstream COMIDs are found (headwater site)
  if (is.null(upstream) | nrow(upstream) == 0) {
    message("No upstream COMIDs found for ", site_no, " - treating as headwater site")
    upstream <- comid  # Use the site's own COMID
  }
  
  # Retrieve catchments with possibly() to account for possible errors
  catchments <- possibly(
    nhdplusTools::get_nhdplus,
    otherwise = NULL,
    quiet = TRUE
  )(
    comid = upstream,
    realization = "catchment",
    t_srs = 4269
  )
  
  # Process catchment data
  if (is.null(catchments)) {
    message("Catchment retrieval failed for ", site_no)
    return(NULL)
  }
  
  watershed <- catchments %>%
    st_make_valid() %>% # ensure geometries are valid
    distinct(featureid, .keep_all = TRUE) %>% # remove duplicates
    summarize() %>%  # Dissolve into single polygon
    nngeo::st_remove_holes() %>% # remove holes
    mutate(
      STUSPS = STUSPS,
      site_no = site_no,
      comid = comid
    ) %>%
    st_as_sf() # convert to sf object if not already
  
  message("Successfully processed: ", station_nm, "\n")
  return(watershed)
}

watersheds <- pmap(gage_sites, safely(watershed_delineator), .progress = TRUE) %>% 
  transpose() %>%  # Separate results and errors
  pluck(1) %>%  # Extract results
  compact() %>% # Remove NULL results
  bind_rows() # Combine into a single data frame

# Save watershed dataframe
# write_rds(watersheds, here("data", "watershed_polygons.RDS"))

# View map 
mapview::mapview(watersheds) 

# remove unnecessary objects
rm("nhd")

# Updated ----

# Set safe functions for server calls ----
# Retry get UT
retry_get_UT <- function(nhd_arg, comid_arg, site_no_arg) {
  
  for (attempt in 1:5) {
    # Random sleep time between 0 and 3 seconds
    Sys.sleep(runif(1,0,3))
    
    # Retrieve upstream COMIDs ----
    upstream_COMIDs <- tryCatch(
      suppressWarnings(nhdplusTools::get_UT(nhd_arg, comid_arg)),
      error = function(e) {
        message("\nError retrieving upstream COMIDs for ", site_no, ": ", e$message)
        NULL
      }
    )
    
    if (!is.null(upstream_COMIDs) & nrow(upstream_COMIDs) > 0) return(upstream_COMIDs)
    
    # Exponential backoff with some randomness to prevent concurrent requests
    if (attempt < 5) {
      wait_time <- 2^attempt + runif(1, 0, 1)
      message("\nWaiting ", round(wait_time, 1), " seconds before retry...")
      Sys.sleep(wait_time)  # Wait before retrying
    }
    
  }
  
  message("\nAll attempts failed for ", site_no)
  return(NULL)
  
}

# Retry get_nhdplus ----
retry_get_nhdplus <- function(comid_arg, site_no_arg) {
  for (attempt in 1:5) {
    # Random sleep time between 0 and 3 seconds
    Sys.sleep(runif(1, 0, 3))
    
    # Retrieve catchments
    catchments <- tryCatch(
      nhdplusTools::get_nhdplus(
        comid = comid_arg,
        realization = "catchment",
        t_srs = 4269
      ),
      error = function(e) {
        message("\nError retrieving catchments for ", site_no_arg, " (attempt ", attempt, "): ", e$message)
        NULL
      }
    )
    
    # Check if we got valid results
    if (!is.null(catchments) & nrow(catchments) > 0) return(catchments)
    
    # Only implement backoff if we're going to retry
    if (attempt < 5) {
      # Exponential backoff with some randomness
      wait_time <- 2^attempt + runif(1, 0, 1)
      message("\nWaiting ", round(wait_time, 1), " seconds before retry ", attempt + 1, " for site ", site_no_arg, "...")
      Sys.sleep(wait_time)
    }
  }
  
  message("\nAll catchment retrieval attempts failed for ", site_no_arg)
  return(NULL)
}

# Create the watershed delineator ----
watershed_delineator <- function(STUSPS, site_no, station_nm, comid, geometry) {
  
  # Retrieve upstream COMIDS ----
  upstream <- retry_get_UT(nhd_arg = nhd, comid_arg = comid, site_no_arg = site_no)
  
  ## Handle cases where no upstream COMIDs are found (headwater site)
  if (is.null(upstream) || nrow(upstream) == 0) {
    message("\nNo upstream COMIDs found for ", site_no, "; ", e$message)
    upstream <- comid # Use the site's own COMID
  } 
  
  # Retrieve catchments with possibly() to account for possible errors
  catchments <- retry_get_nhdplus(upstream, site_no)
  
  ## Handle catchment retrieval errors
  if (is.null(catchments)) {
    message("\nCatchment retrieval failed for ", site_no)
    return(NULL)
  }
  
  # Process catchment data
  watershed <- catchments %>% 
    st_make_valid() %>% # ensure geometries are valid
    distinct(featureid, .keep_all = TRUE) %>% # remove duplicates
    summarize() %>%  # Dissolve into single polygon
    nngeo::st_remove_holes() %>% # remove holes
    mutate(
      STUSPS = STUSPS,
      site_no = site_no,
      comid = comid
    ) %>%
    st_as_sf() # convert to sf object if not already
  
  message("Successfully processed: ", station_nm, "\n")
  return(watershed)
}

# Set up parallel processing ----
# Set up parallel processing with fewer workers to reduce server load
num_workers <- min(availableCores() - 1, 6) # Use at most 6 workers
message("Setting up parallel processing with ", num_workers, " workers")
plan(multisession, workers = num_workers)

# Explicitly export the nhd_hr_flowlines object to workers
furrr_options(
  globals = TRUE,
  packages = c("dplyr", "sf", "arcgislayers")
)

# Select the data we need 
site_data <- gage_sites %>% 
  select(STUSPS, site_no, station_nm, comid, geometry)

# Split the data into chunks to process in parallel ----
chunk_size <- 10
total_sites <- nrow(site_data)
chunks <- split(1:total_sites, ceiling(seq_along(1:total_sites) / chunk_size))

watershed_delineator_results <- list()

for (chunk_idx in seq_along(chunks)) {
  
  message("\n=== Processing chunk ", chunk_idx, " of ", length(chunks), " ===")
  
  # Get the indices for this chunk
  indices <- chunks[[chunk_idx]]
  chunk_data <-site_data[indices, ]
  
  # Process this chunk in parallel
  chunk_results <- future_pmap(
    list(
      STUSPS = chunk_data$STUSPS,
      site_no = chunk_data$site_no,
      station_nm = chunk_data$station_nm,
      comid = chunk_data$comid,
      geometry = chunk_data$geometry
    ),
    safely(function(STUSPS, site_no, station_nm, comid, geometry){
      watershed_delineator(STUSPS, site_no, station_nm, comid, geometry)
    }),
    .progress = TRUE
  )
  
  # Progress chunk results
  chunk_transposed <- transpose(chunk_results)
  chunk_successful <- chunk_transposed %>% pluck(1) %>% compact()
  chunk_errors <- chunk_transposed %>% pluck(2) %>% compact()
  
  # Print diagnostics for this chunk
  message("Chunk ", chunk_idx, ": ", length(chunk_successful), " successful, ", 
          length(chunk_errors), " errors")
  
  # Add successful results to our collection
  watershed_delineator_results <- c(watershed_delineator_results, chunk_successful)
  
  # Take a short break between chunks to avoid overloading the server with parallel requests
  if (chunk_idx < length(chunks)) {
    message("Taking a short break before next chunk...")
    Sys.sleep(5)
  }
}

# Process final results ----
watershed_delineator_results <- watershed_delineator_results %>% 
  transpose() %>% 
  pluck(1) %>% 
  compact() %>% 
  bind_rows()

# Save watershed dataframe
# write_rds(watersheds, here("data", "watershed_polygons.RDS"))

# View map ----
mapview::mapview(watersheds) 

# remove unnecessary objects
rm("nhd")