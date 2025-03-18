# Enhanced watershed delineation function with headwater handling

# TODO: Retry wrapper for get_UT()

# TODO: Retry wrapper for get_nhdplus() 

watershed_delineator <- function(STUSPS, site_no, station_nm, comid, geometry) {
  
  # Retrieve upstream COMIDs ----
  upstream <- tryCatch(
    nhdplusTools::get_UT(nhd, comid),
    error = function(e) {
      message("Error retrieving upstream COMIDs for ", site_no, ": ", e$message)
      return(NULL)
    }
  )
  
  ## Handle cases where no upstream COMIDs are found (headwater site)
  if (is.null(upstream) | length(upstream) == 0) {
    message("No upstream COMIDs found for ", site_no, " - treating as headwater site")
    upstream <- comid  # Use the site's own COMID
  }
  
  # Retrieve catchments with retry logic
  catchments <- possibly(
    function(comids) {
      retry(nhdplusTools::get_nhdplus(comid = comids, # comids plural via upstream object
                                      realization = "catchment",
                                      t_srs = 4269),
            max_tries = 3, interval = 2, backoff = T)
    },
    otherwise = NULL,
    quiet = T)(upstream)
  
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
  
  message("Successfully processed: ", station_nm)
  return(watershed)
}

# Use pmap to do this function rowwise implicitly
watersheds <- pmap(nwis_sites, ~ safely(watershed_delineator)(.)) %>%  # safely handles errors in the map
  transpose() %>% # Separate results and errors
  pluck("result") %>% # Extract results
  compact() %>% # Remove NULL results
  bind_rows() # Combine into a single data frame
  
# Save watershed dataframe
write_rds(watersheds, here("data", "watersheds_df.RDS"))

# scratch tests ----
test_UT <- get_UT(nhd, nwis_sites$comid[1])

test_catchment <- nhdplusTools::get_nhdplus(comid = test_UT, 
                                            realization = "catchment",
                                            t_srs = 4269)

test_catchments <- possibly(
  nhdplusTools::get_nhdplus,
  otherwise = NULL,
  quiet = TRUE
)(
  comid = test_UT,
  realization = "catchment",
  t_srs = 4269
)

# TODO: parallelize this step

# Parallel function ----

# Chunk the data
chunk_size <- 10
total_sites <- nrow(nwis_sites)
chunks <- split(1:total_sites, ceiling(seq_along(1:total_sites)/chunk_size))

all_results <- list()
all_successful <- list()

# For loop for parallelization in chunks
for(chunk_idx in seq_along(chunks)) {
  message("\n=== Processing chunk ", chunk_idx, " of ", length(chunks), " ===")
  max_attempts <- 5
  
  # Get the indices for this chunk
  indices <- chunks[[chunk_idx]]
  chunk_data <- site_data[indices,]
  
  # Process this chunk in parallel
  chunk_results <- future_pmap(
    list(STUSPS = chunk_data$STUSPS, 
         site_no = chunk_data$site_no, 
         station_nm = chunk_data$station_nm, 
         comid = chunk_data$comid, 
         geometry = chunk_data$geometry),
    safely(function(STUSPS, site_no, station_nm, comid, geometry) {
      watershed_delineator(STUSPS, site_no, station_nm, comid, geometry)
    }),
    .progress = TRUE
  )
}

# furrr_options(
#   globals = TRUE,
#   packages = c("dplyr", "sf", "arcgislayers")
# )