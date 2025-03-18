# OG ----
# open the nhd_hr - which contains a bunch of layers
nhd_hr <- arcgislayers::arc_open("https://hydro.nationalmap.gov/arcgis/rest/services/NHDPlus_HR/MapServer")
nhd_hr_flowlines <- arcgislayers::get_layer(nhd_hr, 3)

fetch_flowlines <- function(site_list, nhd_flowlines = nhd_hr_flowlines){
  browser()
  site <- ref_watersheds %>%
    filter(site_no == site_list)
  
  # use bbox to return associated flowlines
  geospatial_aoi <- site %>% 
    # add a buffer around the watershed for visualization later on
    st_buffer(1000) %>% 
    # Convert sf object to sfc object (required for downloading from the map server)
    st_as_sfc(.)
  
  nhd_flowlines <- vector("list", length = length(geospatial_aoi))
  
  tryCatch({
    nhd_flowlines <- arcgislayers::arc_select(nhd_flowlines,
                                              # where = query,
                                              filter_geom = geospatial_aoi,
                                              crs = st_crs(geospatial_aoi)) %>% 
      st_make_valid()},
    error = function(e){
      cat("Index ", i, " from input data failed.")
    }) 
  
  nhd_flowlines <- nhd_flowlines %>% 
    keep(~!is.null(.))
  
  try(nhd_flowlines <- nhd_flowlines %>% 
        dplyr::bind_rows() %>%
        dplyr::distinct() %>%
        mutate(#natural = ifelse(ftype == 460, T, F),
          flowline_type = case_when(ftype == 460 ~ "natural",
                                    ftype == 558 ~ "artificial path",
                                    ftype == 468 ~ "drainageway",
                                    ftype == 336 ~ "canal ditch",
                                    ftype == 566 ~ "coastline",
                                    ftype == 334 ~ "connector",
                                    ftype == 428 ~ "pipeline",
                                    ftype == 420 ~ "underground conduit",
                                    .default = "unnatural")), 
      silent = TRUE)
  
  saveRDS(nhd_flowlines, here("data", "flowlines", paste0(site$state, "_", site$site_no,".RDS")))
  # saveRDS(nhd_flowlines, paste0("data/flowlines/", site_list, ".RDS"))
  
  # print(paste0(site_list, " done!"))
  
  return(nhd_flowlines)
  
}

# How to make this run in parallel (multithreading?)
all_flowlines <- ref_watersheds$site_no %>%
  map(~fetch_flowlines(.)) %>%
  bind_rows() %>%
  distinct()

# remove unnecessary objects
rm("nhd_hr", "nhd_hr_flowlines")

# updated ----
# Open the nhd_hr - which contains a bunch of layers
nhd_hr <- arcgislayers::arc_open("https://hydro.nationalmap.gov/arcgis/rest/services/NHDPlus_HR/MapServer")
nhd_hr_flowlines <- arcgislayers::get_layer(nhd_hr, 3)

# Improved retry wrapper with exponential backoff
retry_arc_select <- function(nhd_flowlines, filter_geom_arg, crs_arg, site_info = "", max_attempts = 5) {
  for (attempt in 1:max_attempts) {
    result <- tryCatch({
      message("Attempt ", attempt, " for ", site_info)
      arcgislayers::arc_select(
        nhd_flowlines, 
        filter_geom = filter_geom_arg,
        crs = crs_arg)
    }, error = function(e) {
      message("\nAttempt ", attempt, " failed for ", site_info, ": ", e$message)
      NULL
    })
    
    if (!is.null(result)) return(result)
    
    # Exponential backoff with some randomness to prevent concurrent requests
    wait_time <- 2^attempt + runif(1, 0, 1)
    message("Waiting ", round(wait_time, 1), " seconds before retry...")
    Sys.sleep(wait_time)
  }
  message("All attempts failed for ", site_info)
  return(NULL)
}

# Improved fetch_flowlines function
fetch_flowlines <- function(site_no, STUSPS, geometry, nhd_flowlines) {
  site_info <- paste0(site_no, " (", STUSPS, ")")
  message("\nProcessing site: ", site_info)
  
  # Buffer the watershed geometry and handle errors
  watershed_aoi <- tryCatch({
    # Ensure geometry is handled properly
    if(inherits(geometry, "sfc")) {
      geom_with_crs <- geometry
    } else {
      geom_with_crs <- st_sfc(geometry, crs = 4269)
    }
    
    st_buffer(geom_with_crs, 1000)
  }, error = function(e) {
    message("\nError buffering geometry for ", site_info, ": ", e$message)
    return(NULL)
  })
  
  if (is.null(watershed_aoi)) {
    message("Couldn't create watershed AOI for ", site_info)
    return(NULL)
  }
  
  arc_search_result <- retry_arc_select(
    nhd_flowlines, 
    filter_geom_arg = watershed_aoi, 
    crs_arg = st_crs(watershed_aoi),
    site_info = site_info
  )
  
  if (is.null(arc_search_result) || nrow(arc_search_result) == 0) {
    message("\nNo flowlines found for ", site_info)
    return(NULL)
  }
  
  # Process and classify flowlines
  flowlines <- tryCatch({
    arc_search_result %>%
      st_make_valid() %>%
      dplyr::distinct() %>%
      mutate(
        flowline_type = case_when(
          ftype == 460 ~ "natural",
          ftype == 558 ~ "artificial path",
          ftype == 468 ~ "drainageway",
          ftype == 336 ~ "canal ditch",
          ftype == 566 ~ "coastline",
          ftype == 334 ~ "connector",
          ftype == 428 ~ "pipeline",
          ftype == 420 ~ "underground conduit",
          .default = "unnatural"
        ),
        site_no = site_no,
        STUSPS = STUSPS
      )
  }, error = function(e) {
    message("\nError processing flowlines for ", site_info, ": ", e$message)
    return(NULL)
  })
  
  # Save individual result immediately as a safeguard
  if (!is.null(flowlines) && nrow(flowlines) > 0) {
    tryCatch({
      write_rds(flowlines, here("data", "flowlines", paste0(STUSPS, "_", site_no, ".RDS")))
      message("Saved ", nrow(flowlines), " flowlines for ", site_info)
    }, error = function(e) {
      message("Error saving flowlines for ", site_info, ": ", e$message)
    })
  }
  
  return(flowlines)
}

# First, test with a single site to ensure the function works
message("\n=== Testing with a single site ===")
single_test <- fetch_flowlines(
  site_no = ref_watersheds$site_no[1],
  STUSPS = ref_watersheds$STUSPS[1],
  geometry = ref_watersheds$geometry[1],
  nhd_flowlines = nhd_hr_flowlines
)

if (is.null(single_test) || nrow(single_test) == 0) {
  stop("Single test failed - check the function before trying parallel processing")
} else {
  message("\nSingle test succeeded with ", nrow(single_test), " flowlines")
  
  # Process in chunks using parallel processing
  # This helps prevent overwhelming the server and reduces memory pressure
  
  # Set up parallel processing with fewer workers to reduce server load
  num_workers <- min(availableCores() - 1, 4)  # Use at most 4 workers
  message("Setting up parallel processing with ", num_workers, " workers")
  plan(multisession, workers = num_workers)
  
  # Explicitly export the nhd_hr_flowlines object to workers
  future::future_options(
    globals = TRUE,         # Automatically detect global variables
    packages = c("dplyr", "sf", "arcgislayers")  # Ensure packages are available
  )
  
  # Create a dataframe with the data we need
  site_data <- ref_watersheds %>%
    select(site_no, STUSPS, geometry)
  
  # Process in chunks to reduce memory pressure and server load
  chunk_size <- 10
  total_sites <- nrow(site_data)
  chunks <- split(1:total_sites, ceiling(seq_along(1:total_sites) / chunk_size))
  
  all_results <- list()
  all_successful <- list()
  
  for (chunk_idx in seq_along(chunks)) {
    message("\n=== Processing chunk ", chunk_idx, " of ", length(chunks), " ===")
    
    # Get the indices for this chunk
    indices <- chunks[[chunk_idx]]
    chunk_data <- site_data[indices, ]
    
    # Process this chunk in parallel
    chunk_results <- future_pmap(
      list(
        site_no = chunk_data$site_no,
        STUSPS = chunk_data$STUSPS,
        geometry = chunk_data$geometry
      ),
      safely(function(site_no, STUSPS, geometry) {
        fetch_flowlines(site_no, STUSPS, geometry, nhd_flowlines = nhd_hr_flowlines)
      }),
      .progress = TRUE
    )
    
    # Process chunk results
    chunk_transposed <- transpose(chunk_results)
    chunk_successful <- chunk_transposed$result %>% compact()
    chunk_errors <- chunk_transposed$error %>% compact()
    
    # Print diagnostics for this chunk
    message("Chunk ", chunk_idx, ": ", length(chunk_successful), " successful, ", 
            length(chunk_errors), " errors")
    
    # Add successful results to our collection
    all_successful <- c(all_successful, chunk_successful)
    all_results <- c(all_results, chunk_results)
    
    # Save intermediate combined results after each chunk
    if (length(all_successful) > 0) {
      message("Combining intermediate results after chunk ", chunk_idx)
      intermediate_flowlines <- bind_rows(all_successful)
      write_rds(intermediate_flowlines, 
                here("data", paste0("intermediate_flowlines_chunk_", chunk_idx, ".RDS")))
    }
    
    # Take a short break between chunks to let the server rest
    if (chunk_idx < length(chunks)) {
      message("Taking a short break before next chunk...")
      Sys.sleep(5)
    }
  }
  
  # Process final results
  results_transposed <- transpose(all_results)
  successful_results <- results_transposed$result %>% compact()
  errors <- results_transposed$error %>% compact()
  
  # Print diagnostics
  message("\n=== Processing complete ===")
  message("Number of successful results: ", length(successful_results))
  message("Number of errors: ", length(errors))
  
  if (length(errors) > 0) {
    message("First few error messages:")
    for (i in 1:min(3, length(errors))) {
      message(as.character(errors[[i]]))
    }
  }
  
  # Make and save combined flowlines
  if (length(successful_results) > 0) {
    message("Combining and saving all flowlines...")
    all_flowlines <- bind_rows(successful_results)
    write_rds(all_flowlines, here("data", "all_flowlines.RDS"))
    
    message("All done! Processed ", length(successful_results), " sites with a total of ", 
            nrow(all_flowlines), " flowlines.")
  } else {
    message("No successful results to process.")
  }
}

# Clean up
message("Cleaning up...")
rm("nhd_hr", "nhd_hr_flowlines")