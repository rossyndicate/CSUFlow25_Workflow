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

# Testing catchment ratios----
nhd <- read_csv(here("data", "nhd_flow_network.csv"))
# Set safe functions for server calls ----
# Retry get UT

retry_get_UT <- function(nhd_arg, comid_arg, site_no_arg, geometry_arg = geometry) {
  
  for (attempt in 1:5) {
    # Random sleep time between 0 and 3 seconds
    Sys.sleep(runif(1,0,3))
    
    # Retrieve upstream COMIDs ----
    upstream_COMIDs <- tryCatch(
      suppressWarnings(nhdplusTools::get_UT(nhd_arg, comid_arg)),
      error = function(e) {
        message("\nError retrieving upstream COMIDs for ", site_no_arg, ": ", e$message)
        NULL
      }
    )
    
    point <- st_sfc(geometry_arg) %>% 
      st_set_crs(4269)
    
    test_basin <- get_split_catchment(point, upstream = T)
    
    # browser()
    
    if (!is.null(upstream_COMIDs)) return(upstream_COMIDs)
    
    # Exponential backoff with some randomness to prevent concurrent requests
    if (attempt < 5) {
      wait_time <- 2^attempt + runif(1, 0, 1)
      message("\nWaiting ", round(wait_time, 1), " seconds before retry...")
      Sys.sleep(wait_time)  # Wait before retrying
    }
    
  }
  
  message("\nAll attempts failed for ", site_no_arg)
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
  upstream <- retry_get_UT(nhd_arg = nhd, comid_arg = comid, site_no_arg = site_no, geometry_arg = geometry)
  
  ## Handle cases where no upstream COMIDs are found (headwater site)
  if (is.null(upstream)) {
    message("\nNo upstream COMIDs found for ", site_no)
    upstream <- comid # Use the site's own COMID
  } else if (is.data.frame(upstream)) {
    # Check if the data frame has rows
    if (nrow(upstream) == 0) {
      message("\nEmpty upstream data frame for ", site_no)
      upstream <- comid
    }
  } else {
    # Handle case where upstream is neither NULL nor a data frame (could be a single value)
    message("\nUpstream is not a data frame for ", site_no, ". Type: ", class(upstream)[1])
    # If it's already a single value (like a character string), no transformation needed
    # But we need to ensure it's in a format that subsequent functions can handle
    if (!is.character(upstream) && !is.numeric(upstream)) {
      # If it's some other unexpected type, default to the site's COMID
      upstream <- comid
    }
  }
  
  # Retrieve catchments 
  catchments <- retry_get_nhdplus(upstream, site_no)
  browser()
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

# Single site test ----
test_site_no <- "10172200"
test_site_no <- "06716100"
test_site <- filter(gage_sites, site_no == test_site_no)
test_watershed_delineator <- watershed_delineator(
  STUSPS = gage_sites$STUSPS[[which(gage_sites$site_no == test_site_no)]],
  site_no = test_site_no,  # Directly use the test site number
  station_nm = gage_sites$station_nm[[which(gage_sites$site_no == test_site_no)]],
  comid = gage_sites$comid[[which(gage_sites$site_no == test_site_no)]],
  geometry = gage_sites$geometry[[which(gage_sites$site_no == test_site_no)]]
)

# Other watershed delineation tools:
test_point <- readNWISsite("10172200")
test_point <- st_as_sf(test_point,
                       coords= c("dec_long_va", "dec_lat_va"),
                       crs=4269) %>% 
  mutate(comid = discover_nhdplus_id(.))

st_cast_point <- st_cast(test_point, "POINT")

cast_catchment <- get_split_catchment(st_cast_point, upstream = T)


# test with get UT output, too
point_catchment <- get_split_catchment(test_point, upstream = T)[1,]

mapview(test_point) +
  mapview(cast_catchment) +
  mapview(point_catchment)

# whitebox watershed ... ----

# Load the watershed
watershed_polygon <- read_rds(here("data", ""))

# Install and load required packages
# install.packages("whitebox")
library(whitebox)
library(sf)
library(raster)

# Initialize WhiteboxTools
# wbt_init()
# get elevation data for watershed:
watershed_raster <- elevatr::get_elev_raster(point_catchment, z = 12, clip = "locations")

# SAVE DEM for processing
dem_path <- here("data", "dem_temp", "dem_temp.tif")
writeRaster(watershed_raster, dem_path, overwrite = T)

# Prepare pour point
pour_point_path <- here("data", "temp_pour_point", "temp_pour_point.shp")
st_write(test_point, pour_point_path, delete_dsn = T)

# Hydrological processing to get true watershed
# dir.create(here("data", "dem_depression_temp"))
dem_depression_path <- here("data", "dem_depression_temp", "filled_dem.tif")
wbt_fill_depressions(dem = dem_path, output = dem_depression_path)

# dir.create(here("data", "dem_direction_temp"))
dem_direction_path <- here("data", "dem_direction_temp", "flow_dir.tif")
wbt_d8_pointer(dem = dem_depression_path, output = dem_direction_path)

# dir.create(here("data", "dem_flowaccumulation_temp"))
dem_flowaccumulation_path <- here("data", "dem_flowaccumulation_temp", "flow_accum.tif")
wbt_d8_flow_accumulation(input = dem_direction_path, output = dem_flowaccumulation_path)

# dir.create(here("data", "dem_streams_temp"))
dem_stream_path <- here("data", "dem_streams_temp", "streams.tif")
wbt_extract_streams(flow_accum = dem_flowaccumulation_path,
                    output = dem_stream_path,
                    threshold = 10000)

# Snap pour point to highest flow accumulation (stream)
dir.create(here("data", "pourpoint_snap_temp"))
snapped_point_path <- here("data", "pourpoint_snap_temp", "snapped_point.shp")

# Create buffer of point
point <- st_read(pour_point_path)
flow <- raster(dem_flowaccumulation_path)

cell_size <- res(flow)[1]
buffer <- 0.4e+6 * cell_size
point_buffer <- st_buffer(point, buffer)

# Crop the raster
flow_accum_subset <- crop(flow, extent(point_buffer))

# Save the crop of the raster
dir.create(here("data", "dem_flow_accum_crop_temp"))
flow_crop_path <- here("data", "dem_flow_accum_crop_temp", "crop.tif")
writeRaster(flow_accum_subset, flow_crop_path, overwrite = T)

# Use the subset for snapping
dir.create(here("data", "pour_point_snap_temp"))
snapped_point_path <- here("data", "pour_point_snap_temp", "snapped_point.shp")

wbt_snap_pour_points(
  pour_pts = pour_point_path,
  flow_accum = flow_crop_path,
  output = snapped_point_path,
  snap_dist = 50
)

# dir.create(here("data", "wbt_watershed_temp"))
# wbt_watershed_path <- here("data", "wbt_watershed_temp", "wbt_watershed.tif")
# whitebox::wbt_watershed(
#   d8_pntr = dem_direction_path,
#   pour_pts = pour_point_path,
#   output = wbt_watershed_path
# )

#-------

# 1. Prepare inputs
dem_file <- "your_dem.tif"  # Your Digital Elevation Model
pour_point <- st_read("your_point.shp")  # Or create one at your funneling point

# 2. Basic DEM processing
wbt_fill_depressions(dem = dem_file, output = "filled_dem.tif")
wbt_d8_pointer(dem = "filled_dem.tif", output = "flow_dir.tif")
wbt_d8_flow_accumulation(input = "flow_dir.tif", output = "flow_accum.tif")

# 3. Snap your pour point to the highest flow accumulation cell nearby
wbt_jenson_snap_pour_points(
  pour_pts = "your_point.shp",
  flow_accum = "flow_accum.tif", 
  output = "snapped_point.shp",
  snap_dist = 50  # Distance in cells to search for stream
)

# 4. Delineate watershed upstream of the snapped point
wbt_watershed(
  d8_pntr = "flow_dir.tif",
  pour_pts = "snapped_point.shp",
  output = "watershed.tif"
)

# 5. Convert raster watershed to vector polygon
wbt_raster_to_vector_polygons(
  input = "watershed.tif", 
  output = "watershed_polygon.shp"
)

# 6. Load and visualize the watershed
watershed <- st_read("watershed_polygon.shp")
plot(watershed)

