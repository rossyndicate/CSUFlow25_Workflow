get_historic_climate <- function(aoi_arg) {
  
  message(paste0('\nDownloading GridMET for ', aoi_arg$site_no, "."))
  
  if(any(unique(sf::st_geometry_type(aoi_arg)) %in% c("POLYGON", "MULTIPOLYGON"))){
    # Set random sleep time before server call
    Sys.sleep(runif(1, 1, 3))
    
    historic_climate <- climateR::getGridMET(AOI = aoi_arg,
                                             varname = c("pet", "pr", "tmmn", "tmmx"),
                                             startDate = "2001-01-01",
                                             endDate = "2020-12-31")
    
    # Check if returned data is in SpatRaster format
    if(inherits(historic_climate[[1]], "SpatRaster")){
      
      historic_climate_crs <- st_crs(historic_climate[[1]])
      
      # Handle CRS mismatches between climate data and input features
      if(st_crs(historic_climate[[1]]) != st_crs(aoi_arg)){
        # Transform feature to climate data CRS and crop/mask climate data to feature boundary
        historic_climate <- historic_climate %>%
          purrr::map(
            # getGridMET defaults AOI to bbox - so crop & mask results to actual feature boundary
            ~terra::crop(., st_transform(aoi_arg, crs = historic_climate_crs), mask = TRUE),
            crs = historic_climate_crs)
      } else {
        # If CRS already matches, just crop/mask without transformation
        historic_climate <- historic_climate %>%
          purrr::map(
            ~terra::crop(., aoi_arg, mask = TRUE),
            crs = historic_climate_crs)
      }
      
      # Process climate data into a tidy dataframe
      historic_climate_df <- historic_climate %>%
        # Convert raster to df with coordinates
        purrr::map_dfr(~ as.data.frame(., xy = TRUE)) %>%
        data.table() %>%
        # Reshape data from wide to long
        pivot_longer(-(x:y),
                     names_to = "var_temp",
                     values_to = "val") %>%
        separate_wider_delim(var_temp, "_", names = c("var", "date")) %>% # Split variable name from date
        drop_na(val) %>%
        group_by(x, y, date) %>%
        # Reshape back to wide format with clean variable names
        pivot_wider(names_from = "var", values_from = "val") %>%
        # Calculate derived variables and convert units
        dplyr::mutate(date = as.Date(date),
                      pet_mm = pet,                   # Potential evapotranspiration (mm)
                      ppt_mm = pr,                    # Precipitation (mm)
                      tmax_C = tmmx - 273.15,         # Convert max temp from Kelvin to Celsius
                      tmin_C = tmmn - 273.15,         # Convert min temp from Kelvin to Celsius
                      tmean_C = (tmax_C + tmin_C)/2,   # Calculate mean temperature
                      site_no = aoi_arg$site_no) %>%  
        dplyr::select(-c("tmmx", "tmmn", "pr", "pet"))
      
      # save the data so far in a file
      write_rds(historic_climate_df, here("data", "parallel_gridmet_data", paste0(aoi_arg$site_no,".rds")))
      
      # generate the row that will get saved to join to the data
      historic_climate_row <- historic_climate_df %>% 
        group_by(site_no) %>% 
        summarize(across(c(pet_mm, ppt_mm, tmax_C, tmin_C, tmean_C), 
                         ~mean(as.numeric(.),na.rm = T)), 
                  .groups = "drop") %>% 
        rename(
          pet_mm_2001_2020 = pet_mm,
          ppt_mm_2001_2020 = ppt_mm, 
          tmax_C_2001_2020 = tmax_C, 
          tmin_C_2001_2020 = tmin_C,
          tmean_C_2001_2020 = tmean_C
        )
    } else {
      # Handle case where climate data is returned as a single grid cell (point data)
      
      historic_climate_df <- historic_climate %>%
        data.table() %>%
        # Clean variable names
        rename_with(~ str_split(.x, "_", n = 2) %>% map_chr(1)) %>%
        # Since point features only have one grid cell, manually add coordinates
        dplyr::mutate(x = sf::st_coordinates(aoi_arg)[[1]],
                      y = sf::st_coordinates(aoi_arg)[[2]]) %>%
        # Calculate derived variables as with polygon features
        dplyr::mutate(date = as.Date(date),
                      pet_mm = pet,
                      ppt_mm = pr,
                      tmax_C = tmmx - 273.15,
                      tmin_C = tmmn - 273.15,
                      tmean_C = (tmax_C + tmin_C)/2,
                      site_no = aoi_arg$site_no) %>%
        dplyr::select(-c("tmmx", "tmmn", "pr", "pet")) 
      
      # save the data so far in a file
      write_rds(historic_climate_df, here("data", "parallel_gridmet_data", paste0(aoi_arg$site_no,".rds")))
      
      # generate the row that will get saved to join to the data
      historic_climate_row <- historic_climate_df %>% 
        group_by(site_no) %>% 
        summarize(across(c(pet_mm, ppt_mm, tmax_C, tmin_C, tmean_C), 
                         ~mean(as.numeric(.),na.rm = T)), 
                  .groups = "drop") %>% 
        rename(
          pet_mm_2001_2020 = pet_mm,
          ppt_mm_2001_2020 = ppt_mm, 
          tmax_C_2001_2020 = tmax_C, 
          tmin_C_2001_2020 = tmin_C,
          tmean_C_2001_2020 = tmean_C
        )
    }
    
    return(historic_climate_row)
    
  } else {
    stop("Your watershed_aoi_row feature is neither a polygon nor point feature, or it needs to be made valid.")
  }
}

# Set up parallel processing ----

# Set up parallel processing with fewer workers to reduce server load
num_workers <- min(availableCores() - 1, 6) # Use at most 6 workers
message("Setting up parallel processing with ", num_workers, " workers")
plan(multisession, workers = num_workers)
furrr_options(
  packages = c("sf", "terra", "dplyr", "data.table", "tidyr", "stringr", "climateR"),
  globals = TRUE,
  seed = TRUE
)

# Process in chunks to reduce memory pressure
chunk_size <- 10
gridmet_total_sites <- nrow(aspect_results)
gridmet_chunks <- split(1:gridmet_total_sites, ceiling(seq_along(1:gridmet_total_sites)/chunk_size))

gridmet_results <- list()

for (chunk_idx in seq_along(gridmet_chunks)) {
  message("\n=== Processing chunk ", chunk_idx, " of ", length(gridmet_chunks), " ===")
  
  # Get the indices for this chunk
  indices <- gridmet_chunks[[chunk_idx]]
  chunk_data <- aspect_results[indices, ]
  
  # Process this chunk in parallel
  chunk_results <- future_map(split(chunk_data, 1:nrow(chunk_data)),
                              ~safely(get_historic_climate)(aoi_arg = .x),
                              .options = furrr_options(seed = TRUE),
                              .progress = TRUE) 
  
  chunk_results <- chunk_results %>% 
    transpose() %>% 
    pluck(1) %>% 
    compact() %>% 
    bind_rows()
  
  # Add successful results to our collection
  gridmet_results[[chunk_idx]] <- chunk_results
  
  # Take a short break between chunks to avoid memory issues
  if (chunk_idx < length(gridmet_chunks)) {
    message("Taking a short break before next chunk...")
    Sys.sleep(3)
  }
  
}

# Process final results ----
gridmet_results <- gridmet_results %>% 
  bind_rows()

gridmet_results <- aspect_results %>% 
  left_join(gridmet_results, by = "site_no")

# Save sites with streamcat, aspect, and gridmet data
write_rds(gridmet_results, here("data", "streamcat_aspect_gridmet_variables.rds"))
