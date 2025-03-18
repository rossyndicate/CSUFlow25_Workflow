calculate_watershed_geometry <- function(watersheds_sf, output_dir = tempdir(), dem = NULL) {
  
  # Set up temporary directory
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Initialize whitebox
  wbt_init()
  
  # Input validation
  if (!inherits(watersheds_sf, "sf")) {
    stop("Input must be an sf object")
  }
  
  # Save watersheds to shapefile for whitebox processing
  watersheds_path <- file.path(output_dir, "watersheds.shp")
  st_write(watersheds_sf, watersheds_path, delete_dsn = TRUE, quiet = TRUE)
  
  message("Calculating shape metrics...")
  
  # Calculate shape complexity index
  wbt_shape_complexity_index(
    input = watersheds_path,
    wd = output_dir
  )
  
  # Calculate elongation ratio
  wbt_elongation_ratio(
    input = watersheds_path,
    wd = output_dir
  )
  
  # Calculate compactness ratio
  wbt_compactness_ratio(
    input = watersheds_path,
    wd = output_dir
  )
  
  # Calculate perimeter-area ratio
  wbt_perimeter_area_ratio(
    input = watersheds_path,
    wd = output_dir
  )
  
  # Calculate linearity index
  wbt_linearity_index(
    input = watersheds_path,
    wd = output_dir
  )
  
  # Calculate patch orientation
  wbt_patch_orientation(
    input = watersheds_path,
    wd = output_dir
  )
  
  # Calculate related circumscribing circle
  wbt_related_circumscribing_circle(
    input = watersheds_path,
    wd = output_dir
  )
  
  # Calculate hole proportion (if polygons have holes)
  wbt_hole_proportion(
    input = watersheds_path,
    wd = output_dir
  )
  
  # Read back the shapefile with all the metrics
  watersheds_with_metrics <- st_read(watersheds_path, quiet = TRUE)
  
  # If DEM is provided, calculate additional terrain-based metrics
  # At the moment, no DEM is provided
  if (!is.null(dem)) {
    message("Calculating terrain-based metrics...")
    dem_path <- file.path(output_dir, "dem.tif")
    
    if (is.character(dem)) {
      # If dem is a path, make sure it exists
      if (!file.exists(dem)) {
        stop("DEM file not found")
      }
      dem_path <- dem
    } else if (inherits(dem, "SpatRaster") || inherits(dem, "RasterLayer")) {
      # If dem is a raster object, write it to file
      terra::writeRaster(dem, dem_path, overwrite = TRUE)
    } else {
      stop("DEM must be a file path, SpatRaster, or RasterLayer")
    }
    
    # Convert watersheds to raster format
    watersheds_raster <- file.path(output_dir, "watersheds_raster.tif")
    wbt_vector_polygons_to_raster(
      input = watersheds_path,
      field = "FID",
      output = watersheds_raster,
      cell_size = min(terra::res(terra::rast(dem_path))),
      wd = output_dir
    )
    
    # Calculate hypsometric integral for each watershed
    hypsometric_output <- file.path(output_dir, "hypsometric.txt")
    wbt_hypsometric_analysis(
      dem = dem_path,
      features = watersheds_raster,
      output = hypsometric_output,
      wd = output_dir
    )
    
    # Read the hypsometric results and join them
    if (file.exists(hypsometric_output)) {
      hyps_data <- read.table(hypsometric_output, header = TRUE)
      watersheds_with_metrics <- watersheds_with_metrics %>%
        cbind(hyps_data)
    }
    
    # Calculate additional metrics using the DEM
    # Watershed average slope
    slope_file <- file.path(output_dir, "slope.tif")
    wbt_slope(
      dem = dem_path,
      output = slope_file,
      wd = output_dir
    )
    
    # Zonal statistics for slope
    slope_stats <- file.path(output_dir, "slope_stats.txt")
    wbt_zonal_statistics(
      input = slope_file,
      features = watersheds_raster,
      output = slope_stats,
      wd = output_dir
    )
    
    if (file.exists(slope_stats)) {
      slope_data <- read.table(slope_stats, header = TRUE)
      colnames(slope_data) <- paste0("slope_", colnames(slope_data))
      watersheds_with_metrics <- watersheds_with_metrics %>%
        cbind(slope_data)
    }
  }
  
  # Add basic geometric calculations
  watersheds_with_metrics <- watersheds_with_metrics %>%
    mutate(
      area_km2 = as.numeric(st_area(geometry)) / 1e6,
      perimeter_km = as.numeric(st_length(st_cast(geometry, "MULTILINESTRING"))) / 1000,
      circularity = (4 * pi * area_km2) / (perimeter_km^2)
    )
  
  # Rename fields to be more descriptive
  watersheds_with_metrics <- watersheds_with_metrics %>%
    rename(
      shape_complexity = COMPLEXITY,
      elongation = ELONGATION,
      compactness = COMPACT,
      perimeter_area = P_A_RATIO,
      linearity = LINEARITY,
      orientation_deg = ORIENT,
      rel_circ_circle = RC_CIRCLE,
      hole_proportion = HOLE_PROP
    )
  
  # Return the enhanced SF object
  return(watersheds_with_metrics)
}