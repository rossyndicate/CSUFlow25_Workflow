csuflow18_var_grabber <- function(watersheds, raster_dir = "data/csuflow18/") {
  
  # Load rasters once
  cat("Loading rasters...\n")
  elevation_raster <- terra::rast(file.path(raster_dir, "elevation.tif"))
  slope_raster <- terra::rast(file.path(raster_dir, "slope.tif"))  # Not currently used but kept in case needed
  
  # Lookup table for cardinal directions
  aspect_lookup <- tibble(val = c(1, 2, 3, 4),
                          aspect = c("North", "East", "South", "West"))
  
  results <- vector("list", length = nrow(watersheds))
  
  for (i in seq_len(nrow(watersheds))) {
    site <- watersheds[i, ]
    
    tryCatch({
      # Transform watershed CRS if needed
      if (st_crs(site) != terra::crs(elevation_raster)) {
        site_proj <- st_transform(site, crs = terra::crs(elevation_raster))
      } else {
        site_proj <- site
      }
      
      site_vect <- terra::vect(site_proj)
      elev <- terra::mask(terra::crop(elevation_raster, site_vect), site_vect, touches = FALSE)
      mean_elevation <- terra::global(elev, fun = "mean", na.rm = TRUE)
      
      aspect_raw <- terra::terrain(elev, v = 'aspect', unit = 'radians')
      aspect_deg <- terra::terrain(elev, v = 'aspect')
      slope_deg <- terra::terrain(elev, v = 'slope', unit = 'degrees')
      slope_rad <- terra::terrain(elev, v = 'slope', unit = 'radians')
      slope_mm <- terra::app(slope_deg, function(x) tan(x * pi / 180))
      
      convert_to_direction <- function(aspect) {
        direction <- rep(NA, length(aspect))
        direction[aspect >= 0 & aspect <= 45 | aspect > 315 & aspect <= 360] <- 1
        direction[aspect > 45 & aspect <= 135] <- 2
        direction[aspect > 135 & aspect <= 225] <- 3
        direction[aspect > 225 & aspect <= 315] <- 4
        return(direction)
      }
      
      aspect_cardinal_raster <- terra::app(aspect_deg, fun = convert_to_direction)
      north_facing <- terra::app(aspect_deg, function(x) {
        ifelse((x >= 0 & x <= 45) | (x >= 315 & x <= 360), 1, 0)
      })
      steep_slopes <- terra::app(slope_mm, function(x) ifelse(x > 0.3, 1, 0))
      northness <- terra::app(c(slope_rad, aspect_raw), function(x) sin(x[1]) * cos(x[2]))
      
      dominant_aspect <- as.data.table(aspect_cardinal_raster) %>%
        rename(val = lyr.1) %>%
        group_by(val) %>%
        summarize(count = n(), .groups = "drop") %>%
        filter(count == max(count)) %>%
        left_join(aspect_lookup, by = "val") %>%
        mutate(index = site$index)
      
      mean_slope <- terra::global(slope_mm, fun = "mean", na.rm = TRUE)
      total_cells <- terra::global(slope_mm, fun = "notNA")
      steep_cells <- terra::global(steep_slopes, fun = "sum", na.rm = TRUE)
      slope30 <- steep_cells / total_cells
      north_cells <- terra::global(north_facing, fun = "sum", na.rm = TRUE)
      north_fraction <- north_cells / total_cells
      northness_mean <- terra::global(northness, fun = "mean", na.rm = TRUE)
      
      # Append results
      results[[i]] <- tibble(
        index = site$index,
        dominant_aspect = dominant_aspect$aspect[1],
        mean_slope_mm = as.numeric(mean_slope) * 100,
        slope30 = as.numeric(slope30),
        northness_mean = as.numeric(northness_mean),
        north_fraction = as.numeric(north_fraction),
        mean_elevation = as.numeric(mean_elevation)
      )
    }, error = function(e) {
      message(sprintf("Skipping site %s due to error: %s", site$index, e$message))
      results[[i]] <- NULL
    })
    
    print(i)
  }
  
  final_table <- bind_rows(results)
  watersheds <- left_join(watersheds, final_table, by = "index")
  
  
  hydro_data <- sf::st_read("data/csuflow18/HydroRegion.shp") 
  
  # Ensure same CRS
  if(st_crs(watersheds) != st_crs(hydro_data)){
    watersheds_proj <- st_transform(watersheds, crs = st_crs(hydro_data))
  } else {
    watersheds_proj <- watersheds
  }
  
  dominant_region <- vector("list", length = nrow(watersheds_proj))
  
  for(i in 1:nrow(watersheds_proj)){
    
    # Perform spatial intersection
    intersect_hydro <- st_intersection(hydro_data, watersheds_proj[i,])
    
    # If no intersection, return NA
    if(nrow(intersect_hydro) == 0) {
      
      nearest_region <- hydro_data[st_nearest_feature(watersheds_proj[i,], hydro_data),] %>%
        pull(Region)
      
      dominant_region[[i]] <- watersheds_proj[i,] %>%
        mutate(csu_hydro_old_region = nearest_region) %>%
        st_drop_geometry()
      
    } else {
      
      # Calculate intersection areas
      intersect_hydro$intersection_area <- st_area(intersect_hydro)
      
      # Find the region with the largest intersection area
      region <- intersect_hydro %>%
        st_drop_geometry() %>%
        group_by(Region) %>%
        summarize(total_area = sum(as.numeric(intersection_area)), .groups = "drop") %>%
        arrange(desc(total_area)) %>%
        slice(1) %>%
        pull(Region)
      
      dominant_region[[i]] <- watersheds_proj[i,] %>%
        mutate(csu_hydro_old_region = region) %>%
        st_drop_geometry()
      
    }
    
  }
  
  dominant_regions <- bind_rows(dominant_region)  %>%
    select(index, csu_hydro_old_region)
  
  
  watersheds <- left_join(watersheds, dominant_regions, by = "index") %>%
    rename(Dom_Aspect = dominant_aspect,
           Mean_Slope = mean_slope_mm, 
           Elevation = mean_elevation,
           Hyd_Reg = csu_hydro_old_region)
  

  geo_data <- sf::st_read("data/csuflow18/Geology.shp")
  
  # Ensure CRS match
  if (st_crs(watersheds) != st_crs(geo_data)) {
    watersheds_proj <- st_transform(watersheds, crs = st_crs(geo_data))
  } else {
    watersheds_proj <- watersheds
  }
  
  dominant_geology <- vector("list", length = nrow(watersheds_proj))
  
  for (i in seq_len(nrow(watersheds_proj))) {

    intersect_geo <- st_intersection(geo_data, watersheds_proj[i, ])
    
    if (nrow(intersect_geo) == 0) {
      dominant_geology[[i]] <- watersheds_proj[i, ] %>%
        mutate(dominant_geology = NA_character_) %>%
        st_drop_geometry()
      
    } else {
      
      intersect_geo$intersection_area <- st_area(intersect_geo)
      
      dominant <- intersect_geo %>%
        st_drop_geometry() %>%
        group_by(ROCKTYPE1) %>%  
        summarize(total_area = sum(as.numeric(intersection_area)), .groups = "drop") %>%
        arrange(desc(total_area)) %>%
        slice(1) %>%
        pull(ROCKTYPE1)
      
      dominant_geology[[i]] <- watersheds_proj[i, ] %>%
        mutate(dominant_geology = dominant) %>%
        st_drop_geometry()
    }
  }
  
  dominant_geology_df <- bind_rows(dominant_geology) %>%
    select(index, dominant_geology)
  
  watersheds <- left_join(watersheds, dominant_geology_df, by = "index") %>%
    rename(Geo_Group = dominant_geology)
  
  return(watersheds)
}
