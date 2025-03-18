# OG ----
# in fetchNHD_flowlines, we have categorized each flowline as being natural or unnatural.
# So, we can subset the flowlines to just the unnatural ones. 
all_flowlines_unnatural <- all_flowlines %>%
  filter(flowline_type != "natural") 

transbasin_finder <- function(site_list){ 
  
  # filter our master list to just the gage's watershed we are iterating over
  site <- ref_watersheds %>%
    filter(site_no == site_list)
  
  flowlines <- read_rds(here("data", "flowlines2", paste0(site$state, "_", site$site_no, ".RDS"))) 
  
  # if there are no flow lines return NULL
  if (length(flowlines) == 0) {
    print(paste(site$site_no, "has an empty list"))
    return(NULL)
  }
  
  flowlines_unnatural <- flowlines %>%
    filter(flowline_type != "natural") 
  
  # For linestring transformation step to work, need the watershed to be a polygon object:
  if (st_geometry_type(site) != "POLYGON") {
    # If not, cast to a Polygon... which will "explode" it into multiple. 
    # This is a rare thing... I think...
    site <- st_cast(site, "POLYGON") 
  }
  
  polyline <-  site %>% st_cast("LINESTRING")
  
  crossovers <- flowlines_unnatural %>%
    .[polyline,] %>%
    nrow()
  
  # Some watersheds are multipolygons and therefore need to be put back together here:
  site <- site %>% group_by(site_no, comid) %>% summarize() %>%
    mutate(transbasin = ifelse(crossovers > 0, "TRANSBASIN DIVERSION", "NATURAL")) 
  
  # Extract the bounding box of the site_data
  bbox_site <- st_bbox(site)
  
  # Create the ggplot map, zoomed to the bounding box of site_data
  # This should be a map view thing that we can move around in instead of a ggplot
  gg_map <- ggplot() +
    # Plot the site data
    geom_sf(data = site, color = "black", fill = "white", size = 1) + 
    # Plot the site data
    geom_sf(data = filter(nwis_sites, site_no == site$site_no), color = "lightblue", size = 5.5) + 
    # Plot the natural flowlines in blue
    geom_sf(data = flowlines, color = "blue", size = 0.5) + 
    # Plot the unnatural flowlines in red
    geom_sf(data = flowlines_unnatural, color = "red", size = 2) + 
    # Set the xlim and ylim based on the bounding box of site_data
    xlim(bbox_site["xmin"], bbox_site["xmax"]) +
    ylim(bbox_site["ymin"], bbox_site["ymax"]) +
    coord_sf() + 
    theme_void() +
    labs(title = paste0(site$site_no, " ", site$transbasin)) +  
    theme(
      plot.title = element_text(size = 14, hjust = 0.5),  
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()
    )
  
  # Save the map as an image
  ggsave(gg_map, filename = here("data", "transbasin_confirm", paste0(site$transbasin, "_", site$state, "_", site$site_no, ".png")))
  
  print(paste0(site$site_no, " was successful."))
  return(site)
  
}

watersheds_div <- ref_watersheds$site_no %>%
  map(~transbasin_finder(.)) %>%
  compact() %>% 
  bind_rows() %>%
  st_make_valid()

# Updated transbasin_finder function with better error handling ----
# in fetchNHD_flowlines, we have categorized each flowline as being natural or unnatural.
# So, we can subset the flowlines to just the unnatural ones. 

# Create inner buffer function
create_inner_buffer <- function(geometry, buffer_distance = -10) {
  # Store original CRS
  original_crs <- st_crs(geometry)
  
  # Check if the CRS is geographic (uses degrees)
  if (st_is_longlat(geometry)) {
    # Find centroid to determine appropriate UTM zone
    centroid <- st_centroid(geometry)
    coords <- st_coordinates(centroid)[1,]
    
    # Calculate UTM zone from longitude
    utm_zone <- floor((coords[1] + 180) / 6) + 1
    
    # Determine hemisphere
    hemisphere <- ifelse(coords[2] >= 0, "north", "south")
    
    # Get the EPSG code for the UTM zone
    utm_epsg <- ifelse(hemisphere == "north", 
                       26900 + utm_zone,  # NAD83 UTM North zones
                       32700 + utm_zone)  # UTM South zones
    
    # Transform to appropriate UTM projection (using meters)
    geometry_utm <- st_transform(geometry, utm_epsg)
    
    # Apply buffer in meters
    buffered_utm <- st_buffer(geometry_utm, buffer_distance)
    
    # Transform back to original CRS
    buffered_original <- st_transform(buffered_utm, original_crs)
    
    # Verify result isn't empty
    if (st_is_empty(buffered_original)) {
      warning("Buffering created an empty geometry. Using original geometry.")
      return(geometry)
    }
    
    return(buffered_original)
  } else {
    # Already in a projected CRS, buffer directly
    buffered <- st_buffer(geometry, buffer_distance)
    
    # Verify result isn't empty
    if (st_is_empty(buffered)) {
      warning("Buffering created an empty geometry. Using original geometry.")
      return(geometry)
    }
    
    return(buffered)
  }
}

# Function to create interactive leaflet map
create_interactive_watershed_map <- function(watershed_processed, gage_sites, site, 
                                             flowlines, flowlines_unnatural = NULL, 
                                             outlet_buffer = NULL) {
  
  # Transform all spatial objects to WGS84 (EPSG:4326) which is required by Leaflet
  # This is critical - Leaflet requires WGS84 coordinates
  watershed_processed <- st_transform(watershed_processed, 4326)
  gage_sites <- st_transform(gage_sites, 4326)
  flowlines <- st_transform(flowlines, 4326)
  
  if (!is.null(flowlines_unnatural) && nrow(flowlines_unnatural) > 0) {
    flowlines_unnatural <- st_transform(flowlines_unnatural, 4326)
  }
  
  if (!is.null(outlet_buffer)) {
    outlet_buffer <- st_transform(outlet_buffer, 4326)
  }
  
  # Get site point
  site_point <- gage_sites %>% filter(site_no == site$site_no)
  
  # Create a leaflet map
  m <- leaflet() %>%
    # Add base maps with layer control
    # addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
    # Add watershed
    addPolygons(
      data = watershed_processed,
      color = "black",
      weight = 1,
      fillColor = "white", 
      fillOpacity = 0.5,
      popup = paste0("<strong>Watershed:</strong> ", 
                     ifelse(length(watershed_processed$transbasin) > 0, 
                            watershed_processed$transbasin[1], 
                            "Watershed")),
      group = "Watershed"
    )

  # Add site point if it exists
  if (nrow(site_point) > 0) {
    m <- m %>% 
      addCircleMarkers(
        data = site_point,
        color = "lightblue",
        fillColor = "blue",
        radius = 8,
        stroke = TRUE,
        weight = 2,
        opacity = 1,
        fillOpacity = 0.8,
        popup = paste0("<strong>Site No:</strong> ", site$site_no, "<br>",
                       "<strong>State:</strong> ", site$STUSPS),
        group = "Gage Site"
      )
  }
  
  # Add flowlines
  m <- m %>%
    addPolylines(
      data = flowlines,
      color = "blue",
      weight = 1.5,
      opacity = 0.8,
      popup = "Natural Flowline",
      group = "Natural Flowlines"
    )
  
  # Add unnatural flowlines if they exist
  if (!is.null(flowlines_unnatural) && nrow(flowlines_unnatural) > 0) {
    m <- m %>%
      addPolylines(
        data = flowlines_unnatural,
        color = "red",
        weight = 3,
        opacity = 0.8,
        popup = "Unnatural Flowline",
        group = "Unnatural Flowlines"
      )
  }
  
  # Add outlet buffer if it exists
  if (!is.null(outlet_buffer)) {
    m <- m %>%
      addPolygons(
        data = outlet_buffer,
        color = "green",
        weight = 1,
        fillColor = "lightgreen",
        fillOpacity = 0.3,
        popup = "Outlet Buffer",
        group = "Outlet Buffer"
      )
  }
  
  # Add layer controls
  m <- m %>%
    addLayersControl(
      baseGroups = c("Satellite", "OpenStreetMap"),
      overlayGroups = c("Watershed", "Gage Site", "Natural Flowlines", 
                        "Unnatural Flowlines", "Outlet Buffer"),
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
    addScaleBar(position = "bottomleft") %>%
    addMiniMap(
      toggleDisplay = TRUE,
      position = "bottomright"
    )
  
  # Add a title - using a list to avoid jsonlite warnings
  title_text <- paste0("<h3>", site$site_no, " \n", 
                       ifelse(length(watershed_processed$transbasin) > 0, 
                              watershed_processed$transbasin[1], 
                              "Watershed"), 
                       "</h3>")
  m <- m %>%
    addControl(
      html = title_text,
      position = "topright"
    )
  
  return(m)
}

# Updated transbasin finder ====
transbasin_finder <- function(site_no, site_data = ref_watersheds) {
  
  # Filter our master list to just the gage watershed we are iterating over ----
  site <- site_data %>% 
    filter(site_no == !!site_no)
  
  if (nrow(site) == 0) {
    message("\nSite ", site_no, " not found in reference data")
    return(NULL)
  }
  
  # Create a descriptive identifier for messaging ----
  site_info <- paste0(site_no, " (", site$STUSPS, ")")
  message("\nProcessing ", site_info)
  
  # Try to read the flowline file ----
  flowlines_path <- here("data", "flowlines2", paste0(site$STUSPS, "_", site$site_no, ".RDS"))
  
  ## Error handling
  if(!file.exists(flowlines_path)) {
    message("\nFlowline file not found for ", site_info,)
    return(NULL)
  }
  
  flowlines <- tryCatch({
    read_rds(flowlines_path)
  }, error = function(e) {
    message("Error reading flowlines for ", site_info, ": ", e$message)
    return(NULL)
  })
  
  if (is.null(flowlines) || (is.data.frame(flowlines) & nrow(flowlines) == 0)) {
    message(site_info, " has no flowlines data")
    return(NULL)
  }
  
  # Filter for unnatural flowlines ----
  
  ## Unnatural flowline percentage
  unnatural_flowline_percentage <- tryCatch({
    if (nrow(flowlines) > 0) {
      # Create a slightly smaller polygon to avoid counting boundary-touching lines
      inner_polygon <- create_inner_buffer(site, -10)  # Buffer inward by 10 meters
      
      # Find which flowlines intersect this inner polygon
      within_basin_indices <- st_intersects(flowlines, inner_polygon, sparse = FALSE)
      
      # Extract the flowlines that are within the watershed
      within_basin_flowlines <- flowlines[within_basin_indices[,1], ]
      
      # Calculate length of each flowline
      within_basin_flowlines <- within_basin_flowlines %>%
        mutate(length_m = as.numeric(st_length(.)))  # Convert units to numeric
      
      # Calculate total length of all flowlines
      total_length <- sum(within_basin_flowlines$length_m)
      
      # Calculate total length of unnatural flowlines
      unnatural_length <- within_basin_flowlines %>%
        filter(flowline_type != "natural") %>%
        summarize(sum_length = sum(length_m)) %>%
        pull(sum_length)
      
      # Handle case where no unnatural flowlines exist
      if (is.na(unnatural_length)) unnatural_length <- 0
      
      # Calculate percentage (with safety check for division by zero)
      if (total_length > 0) {
        unnatural_percentage <- round((unnatural_length / total_length) * 100, 1)
      } else {
        unnatural_percentage <- 0
      }
      
      unnatural_percentage
    } else {
      unnatural_percentage <- 0
      unnatural_percentage
    }
  }, error = function(e) {
    message("Error analyzing flowline lengths within basin: ", e$message)
    unnatural_percentage <- NA
    unnatural_percentage
  })
  
  ## Get just the unnatural flowlines
  flowlines_unnatural <- tryCatch({
    flowlines %>% 
      filter(flowline_type != "natural")
  }, error = function(e) {
    message("Error filtering unnatural flowlines for ", site_info, ": ", e$message)
    return(NULL)
  })
  
  # Find unnatural flowlines that are within the watershed polygon
  unnatural_within_basin_classification <- tryCatch({
    if (nrow(flowlines_unnatural) > 0) {
      # Create a slightly smaller polygon to avoid counting boundary-touching lines
      # This helps with topological edge cases
      inner_polygon <- create_inner_buffer(site, -10)   # Buffer inward by 10 meters
      
      # Find which unnatural flowlines intersect this inner polygon
      within_basin_indices <- st_intersects(flowlines_unnatural, inner_polygon, sparse = FALSE)
      
      # Extract the unnatural flowlines that are within the watershed
      within_basin_flowlines <- flowlines_unnatural[within_basin_indices, ]
      
      count = nrow(within_basin_flowlines)
      
      classification <- if (count > 0) {
        "WITHIN BASIN MODIFICATION"
      } else {
        "NO WITHIN BASIN MODIFICATION"
      }
      
      classification
      
    } else {
      # No unnatural flowlines at all
      classification <- "NO WITHIN BASIN MODIFICATION"
      classification
    }
  }, error = function(e) {
    message("Error checking for within-basin unnatural flowlines: ", e$message)
    return(NA)
  })
  
  # Retrieve gage location for outlet identification ----
  gage_point <- tryCatch({
    gage_sites %>% 
      filter(site_no == site$site_no) %>% 
      st_transform(st_crs(site)) # ensure matching crs
  }, error = function(e) {
    message("Error retrieving gage location for ", site_info, ": ", e$message)
    return(NULL)
  })
  
  # Create buffer around gage point to identify outlet area ----
  outlet_buffer <- NULL
  if (!is.null(gage_point) && nrow(gage_point) > 0) {
    outlet_buffer <- st_buffer(gage_point, 100)
  } else {
    message("Gage location not found for ", site_info, ". Using original classification method.")
  }
  
  # Process watershed geometry ----
  watershed_processed <- tryCatch({
    # For linestring transformation to work, watershed must be a polygon
    site_geom <- site
    
    if (st_geometry_type(site_geom)[1] != "POLYGON") { 
      # Cast to Polygon (may create multiple features)
      site_geom <- st_cast(site_geom, "POLYGON")
    }
    
    # Create polyline from the boundary
    polyline <- site_geom %>% st_cast("LINESTRING")
    
    # Initialize crossing counters
    crossovers_total <- 0
    crossovers_at_outlet <- 0
    crossovers_elsewhere <- 0
    
    # Find boundary crossings if unnatural flowlines exist
    if (nrow(flowlines_unnatural) > 0) {
      
      # Find all intersections between unnatural flowlines and watershed boundary
      watershed_crossovers <- flowlines_unnatural %>%
        st_intersection(polyline) 
      
      watershed_crossovers_total <- nrow(watershed_crossovers)
      
      # If we have gage site information, classify crossings by location
      if (!is.null(outlet_buffer) && watershed_crossovers_total > 0) {
        # Identify which crossings are withing the outlet buffer
        crossings_within_buffer <- st_intersects(watershed_crossovers, outlet_buffer, sparse = FALSE)
        
        # Count crossings by location
        crossovers_at_outlet <- sum(crossings_within_buffer)
        crossovers_elsewhere <- watershed_crossovers_total - crossovers_at_outlet
      } else {
        # Without outlet info, assume all crossings are elsehwere
        crossovers_elsewhere <- watershed_crossovers_total
      }
    }
   
    # Classify the watershed
    classification <- if (watershed_crossovers_total == 0) {
      "NATURAL" # no unnatural crossings
    } else if (!is.null(outlet_buffer) && crossovers_elsewhere == 0) {
      "NATURAL" # All unnatural crossings are at the outlet
    } else if (crossovers_elsewhere == 1) {
      "POSSIBLE_TRANSBASIN_DIVERSION" # A single crossing can imply a canal out of the watershed
    } else {
      "TRANSBASIN_DIVERSION" # Unnatural crossings away from outlet
    }
    
    # Process result
    site_geom %>% 
      group_by(site_no, comid) %>% 
      summarize(.groups = "drop") %>%
      mutate(
        percentage_of_flowlines_unnatural = unnatural_flowline_percentage,
        within_basin = unnatural_within_basin_classification,
        transbasin = classification,
        total_crossings = crossovers_total,
        outlet_crossings = crossovers_at_outlet,
        other_crossings = crossovers_elsewhere
      )
    
  }, error = function(e) {
    message("Error in spatial analysis for ", site_info, ": ", e$message)
    return(NULL)
  })
  
  ## Error handling
  if (is.null(watershed_processed)) {
    return(NULL)
  }
  
  # Create visualization of watershed and flowlines ----
  tryCatch({
    # Extract the bounding box
    bbox_site <- st_bbox(watershed_processed)
    # Create the ggplot map
    gg_map <- ggplot() +
      # Plot the watershed
      geom_sf(data = watershed_processed, color = "black", fill = "white", size = 1) + 
      # Plot the site point (with safe filtering)
      {
        site_point <- gage_sites %>% filter(site_no == site$site_no)
        if (nrow(site_point) > 0) {
          geom_sf(data = site_point, color = "lightblue", size = 5.5)
        }
      } +
      # Plot all flowlines in blue
      geom_sf(data = flowlines, color = "blue", size = 0.5) + 
      # Plot unnatural flowlines in red (if they exist)
      {
        if (nrow(flowlines_unnatural) > 0) {
          geom_sf(data = flowlines_unnatural, color = "red", size = 2)
        }
      } +
      {
        if (!is.null(outlet_buffer)) {
          geom_sf(data = outlet_buffer, fill = "lightgreen", alpha = 0.3, color = "green")
        }
      } +
      # Set map extents
      xlim(bbox_site["xmin"], bbox_site["xmax"]) +
      ylim(bbox_site["ymin"], bbox_site["ymax"]) +
      coord_sf() + 
      theme_void() +
      labs(title = paste0(site$site_no, " ", watershed_processed$transbasin[1])) +
      theme(
        plot.title = element_text(size = 14, hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
    
    # Create output directory if it doesn't exist
    dir.create(here("data", "transbasin_confirm3"), showWarnings = FALSE, recursive = TRUE)
    
    # Save the gg_map
    ggsave(filename = paste0(watershed_processed$transbasin[1], "_", 
                             site$STUSPS, "_", site$site_no, ".png"), 
           plot = gg_map,
           path = here("data", "transbasin_confirm3"))
    
    message("Successfully created interactive Leaflet visualization for ", site_info)
    
  }, error = function(e) {
    message("Error creating ggmap visualization for ", site_info, ": ", e$message)
    # Return watershed data even if visualization fails
  })
  
  # Leaflet map
  tryCatch({
    # Create the interactive map
    interactive_map <- create_interactive_watershed_map(
      watershed_processed = watershed_processed,
      gage_sites = gage_sites,
      site = site,
      flowlines = flowlines,
      flowlines_unnatural = flowlines_unnatural,
      outlet_buffer = outlet_buffer
    )
    
    # Create output directory if it doesn't exist
    dir.create(here("data", "transbasin_confirm3_interactive"), 
               showWarnings = FALSE, recursive = TRUE)
    
    # Define file path
    html_file_path <- here("data", "transbasin_confirm3_interactive",
                           paste0(watershed_processed$transbasin[1], "_", 
                                  site$STUSPS, "_", site$site_no, ".html"))
    
    # Save the interactive map as HTML
    saveWidget(
      widget = interactive_map,
      file = html_file_path,
      selfcontained = TRUE  # Ensures all dependencies are included in one file
    )
    
    message("Successfully created interactive Leaflet visualization saved to: ", html_file_path)
    
  }, error = function(e) {
    message("Error creating Leaflet visualization: ", e$message)
  })
  
  message("Completed processing for ", site_info)
  return(watershed_processed)
  
}

# Test the function on a single site first ----
test_site <- ref_watersheds$site_no[1]
message("\n=== Testing with site ", test_site, " ===")
test_result <- transbasin_finder(test_site)

# Test the function sequentially

