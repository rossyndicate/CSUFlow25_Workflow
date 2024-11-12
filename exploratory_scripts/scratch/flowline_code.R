fetchNHD <- function(.data, resolution = "Hi", features = "catchments"){
  
  suppressMessages(suppressWarnings({
    
    sf::sf_use_s2(FALSE)
    # If data is already spatial, just make sure it is in the right CRS
    if (!is.null(.data) & inherits(.data, "sf")) {
      if (sf::st_crs(.data)$epsg != 4326) {
        geospatial_data <- .data %>%
          sf::st_transform(4326)
      } else {
        geospatial_data <- .data
      }
    } else {
      # ... Otherwise transform into a spatial object then do the same thing:
      geospatial_data <- .data %>%
        # convert dataframe to a spatial object
        TADA_MakeSpatial(.data = ., crs = 4326) %>%
        dplyr::mutate(geometry_join = geometry)
    }
    
  }))
  
  # Reduce WQP data to unique coordinates
  unique_sites <- dplyr::distinct(geospatial_data, geometry)
  
  # If user wants HighRes NHD...
  if(resolution %in% c("Hi", "hi")){
    suppressMessages(suppressWarnings({
      # ... we first must identify the HUC4s that contain the WQP data...
      
      # here is the sauce ====
      geospatial_aoi <- unique_sites %>%
        # convert XY sites into super tiny polygons for "AOI"
        sf::st_buffer(0.0000001) %>%
        # convert sf object to sfc object (required for downloading from the map server)
        sf::st_as_sfc(.)
      
      nhd_plus_hr_url <- "https://hydro.nationalmap.gov/arcgis/rest/services/NHDPlus_HR/MapServer"
      
      # open the nhd_hr - which contains a bunch of layers
      nhd_hr <- arcgislayers::arc_open(nhd_plus_hr_url)
      
      # list the layers of the nhdhr object
      # need to uncomment this ----
        # hoping to find ditches here 
      # arcgislayers::list_items(nhd_hr)
      
      # select the layer by id from the items list called above (10 is HR catchments)
      nhd_hr_catchments <- arcgislayers::get_layer(nhd_hr, 10) # change this to ditch object (which are inside of flowlines)
      
      # fields <- arcgislayers::list_fields(nhd_hr_catchments)
      # View(fields)
      
      # you can use SQL-style queries to make this smaller - you have to use the alias
      # field here.
      # query <- "Ftype = 390 AND AreaSqKm > 0.1" # this is where flowlines are queried for ditches
      
      # use bbox to return associated catchments
      nhd_catchments <- vector("list", length = length(geospatial_aoi)) # dataframe of each watershed
      
      for(i in 1:length(geospatial_aoi)){
        try(nhd_catchments[[i]] <- arcgislayers::arc_select(nhd_hr_catchments,
                                                            # where = query,
                                                            filter_geom = geospatial_aoi[i],
                                                            crs = sf::st_crs(geospatial_aoi[i])) %>%
              sf::st_make_valid(), silent = TRUE)
      }
      
      nhd_catchments <- nhd_catchments %>%
        purrr::keep(~!is.null(.))
      
      try(nhd_catchments <- nhd_catchments %>% 
            dplyr::bind_rows() %>%
            dplyr::distinct(), 
          silent = TRUE)
      
      # the sauce ends here ====
      
      try(nhd_catchments <- nhd_catchments %>%
            dplyr::select(nhdplusid,
                          catchmentareasqkm = areasqkm) %>%
            dplyr::mutate(NHD.nhdplusid = as.character(nhdplusid),
                          NHD.resolution = "HR",
                          NHD.catchmentareasqkm = as.numeric(catchmentareasqkm)) %>%
            dplyr::select(NHD.nhdplusid, NHD.resolution, NHD.catchmentareasqkm, geometry), silent = TRUE)
      
      
    }))
    
    # Empty version of the df will be returned if no associated catchments
    # to avoid breaking downstream fxns reliant on catchment info. 
    if(nrow(nhd_catchments) == 0 && "catchments" %in% features){
      print("No NHD HR features associated with your area of interest.")
      nhd_catchments <- tibble::tibble(NHD.nhdplusid = character(),
                                       NHD.resolution = character(),
                                       NHD.catchmentareasqkm = numeric())
    }
    
    if(nrow(nhd_catchments) == 0 && !"catchments" %in% features){
      stop("No NHD HR features associated with your area of interest.")
    }
    
    if(length(features) == 1 && features == "catchments") {
      return(nhd_catchments)
    }
    
    # Grab flowlines - 
    if("flowlines" %in% features && nrow(nhd_catchments) > 0){
      
      suppressMessages(suppressWarnings({
        
        # use catchments to grab other NHD features
        geospatial_aoi <- nhd_catchments %>%
          sf::st_as_sfc()
        
        # select the layer by id from the items list called above (3 is HR flowlines)
        nhd_hr_flowlines <- arcgislayers::get_layer(nhd_hr, 3)
        
        # use catchments to return associated flowlines
        nhd_flowlines <- vector("list", length = length(geospatial_aoi))
        
        for(i in 1:length(geospatial_aoi)){
          try(nhd_flowlines[[i]] <- arcgislayers::arc_select(nhd_hr_flowlines,
                                                             # where = query,
                                                             filter_geom = geospatial_aoi[i],
                                                             crs = sf::st_crs(geospatial_aoi[i])) %>%
                sf::st_make_valid(), silent = TRUE)
          
          try(geometry_col <- sf::st_geometry(nhd_flowlines[[i]])
              , silent = TRUE)
          
          try(nhd_flowlines[[i]] <- nhd_flowlines[[i]] %>%
                dplyr::mutate(dplyr::across(dplyr::where(~ !identical(., geometry_col)), ~ as.character(.)))
              , silent = TRUE)
        }
        
        nhd_flowlines <- nhd_flowlines %>%
          purrr::keep(~!is.null(.))
        
        try(nhd_flowlines <- nhd_flowlines %>%
              dplyr::bind_rows() %>%
              dplyr::distinct(),
            silent = TRUE)
      }))
      
      if(length(features) == 1 && features == "flowlines") {
        if(length(nhd_flowlines) == 0 || is.null(nhd_flowlines)){print("There are no NHD flowlines associated with your area of interest.")}
        return(nhd_flowlines)
      }
      
      if(length(nhd_flowlines) == 0 || is.null(nhd_flowlines)){print("There are no NHD flowlines associated with your area of interest.")}
      
    }
    
    # Grab waterbodies -
    if("waterbodies" %in% features & nrow(nhd_catchments) > 0){
      suppressMessages(suppressWarnings({
        geospatial_aoi <- nhd_catchments %>%
          sf::st_as_sfc()
        
        # select the layer by id from the items list called above (9 is HR waterbodies)
        nhd_hr_waterbodies <- arcgislayers::get_layer(nhd_hr, 9)
        
        # use bbox to return associated waterbodies
        nhd_waterbodies <- vector("list", length = length(geospatial_aoi))
        
        for(i in 1:length(geospatial_aoi)){
          try(nhd_waterbodies[[i]] <- arcgislayers::arc_select(nhd_hr_waterbodies,
                                                               # where = query,
                                                               filter_geom = geospatial_aoi[i],
                                                               crs = sf::st_crs(geospatial_aoi[i])) %>%
                sf::st_make_valid(), silent = TRUE)
          
          try(geometry_col <- sf::st_geometry(nhd_waterbodies[[i]])
              , silent = TRUE)
          
          try(nhd_waterbodies[[i]] <- nhd_waterbodies[[i]] %>%
                dplyr::mutate(dplyr::across(dplyr::where(~ !identical(., geometry_col)), ~ as.character(.)))
              , silent = TRUE)
          
        }
        
        nhd_waterbodies <- nhd_waterbodies %>%
          purrr::keep(~!is.null(.))
        
        try(nhd_waterbodies <- nhd_waterbodies %>% 
              dplyr::bind_rows() %>%
              dplyr::distinct(),
            silent = TRUE)
      }))
      
      if(length(features) == 1 && features == "waterbodies") {
        
        if(length(nhd_waterbodies) == 0 || is.null(nhd_waterbodies)){print("There are no NHD waterbodies associated with your area of interest.")}
        
        return(nhd_waterbodies)
      }
      
      if(length(nhd_waterbodies) == 0 || is.null(nhd_waterbodies)){print("There are no NHD waterbodies associated with your area of interest.")}
      
    }
    
    # Combinations of features selected, and what they return:
    
    if(length(features) == 2 && "catchments" %in% features && "flowlines" %in% features){
      
      nhd_list <- list("NHD_catchments" = nhd_catchments,
                       "NHD_flowlines" = nhd_flowlines)
      
      return(nhd_list)
      
    } else if(length(features) == 2 && "catchments" %in% features && "waterbodies" %in% features){
      
      nhd_list <- list("NHD_catchments" = nhd_catchments,
                       "NHD_waterbodies" = nhd_waterbodies)
      
      return(nhd_list)
      
    } else if(length(features) == 2 && "flowlines" %in% features && "waterbodies" %in% features){
      
      nhd_list <- list("NHD_flowlines" = nhd_flowlines,
                       "NHD_waterbodies" = nhd_waterbodies)
      
      return(nhd_list)
      
    } else if(length(features) == 3  && "catchments" %in% features && "flowlines" %in% features && "waterbodies" %in% features){
      
      nhd_list <- list("NHD_catchments" = nhd_catchments,
                       "NHD_flowlines" = nhd_flowlines,
                       "NHD_waterbodies" = nhd_waterbodies)
      
    } else {stop("Please select between 'catchments', 'flowlines', 'waterbodies', or any combination for `feature` argument.")}
    
    # If user wants NHDPlus V2...
  } else if(resolution %in% c("Med", "med")){
    
    suppressMessages(suppressWarnings({
      
      nhd_catchments <- vector("list", length = nrow(unique_sites))
      
      for(i in 1:nrow(unique_sites)){
        
        # Use {nhdplusTools} to grab associated catchments...
        try(nhd_catchments[[i]] <- nhdplusTools::get_nhdplus(AOI = unique_sites[i,], realization = "catchment") %>%
              sf::st_make_valid() %>%
              dplyr::select(comid = featureid,
                            catchmentareasqkm = areasqkm) %>%
              dplyr::mutate(NHD.comid = as.character(comid),
                            NHD.resolution = "nhdplusV2",
                            NHD.catchmentareasqkm = as.numeric(catchmentareasqkm)) %>%
              dplyr::select(NHD.comid, NHD.resolution, NHD.catchmentareasqkm, geometry)
            , silent = TRUE) 
        
      }
      
      nhd_catchments <- nhd_catchments %>%
        purrr::keep(~!is.null(.))
      
      try(nhd_catchments <- dplyr::bind_rows(nhd_catchments) %>%
            dplyr::distinct(), silent = TRUE)
      
      # if NHD catchments are not in the correct CRS, transform them
      try(if (sf::st_crs(nhd_catchments) != sf::st_crs(geospatial_data)) {
        nhd_catchments <- nhd_catchments %>%
          sf::st_transform(sf::st_crs(geospatial_data)$epsg)
      }, silent = TRUE)
      
    }))
    
    if(nrow(nhd_catchments) == 0 && "catchments" %in% features){
      print("No NHDPlus V2 features associated with your WQP observations.")
      nhd_catchments <- tibble::tibble(NHD.comid = character(),
                                       NHD.resolution = character(),
                                       NHD.catchmentareasqkm = numeric())
    }
    
    if(nrow(nhd_catchments) == 0 && !"catchments" %in% features){
      stop("No NHDPlus V2 features associated with your WQP observations.")
    }
    
    if(length(features) == 1 && features == "catchments") {
      return(nhd_catchments)
    }
    
    
    # Grab flowlines - 
    if("flowlines" %in% features && nrow(nhd_catchments) > 0){
      suppressMessages(suppressWarnings({
        
        nhd_flowlines <- vector("list", length = nrow(nhd_catchments))
        
        # use catchments to grab other NHD features:
        unique_sites <- nhd_catchments
        
        for(i in 1:nrow(unique_sites)){
          
          # Use {nhdplusTools} to grab associated flowlines...
          try(nhd_flowlines[[i]] <- nhdplusTools::get_nhdplus(AOI = unique_sites[i,], realization = "flowline") %>%
                sf::st_make_valid()
              , silent = TRUE)
          
          try(geometry_col <- sf::st_geometry(nhd_flowlines[[i]])
              , silent = TRUE)
          
          try(nhd_flowlines[[i]] <- nhd_flowlines[[i]] %>%
                dplyr::mutate(dplyr::across(dplyr::where(~ !identical(., geometry_col)), ~ as.character(.)))
              , silent = TRUE)
          
        }
        
        nhd_flowlines <- nhd_flowlines %>%
          purrr::keep(~!is.null(.))
        
        try(nhd_flowlines <- dplyr::bind_rows(nhd_flowlines)) %>%
          dplyr::distinct()
        
        # if NHD flowlines are not in the correct CRS, transform them
        try(if (sf::st_crs(nhd_flowlines) != sf::st_crs(geospatial_data)) {
          nhd_flowlines <- nhd_flowlines %>%
            sf::st_transform(sf::st_crs(geospatial_data)$epsg) 
        }, silent = TRUE)
        
      }))
      
      if(nrow(nhd_flowlines) == 0 && "flowlines" %in% features){
        print("No NHDPlus V2 flowlines associated with your WQP observations.")
      }
      
      if(length(features) == 1 && features == "flowlines") {
        return(nhd_flowlines)
      }
    }
    
    # Grab waterbodies -
    if("waterbodies" %in% features && nrow(nhd_catchments) > 0){
      suppressMessages(suppressWarnings({
        
        nhd_waterbodies <- vector("list", length = nrow(nhd_catchments))
        
        # use catchments to grab other NHD features:
        unique_sites <- nhd_catchments
        
        for(i in 1:nrow(unique_sites)){
          
          # Use {nhdplusTools} to grab associated flowlines...
          try(nhd_waterbodies[[i]] <- nhdplusTools::get_waterbodies(AOI = unique_sites[i,]) %>%
                sf::st_make_valid()
              , silent = TRUE)
          
          try(geometry_col <- sf::st_geometry(nhd_waterbodies[[i]])
              , silent = TRUE)
          
          try(nhd_waterbodies[[i]] <- nhd_waterbodies[[i]] %>%
                dplyr::mutate(dplyr::across(dplyr::where(~ !identical(., geometry_col)), ~ as.character(.)))
              , silent = TRUE)
          
        }
        
        nhd_waterbodies <- nhd_waterbodies %>%
          purrr::keep(~!is.null(.))
        
        try(nhd_waterbodies <- dplyr::bind_rows(nhd_waterbodies) %>%
              dplyr::distinct(),
            silent = TRUE)
        
        # if NHD waterbodies are not in the correct CRS, transform them
        try(if (sf::st_crs(nhd_waterbodies) != sf::st_crs(geospatial_data)) {
          nhd_waterbodies <- nhd_waterbodies %>%
            sf::st_transform(sf::st_crs(geospatial_data)$epsg)
        }, silent = TRUE)
        
      }))
      
      if(nrow(nhd_waterbodies) == 0 && "waterbodies" %in% features){
        print("No NHDPlus V2 waterbodies associated with your WQP observations.")
      }
      
      if(length(features) == 1 && features == "waterbodies") {
        return(nhd_waterbodies)
      }
    }
    
    # Combinations of features selected, and what they return:
    
    if(length(features) == 2 && "catchments" %in% features && "flowlines" %in% features){
      
      nhd_list <- list("NHD_catchments" = nhd_catchments,
                       "NHD_flowlines" = nhd_flowlines)
      
      return(nhd_list)
      
    } else if(length(features) == 2 && "catchments" %in% features && "waterbodies" %in% features){
      
      nhd_list <- list("NHD_catchments" = nhd_catchments,
                       "NHD_waterbodies" = nhd_waterbodies)
      
      return(nhd_list)
      
    } else if(length(features) == 2 && "flowlines" %in% features && "waterbodies" %in% features){
      
      nhd_list <- list("NHD_flowlines" = nhd_flowlines,
                       "NHD_waterbodies" = nhd_waterbodies)
      
      return(nhd_list)
      
    } else if(length(features) == 3  && "catchments" %in% features && "flowlines" %in% features && "waterbodies" %in% features){
      
      nhd_list <- list("NHD_catchments" = nhd_catchments,
                       "NHD_flowlines" = nhd_flowlines,
                       "NHD_waterbodies" = nhd_waterbodies)
      
    } else {stop("Please select between 'catchments', 'flowlines', 'waterbodies', or any combination for `feature` argument.")}
    
  } else {
    stop('User-supplied resolution unavailable. Please select between "Med" or "Hi".')
  }
}

NHD_flowlines <- function(.data, features = "catchments"){
  
  suppressMessages(suppressWarnings({
    
    sf::sf_use_s2(FALSE)
    # If data is already spatial, just make sure it is in the right CRS
    if (!is.null(.data) & inherits(.data, "sf")) {
      if (sf::st_crs(.data)$epsg != 4326) {
        geospatial_data <- .data %>%
          sf::st_transform(4326)
      } else {
        geospatial_data <- .data
      }
    } else {
      # ... Otherwise transform into a spatial object then do the same thing:
      geospatial_data <- .data %>%
        # convert dataframe to a spatial object
        TADA_MakeSpatial(.data = ., crs = 4326) %>% # Is this a function that I don't have?
        dplyr::mutate(geometry_join = geometry)
    }
    
  }))
  
  # Reduce WQP data to unique coordinates
  unique_sites <- dplyr::distinct(geospatial_data, geometry)
  
  # If user wants HighRes NHD...
  suppressMessages(suppressWarnings({
    # ... we first must identify the HUC4s that contain the WQP data...
    
    # here is the sauce ====
    geospatial_aoi <- unique_sites %>%
      # convert XY sites into super tiny polygons for "AOI"
      sf::st_buffer(0.0000001) %>%
      # convert sf object to sfc object (required for downloading from the map server)
      sf::st_as_sfc(.)
    
    nhd_plus_hr_url <- "https://hydro.nationalmap.gov/arcgis/rest/services/NHDPlus_HR/MapServer"
    
    # open the nhd_hr - which contains a bunch of layers
    nhd_hr <- arcgislayers::arc_open(nhd_plus_hr_url)
    
    # list the layers of the nhdhr object
    # need to uncomment this ----
    # hoping to find ditches here 
    # arcgislayers::list_items(nhd_hr)
    
    # 0: NHDPlusGage 
    # 1: NHDPlusSink
    # 2: NHDPoint
    # 3: NetworkNHDFlowline, this is where you want to start
    # 4: NonNetworkNHDFlowline
    # 5: FlowDirection
    # 6: NHDPlusWall
    # 7: NHDLine
    # 8: NHDArea
    # 9: NHDWaterbody, NOT THIS ONE
    # 10: NHDPlusCatchment, this is like a small watershed that feeds into the big ones
    # 11: NHDPlusBoundaryUnit
    # 12: WBDHU12
    
    # select the layer by id from the items list called above (10 is HR catchments)
    nhd_hr_catchments <- arcgislayers::get_layer(nhd_hr, 3) # change this to ditch object (which are inside of flowlines)
    
    fields <- arcgislayers::list_fields(nhd_hr_catchments)
    View(fields)
    browser()
    # you can use SQL-style queries to make this smaller - you have to use the alias
    # field here.
    # query <- "Ftype = 390 AND AreaSqKm > 0.1" # this is where flowlines are queried for ditches
    
    # use bbox to return associated catchments
    nhd_catchments <- vector("list", length = length(geospatial_aoi)) # dataframe of each watershed
    
    for(i in 1:length(geospatial_aoi)){
      try(nhd_catchments[[i]] <- arcgislayers::arc_select(nhd_hr_catchments,
                                                          # where = query,
                                                          filter_geom = geospatial_aoi[i],
                                                          crs = sf::st_crs(geospatial_aoi[i])) %>%
            sf::st_make_valid(), silent = TRUE)
    }
    
    nhd_catchments <- nhd_catchments %>%
      purrr::keep(~!is.null(.))
    
    try(nhd_catchments <- nhd_catchments %>% 
          dplyr::bind_rows() %>%
          dplyr::distinct(), 
        silent = TRUE)
    
    # the sauce ends here ====
    
    try(nhd_catchments <- nhd_catchments %>%
          dplyr::select(nhdplusid,
                        catchmentareasqkm = areasqkm) %>%
          dplyr::mutate(NHD.nhdplusid = as.character(nhdplusid),
                        NHD.resolution = "HR",
                        NHD.catchmentareasqkm = as.numeric(catchmentareasqkm)) %>%
          dplyr::select(NHD.nhdplusid, NHD.resolution, NHD.catchmentareasqkm, geometry), silent = TRUE)
    
    
    browser()
  }))
  
  # Empty version of the df will be returned if no associated catchments
  # to avoid breaking downstream fxns reliant on catchment info. 
  if(nrow(nhd_catchments) == 0 && "catchments" %in% features){
    print("No NHD HR features associated with your area of interest.")
    nhd_catchments <- tibble::tibble(NHD.nhdplusid = character(),
                                     NHD.resolution = character(),
                                     NHD.catchmentareasqkm = numeric())
  }
  
  if(nrow(nhd_catchments) == 0 && !"catchments" %in% features){
    stop("No NHD HR features associated with your area of interest.")
  }
  
  if(length(features) == 1 && features == "catchments") {
    return(nhd_catchments)
  }
  
  # Grab flowlines - 
  if("flowlines" %in% features && nrow(nhd_catchments) > 0){
    
    suppressMessages(suppressWarnings({
      
      # use catchments to grab other NHD features
      geospatial_aoi <- nhd_catchments %>%
        sf::st_as_sfc()
      
      # select the layer by id from the items list called above (3 is HR flowlines)
      nhd_hr_flowlines <- arcgislayers::get_layer(nhd_hr, 3)
      
      # use catchments to return associated flowlines
      nhd_flowlines <- vector("list", length = length(geospatial_aoi))
      
      for(i in 1:length(geospatial_aoi)){
        try(nhd_flowlines[[i]] <- arcgislayers::arc_select(nhd_hr_flowlines,
                                                           # where = query,
                                                           filter_geom = geospatial_aoi[i],
                                                           crs = sf::st_crs(geospatial_aoi[i])) %>%
              sf::st_make_valid(), silent = TRUE)
        
        try(geometry_col <- sf::st_geometry(nhd_flowlines[[i]])
            , silent = TRUE)
        
        try(nhd_flowlines[[i]] <- nhd_flowlines[[i]] %>%
              dplyr::mutate(dplyr::across(dplyr::where(~ !identical(., geometry_col)), ~ as.character(.)))
            , silent = TRUE)
      }
      
      nhd_flowlines <- nhd_flowlines %>%
        purrr::keep(~!is.null(.))
      
      try(nhd_flowlines <- nhd_flowlines %>%
            dplyr::bind_rows() %>%
            dplyr::distinct(),
          silent = TRUE)
    }))
    
    if(length(features) == 1 && features == "flowlines") {
      if(length(nhd_flowlines) == 0 || is.null(nhd_flowlines)){print("There are no NHD flowlines associated with your area of interest.")}
      return(nhd_flowlines)
    }
    
    if(length(nhd_flowlines) == 0 || is.null(nhd_flowlines)){print("There are no NHD flowlines associated with your area of interest.")}
    
  }
  
  # Grab waterbodies -
  if("waterbodies" %in% features & nrow(nhd_catchments) > 0){
    suppressMessages(suppressWarnings({
      geospatial_aoi <- nhd_catchments %>%
        sf::st_as_sfc()
      
      # select the layer by id from the items list called above (9 is HR waterbodies)
      nhd_hr_waterbodies <- arcgislayers::get_layer(nhd_hr, 9)
      
      # use bbox to return associated waterbodies
      nhd_waterbodies <- vector("list", length = length(geospatial_aoi))
      
      for(i in 1:length(geospatial_aoi)){
        try(nhd_waterbodies[[i]] <- arcgislayers::arc_select(nhd_hr_waterbodies,
                                                             # where = query,
                                                             filter_geom = geospatial_aoi[i],
                                                             crs = sf::st_crs(geospatial_aoi[i])) %>%
              sf::st_make_valid(), silent = TRUE)
        
        try(geometry_col <- sf::st_geometry(nhd_waterbodies[[i]])
            , silent = TRUE)
        
        try(nhd_waterbodies[[i]] <- nhd_waterbodies[[i]] %>%
              dplyr::mutate(dplyr::across(dplyr::where(~ !identical(., geometry_col)), ~ as.character(.)))
            , silent = TRUE)
        
      }
      
      nhd_waterbodies <- nhd_waterbodies %>%
        purrr::keep(~!is.null(.))
      
      try(nhd_waterbodies <- nhd_waterbodies %>% 
            dplyr::bind_rows() %>%
            dplyr::distinct(),
          silent = TRUE)
    }))
    
    if(length(features) == 1 && features == "waterbodies") {
      
      if(length(nhd_waterbodies) == 0 || is.null(nhd_waterbodies)){print("There are no NHD waterbodies associated with your area of interest.")}
      
      return(nhd_waterbodies)
    }
    
    if(length(nhd_waterbodies) == 0 || is.null(nhd_waterbodies)){print("There are no NHD waterbodies associated with your area of interest.")}
    
  }
  
  # Combinations of features selected, and what they return:
  
  if(length(features) == 2 && "catchments" %in% features && "flowlines" %in% features){
    
    nhd_list <- list("NHD_catchments" = nhd_catchments,
                     "NHD_flowlines" = nhd_flowlines)
    
    return(nhd_list)
    
  } else if(length(features) == 2 && "catchments" %in% features && "waterbodies" %in% features){
    
    nhd_list <- list("NHD_catchments" = nhd_catchments,
                     "NHD_waterbodies" = nhd_waterbodies)
    
    return(nhd_list)
    
  } else if(length(features) == 2 && "flowlines" %in% features && "waterbodies" %in% features){
    
    nhd_list <- list("NHD_flowlines" = nhd_flowlines,
                     "NHD_waterbodies" = nhd_waterbodies)
    
    return(nhd_list)
    
  } else if(length(features) == 3  && "catchments" %in% features && "flowlines" %in% features && "waterbodies" %in% features){
    
    nhd_list <- list("NHD_catchments" = nhd_catchments,
                     "NHD_flowlines" = nhd_flowlines,
                     "NHD_waterbodies" = nhd_waterbodies)
    
  } else {stop("Please select between 'catchments', 'flowlines', 'waterbodies', or any combination for `feature` argument.")}
  
}
