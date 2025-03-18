nldi_finder <- function(site_no) {
  # get those gages' watersheds using get_nldi_basin in {nhdplusTools}
  nldi_nwis <- list(featureSource = "nwissite",
                    featureID = paste0("USGS-", site_no))
  
  tryCatch({
    invisible(
      suppressMessages(
        gage_basin <- nhdplusTools::get_nldi_basin(nldi_feature = nldi_nwis) %>% 
          sf::st_transform(., 4269) %>% 
          dplyr::mutate(site_no = site_no)
      )
    )
    return(gage_basin)
  }, error = function(e) {
    return(dplyr::tibble(site_no = site_no))
  })
 
}

nldi_meta <- function(site_no) {
  # get metadata
  nldi_nwis <- list(featureSource = 'nwissite', 
                    featureID = paste0("USGS-", site_no))
  
  tryCatch({
    gage_basin <- nhdplusTools::get_nldi_characteristics(nldi_feature = nldi_nwis,
                                                         type = 'total')[[1]] 
  }, error = function(e) {
    return(tibble(site_no = site_no))
  })
  
  gage_basin <- gage_basin %>% 
    pivot_wider(id_cols = -percent_nodata,
                values_from = characteristic_value,
                names_from = characteristic_id)
  return(gage_basin)
}

listNWIS <- function(state_usps, dist) {
  sf::sf_use_s2(FALSE)
  
  # add buffer around area of interest
  # aoi_buffer <- aoi %>% 
  #   sf::st_buffer(., dist = dist)
  # 
  # gage_sites <- vector("list", length = nrow(aoi_buffer))
  
  # for (i in 1:nrow(aoi_buffer)){
  #   
  #   bbox <- sf::st_bbox(aoi_buffer[i]) %>% 
  #     as.vector() %>% 
  #     round(., digits = 7) %>% 
  #     paste(collapse = ",")
  #   
  #   gage_sites[[i]] <- dataRetrieval::whatNWISdata(stateCd = state_usps, parameterCd = "00060")
  #   
  # }
  
  gage_sites <- dataRetrieval::whatNWISdata(stateCd = state_usps, parameterCd = "00060")
  
  gage_sites <- gage_sites %>%
    sf::st_as_sf(coords=c('dec_long_va', 'dec_lat_va'), crs=4269)
  
  tables <- rvest::read_html('https://help.waterdata.usgs.gov/parameter_cd?group_cd=%') %>%
    rvest::html_nodes('table') %>%
    rvest::html_table()
  
  pcodes <- tables[[1]] %>% 
    janitor::clean_names() %>% 
    dplyr::mutate(parm_cd = stringr::str_pad(as.character(parameter_code), 5, pad = "0"))
  
  inventory <- gage_sites %>%
    dplyr::left_join(pcodes,by="parm_cd") %>%
    # sf::st_intersection(.,aoi_buffer) %>%
    sf::st_join(., dplyr::select(aoi, UNIT_CODE), left=T) %>%
    # dplyr::mutate(within_park=ifelse(is.na(UNIT_CODE.y), "Outside","Within")) %>%
    dplyr::rename(c(site_name=station_nm,
                    data_type_cd,
                    site_type_cd=site_tp_cd,
                    n_obs=count_nu,
                    begin_date,
                    end_date,
                    parameter=parameter_name_description,
                    code=parm_cd))
  
  site_url <- 'https://maps.waterdata.usgs.gov/mapper/help/sitetype.html'
  
  table <- rvest::read_html(site_url) %>%
    rvest::html_nodes('table') %>%
    rvest::html_table() #%>%
  
  table <- rbind(table[[1]],table[[2]],table[[3]],table[[4]],table[[5]]) %>%
    dplyr::select(site_type_cd = 1,
                  site_type = 2)
  
  inventory <- left_join(inventory,table,by='site_type_cd') %>%
    mutate(data_type=case_when(data_type_cd=="dv" ~ "Daily",
                               data_type_cd=="uv" ~ "Unit",
                               data_type_cd=="qw" ~ "Water Quality",
                               data_type_cd=="gw" ~ "Groundwater Levels",
                               data_type_cd=="iv" ~ "Unit",
                               data_type_cd=="sv" ~ "Site Visits",
                               data_type_cd=="pk" ~ "Peak Measurements",
                               data_type_cd=="ad" ~ "USGS Annual Water Data Report",
                               data_type_cd=="aw" ~ "Active Groundwater Level Network",
                               data_type_cd=="id" ~ "Historic Instantaneous")) %>%
    sf::st_join(., dplyr::select(aoi_buffer))#, UNIT_CODE))
  
  return(inventory)
  
}

# .data is a sf data frame I guess
fetchNHD_flowlines <- function(.data){
  
  nhd_plus_hr_url <- "https://hydro.nationalmap.gov/arcgis/rest/services/NHDPlus_HR/MapServer"
  
  # open the nhd_hr - which contains a bunch of layers
  nhd_hr <- arcgislayers::arc_open(nhd_plus_hr_url)
  
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
  
  nhd_hr_flowlines <- arcgislayers::get_layer(nhd_hr, 3)
  
  # use bbox to return associated flowlines
  geospatial_aoi <- .data %>% 
    # st_buffer(0.0000001) %>% 
    # Convert sf object to sfc object (required for downloading from the map server)
    st_as_sfc(.)
  
  nhd_flowlines <- vector("list", length = length(geospatial_aoi)) #data frame of each water shed?
  
  # fields <- arcgislayers::list_fields(nhd_hr_flowlines)
  
  # TODO: update the query for only artificial stuff
  # query <- "Ftype IN (428, 420, 336, 468, 558)"
  
  # Here is where the intersection happens, which is really just a filter based on 
  # the shape of the watersheds. The flow lines seem to be cut off at the edge of the polygons,
  # because it is a filter instead of an intersection.
  for(i in 1:length(geospatial_aoi)){
    tryCatch({
      nhd_flowlines[[i]] <- arcgislayers::arc_select(nhd_hr_flowlines,
                                                     # where = query,
                                                     filter_geom = geospatial_aoi[i],
                                                     crs = st_crs(geospatial_aoi[i])) %>% 
        st_make_valid()},
      error = function(e){
        cat("Index ", i, " from input data failed.")
      }) 
  }
  
  nhd_flowlines <- nhd_flowlines %>% 
    keep(~!is.null(.))
  
  try(nhd_flowlines <- nhd_flowlines %>% 
        dplyr::bind_rows() %>%
        dplyr::distinct(), 
      silent = TRUE)
  
  # transform polygon to polyline and find those intersections instead
  return(nhd_flowlines)
  
}

# fetchNHD_flowlines(.data = sample_nwis_watersheds)

# ----

# load in the NHD as a table. This table lists all COMIDs in CONUS and allows you to "navigate" the NHD.
nhd <- read_csv(here("data", "nhd_flow_network.csv"))

# function to delineate each gage's watershed:
# - gut check with better termination
watershed_delineator <- function(site_list){
  
  # filter our master list to just the gage we are iterating over
  site <- nwis_sites %>%
    filter(site_no == site_list)
  
  # use get_UT to list all comids that are upstream of our gage using the comid the
  # gage falls on:
  upstream <- nhdplusTools::get_UT(nhd, site$comid)
  
  # grab all the catchments associated with the upstream comids:
  nhd_catch <- nhdplusTools::get_nhdplus(comid = upstream,
                                         realization = 'catchment',
                                         t_srs = 4269) %>%
    # remove dupes (precautionary step, not likely necessary)
    dplyr::distinct(featureid, .keep_all=TRUE) %>%
    # "dissolve" all the catchments into a single polygon
    dplyr::summarize() %>% # this makes the watershed
    # remove weird hole by-products that exist if the catchment boundaries don't
    # line up perfectly:
    nngeo::st_remove_holes() %>%
    # tack on the state, site name, and comid to the watershed
    dplyr::mutate(state = site$STUSPS,
                  site_no = site$site_no,
                  comid = site$comid)
  
  # return the delineated watershed
  return(nhd_catch)
  
}
