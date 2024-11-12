library(tidyverse)
library(sf)
library(dataRetrieval)
library(nhdplusTools)
library(elevatr)
library(terra)

# Fxns needed for workflow ---- 

nldi_finder <- function(site_no) {
  #get those gages' watersheds using get_nldi_basin in the {nhdplusTools}.
  nldi_nwis <- list(featureSource = "nwissite",
                    featureID = paste0("USGS-", site_no))
  invisible(
    suppressMessages(
      gage_basin <- nhdplusTools::get_nldi_basin(nldi_feature = nldi_nwis) %>%
        sf::st_transform(., 4269) %>%
        dplyr::mutate(site_no = site_no)))
  return(gage_basin)
}

listNWIS <- function(aoi, dist){
  
  sf::sf_use_s2(FALSE)
  
  # add buffer around area of interest
  aoi_buffer <- aoi %>% 
    sf::st_buffer(., dist = dist)
  
  # aoi <- getParkBoundary("DEVA")
  
  gage_sites <- vector("list", length = nrow(aoi_buffer))
  
  for (i in 1:nrow(aoi_buffer)){
    
    bbox <- sf::st_bbox(aoi_buffer[i]) %>%
      as.vector() %>%
      round(., digits=7) %>% 
      paste(collapse = ",")
    
    gage_sites[[i]] <- dataRetrieval::whatNWISdata(bBox = bbox, parameterCd = "00060")
    
  }
  
  gage_sites <- dplyr::bind_rows(gage_sites) %>%
    sf::st_as_sf(coords=c('dec_long_va','dec_lat_va'),crs=4269)
  
  tables <- rvest::read_html('https://help.waterdata.usgs.gov/parameter_cd?group_cd=%') %>%
    rvest::html_nodes('table') %>%
    rvest::html_table()
  
  pcodes <- tables[[1]] %>%
    janitor::clean_names() %>%
    dplyr::mutate(parm_cd=stringr::str_pad(as.character(parameter_code),5,pad="0"))
  
  inventory <- gage_sites %>%
    dplyr::left_join(pcodes,by="parm_cd") %>%
    sf::st_intersection(.,aoi_buffer) %>%
    # sf::st_join(., dplyr::select(aoi, UNIT_CODE), left=T) %>%
    # dplyr::mutate(within_park=ifelse(is.na(UNIT_CODE.y), "Outside","Within")) %>%
    dplyr::select(c(site_no,
                    site_name=station_nm,
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

nldi_meta <- function(site_no) {
  # get metadata
  nldi_nwis <- list(featureSource = "nwissite",
                    featureID = paste0("USGS-", site_no))
  gage_basin <- 
    nhdplusTools::get_nldi_characteristics(nldi_feature = nldi_nwis, 
                                           type = "total")[[1]] %>%
    pivot_wider(id_cols = -percent_nodata, 
                values_from = characteristic_value, 
                names_from = characteristic_id)
  
  return(gage_basin)
  
}

## START OF WORKFLOW ## ----

aoi <- tigris::states() %>% #ANY SF OBJECT!
  filter(NAME %in% c("Colorado"))#,"Wyoming" "Utah", "New Mexico", "Arizona")) 

# Get a list of NWIS sites
nwis <- listNWIS(aoi = aoi %>%
                   # some distance around the AOI:
                   dplyr::summarize(), dist = 0)

nwis <- nwis %>% 
  filter(site_type_cd == 'ST',
         year(end_date) >= 2000)

# Get NWIS Stream Gages of reference quality
# This is where we can get the reference "good quality" watersheds
ref_gages <- nhdplusTools::get_gagesII(id = nwis$site_no) %>%
  dplyr::filter(class == "Ref")


# Subset to only gages with daily streamflow and add meta data from gages_ii:
nwis_select_stream_gage <- nwis %>%
  dplyr::filter(site_no %in% ref_gages$staid,
                # Daily flow data:
                data_type_cd == "dv") %>%
  dplyr::left_join(st_drop_geometry(ref_gages), 
                   by = c("site_no" ="staid"))

# Get watersheds associated with those stream gages:
nwis_select_watershed <- 
  nwis_select_stream_gage$site_no %>%
  purrr::map_dfr(~nldi_finder(site_no = .)) %>%
  dplyr::mutate(data = map(site_no, ~nldi_meta(site_no = .))) %>%
  unnest(cols = c(data)) %>%
  dplyr::left_join(st_drop_geometry(nwis_select_stream_gage), by = "site_no")


# Download data from those stream gages:
nwis_select_discharge <- 
  dataRetrieval::readNWISdv(siteNumbers = nwis_select_stream_gage$site_no,
                            parameterCd = c('00060','00065')) %>%
  dplyr::rename(c("discharge" = "X_00060_00003",
                  "date" = "Date")) 

# ---- Updating workflow here ----

aoi <- tigris::states() %>% #ANY SF OBJECT!
  filter(NAME %in% c("Wyoming")) # bounding box is too big if I use all the states that work

# aoi_list <- map(c("Colorado","Wyoming", "Utah"),
#                 function(state){
#                   aoi <- tigris::states() %>% 
#                     filter(NAME == state)
#                 })

# Get a list of NWIS sites
nwis <- listNWIS(aoi = aoi %>%
                   # some distance around the AOI:
                   dplyr::summarize(), dist = 0)

nwis <- nwis %>%
  filter(site_type_cd == 'ST',
         year(end_date) >= 2000)

# nwis_list <- map(aoi_list, 
#                  function(aoi){
#                    nwis_obj <- listNWIS(aoi = aoi %>%
#                                           # some distance around the AOI:
#                                           dplyr::summarize(), dist = 0)
#                    nwis_obj <- nwis_obj %>% 
#                      filter(site_type_cd = 'ST',
#                             year(end_date) >= 2000)
#                  })



# Get elevation data for all of the states
co_geo <- st_transform(aoi, 4326)
elev_data <- get_elev_raster(co_geo, z = 7)

# Convert to terra SpatRaster and then to points
elev_terra <- rast(elev_data)
elev_points <- as.data.frame(elev_terra, xy = TRUE)
names(elev_points)[3] <- "elevation"

# Create the CO map
ggplot() +
  # Add elevation layer
  geom_raster(data = elev_points,
              aes(x = x, y = y, fill = elevation)) +
  scale_fill_gradientn(
    colors = terrain.colors(10),
    name = "Elevation (m)") +
  # Add state outline
  geom_sf(data = co_geo,
          fill = NA,
          color = "black",
          size = 0.5) +
  # Add NWIS sites
  geom_sf(data = st_transform(nwis, 4326),
          color = "black",
          size = 2,
          alpha = 0.7) +
  # Customize the theme
  theme_minimal() +
  labs(title = "NWIS Sites in Wyoming",
       subtitle = "with Topographic Relief",
       caption = "Data: USGS National Water Information System & elevatr package",
       color = "Site Type") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "right") +
  coord_sf()
