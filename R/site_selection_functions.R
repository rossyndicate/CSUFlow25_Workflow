#' Get USGS NWIS Stream Gauge Sites by State
#'
#' @description
#' Retrieves USGS National Water Information System (NWIS) stream gauge sites
#' for a specified state and converts them to a spatial (sf) object.
#'
#' @param state_postal Character string of the two-letter state postal code (e.g., "CO" for Colorado).
#'
#' @return An sf object containing stream gauge sites with associated metadata and
#'   geometric points in NAD83 coordinate reference system (EPSG:4269).
#'
#' @details
#' This function queries sites that have discharge data (parameter code 00060)
#' for the specified state, retrieves detailed site information, and returns
#' the data as a spatial features (sf) object.
#'
#' @importFrom dataRetrieval whatNWISsites readNWISsite
#' @importFrom sf st_as_sf
#'
#' @examples
#' \dontrun{
#' # Get stream gauge sites for Colorado
#' co_sites <- get_nwis_sites_by_state("CO")
#' }

get_nwis_sites_by_state <- function(state_postal) {
  # Get sites with discharge data for the specified state
  nwis_sites <- dataRetrieval::whatNWISsites(
    stateCd = state_postal,
    parameterCd = "00060"
  ) 
  
  # Read site information for the sites we found
  site_info <- dataRetrieval::readNWISsite(nwis_sites$site_no)
  
  # Convert to spatial object
  sites_sf <- sf::st_as_sf(
    site_info, 
    coords = c("dec_long_va", "dec_lat_va"), 
    crs = 4269
  )
  
  return(sites_sf)
}
