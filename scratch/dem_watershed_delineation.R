# Load libraries ----

# To install StreamCatTools:
# library(remotes)
# install_github("USEPA/StreamCatTools", build_vignettes=FALSE)

# To install climateR:
# library(remotes)
# remotes::install_github("mikejohnson51/AOI") # suggested!
# remotes::install_github("mikejohnson51/climateR")

packages <- c(
  'tidyverse',
  # Spatial data handling
  'sf',
  'terra',
  'raster',
  'stars',
  # Hydrological analysis
  'whitebox',
  'nhdplusTools',
  'arcgislayers',
  # DEM processing and retrieval
  'elevatr', 
  'FedData',
  'tmap',
  'climateR',
  'data.table',
  'mapview',
  'here',
  'furrr',
  'nngeo',
  'retry',
  'units',
  'leaflet',
  'htmlwidgets'
  )

# this package loader avoids unloading and reloading packages 
package_loader <- function(x) {
  if (!requireNamespace(x, quietly = TRUE)) {
    install.packages(x)
  }
  require(x, character.only = TRUE)
}

lapply(packages, package_loader)

# Load relevant data ----
gage_sites <- read_rds(here("data", "usgs_cdwr_sites.rds"))

watersheds <- read_rds(here("data", "watersheds_div.rds"))

flowlines_list <- map(list.files(here("data", "flowlines2"), full.names = T), ~read_rds(.x))
# Name flowlines list elements by their site_no values
names(flowlines_list) <- map_chr(flowlines_list, ~unique(.x$site_no)[1])

# Grab samples ----

gage_point <- "09010500"

nldi_point <- paste0("USGS-", gage_point)

# Sample Point
point <- filter(gage_sites, site_no == gage_point)

# Sample Watershed
watershed <- filter(watersheds, site_no == gage_point)

# Sample Flowline
flowlines <- flowlines_list[[gage_point]]

# Acquire DEM data ----

## Download DEM for area plus buffer
watershed_bbox_dem <- elevatr::get_elev_raster(watershed, z = 10, expand = 1.2)
writeRaster(here("data", "rasters", paste0(gage_point, "_dem.tif")))

## Prepare you pour point for interaction with WhiteboxTools
st_write(here("data", ))