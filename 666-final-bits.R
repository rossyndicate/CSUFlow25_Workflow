final_list <- read_csv("data/0-FINAL-DELIVERABLES/watersheds_with_vars_20250624.csv") %>%
  select(index, name, usgs_id, cdwr_id, gage_used,)

# `all_sites` produced in 0_Delineating_Watersheds.Rmd
test <- all_sites %>%
  select(site_no, abbrev)

coords_list_usgs <- test %>%
  filter(!is.na(site_no)) %>%
  inner_join(., final_list, by = c("site_no" = "usgs_id"))

coords_list_cdwr <- test %>%
  filter(!is.na(abbrev)) %>%
  inner_join(., final_list, by = c("abbrev" = "cdwr_id"))

p1_coords_list <- bind_rows(coords_list_cdwr, coords_list_usgs) %>%
  select(index, gage_used, geometry) %>%
  distinct(gage_used, .keep_all = TRUE) 

missing_coords <- final_list %>%
  anti_join(., coords_list, by = "gage_used")

usgs <- dataRetrieval::whatNWISdata(siteNumber = missing_coords %>% filter(!is.na(usgs_id)) %>% .$usgs_id,
                                    parameterCd = "00060") %>%
  st_as_sf(coords = c("dec_long_va", "dec_lat_va"), crs = 4269) %>%
  distinct(site_no, geometry) %>%
  inner_join(., final_list, by = c("site_no" = "usgs_id")) %>%
  rename(usgs_id = site_no)

cdwr <- cdwr_sites %>%
  filter(!is.na(abbrev)) %>%
  filter(abbrev %in% missing_coords$cdwr_id) %>%
  inner_join(., final_list, by = c("abbrev" = "cdwr_id")) %>%
  rename(cdwr_id = abbrev)

final_coords <- p1_coords_list %>%
  bind_rows(usgs) %>%
  bind_rows(cdwr) %>% 
  select(index, gage_used, geometry) %>%
  distinct(index, .keep_all = TRUE)

st_write(final_coords, "data/0-FINAL-DELIVERABLES/site_coordinates_20250731.shp")
