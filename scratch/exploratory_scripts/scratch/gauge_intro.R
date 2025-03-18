# Stephanie's hydrology class
  # wr416 (sit in this semester)
  # wr616 next semester
  # Diff EQ?

library(dataRetrieval)
library(tidyverse)
library(here)

# need site numbers and names
flow_model_data <- read_csv(here("prediction_paper_original_work", "CWCB_USGS_CDWR", "CO_streamflow", "Data", "Models","Flow_Model_Data.csv")) %>% 
  select(!X)

# get site information
site_description <- readNWISsite("06746095")

# remove sites that don't have drainage area

# download daily discharge data
daily_q <- readNWISdv(siteNumber = "06746095", parameterCd = "00060", startDate = "1999-10-01", endDate = "2015-09-30")

# clean the data (we need to do unit conversions)
clean_q <- daily_q %>% 
  addWaterYear() %>% 
  renameNWISColumns() %>% 
  filter(!is.na(Flow)) %>% 
  inner_join(candidate_sites %>% 
               select(site_no, drain_area_va)) %>% 
  mutate(mmday = (((Flow/drain_area_va*5280*5280))*304.8)*(60*60*24)) # unit conversion to mm to day *make sure that this is right

summary_clean_q

# some more cleaning
complete_sites <- clean_q %>% 
  group_by(site_no, waterYear) %>% 
  summarise(n = n())

summary(complete_sites$n) # need to set some trimming to make sure that there is enough

# make some site summary stats
site_summary_stats <- clean_q %>% 
  group_by(site_no) %>% 
  summarize(mean_flow = mean(mmday), # do this on the converted units, not the original data
            median_flow,
            q99, #max
            q01, #min
            q9999,
            q0001,)

# site_summary_stats will get paired with all the other data in the sites so that we can actually make a model