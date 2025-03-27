# General purpose targets list for the harmonization step

# Source the functions that will be used to build the targets in p3_targets_list
tar_source(files = "R/site_selection_functions.R")

site_selection_workflow_list <- list(
  
  # Initial NWIS Site Compilation ====
  
  ## NWIS site continuity and completeness filter ----
  
  # Initial CDWR Site Compilation ====
  
  ## CDWR site continuity and completeness filter ----
  
  # NWIS Site and CDWR Site Combination ====
  
  # Watershed Delineation for Stream Gages ====
  
  ## COMID Retrieval ----
  
  ## Watershed Delineation ---- 
  
  # Filtering watersheds via StreamCAT variables ====
  
  # Retrieving flowlines based on catchment ====
  
  # Potentially add the old sites here?
  
  
  # Classifying watersheds based on transbasin diverions ====
  
  # Hard Coding Previous sites into workflow ====
  # In order to Hard code the previous sites that were used we re run the watershed
  # delineation functions along with the flowline functions to keep those sites. 
  
)
