There are 3 steps to this workflow:

usgs_pull.py - Pulls data from USGS for 800~ gages in and around CO and exports it as one CSV. This can
be modified for a different list of gages easily.

wrangling_data.py - Merges streamflow data with climate data from Daymet and Gridmet. Exports csvs for
each gage. I got the climate data from GEE in climate_download.js. Not sure if you guys need this but I
already had it. You need precip for some of the signatures.

calc_hydrologic_signatures.py - Calculates 18 hydrologic signatures for each gage, exports a csv.