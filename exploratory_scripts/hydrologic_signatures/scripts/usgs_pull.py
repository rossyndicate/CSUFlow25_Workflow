"""
Grab streamflow from USGS API
"""
import pandas as pd
from dataretrieval import nwis
import geopandas as gpd
import os

# define working directory
wd = os.getcwd()
# I start with these ~800 basins. They are all <300km2 and within 300km of the border of CO.
basins = gpd.read_file(os.path.join(wd, "exploratory_scripts\hydrologic_signatures\GIS_Data\CO_basins.shp"))
# get a list of the gage IDs
gages = list(basins['gage'])
# pull data for this time period
startDate = '2000-10-01'
endDate = '2024-09-01'
parameterCode = '00060' # daily discharge
# get streamflow
flow = nwis.get_dv(sites=gages, parameterCd=parameterCode, start=startDate, end=endDate)[0] # this takes awhile

# make this a column instead of the index
flow['gage'] = flow.index
# split the gage and date into two columns
flow[['gage', 'dt']] = pd.DataFrame(flow['gage'].tolist(), index=flow.index)
# make datetime the index and export
flow.index = pd.to_datetime(flow['dt'])
# delete these columns
flow = flow.drop(columns=['00060_Minimum', '00060_Minimum_cd', '00060_Maximum', '00060_Maximum_cd', 'dt'])
# export it
flow.to_csv(r'C:\Users\willy\Documents\CSU_GIS_Data\CWCB24\data\CObasins_flow.csv')
