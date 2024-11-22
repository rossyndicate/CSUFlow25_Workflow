"""
Combines streamflow data with meteorological data from Gridmet and Daymet that I got from Google Earth Engine.
Creates individual CSVs for each basin

NOTE: gridmet_2000-01-01_2024-11-15.csv, daymet_2000-01-01_2024-01-01.csv, and CObasins_flow.csv are over 100mb so I
couldn't add them to the repo. So this script won't run, but the output of it is already generated in
exploratory_scripts\hydrologic_signatures\data\basins
"""
import pandas as pd
import os
wd = os.getcwd()
# read in gridmet
gDf = pd.read_csv(os.path.join(wd,r"exploratory_scripts\hydrologic_signatures\data\gridmet_2000-01-01_2024-11-15.csv")) # Gridmet variables
gDf.index = pd.to_datetime(gDf['system:index'].str[0:8], format="%Y%m%d", utc=True) # convert the first 8 characters into a datetime object, make it the index
gDf['gage'] = gDf['gage'].astype(str).str.zfill(8) # convert to string and put zeros at the beginning until its 8 characters long since that's what USGS gage IDs are
gDf['tmmn'] = gDf['tmmn'] - 273.15 # convert kelvin to celcius
gDf['tmmx'] = gDf['tmmx'] - 273.15 # convert kelvin to celcius
gDf = gDf.drop(columns=['.geo', 'system:index', 'date']) # delete these useless columns

# read in daymet
dDf = pd.read_csv(os.path.join(wd, r"exploratory_scripts\hydrologic_signatures\data\daymet_2000-01-01_2024-01-01.csv"))
dDf.index = pd.to_datetime(dDf['system:index'].str[0:8], format="%Y%m%d", utc=True) # convert the first 8 characters into a datetime object, make it the index
dDf['gage'] = dDf['gage'].astype(str).str.zfill(8) # convert to string and put zeros at the beginning until its 8 characters long since that's what USGS gage IDs are
dDf = dDf.drop(columns=['.geo','system:index', 'date']) # delete this useless column

# read in flow
fDf = pd.read_csv(os.path.join(wd,r'exploratory_scripts\hydrologic_signatures\data\CObasins_flow.csv'))
fDf = fDf.rename(columns={'00060_Mean':'Q_cfs'}) # rename this column
fDf['gage'] = fDf['gage'].astype(str).str.zfill(8) # convert to string and put zeros at the beginning until its 8 characters long since that's what USGS gage IDs are
fDf.index = pd.to_datetime(fDf['dt'], utc=True) # make the datetime the index, make it timezone unaware
fDf = fDf.drop(columns=['dt'])

# create a list for static attributes
basinChars = []

# function to fill a dataframe with all the days in it's date range, even it it's missing some
def fill_missing_days(df):
    try:
        df.index.freq = 'D' # if there's missing days, this will error
        return df
    except:
        full_date_range = pd.date_range(start=df.index.min(), end=df.index.max(), freq='D')
        # Reindex to include all dates, filling missing values with NaN
        return df.reindex(full_date_range)

# loop through every gage
gages = gDf['gage'].unique() # grab all the gage IDs
for gage in gages:
    # make sure there are no days missing in the dataframes and subset each dataframe to only have that gage
    sub_gDf = fill_missing_days(gDf[gDf['gage'] == gage]) #
    sub_fDf = fill_missing_days(fDf[fDf['gage'] == gage]) #
    sub_dDf = fill_missing_days(dDf[dDf['gage'] == gage])

    # area is the only static attribute we have right now
    basinDict = {}
    basinDict['gage'] = gage
    basinDict['area'] = sub_dDf['area'].median() # areas are all the same, median just grabs one
    basinChars.append(basinDict)

    # combine the dfs, start with flow
    oneDf = sub_fDf.merge(sub_dDf, how='inner', right_index=True, left_index=True)
    oneDf = oneDf.merge(sub_gDf, how='inner', right_index=True, left_index=True)

    # drop extra columns
    oneDf = oneDf.drop(columns=['gage_x', 'gage_y', 'area_x', 'area_y'])

    # # assert a daily frequency, ensure
    # oneDf.index.freq = 'D'

    # export csv
    oneDf.to_csv(os.path.join(wd, rf"exploratory_scripts\hydrologic_signatures\data\basins\{gage}.csv"), index_label='date')

# create a dataframe of areas
basinCharsDf = pd.DataFrame.from_dict(basinChars)
# export it
basinCharsDf.to_csv(os.path.join(wd, rf"exploratory_scripts\hydrologic_signatures\data\basinCharacteristics.csv"), index=False)
