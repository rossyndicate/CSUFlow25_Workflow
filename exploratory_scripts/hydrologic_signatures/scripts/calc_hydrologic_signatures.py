"""
Calculates hydrologic signatures:
high_q_freq: Frequency of high-flow events (>`threshold` times the median flow) threshold = 9
high_q_dur: Average duration of high-flow events (number of consecutive steps >`threshold` times the median flow) threshold = 9
low_q_freq: Frequency of low-flow events (<`threshold` times the median flow) threshold = 0.2
low_q_dur: Average duration of low-flow events (number of consecutive steps <`threshold` times the median flow) threshold = 0
zero_q_freq: percent of the time with zero flow.2
q95: 95th percentile q
q5: 5th percentile q
q_mean: mean q
baseflow_index:  Ratio of mean baseflow to mean discharge, see https://www.tandfonline.com/doi/abs/10.7158/13241583.2013.11465417 for baseflow calculation
hfd_mean: Average duration of high-flow events (number of consecutive steps >`threshold` times the median flow) threshold = 9
runoff_ratio: Runoff ratio (ratio of mean discharge to mean precipitation)
stream_elas: Streamflow precipitation elasticity (sensitivity of streamflow to changes in precipitation at the annual time scale) see doi:10.1029/2000WR900330
q50_meanDOY: The average day of the year that the 50% of the total yearly runoff has occurred
q50_meanDate: The average date that the 50% of the total yearly runoff has occurred (same as q50_meanDOY just in date format)
slope_fdc: Calculates flow duration curve slope. Slope of the flow duration curve (between the log-transformed `lower_quantile` and `upper_quantile`)

data was downloaded in usgs_pull.py
wrangled into individual basin files in wrangling_data.py
"""
import pandas as pd
import os
from neuralhydrology.evaluation.signatures import *
import xarray as xr
import numpy as np
import warnings
warnings.filterwarnings("ignore", category=UserWarning) # there's a warning baked into neuralhydrology.evaluation.signatures that I want to ignore

# get the working directory
wd = os.getcwd()
basinsDir = os.path.join(wd, r'exploratory_scripts\hydrologic_signatures\data\basins')

signaturesList = [] # list to store the signatures from each basin
# loop through all the basins
for file in os.listdir(basinsDir):
    df = pd.read_csv(os.path.join(basinsDir, file)) # read file
    df.index = pd.to_datetime(df['date']) # read date as index
    df.index = df.index.tz_localize(None) # pandas assumes a timezone, tell it no
    df.index.freq = 'D' # make the frequency daily (weird pandas thing)
    gage = file[0:8]

    # convert to CMS
    df['Q_cms'] = df['Q_cfs'] * 0.0283168

    # data array, just includes date and flow. Gotta convert from pd.Series object to xr.DataArray for the Neuralhydrology function
    Q_da = xr.DataArray(df['Q_cms'], coords=[df.index], dims=["date"])
    P_da = xr.DataArray(df['pr'], coords=[df.index], dims=["date"]) # precip for runoff ratio calc. 'pr' is gridmet derived precip

    # calculate a bunch of signatures using this function from neuralhydrology (see https://github.com/neuralhydrology/neuralhydrology/blob/master/neuralhydrology/evaluation/signatures.py)
    signatures = calculate_signatures(Q_da, signatures=get_available_signatures(), prcp=P_da)

    # add a few other things to the data
    signatures['gage'] = gage
    signatures['minDate'] = df.index.min()
    signatures['maxDate'] = df.index.max()
    signatures['missing_rows'] = df['Q_cms'].isna().sum()

    # calculate Q50_meanDOY
    # Q50_meanDOY
    Q50_DOYs = []
    for year in df.index.year.unique(): # loops through every year this gage has data for
        year_df = df[df.index.year == year].copy() # subset the df
        if len(year_df.dropna()) < 360: # if there are more than 5 missing days, don't use that year
            #print(gage, year, 'missing') # uncomment if you want to see what's missing
            continue
        else:
            year_df['Q_cms_cumsum'] = year_df['Q_cms'].cumsum()  # cumulative sum column
            annualQ = year_df['Q_cms'].sum() # total annual Q
            year_df['perc_ann_Q'] = year_df['Q_cms_cumsum'] / annualQ # percent of total Q
            Q50_DOY = year_df[year_df['perc_ann_Q'] > 0.5].index[0].dayofyear # grab the first day that goes over 0.5
            Q50_DOYs.append(Q50_DOY) # append to list
    # need at least 5 valid years
    if len(Q50_DOYs) > 5:
        signatures['Q50_meanDOY'] = round(sum(Q50_DOYs) / len(Q50_DOYs)) # get the mean, round to integer
        dateQ50 = pd.to_datetime('2000'+str(signatures['Q50_meanDOY']), format='%Y%j')
        signatures['Q50_meanDate'] = f'{dateQ50.month}-{dateQ50.day}'
    else:
        signatures['Q50_meanDOY'] = np.nan

    # add to list
    signaturesList.append(signatures)

signaturesDf = pd.DataFrame().from_dict(signaturesList)
signaturesDf.index = signaturesDf['gage']
signaturesDf.to_csv(r'C:\Users\willy\Documents\GitHub\flow_prediction\exploratory_scripts\signatures_by_basin.csv', index_label='gage')