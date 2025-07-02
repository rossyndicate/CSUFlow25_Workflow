"""
Grab streamflow from USGS API and CDWR API
"""
import pandas as pd
from dataretrieval import nwis
import geopandas as gpd
import os
import cdsspy
import numpy as np


def fill_missing_days(df):
    try:
        df.index.freq = 'D' # if there's missing days, this will error
        return df
    except:
        full_date_range = pd.date_range(start=df.index.min(), end=df.index.max(), freq='D')
        # Reindex to include all dates, filling missing values with NaN
        try:
            df = df.reindex(full_date_range)
            df.index.freq = 'D'
            return df
        except:
            # this might trigger if the index has duplicates
            dup_mask = df.index.duplicated(keep='first')
            df = df[~dup_mask]
            df = df.reindex(full_date_range)
            df.index.freq = 'D'
            return df

#
# # define working directory
wd = os.getcwd()
saveDir = fr'C:\Users\C830645719\OneDrive - Colostate\flow_prediction\data\streamflow\gage_csvs_2'

# pull data for this time period
startDate = '1999-10-01'
endDate = '2024-09-30'
parameterCode = '00060' # daily discharge

dataset = pd.read_csv(r"C:\Users\C830645719\OneDrive - Colostate\flow_prediction\data\streamflow\dataset_complete_20250527.csv")
usgs_gages = dataset['usgs_id'].dropna().astype('int64').astype(str).str.zfill(8).tolist()
dataset['usgs_id'] = dataset['usgs_id'].astype('Int64').astype(str).str.zfill(8)
dataset['cdwr_gage_only'] = np.where(pd.isna(dataset['usgs_id']), dataset['cdwr_id'], np.nan)
co_gages = dataset['cdwr_gage_only'].dropna().tolist()

# doing it again with a new list of basins

# get CO gages first
# extra_gages = ['BEAPENCO', 'COSBELNM', 'TARCOMCO', 'WINDESCO']
# co_gages = list(co_gages) + extra_gages
# grab the Colorado gages
for gage in co_gages:
    # Daily discharge at "ANDDITCO" telemetry station
    outCsv = fr'{saveDir}\{gage}.csv'
    if not os.path.exists(outCsv):
        print(f'getting {gage}')
        try:
            flow = cdsspy.get_telemetry_ts(
                abbrev=gage,  # Site abbreviation from the outputs of get_telemetry_stations()
                parameter="DISCHRG",  # Desired parameter, identified by the get_reference_tbl()
                start_date=startDate,  # Starting date
                end_date=endDate,  # Ending date
                timescale="day"  # select daily timescale
            )
        except Exception as e:
            print(f'{gage} failed: {e}')
            continue
        # get some info about the station
        info = cdsspy.get_telemetry_stations(abbrev=gage)
        #cdwr2usgs[gage] = info['usgsStationId'].iloc[0]
        if len(flow) < 365 * 2:
            print(f'{gage} has less than 2 years')
            continue

        flow['lat'] = info['latitude'].iloc[0]
        flow['lon'] = info['longitude'].iloc[0]
        flow['name'] = info['stationName'].iloc[0]
        flow['gage'] = gage
        flow.drop(columns=['abbrev', 'parameter', 'measUnit','modified'], inplace=True)
        flow.rename(columns={'measValue':'Q_cfs', 'measDate':'datetime'}, inplace=True)
        flow.index = pd.to_datetime(flow['datetime'])
        flow.drop(columns='datetime', inplace=True)
        flow = fill_missing_days(flow)
        flow = fill_missing_days(flow)
        flow.index.freq = 'D' # make the frequency daily (weird pandas thing)
        flow.to_csv(outCsv, index=True, index_label='datetime')
    # test if it can be read as daily freq

# get streamflow from USGS gages
#usgs_gages = list(usgs_gages) + ['401733105392404']
#usgs_gages = ['401733105392404']
lessthan2years = []
for gage in usgs_gages:
    gage = gage.zfill(8)
    outCsv = fr'{saveDir}\{gage}.csv'
    if not os.path.exists(outCsv):
    #if True:
        try:
            print(f'getting {gage}')
            flow = nwis.get_dv(sites=gage, parameterCd=parameterCode, start=startDate, end=endDate)[0] # this takes awhile
            info = nwis.get_info(sites=gage)[0]
            # add a bunch of stuff
            if len(flow) < 365*2:
                print(f'{gage} has less than 2 years')
                lessthan2years.append(gage)
                continue
            flow['datetime'] = flow.index
            flow['gage'] = gage
            flow['lat'] = info['dec_lat_va'].iloc[0]
            flow['lon'] = info['dec_long_va'].iloc[0]
            flow['name'] = info['station_nm'].iloc[0]
            flow.rename(columns={'00060_Mean': 'Q_cfs', '00060_Mean_cd':'qc_code'}, inplace=True)

            flow = fill_missing_days(flow) # sometimes whole days are just missing
            flow.drop(columns='datetime', inplace=True)

            flow.index.freq = 'D'  # make the frequency daily (weird pandas thing)
            flow.to_csv(outCsv, index=True, index_label='datetime')

        except Exception as e:
            print(gage, e)
#
# # this decides which type to use
# gageUsed = []
# for index, row in dataset.iterrows():
#     cdwr = row['cdwr_id']
#     usgs = row['usgs_id']
#
#
#     if not pd.isna(cdwr):
#         print('getting ', cdwr)
#         try:
#             cflow = cdsspy.get_telemetry_ts(
#                 abbrev=cdwr,  # Site abbreviation from the outputs of get_telemetry_stations()
#                 parameter="DISCHRG",  # Desired parameter, identified by the get_reference_tbl()
#                 start_date=startDate,  # Starting date
#                 end_date=endDate,  # Ending date
#                 timescale="day"  # select daily timescale
#             )
#             cinfo = cdsspy.get_telemetry_stations(abbrev=cdwr)
#         except Exception as e:
#             print(f'{cdwr} failed: {e}')
#             continue
#         cflow['lat'] = cinfo['latitude'].iloc[0]
#         cflow['lon'] = cinfo['longitude'].iloc[0]
#         cflow['name'] = cinfo['stationName'].iloc[0]
#         cflow['gage'] = cdwr
#         cflow.drop(columns=['abbrev', 'parameter', 'measUnit', 'modified'], inplace=True)
#         cflow.rename(columns={'measValue': 'Q_cfs', 'measDate': 'datetime'}, inplace=True)
#         cflow.index = pd.to_datetime(cflow['datetime'])
#         cflow.drop(columns='datetime', inplace=True)
#         cflow = fill_missing_days(cflow)
#         cflow = fill_missing_days(cflow)
#         cflow.index.freq = 'D'  # make the frequency daily (weird pandas thing)
#
#     if 'NA' not in usgs:
#         print('getting ', usgs)
#         try:
#             uflow = nwis.get_dv(sites=usgs, parameterCd=parameterCode, start=startDate, end=endDate)[0]  # this takes awhile
#             uinfo = nwis.get_info(sites=usgs)[0]
#         except Exception as e:
#             print(f'{usgs} failed: {e}')
#
#         uflow['datetime'] = uflow.index
#         uflow['gage'] = usgs
#         uflow['lat'] = uinfo['dec_lat_va'].iloc[0]
#         uflow['lon'] = uinfo['dec_long_va'].iloc[0]
#         uflow['name'] = uinfo['station_nm'].iloc[0]
#         uflow.rename(columns={'00060_Mean': 'Q_cfs', '00060_Mean_cd': 'qc_code'}, inplace=True)
#         uflow = fill_missing_days(uflow)  # sometimes whole days are just missing
#         uflow.drop(columns='datetime', inplace=True)
#         uflow.index.freq = 'D'  # make the frequency daily (weird pandas thing)
#
#     # ignore na gages
#     if pd.isna(cdwr) or len(cflow) < 1:
#         #uflow.dropna(subset=['Q_cfs'], inplace=True)
#         print(f'using usgs {usgs}')
#         uflow.to_csv(fr'{saveDir}\{usgs}.csv', index=True, index_label='datetime')
#         gageUsed.append(usgs)
#         continue
#     elif 'NA' in usgs or len(uflow) < 1:
#         #cflow.dropna(subset=['Q_cfs'], inplace=True)
#         print(f'using cdwr {cdwr}')
#         cflow.to_csv(fr'{saveDir}\{cdwr}.csv', index=True, index_label='datetime')
#         gageUsed.append(cdwr)
#         continue
#     else:
#         ulen = len(uflow.dropna(subset=['Q_cfs']))
#         clen = len(cflow.dropna(subset=['Q_cfs']))
#         print(f"usgs len: {ulen}, cdwr len: {clen}")
#         if ulen >= clen:
#             print(f'using usgs {usgs}')
#             uflow.to_csv(fr'{saveDir}\{usgs}.csv', index=True, index_label='datetime')
#             gageUsed.append(usgs)
#         else:
#             print(f'using cdwr {cdwr}')
#             cflow.to_csv(fr'{saveDir}\{cdwr}.csv', index=True, index_label='datetime')
#             gageUsed.append(cdwr)
#
# dataset['gage_used'] = gageUsed
