"""
data was downloaded in usgs_pull.py and saved in gage_csvs_2
"""
import pandas as pd
import os
import xarray as xr
import numpy as np
import warnings
import geopandas as gpd
import dataretrieval.nwis as nwis
from matplotlib.backends.backend_pdf import PdfPages
import matplotlib

matplotlib.use('TkAgg')
import matplotlib.pyplot as plt
warnings.filterwarnings("ignore", category=UserWarning) # there's a warning baked into neuralhydrology.evaluation.signatures that I want to ignore

# get the working directory
wd = os.getcwd()
basinsDir = r'C:\Users\C830645719\OneDrive - Colostate\flow_prediction\data\streamflow\gage_csvs_2'

basinsWarea = gpd.read_file(r"C:\Users\C830645719\OneDrive - Colostate\flow_prediction\data\streamflow\watersheds_shapefile_20250527.shp")
basinsWarea = basinsWarea.to_crs(epsg=3857)
basinsWarea = basinsWarea.sort_values(by='cdwr_id')
basinsWarea["area"] = basinsWarea['area_km2'] * 1e6
basinsWarea = basinsWarea.sort_values(by='area')

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

def valid_fraction(df, varName=None):
    """
    Returns the fraction of rows that are completely valid (no NaNs).
    """
    if varName:
        df = df[varName]
    total_rows = len(df)
    valid_rows = df.dropna().shape[0]
    if total_rows == 0:
        return 0  # or np.nan if you prefer
    return valid_rows / total_rows

def calc_flood_freq(Q_df, freq, varName='Q_cfs'):
    Q_df = drop_bad_years(Q_df, varName)

    if len(Q_df.index.year.unique()) < 15:
        print('Not enough years of data to calculate flood frequency.')
        return np.nan
    # Step 1: Extract annual maximum streamflow
    Q_df = pd.DataFrame(Q_df[varName])
    annual_max = Q_df.resample("YE-SEP").max()

    # Step 2: Sort annual max values in descending order
    annual_max_sorted = annual_max.sort_values(by=varName, ascending=False)
    annual_max_sorted["Rank"] = range(1, len(annual_max_sorted) + 1)

    # Step 3: Compute exceedance probability using Weibull formula
    n = len(annual_max_sorted)
    annual_max_sorted["Exceedance Probability"] = annual_max_sorted["Rank"] / (n + 1)

    # Step 4: Compute return period
    annual_max_sorted["Return Period (Years)"] = 1 / annual_max_sorted["Exceedance Probability"]
    annual_max_sorted = annual_max_sorted.sort_values(by='Return Period (Years)', ascending=True)

    # Step 5: Find the flow corresponding to a 1.5-year return period (interpolating if necessary)
    flood_freq = np.interp(freq, annual_max_sorted['Return Period (Years)'], annual_max_sorted[varName])

    return flood_freq

def Qmean_total(Q_df, varName='Q_cfs'):
    Q_df = drop_bad_years(Q_df, varName)
    Q_df = Q_df[varName]
    annual_total = Q_df.resample("YE-SEP").sum()
    return annual_total.mean()

def Qmax(Q_df, varName='Q_cfs'):
    Q_df = drop_bad_years(Q_df, varName)
    Q_df = Q_df[varName]
    annual_max = Q_df.resample("YE-SEP").max()
    return annual_max.mean()

def Qmin(Q_df, varName='Q_cfs'):
    Q_df = drop_bad_years(Q_df, varName)
    Q_df = Q_df[varName]
    annual_min = Q_df.resample("YE-SEP").min()
    return annual_min.mean()

def Fmonsoon(Q_df, varName='Q_cfs'):
    Q_df = drop_bad_years(Q_df, varName)
    Q_df = Q_df[varName]
    monsoon_szn = Q_df[Q_df.index.month.isin([7,8,9])] # flow in these months

    if valid_fraction(monsoon_szn) < 0.9:
        return np.nan
    else:
        monsoon_szn_sum = monsoon_szn.sum()
    totalQ = Q_df.sum()
    return monsoon_szn_sum / totalQ

month_abbr = {
    1: "jan",  2: "feb",  3: "mar",  4: "apr",
    5: "may",  6: "jun",  7: "jul",  8: "aug",
    9: "sep", 10: "oct", 11: "nov", 12: "dec"
}

def mean_monthly_Q(Q_df, type, varName='Q_cfs'):

    mean_monthly_dict = {}

    # resample to monthly mean streamflow
    for month in range(1,13):
        monthName = month_abbr[month]
        month_df = Q_df[Q_df.index.month == month][[varName,'water_year']] # get the Q for the month
        month_avgs = [] # fill with means
        yearsCounted = 0
        for year in month_df['water_year'].unique():
            monthYeardf = month_df[month_df['water_year'] == year]
            days_in_month = monthYeardf[varName].size # .size counts NAs
            not_na_month = monthYeardf[varName].count() # .count doesn't
            valid_frac = not_na_month / days_in_month
            # only count that month is 80% is valid
            if valid_frac > 0.8:
                month_avgs.append(monthYeardf[varName].mean()) # append the mean of that month and water year
                yearsCounted += 1
            else:
                continue
        total_years = len(month_df.index.year.unique())
        frac_valid_years = yearsCounted / total_years
        if frac_valid_years < 0.5 or yearsCounted < 6:
            mean_monthly_dict[f'{type}_{monthName}_{varName}'] = np.nan
        else:
            mean_monthly_dict[f'{type}_{monthName}_{varName}'] = sum(month_avgs) / yearsCounted

    if len(mean_monthly_dict) < 12:
        raise ValueError('your month function sucks')
    return mean_monthly_dict

def sum_monthly_Q(Q_df, type, varName='Q_mmd'):

    mean_monthly_dict = {}

    # resample to monthly mean streamflow
    for month in range(1,13):
        monthName = month_abbr[month]
        month_df = Q_df[Q_df.index.month == month][[varName,'water_year']] # get the Q for the month
        month_avgs = [] # fill with means
        yearsCounted = 0
        for year in month_df['water_year'].unique():
            monthYeardf = month_df[month_df['water_year'] == year]
            days_in_month = monthYeardf[varName].size # .size counts NAs
            not_na_month = monthYeardf[varName].count() # .count doesn't
            valid_frac = not_na_month / days_in_month
            # only count that month is 80% is valid
            if valid_frac > 0.8:
                month_avgs.append(monthYeardf[varName].sum()) # append the mean of that month and water year
                yearsCounted += 1
            else:
                continue
        total_years = len(month_df.index.year.unique())
        frac_valid_years = yearsCounted / total_years
        if frac_valid_years < 0.5 or yearsCounted < 6:
            mean_monthly_dict[f'{type}_{monthName}_{varName}'] = np.nan
        else:
            mean_monthly_dict[f'{type}_{monthName}_{varName}'] = sum(month_avgs) / yearsCounted

    if len(mean_monthly_dict) < 12:
        raise ValueError('your month function sucks')
    return mean_monthly_dict

def calendar_to_water_year_day(date):
    if date.year % 4 == 0:
        if date.month > 9:
            return date.timetuple().tm_yday - 274
        else:
            return date.timetuple().tm_yday + 91
    else:
        if date.month > 9:
            return date.timetuple().tm_yday - 273
        else:
            return date.timetuple().tm_yday + 92

def water_to_calendar_day(doy):
    if doy < 92:
        return doy + 273
    else:
        return doy - 91

def cumulative_Q_quantiles(df, type, varName='Q_cfs', quantiles=[0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9]):
    # calc for each water year
    results = []
    for year in df['water_year'].unique():
        # subset the data to the water year
        df_wy = df.loc[f'{year-1}-10-01':f'{year}-09-30'].copy()
        # check if there's enough data
        if len(df_wy) < 365 * .9:  # if not enough data for that year, fill the whole list with nan
            continue
        totalQ = df_wy[varName].sum()
        resultDict = {'year': year}
        df_wy['Q_cumsum'] = df_wy[varName].cumsum()
        df_wy['Q_cumsum_frac'] = df_wy['Q_cumsum'] / totalQ
        # get the quantiles
        for q in quantiles:
            try:
                resultDict[f'{type}_mean_flowdate_{q}'] = calendar_to_water_year_day(df_wy[df_wy['Q_cumsum_frac'] > q].index[0].date()) # gotta convert to water year or the averaging doesn't work
            except:
                # if empty df because of no flow, set to nan
                resultDict[f'{type}_mean_flowdate_{q}'] = np.nan
        results.append(resultDict)

    results_df = pd.DataFrame(results)
    means = results_df.mean().to_dict()
    means.pop('year')
    means = {k: np.round(v) for k, v in means.items()}
    #means = {k: water_to_calendar_day(v) for k, v in means.items()}
    return means

def missing_days_bymonth(df, type, varName='Q_cfs'):
    # calc for each water year
    resultDict = {}
    # get the quantiles
    for month in range(1, 13):
        month_name = month_abbr[month]
        month_df = df[df.index.month == month][varName].copy()
        resultDict[f'{type}_perc_missing_{month_name}'] = (month_df.isna().sum() / len(month_df)) * 100

    return resultDict

def mean_annual_Q(Q_df, varName='Q_cfs'):
    Q_df = drop_bad_years(Q_df, varName)
    Q_df = Q_df[varName]
    annual_mean = Q_df.resample("YE-SEP").mean()
    return annual_mean.mean()

def flag_seasonal(signatures, type):
    # flag seasonal gages
    Q = 'Q_cfs'
    flag = False
    for month in ['jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec']:
        if pd.isna(signatures[f'{type}_{month}_{Q}']):
            flag = True

    return flag

def mean_total_Q(Q_df, varName='Q_cfs'):
    Q_df = drop_bad_years(Q_df, varName)
    Q_df = Q_df[varName]
    annual_sum = Q_df.resample("YE-SEP").sum()
    annual_sum = annual_sum[(annual_sum != 0)] # removes years that aren't represented... annoying but necessary
    return annual_sum.mean()

def split_wet_dry(df, varName='Q_cfs'):
    # split the dataframe into wet years and dry years
    # wet years are greater than the mean
    results = []
    for year in df.index.year.unique()[1:-1]:
        # subset the data to the water year
        df_wy = df.loc[f'{year-1}-10-01':f'{year}-09-30'].copy()
        totalQ = df_wy[varName].sum()
        resultDict = {'year': year}
        resultDict['totalQ'] = totalQ
        results.append(resultDict)

    resultDf = pd.DataFrame().from_dict(results)
    resultDf['percentile'] = resultDf['totalQ'].rank(pct=True)

    # find wet and dry years
    wet_years = resultDf[resultDf['percentile'] >= 0.75]['year'].tolist()
    dry_years = resultDf[resultDf['percentile'] <= 0.25]['year'].tolist()

    df['water_year'] = df.index.year
    df['water_year'] += (df.index.month >= 10).astype(int)

    wet_df = df[df['water_year'].isin(wet_years)].copy()
    dry_df = df[df['water_year'].isin(dry_years)].copy()

    return wet_df, dry_df

def q95(Q_df, varName='Q_cfs'):
    Q_df = Q_df[varName]
    q95 = Q_df.quantile(0.95)
    return q95

def q5(Q_df, varName='Q_cfs'):
    Q_df = Q_df[varName]
    q5 = Q_df.quantile(0.05)
    return q5

def drop_bad_years(Q_df, varName='Q_cfs', t=328):
    # drop years with less than t days of data
    df = Q_df[varName]
    annual_counts = df.resample("YE-SEP").count()
    valid_years = annual_counts[annual_counts >= t].index.year.tolist()
    return Q_df[Q_df.index.year.isin(valid_years)]

signaturesList = [] # list to store the signatures from each basin
empties = []
for usgs, cdwr in zip(basinsWarea['usgs_id'], basinsWarea['cdwr_id']):
    # print(file)
    # df = pd.read_csv(os.path.join(basinsDir, file)) # read file
    # gage = os.path.splitext(file)[0]
    # usgs = '09131490'
    #cdwr = 'HURREDCO'

    signatures = {}
    signatures['usgs_id'] = usgs
    signatures['cdwr_id'] = cdwr

    # this try except clause stuff is irrelevant now, the csvs in gage_csvs2 just get used. but I left it in case I want to use it later
    try:
        all_df = pd.read_csv(os.path.join(basinsDir, f'{usgs}.csv')) # read file
        gage = usgs
        signatures['gage_used'] = usgs
        try:
            signatures['area'] = basinsWarea[basinsWarea['usgs_id'] == usgs]['area'].iloc[0]
            signatures['index'] = basinsWarea[basinsWarea['usgs_id'] == usgs]['index'].iloc[0]
            #signatures['perc_burn'] = basinsWarea[basinsWarea['usgs_id'] == usgs]['perc_burn'].iloc[0]
        except:
            signatures['area'] = basinsWarea[basinsWarea['cdwr_id'] == cdwr]['area'].iloc[0]
            signatures['index'] = basinsWarea[basinsWarea['cdwr_id'] == cdwr]['index'].iloc[0]
            #signatures['perc_burn'] = basinsWarea[basinsWarea['cdwr_id'] == cdwr]['perc_burn'].iloc[0]
    except Exception as e:
        #print(f'usgs {usgs} not found')
        try:
            all_df = pd.read_csv(os.path.join(basinsDir, f'{cdwr}.csv')) # read file
            gage = cdwr
            signatures['gage_used'] = cdwr
            try:
                signatures['area'] = basinsWarea[basinsWarea['cdwr_id'] == cdwr]['area'].iloc[0]
                signatures['index'] = basinsWarea[basinsWarea['cdwr_id'] == cdwr]['index'].iloc[0]
                #signatures['perc_burn'] = basinsWarea[basinsWarea['cdwr_id'] == cdwr]['perc_burn'].iloc[0]
            except:
                signatures['area'] = basinsWarea[basinsWarea['usgs_id'] == usgs]['area'].iloc[0]
                signatures['index'] = basinsWarea[basinsWarea['usgs_id'] == usgs]['index'].iloc[0]
                #signatures['perc_burn'] = basinsWarea[basinsWarea['usgs_id'] == usgs]['perc_burn'].iloc[0]
        except:
            print(f'no area {usgs, cdwr}')
            continue

    signatures['name'] = all_df['name'].iloc[0]

    if gage == 'SVCLYOCO':
        # SVCLYOCO is a special case with wildly inflated CFS in fall 1999
        all_df = all_df[~((all_df['datetime'] >= '1999-10-01') & (all_df['datetime'] <= '1999-12-31'))].copy()

    print(gage)
    if len(all_df) < 10:
        print(f'{gage} is empty')
        empties.append(gage)
        continue

    all_df.index = pd.to_datetime(all_df['datetime']) # read date as index
    all_df.index = all_df.index.tz_localize(None) # pandas assumes a timezone, tell it no
    #all_df = fill_missing_days(all_df)
    all_df.index.freq = 'D' # make the frequency daily (weird pandas thing)

    # convert to mm/day
    area = signatures['area']
    # conver to cms
    all_df['Q_cms'] = all_df['Q_cfs'] * 0.0283168
    # convert to cubic meters per day
    all_df['Q_cmd'] = all_df['Q_cms'] * 86400
    # convert cubic meters to square m per day
    all_df['Q_md'] = all_df['Q_cmd'] / area
    # convert cubic mm per day to depth mm per day
    all_df['Q_mmd'] = all_df['Q_md'] * 1000
    flows = ['Q_cfs', 'Q_mmd']

    # print(f'{gage}', e)
    # flows = ['Q_cfs']
    #all_df = drop_bad_years(all_df, varName='Q_mmd', t=328) # drop years with less than 328 days of data

    num_years = all_df.index.year.unique().size
    if num_years < 15:
        signatures['dropped'] = 'less than 15 years of data'
        continue

    #
    wet_df, dry_df = split_wet_dry(all_df, varName='Q_cfs')
    #
    # number of estimated rows
    for type, df in zip(['all', 'wet', 'dry'], [all_df, wet_df, dry_df]):

        df['water_year'] = df.index.year
        df['water_year'] += (df.index.month >= 10).astype(int)

        signatures[f'n_{type}_years'] = len(df['water_year'].unique())
        signatures[f'{type}_years'] = df['water_year'].unique().tolist()

        no_bad_years_df = drop_bad_years(df, varName='Q_cfs', t=328) # drop years with less than 328 days of data
        signatures[f'n_{type}_years_w90%data'] = len(no_bad_years_df['water_year'].unique())
        signatures[f'{type}_years_w90%data'] = no_bad_years_df['water_year'].unique().tolist()


        if gage.isdigit(): # this removes CDWR gages
            no_nas = df[pd.notna(df['Q_cfs'])].dropna()
            signatures[f'{type}_estimates'] = len(no_nas[no_nas['qc_code'].str.contains('e')]) # if the qc code has a e its an estimate
            signatures[f'{type}_estimate_percent'] = signatures[f'{type}_estimates'] / len(no_nas)
        else:
            signatures[f'{type}_estimates'] = np.nan

        signatures[f'{type}_min_date'] = df.index.min()
        signatures[f'{type}_max_date'] = df.index.max()
        signatures[f'{type}_missing_days'] = df['Q_cfs'].isna().sum()
        signatures[f'{type}_percent_missing'] = (signatures[f'{type}_missing_days'] / len(df)) * 100
        #print(len(signatures))

        # experiment with interpolation
        for flow in flows:
            df[flow] = df[flow].interpolate(method='linear', limit=7)

        # calculate monthly means
        month_means = mean_monthly_Q(df, type=type, varName='Q_cfs')
        signatures.update(month_means)

        month_means = sum_monthly_Q(df, type=type, varName='Q_mmd')
        signatures.update(month_means)

        # determine if it's seasonal
        flag = flag_seasonal(signatures, type)
        signatures[f'{type}_seasonal'] = flag

        # if its not seasonal, can calculate all these things
        if not flag:
            signatures[f'{type}_Q_ann_mm'] = mean_total_Q(df, varName='Q_mmd')

            for flow in flows:
                signatures[f'{type}_annual_mean_max_{flow}'] = Qmax(df, varName=flow)
                signatures[f'{type}_annual_mean_min_{flow}'] = Qmin(df, varName=flow)
                signatures[f'{type}_annual_mean_{flow}'] = mean_annual_Q(df, varName=flow)
                signatures[f'{type}_q95_{flow}'] = q95(df, varName=flow)
                signatures[f'{type}_q5_{flow}'] = q5(df, varName=flow)

            # these arent specific to a flow measurement (cfs or mmday)
            cum_quantiles = cumulative_Q_quantiles(df, type=type, varName='Q_cfs')
            signatures.update(cum_quantiles)
            percent_missing = missing_days_bymonth(df, type=type, varName='Q_cfs')
            signatures.update(percent_missing)
            signatures[f'{type}_monsoon_frac'] = Fmonsoon(df)

    # if it's not seasonal for the all type, calc flood freq
    if not signatures['all_seasonal']:
        for flow in flows:
            signatures[f'flood_freq_1.5_{flow}'] = calc_flood_freq(all_df, 1.5, varName=flow)

    # a few more things
    #signatures['hfd_mean_CY'] = signatures['hfd_mean'] - 92
    #P_da = xr.DataArray(df['pr'], coords=[df.index], dims=["date"]) # precip for runoff ratio calc. 'pr' is gridmet derived precip

    # append to list
    signaturesList.append(signatures)

signaturesDf = pd.DataFrame().from_dict(signaturesList)
signaturesDf.index = signaturesDf['gage_used']
signaturesDf['ws_area_sqkm'] = signaturesDf['area'] / 1e6  # convert to km2
signaturesDf.sort_values(by='index', ascending=True, inplace=True)

# save it
outcsv = r"C:\Users\C830645719\OneDrive - Colostate\flow_prediction\data\streamflow\hydro_signatures_wet_dry.csv"
signaturesDf.to_csv(outcsv, index=True, index_label='gage_used')

# fix the mm/d to be just mm total
df = pd.read_csv(r"C:\Users\C830645719\OneDrive - Colostate\flow_prediction\data\streamflow\dataset_20250623.csv")
df['all_jan_Q_mmd'] = df['all_jan_Q_mmd']*31
df['all_feb_Q_mmd'] = df['all_feb_Q_mmd']*28
df['all_mar_Q_mmd'] = df['all_mar_Q_mmd']*31
df['all_apr_Q_mmd'] = df['all_apr_Q_mmd']*30
df['all_may_Q_mmd'] = df['all_may_Q_mmd']*31
df['all_jun_Q_mmd'] = df['all_jun_Q_mmd']*30
df['all_jul_Q_mmd'] = df['all_jul_Q_mmd']*31
df['all_aug_Q_mmd'] = df['all_aug_Q_mmd']*31
df['all_sep_Q_mmd'] = df['all_sep_Q_mmd']*30
df['all_oct_Q_mmd'] = df['all_oct_Q_mmd']*31
df['all_nov_Q_mmd'] = df['all_nov_Q_mmd']*30
df['all_dec_Q_mmd'] = df['all_dec_Q_mmd']*31
df.to_csv(r"C:\Users\C830645719\OneDrive - Colostate\flow_prediction\data\streamflow\dataset_20250623_2.csv")



"""
Compute difference between all/wet/dry
"""
df = pd.read_csv(outcsv)
deltaCols = ['jan_Q_cfs', 'feb_Q_cfs', 'mar_Q_cfs', 'apr_Q_cfs', 'may_Q_cfs', 'jun_Q_cfs', 'jul_Q_cfs', 'aug_Q_cfs', 'sep_Q_cfs', 'oct_Q_cfs', 'nov_Q_cfs', 'dec_Q_cfs', 'jan_Q_mmd', 'feb_Q_mmd', 'mar_Q_mmd', 'apr_Q_mmd', 'may_Q_mmd', 'jun_Q_mmd', 'jul_Q_mmd', 'aug_Q_mmd', 'sep_Q_mmd', 'oct_Q_mmd', 'nov_Q_mmd', 'dec_Q_mmd', 'Q_ann_mm', 'annual_mean_max_Q_cfs', 'annual_mean_min_Q_cfs', 'annual_mean_Q_cfs', 'q95_Q_cfs', 'q5_Q_cfs', 'annual_mean_max_Q_mmd', 'annual_mean_min_Q_mmd', 'annual_mean_Q_mmd', 'q95_Q_mmd', 'q5_Q_mmd', 'mean_flowdate_0.1', 'mean_flowdate_0.2', 'mean_flowdate_0.3', 'mean_flowdate_0.4', 'mean_flowdate_0.5', 'mean_flowdate_0.6', 'mean_flowdate_0.7', 'mean_flowdate_0.8', 'mean_flowdate_0.9', 'perc_missing_jan', 'perc_missing_feb', 'perc_missing_mar', 'perc_missing_apr', 'perc_missing_may', 'perc_missing_jun', 'perc_missing_jul', 'perc_missing_aug', 'perc_missing_sep', 'perc_missing_oct', 'perc_missing_nov', 'perc_missing_dec']

# otherCols = set(df.columns.tolist()) - set(deltaCols)
# df2 = df[['gage_used', 'usgs_id', 'cdwr_id', 'area', 'index', 'name']]

for col in deltaCols:
    try:
        newCol = f'all-dry_{col}'
        df[newCol] = df[f'all_{col}'] - df[f'dry_{col}']
    except:
        df[newCol] = np.nan

    try:
        newCol = f'all-wet_{col}'
        df[newCol] = df[f'all_{col}'] - df[f'wet_{col}']
    except:
        df[newCol] = np.nan

    try:
        newCol = f'wet-dry_{col}'
        df[newCol] = df[f'wet_{col}'] - df[f'dry_{col}']
    except:
        df[newCol] = np.nan

outcsv = r"C:\Users\C830645719\OneDrive - Colostate\flow_prediction\data\streamflow\hydro_signatures_wet_dry_wdeltas.csv"
df.to_csv(outcsv, index=True, index_label='gage_used')
