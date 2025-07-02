import pandas as pd
import numpy as np

# # define working directory
wd = os.getcwd()

# load in stuff
df = pd.read_csv(r"data\model_watersheds.csv")
sweDf = pd.read_csv(r"data\streamflow\meanAPR1swe_wsheds.csv")
cfsDf = pd.read_csv(r"data\streamflow\hydro_signatures_wet_dry.csv")

# merge things, change some names
df = df.merge(sweDf[['mean', 'index']], on='index', how='left') # merge in the SWE data
df.rename({'mean': 'APR1SWE'}, axis=1, inplace=True) # rename the column to match)
df = df.merge(cfsDf[['index', 'all_annual_mean_Q_cfs', 'all_jan_Q_cfs', 'all_feb_Q_cfs', 'all_mar_Q_cfs', 'all_apr_Q_cfs', 'all_may_Q_cfs', 'all_jun_Q_cfs', 'all_jul_Q_cfs', 'all_aug_Q_cfs', 'all_sep_Q_cfs', 'all_oct_Q_cfs', 'all_nov_Q_cfs', 'all_dec_Q_cfs']], on='index', how='left') # merge in the CFS data
df.rename({'all_jan_Q_mmd': 'all_jan_Q_mm', 'all_feb_Q_mmd': 'all_feb_Q_mm', 'all_mar_Q_mmd': 'all_mar_Q_mm', 'all_apr_Q_mmd': 'all_apr_Q_mm', 'all_may_Q_mmd': 'all_may_Q_mm', 'all_jun_Q_mmd': 'all_jun_Q_mm', 'all_jul_Q_mmd': 'all_jul_Q_mm', 'all_aug_Q_mmd': 'all_aug_Q_mm', 'all_sep_Q_mmd': 'all_sep_Q_mm', 'all_oct_Q_mmd': 'all_oct_Q_mm', 'all_nov_Q_mmd': 'all_nov_Q_mm', 'all_dec_Q_mmd': 'all_dec_Q_mm','all_Q_ann_mm':'all_ann_Q_mm', 'all_annual_mean_Q_cfs': 'all_ann_Q_cfs'}, axis=1, inplace=True) # rename the columns to mm

# unit corrections and naming convention changes
df['DRNAREA'] = df['ws_area_sqkm'] * 0.386102 # square kilometers to square miles
df['ALTRELIEF'] = df['relief'] * 3.28084 # meters to feet
df['APR1SWE'] = df['APR1SWE'] * 0.0393701 # mm to inches
df['ALTOUTELEV'] = df['min_elevation'] * 3.28084 # meters to feet
df['ALTBSLDEM'] = df['mean_slope_mm'] * 100
df['LATCENT'] = df['ws_centroid_lat']
df['LONGCENT'] = df['ws_centroid_lon']
df['PRECIP'] = df['prcp_avg_sum'] * 0.0393701 # mm to inches
df['ALTI6H100Y'] = df['precip_6hr_100yr'] * 0.001 # meters to feet
df['ALTELEVMAX'] = df['max_elevation'] * 3.28084 # meters to feet
df['AUGAVEPRE'] = df['prcp_aug'] * 0.0393701 # mm to inches

# columns we care about
cols = ['gage_used', 'index', 'all_jan_Q_mm', 'Q1_mm', 'all_feb_Q_mm', 'Q2_mm', 'all_mar_Q_mm', 'Q3_mm', 'all_apr_Q_mm', 'Q4_mm', 'all_may_Q_mm', 'Q5_mm', 'all_jun_Q_mm', 'Q6_mm', 'all_jul_Q_mm', 'Q7_mm', 'all_aug_Q_mm', 'Q8_mm', 'all_sep_Q_mm', 'Q9_mm', 'all_oct_Q_mm', 'Q10_mm', 'all_nov_Q_mm', 'Q11_mm', 'all_dec_Q_mm', 'Q12_mm', 'all_ann_Q_mm', 'QA_mm', 'all_jan_Q_cfs', 'Q1_cfs', 'all_feb_Q_cfs', 'Q2_cfs', 'all_mar_Q_cfs', 'Q3_cfs', 'all_apr_Q_cfs', 'Q4_cfs', 'all_may_Q_cfs', 'Q5_cfs', 'all_jun_Q_cfs', 'Q6_cfs', 'all_jul_Q_cfs', 'Q7_cfs', 'all_aug_Q_cfs', 'Q8_cfs', 'all_sep_Q_cfs', 'Q9_cfs', 'all_oct_Q_cfs', 'Q10_cfs', 'all_nov_Q_cfs', 'Q11_cfs', 'all_dec_Q_cfs', 'Q12_cfs', 'all_ann_Q_cfs', 'QA_cfs', 'usgs_hydro_region', 'DRNAREA', 'ALTRELIEF', 'APR1SWE', 'ALTOUTELEV','ALTBSLDEM','LATCENT', 'LONGCENT', 'PRECIP', 'ALTI6H100Y', 'ALTELEVMAX', 'AUGAVEPRE', 'ws_area_sqkm']

# equations for "east slope headwaters"
dfE = df[df['usgs_hydro_region'] == 'EastSlopeHeadwaters'].copy()
dfE['QA_cfs'] = (10**-0.704 * dfE.DRNAREA**0.793 * 10**(0.00518*dfE.ALTBSLDEM + 0.0000642*dfE.ALTRELIEF + 0.0459*dfE.APR1SWE))
dfE['Q1_cfs'] = 10**-1.61 * dfE.DRNAREA**0.773 * 10**(0.000143*dfE.ALTRELIEF + 0.0440*dfE.APR1SWE)
dfE['Q2_cfs'] = 10**1.21 * dfE.DRNAREA**0.847 * 10**(-0.0679*dfE.LATCENT + 0.000103*dfE.ALTRELIEF + 0.0364*dfE.APR1SWE)
dfE['Q3_cfs'] = 10**-0.9 * dfE.DRNAREA**0.906 * 10**(0.0000646*dfE.ALTRELIEF + 0.0166*dfE.PRECIP - 0.288*dfE.ALTI6H100Y)
dfE['Q4_cfs'] = 10**0.224 * dfE.DRNAREA**0.960 * 10**(-0.0000664*dfE.ALTOUTELEV + 0.0215*dfE.PRECIP - 0.265*dfE.ALTI6H100Y)
dfE['Q5_cfs'] = 10**-0.259 * dfE.DRNAREA**0.933 * 10**(0.0111*dfE.PRECIP + 0.0332*dfE.APR1SWE)
dfE['Q6_cfs'] = 10**16.4 * dfE.DRNAREA**0.690 * 10**(0.156*dfE.LONGCENT + 0.000127*dfE.ALTRELIEF + 0.0651*dfE.APR1SWE)
dfE['Q7_cfs'] = 10**-2.06 * dfE.DRNAREA**0.780 * 10**(0.000118*dfE.ALTELEVMAX + 0.0130*dfE.ALTBSLDEM + 0.0517*dfE.APR1SWE)
dfE['Q8_cfs'] = 10**-2.21 * dfE.DRNAREA**0.807 * 10**(0.000101*dfE.ALTELEVMAX + 0.00862*dfE.ALTBSLDEM + 0.0217*dfE.PRECIP)
dfE['Q9_cfs'] = 10**-1.25 * dfE.DRNAREA**0.707 * 10**(0.00750*dfE.ALTBSLDEM + 0.00013*dfE.ALTRELIEF + 0.0374*dfE.APR1SWE)
dfE['Q10_cfs'] = 10**-1.06 * dfE.DRNAREA**0.700 * 10**(0.000144*dfE.ALTRELIEF + 0.0333*dfE.APR1SWE)
dfE['Q11_cfs'] = 10**-1.28 * dfE.DRNAREA**0.749 * 10**(0.000140*dfE.ALTRELIEF + 0.0363*dfE.APR1SWE)
dfE['Q12_cfs'] = 10**-1.42 * dfE.DRNAREA**0.763 * 10**(0.000135*dfE.ALTRELIEF + 0.0383*dfE.APR1SWE)

dfE['QA_mm'] = dfE['QA_cfs'] * (0.0283168 * 86400) / (dfE.ws_area_sqkm*1000000)  * 1000 * 365 # convert from CFS to total mm
dfE['Q1_mm'] = dfE['Q1_cfs'] * (0.0283168 * 86400) / (dfE.ws_area_sqkm*1000000)  * 1000 * 31 # convert from CFS to total mm
dfE['Q2_mm'] = dfE['Q2_cfs'] * (0.0283168 * 86400) / (dfE.ws_area_sqkm*1000000)  * 1000 * 28 # convert from CFS to total mm
dfE['Q3_mm'] = dfE['Q3_cfs'] * (0.0283168 * 86400) / (dfE.ws_area_sqkm*1000000)  * 1000 * 31 # convert from CFS to total mm
dfE['Q4_mm'] = dfE['Q4_cfs'] * (0.0283168 * 86400) / (dfE.ws_area_sqkm*1000000)  * 1000 * 30 # convert from CFS to total mm
dfE['Q5_mm'] = dfE['Q5_cfs'] * (0.0283168 * 86400) / (dfE.ws_area_sqkm*1000000)  * 1000 * 31 # convert from CFS to total mm
dfE['Q6_mm'] = dfE['Q6_cfs'] * (0.0283168 * 86400) / (dfE.ws_area_sqkm*1000000)  * 1000 * 30 # convert from CFS to total mm
dfE['Q7_mm'] = dfE['Q7_cfs'] * (0.0283168 * 86400) / (dfE.ws_area_sqkm*1000000)  * 1000 * 31 # convert from CFS to total mm
dfE['Q8_mm'] = dfE['Q8_cfs'] * (0.0283168 * 86400) / (dfE.ws_area_sqkm*1000000)  * 1000 * 31 # convert from CFS to total mm
dfE['Q9_mm'] = dfE['Q9_cfs'] * (0.0283168 * 86400) / (dfE.ws_area_sqkm*1000000)  * 1000 * 30 # convert from CFS to total mm
dfE['Q10_mm'] = dfE['Q10_cfs'] * (0.0283168 * 86400) / (dfE.ws_area_sqkm*1000000)  * 1000 * 31 # convert from CFS to total mm
dfE['Q11_mm'] = dfE['Q11_cfs'] * (0.0283168 * 86400) / (dfE.ws_area_sqkm*1000000)  * 1000 * 30 # convert from CFS to total mm
dfE['Q12_mm'] = dfE['Q12_cfs'] * (0.0283168 * 86400) / (dfE.ws_area_sqkm*1000000)  * 1000 * 31 # convert from CFS to total mm

dfE = dfE[cols].copy() # keep only the columns we care about

dfE.to_csv(r"data\streamflow\east_slope_headwaters.csv", index=False)

# equation for "Green River"
dfG = df[df['usgs_hydro_region'] == 'Green'].copy()
dfG['QA_cfs'] = 10**21.5 * dfG.DRNAREA**0.753 * 10**(0.241*dfG.LONGCENT + 0.000305*dfG.ALTELEVMAX + 0.587*dfG.ALTI6H100Y)
dfG['Q1_cfs'] = 10**13.4 * dfG.DRNAREA**0.792 * 10**(0.164*dfG.LONGCENT + 0.000273*dfG.ALTELEVMAX + 0.0295*dfG.ALTBSLDEM)
dfG['Q2_cfs'] = 10**13.9 * dfG.DRNAREA**0.843 * 10**(0.165*dfG.LONGCENT + 0.000229*dfG.ALTELEVMAX + 0.0277*dfG.ALTBSLDEM)
dfG['Q3_cfs'] = np.nan # 10**23.5 * dfG.DRNAREA**1.22 * 10**(.0237*dfG.LONGCENT + 0.504+dfG.AUGAVEPRE)
dfG['Q4_cfs'] = 10**20.1 * dfG.DRNAREA**1.23 * 10**(0.534*dfG.LATCENT + 0.407*dfG.LONGCENT + 0.792*dfG.AUGAVEPRE)
dfG['Q5_cfs'] = 10**12.7 * dfG.DRNAREA**1.21 * 10**(0.805*dfG.LATCENT +  0.446*dfG.LONGCENT + 1.44*dfG.AUGAVEPRE)
dfG['Q6_cfs'] = 10**-13.7 * dfG.DRNAREA**0.885 * 10**(0.240*dfG.LATCENT + 0.000276*dfG.ALTELEVMAX + 0.0414*dfG.PRECIP)
dfG['Q7_cfs'] = 10**-2.44 * dfG.DRNAREA**0.938 * 10**(0.000109*dfG.ALTRELIEF + 0.573*dfG.AUGAVEPRE + 0.0603*dfG.APR1SWE)
dfG['Q8_cfs'] =  10**-1.90 * dfG.DRNAREA**1.07 * 10**(1.05*dfG.AUGAVEPRE + 0.0419*dfG.APR1SWE - 0.638*dfG.ALTI6H100Y)
dfG['Q9_cfs'] = 10**-3.44 * dfG.DRNAREA**1.13 * 10**(0.0228*dfG.ALTBSLDEM + 0.732*dfG.AUGAVEPRE + 0.0382*dfG.APR1SWE)
dfG['Q10_cfs'] = 10**-4.36 * dfG.DRNAREA**0.877 * 10**(0.000242*dfG.ALTELEVMAX + 0.0329*dfG.ALTBSLDEM+0.0293*dfG.APR1SWE)
dfG['Q11_cfs'] = 10**-3.66 * dfG.DRNAREA**0.903 * 10**(0.000182*dfG.ALTELEVMAX + 0.0284*dfG.ALTBSLDEM + 0.0276*dfG.APR1SWE)
dfG['Q12_cfs'] = 10**-1.31 * dfG.DRNAREA**1.12 * 10**(0.558*dfG.AUGAVEPRE + 0.0384*dfG.APR1SWE - 0.591*dfG.ALTI6H100Y)

dfG['QA_mm'] = dfG['QA_cfs'] * (0.0283168 * 86400) / (dfG.ws_area_sqkm*1000000)  * 1000 * 365 # convert from CFS to total mm
dfG['Q1_mm'] = dfG['Q1_cfs'] * (0.0283168 * 86400) / (dfG.ws_area_sqkm*1000000)  * 1000 * 31 # convert from CFS to total mm
dfG['Q2_mm'] = dfG['Q2_cfs'] * (0.0283168 * 86400) / (dfG.ws_area_sqkm*1000000)  * 1000 * 28 # convert from CFS to total mm
dfG['Q3_mm'] = dfG['Q3_cfs'] * (0.0283168 * 86400) / (dfG.ws_area_sqkm*1000000)  * 1000 * 31 # convert from CFS to total mm
dfG['Q4_mm'] = dfG['Q4_cfs'] * (0.0283168 * 86400) / (dfG.ws_area_sqkm*1000000)  * 1000 * 30 # convert from CFS to total mm
dfG['Q5_mm'] = dfG['Q5_cfs'] * (0.0283168 * 86400) / (dfG.ws_area_sqkm*1000000)  * 1000 * 31 # convert from CFS to total mm
dfG['Q6_mm'] = dfG['Q6_cfs'] * (0.0283168 * 86400) / (dfG.ws_area_sqkm*1000000)  * 1000 * 30 # convert from CFS to total mm
dfG['Q7_mm'] = dfG['Q7_cfs'] * (0.0283168 * 86400) / (dfG.ws_area_sqkm*1000000)  * 1000 * 31 # convert from CFS to total mm
dfG['Q8_mm'] = dfG['Q8_cfs'] * (0.0283168 * 86400) / (dfG.ws_area_sqkm*1000000)  * 1000 * 31 # convert from CFS to total mm
dfG['Q9_mm'] = dfG['Q9_cfs'] * (0.0283168 * 86400) / (dfG.ws_area_sqkm*1000000)  * 1000 * 30 # convert from CFS to total mm
dfG['Q10_mm'] = dfG['Q10_cfs'] * (0.0283168 * 86400) / (dfG.ws_area_sqkm*1000000)  * 1000 * 31 # convert from CFS to total mm
dfG['Q11_mm'] = dfG['Q11_cfs'] * (0.0283168 * 86400) / (dfG.ws_area_sqkm*1000000)  * 1000 * 30 # convert from CFS to total mm
dfG['Q12_mm'] = dfG['Q12_cfs'] * (0.0283168 * 86400) / (dfG.ws_area_sqkm*1000000)  * 1000 * 31 # convert from CFS to total mm

dfG = dfG[cols].copy() # keep only the columns we care about

dfR = df[df['usgs_hydro_region'] == 'RioGrande'].copy()
dfR['QA_cfs'] = 10**21.1 * dfR.DRNAREA**0.839 * 10**(0.129*dfR.LATCENT + 0.250*dfR.LONGCENT + 0.0783*dfR.APR1SWE)
dfR['Q1_cfs'] = 10**58.1 * dfR.DRNAREA**0.817 * 10**(0.544*dfR.LONGCENT + 0.0734*dfR.APR1SWE - 0.579*dfR.ALTI6H100Y)
dfR['Q2_cfs'] = 10**50.9 * dfR.DRNAREA**0.842 * 10**(0.478*dfR.LONGCENT + 0.0647 * dfR.APR1SWE - 0.501*dfR.ALTI6H100Y)
dfR['Q3_cfs'] = 10**-0.648 * dfR.DRNAREA**0.845 * 10**(0.0255*dfR.APR1SWE)
dfR['Q4_cfs'] = np.nan # 10**1.83 * dfR.DRNAREA**0.705 * 10**(-0.000244*dfR.ALTOUTELEV - 0.00544*dfR.ALTBSLDEM + 0.0772*dfR.APR1SWE)
dfR['Q5_cfs'] = 10**20.6 * dfR.DRNAREA**0.878 * 10**(0.2*dfR.LONGCENT + 0.0000655*dfR.ALTRELIEF + 0.0847*dfR.APR1SWE)
dfR['Q6_cfs'] = 10**25.9 * dfR.DRNAREA**0.789 * 10**(0.268*dfR.LONGCENT + 0.000181*dfR.ALTELEVMAX + 0.0926*dfR.APR1SWE)
dfR['Q7_cfs'] = 10**0.160 * dfR.DRNAREA**0.409 * 10**(0.00022*dfR.ALTRELIEF + 0.0946*dfR.APR1SWE - 0.436*dfR.ALTI6H100Y)
dfR['Q8_cfs'] = 10**0.274 * dfR.DRNAREA**0.398 * 10**(0.000187*dfR.ALTRELIEF + 0.067*dfR.APR1SWE - 0.393*dfR.ALTI6H100Y)
dfR['Q9_cfs'] = 10**0.207 * dfR.DRNAREA**0.424 * 10**(0.000189*dfR.ALTRELIEF + 0.068*dfR.APR1SWE - 0.441*dfR.ALTI6H100Y)
dfR['Q10_cfs'] = 10**28.2 * dfR.DRNAREA**0.815 * 10**(0.334*dfR.LATCENT + 0.393*dfR.LONGCENT + 0.0788*dfR.APR1SWE)
dfR['Q11_cfs'] = 10**49.9 * dfR.DRNAREA**0.739 * 10**(0.465*dfR.LONGCENT + 0.0814*dfR.APR1SWE - 0.543*dfR.ALTI6H100Y)
dfR['Q12_cfs'] = 10**60.6 * dfR.DRNAREA**0.798 * 10**(0.567*dfR.LONGCENT + 0.0785*dfR.APR1SWE - 0.590*dfR.ALTI6H100Y)

# do it for mm
dfR['QA_mm'] = dfR['QA_cfs'] * (0.0283168 * 86400) / (dfR.ws_area_sqkm*1000000)  * 1000 * 365 # convert from CFS to total mm
dfR['Q1_mm'] = dfR['Q1_cfs'] * (0.0283168 * 86400) / (dfR.ws_area_sqkm*1000000)  * 1000 * 31 # convert from CFS to total mm
dfR['Q2_mm'] = dfR['Q2_cfs'] * (0.0283168 * 86400) / (dfR.ws_area_sqkm*1000000)  * 1000 * 28 # convert from CFS to total mm
dfR['Q3_mm'] = dfR['Q3_cfs'] * (0.0283168 * 86400) / (dfR.ws_area_sqkm*1000000)  * 1000 * 31 # convert from CFS to total mm
dfR['Q4_mm'] = dfR['Q4_cfs'] * (0.0283168 * 86400) / (dfR.ws_area_sqkm*1000000)  * 1000 * 30 # convert from CFS to total mm
dfR['Q5_mm'] = dfR['Q5_cfs'] * (0.0283168 * 86400) / (dfR.ws_area_sqkm*1000000)  * 1000 * 31 # convert from CFS to total mm
dfR['Q6_mm'] = dfR['Q6_cfs'] * (0.0283168 * 86400) / (dfR.ws_area_sqkm*1000000)  * 1000 * 30 # convert from CFS to total mm
dfR['Q7_mm'] = dfR['Q7_cfs'] * (0.0283168 * 86400) / (dfR.ws_area_sqkm*1000000)  * 1000 * 31 # convert from CFS to total mm
dfR['Q8_mm'] = dfR['Q8_cfs'] * (0.0283168 * 86400) / (dfR.ws_area_sqkm*1000000)  * 1000 * 31 # convert from CFS to total mm
dfR['Q9_mm'] = dfR['Q9_cfs'] * (0.0283168 * 86400) / (dfR.ws_area_sqkm*1000000)  * 1000 * 30 # convert from CFS to total mm
dfR['Q10_mm'] = dfR['Q10_cfs'] * (0.0283168 * 86400) / (dfR.ws_area_sqkm*1000000)  * 1000 * 31 # convert from CFS to total mm
dfR['Q11_mm'] = dfR['Q11_cfs'] * (0.0283168 * 86400) / (dfR.ws_area_sqkm*1000000)  * 1000 * 30 # convert from CFS to total mm
dfR['Q12_mm'] = dfR['Q12_cfs'] * (0.0283168 * 86400) / (dfR.ws_area_sqkm*1000000)  * 1000 * 31 # convert from CFS to total mm
dfR = dfR[cols].copy() # keep only the columns we care about

dfS = df[df['usgs_hydro_region'] == 'SanJuanDolores'].copy()
dfS['QA_cfs'] = 10**39.5 * dfS.DRNAREA**0.631 * 10**(0.383*dfS.LONGCENT + 0.000175*dfS.ALTELEVMAX)
dfS['Q1_cfs'] = 10**42.0 * dfS.DRNAREA**0.811 * 10**(0.407*dfS.LONGCENT + 0.0162*dfS.ALTBSLDEM + 0.000117*dfS.ALTRELIEF)
dfS['Q2_cfs'] = 10**33.5 * dfS.DRNAREA**0.834 * 10**(0.324*dfS.LONGCENT + 0.0113*dfS.ALTBSLDEM + 0.000083*dfS.ALTRELIEF)
dfS['Q3_cfs'] = 10**27.7 * dfS.DRNAREA**0.712 * 10**(0.270*dfS.LONGCENT + 0.000106*dfS.ALTELEVMAX)
dfS['Q4_cfs'] = 10**14.4 * dfS.DRNAREA**0.701 * 10**(0.171*dfS.LONGCENT + 0.000200*dfS.ALTELEVMAX + 0.770*dfS.ALTI6H100Y)
dfS['Q5_cfs'] = 10**29.7 * dfS.DRNAREA**0.561 * 10**(0.309*dfS.LONGCENT + 0.000261*dfS.ALTELEVMAX + 0.502*dfS.ALTI6H100Y)
dfS['Q6_cfs'] = 10**52.3 * dfS.DRNAREA**0.483 * 10**(0.509*dfS.LONGCENT + 0.000289*dfS.ALTELEVMAX)
dfS['Q7_cfs'] = 10**43.8 * dfS.DRNAREA**0.964 * 10**(0.419*dfS.LONGCENT + 0.0322*dfS.ALTBSLDEM)
dfS['Q8_cfs'] = 10**36.7 * dfS.DRNAREA**1.04 * 10**(0.353*dfS.LONGCENT + 0.0233*dfS.ALTBSLDEM)
dfS['Q9_cfs'] = 10**44.4 * dfS.DRNAREA**1.03 * 10**(0.425*dfS.LONGCENT + 0.0211*dfS.ALTBSLDEM)
dfS['Q10_cfs'] = 10**42.1 * dfS.DRNAREA**0.902 * 10**(0.401*dfS.LONGCENT + 0.0195*dfS.ALTBSLDEM)
dfS['Q11_cfs'] = 10**44.7 * dfS.DRNAREA**0.719 * 10**(0.427*dfS.LONGCENT + 0.0140*dfS.ALTBSLDEM + 0.000110*dfS.ALTRELIEF)
dfS['Q12_cfs'] = 10**47.7 * dfS.DRNAREA**0.745 * 10**(0.458*dfS.LONGCENT + 0.0147*dfS.ALTBSLDEM + 0.000129*dfS.ALTRELIEF)

dfS['QA_mm'] = dfS['QA_cfs'] * (0.0283168 * 86400) / (dfS.ws_area_sqkm*1000000)  * 1000 * 365 # convert from CFS to total mm
dfS['Q1_mm'] = dfS['Q1_cfs'] * (0.0283168 * 86400) / (dfS.ws_area_sqkm*1000000)  * 1000 * 31 # convert from CFS to total mm
dfS['Q2_mm'] = dfS['Q2_cfs'] * (0.0283168 * 86400) / (dfS.ws_area_sqkm*1000000)  * 1000 * 28 # convert from CFS to total mm
dfS['Q3_mm'] = dfS['Q3_cfs'] * (0.0283168 * 86400) / (dfS.ws_area_sqkm*1000000)  * 1000 * 31 # convert from CFS to total mm
dfS['Q4_mm'] = dfS['Q4_cfs'] * (0.0283168 * 86400) / (dfS.ws_area_sqkm*1000000)  * 1000 * 30 # convert from CFS to total mm
dfS['Q5_mm'] = dfS['Q5_cfs'] * (0.0283168 * 86400) / (dfS.ws_area_sqkm*1000000)  * 1000 * 31 # convert from CFS to total mm
dfS['Q6_mm'] = dfS['Q6_cfs'] * (0.0283168 * 86400) / (dfS.ws_area_sqkm*1000000)  * 1000 * 30 # convert from CFS to total mm
dfS['Q7_mm'] = dfS['Q7_cfs'] * (0.0283168 * 86400) / (dfS.ws_area_sqkm*1000000)  * 1000 * 31 # convert from CFS to total mm
dfS['Q8_mm'] = dfS['Q8_cfs'] * (0.0283168 * 86400) / (dfS.ws_area_sqkm*1000000)  * 1000 * 31 # convert from CFS to total mm
dfS['Q9_mm'] = dfS['Q9_cfs'] * (0.0283168 * 86400) / (dfS.ws_area_sqkm*1000000)  * 1000 * 30 # convert from CFS to total mm
dfS['Q10_mm'] = dfS['Q10_cfs'] * (0.0283168 * 86400) / (dfS.ws_area_sqkm*1000000)  * 1000 * 31 # convert from CFS to total mm
dfS['Q11_mm'] = dfS['Q11_cfs'] * (0.0283168 * 86400) / (dfS.ws_area_sqkm*1000000)  * 1000 * 30 # convert from CFS to total mm
dfS['Q12_mm'] = dfS['Q12_cfs'] * (0.0283168 * 86400) / (dfS.ws_area_sqkm*1000000)  * 1000 * 31 # convert from CFS to total mm

dfS = dfS[cols].copy() # keep only the columns we care about

# combine all the dataframes
dfAll = pd.concat([dfE, dfG, dfR, dfS], ignore_index=True)
dfAll.to_csv(r"data\streamflow\USGSvsFlow25.csv", index=False)

# calculate NSE for each month and stat
def calculate_nse(df, month_numeric, month_name, flow):
    observed = df[f'all_{month_name}_Q_{flow}']
    predicted = df[f'Q{month_numeric}_{flow}']
    nse = 1 - (np.sum((observed - predicted) ** 2) / np.sum((observed - np.mean(observed)) ** 2))
    return nse

# calculate percent bias for each month and stat
def calculate_bias(df, month_numeric, month_name, flow):
    observed = df[f'all_{month_name}_Q_{flow}']
    predicted = df[f'Q{month_numeric}_{flow}']
    bias = np.mean(predicted - observed) / np.mean(observed) * 100
    return bias

# calculate r squared for each month and stat
def calculate_r_squared(df, month_numeric, month_name, flow):
    observed = df[f'all_{month_name}_Q_{flow}']
    predicted = df[f'Q{month_numeric}_{flow}']
    correlation_matrix = np.corrcoef(observed, predicted)
    r_squared = correlation_matrix[0, 1] ** 2
    return r_squared

results = []
for df, region in zip([dfAll, dfE, dfG, dfR, dfS], ['all', 'east_slope_headwaters', 'green_river', 'rio_grande', 'san_juan_dolores']):
    for flow in ['cfs', 'mm']:
        for numeric, name in zip(['A', 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12], ['ann', 'jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec']):
            resultsDict = {'flow': flow}
            resultsDict['month'] = name
            resultsDict['region'] = region
            resultsDict['nse'] = calculate_nse(df, numeric, name, flow)
            resultsDict['bias'] = calculate_bias(df, numeric, name, flow)
            #resultsDict['r_squared'] = calculate_r_squared(dfAll, numeric, name, flow)
            results.append(resultsDict)

resultsDf = pd.DataFrame.from_dict(results)
resultsDf.to_csv(r"data\streamflow\USGSvsFlow25_nse.csv", index=False)
