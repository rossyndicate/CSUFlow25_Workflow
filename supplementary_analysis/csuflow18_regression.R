library(tidyverse)
library(sf)
library(terra)
library(hydroGOF)     # evaluation metrics (NSE, PBIAS, RMSE)
library(caret)        # model training and cross-validation
library(DT)
library(slickR)
library(broom)
library(MuMIn)

new_vars <- st_read("data/model_watersheds.shp") %>%
  .[1:3,] %>%
  csuflow18_var_grabber(watersheds = .) 

flow <- read_csv("data/streamflow/hydro_signatures_wet_dry.csv") %>%
  select(
    index,
    all_jan_Q_mmd, all_feb_Q_mmd, all_mar_Q_mmd, all_apr_Q_mmd, all_may_Q_mmd, all_jun_Q_mmd,
    all_jul_Q_mmd, all_aug_Q_mmd, all_sep_Q_mmd, all_oct_Q_mmd, all_nov_Q_mmd, all_dec_Q_mmd,
    all_Q_ann_mm,
    all_annual_mean_max_Q_mmd, all_annual_mean_min_Q_mmd,
    all_annual_mean_Q_mmd, all_q95_Q_mmd, all_q5_Q_mmd,
    all_mean_flowdate_0.1, all_mean_flowdate_0.2, all_mean_flowdate_0.3, all_mean_flowdate_0.4,
    all_mean_flowdate_0.5, all_mean_flowdate_0.6, all_mean_flowdate_0.7, all_mean_flowdate_0.8,
    all_mean_flowdate_0.9, all_monsoon_frac,
    flood_freq_1.5_Q_mmd
  )

predictors <- read_csv("data/watersheds_with_vars.csv") %>%
  select(-c(cdwr_id, usgs_id, comid)) %>%
  left_join(new_vars %>% st_drop_geometry() %>% select(index, Dom_Aspect, Mean_Slope, Elevation, Hyd_Reg, Geo_Group), by = "index") 

dataset <- flow %>%
  inner_join(predictors, by = "index") %>%
  select(index, gage_used, cdwr_id, usgs_id, comid, everything()) %>%
  filter(!gage_used %in% c("9358550", "LUARMOCO")) %>%
  select(gage_used,
         mean_monthly.mm_1 = all_jan_Q_mmd,
         mean_monthly.mm_2 = all_feb_Q_mmd,
         mean_monthly.mm_3 = all_mar_Q_mmd,
         mean_monthly.mm_4 = all_apr_Q_mmd,
         mean_monthly.mm_5 = all_may_Q_mmd,
         mean_monthly.mm_6 = all_jun_Q_mmd,
         mean_monthly.mm_7 = all_jul_Q_mmd,
         mean_monthly.mm_8 = all_aug_Q_mmd,
         mean_monthly.mm_9 = all_sep_Q_mmd,
         mean_monthly.mm_10 = all_oct_Q_mmd,
         mean_monthly.mm_11 = all_nov_Q_mmd,
         mean_monthly.mm_12 = all_dec_Q_mmd,
         mean_annual.mm = all_Q_ann_mm,
         Hyd_Reg = Hyd_Reg,
         P = prcp_avg_sum,
         PET = avg_tot_pet,
         SP = mean_sp_2001_2020,
         Mean_Slope = Mean_Slope,
         Dom_Aspect = Dom_Aspect,
         Geo_Group = Geo_Group,
         Elevation = Elevation,
         Area_sq.km = ws_area_sqkm) %>%
  filter(Dom_Aspect != "N")

metrics <- c("Jan", "Feb", "Mar", "Apr", 
             "May", "Jun", "Jul", "Aug", "Sep",
             "Oct", "Nov", "Dec", "Ann")

mod_df <- vector("list", length = length(metrics))
obs_pred_list <- vector("list", length = length(metrics))

for(i in 1:length(metrics)){
  
  filtered_dataset <- dataset %>% 
    select(1+i, 15:length(dataset)) %>%
    na.omit()
  
  original_name <- names(filtered_dataset)[1]
  
  filtered_dataset <- filtered_dataset %>%
    mutate(!!sym(original_name) := !!sym(original_name))
  
  obs <- filtered_dataset %>% select(1) %>% pull()
  
  model <- readRDS(paste0("data/csuflow18/models/Model", metrics[i], ".RDS"))
  
  pred <- predict(model, newdata = filtered_dataset) 
  pred <- pred^2
  
  obs_pred_list[[i]] <- data.frame(flow_stat = metrics[i],
                                   model_type = "CSUFlow18",
                                   obs = as.numeric(obs),
                                   pred = as.numeric(pred))
  
  
  predictions <- data.frame(flow_stat = metrics[i],
                            nobs_mod = sum(!is.na(pred)),
                            model_type = "CSUFlow18",
                            NSE = hydroGOF::NSE(pred, obs),
                            PBIAS = hydroGOF::pbias(pred, obs),
                            RMSE = hydroGOF::rmse(pred, obs),
                            R2 = cor(obs, pred)^2) 
  
  mod_df[[i]] <- predictions #bind_rows(train, cv)
}

new <- bind_rows(mod_df) %>%
  write_csv("data/csuflow18_model_performance.csv")

new_obs <- bind_rows(obs_pred_list)  %>%
  write_csv("data/csuflow18_obs_predict.csv")

ggplot(new_obs) + geom_point(aes(x = obs, y = pred)) + facet_wrap(~flow_stat)
