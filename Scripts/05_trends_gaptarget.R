## PROJECT:  Pump up the jam
## AUTHOR:   A.Chafetz, T.Essam | USAID
## LICENSE:  MIT
## PURPOSE:  trends in HFR reporting against gap targets
## DATE:     2020-03-27
## UPDATED:  2020-03-28


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(vroom)


# GLOBAL VARIABLES --------------------------------------------------------

  out_folder <- "Dataout"
  
  threshold_completeness <- .8

# IMPORT ------------------------------------------------------------------

  #load joint HFR + DATIM dataset 
    df_completeness_viz <- list.files(out_folder, "HFR_Completeness_[[:digit:]]+\\.csv", full.names = TRUE) %>% 
      vroom()
  
    df_wks <- list.files(out_folder, "HFR_DATIM_FY20Q1_AggWks_[[:digit:]]+\\.csv", full.names = TRUE) %>% 
      vroom(col_types = c(.default = "c", hfr_results = "d", mer_targets = "d"))  
    

# IDENTIFY WHICH OU X IM TO KEEP ------------------------------------------

  df_thres_met <- df_completeness_viz %>% 
      filter(site_type == "All",
             completeness >= threshold_completeness)

  df_wks_comp <- df_wks %>% 
    semi_join(df_thres_met, by = c("operatingunit", "indicator"))

# AGGREGATE HFR + MER TARGETS ---------------------------------------------

  #annual targets are repeated each period -> keep just one pd when summing
    df_wks_comp_targets <- df_wks_comp %>% 
      filter(date == "2019-09-30") %>%
      group_by(operatingunit, indicator) %>% 
      summarise_at(vars(mer_targets), sum, na.rm = TRUE) %>% 
      ungroup()
      
  #aggregate HFR results by period
    df_wks_comp_results <- df_wks_comp %>% 
      group_by(operatingunit, indicator, hfr_pd, date) %>% 
      summarise_at(vars(hfr_results), sum, na.rm = TRUE) %>% 
      ungroup()

# CALC PERIOD GAP TARGET --------------------------------------------------

  #merge + gap target
    df_wks_comp_gap <- df_wks_comp_results %>% 
      left_join(df_wks_comp_targets) %>% 
      mutate(gap_target = case_when(indicator %in% c("TX_CURR", "TX_MMD") ~ mer_targets,
                                    TRUE ~ round(mer_targets/52, 0)))
    

# EXPORT ------------------------------------------------------------------

  write_csv(df_wks_comp_gap, file.path(out_folder, 
                                       paste0("HFR_OU_Wk_GapTarget_CompleteOnly_", format(Sys.Date(), "%Y%m%d"), ".csv")),
            na = "")  

