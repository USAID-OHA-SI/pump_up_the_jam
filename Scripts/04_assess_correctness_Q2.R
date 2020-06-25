## PROJECT:  Pump up the jam
## AUTHOR:   tessam | USAID
## LICENSE:  MIT
## PURPOSE:  Create correctness metrics and visuals
## DATE:     2020-03-12
## UPDATED:  2020-06-20


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(vroom)
library(tidytext)
library(glitr)
library(glamr)


# GLOBALS -----------------------------------------------------------------

  out_folder <- "Dataout"
  viz_folder <- "Images"
  quarter <- "Q2"
  

  # To avoid infinities and flag as NA
  ratio_calc <- function(x, y) {
    ifelse(y > 0.000, x / y, NA)
  }
  
  
  pal <- viridis::viridis_pal()(6) #%>% show_col()
  color_hv_sites <- pal[1]
  color_ref <- "#C8C8C8"
  color_all_sites <- "gray30" #"#D3D3D3"
  quarter <- "Q2"
  
# LOAD AND COMPLETENESS CALCULATIONS --------------------------------------
  #import
  file_name <- paste0("HFR_DATIM_FY20", quarter, "_Agg_[[:digit:]]+\\.csv") 
  
  df_datim_agg <- list.files(out_folder, file_name, full.names = TRUE) %>% 
    vroom()  

  # Collapse HFR down to the quarter level, aggregating hfr entries by mech + site
  # TX_CURR and TX_MMD need to use max as they are cumulative indicators, rest can be summe

  # grouping variables  
   grp <- df_datim_agg %>%
     select(-c(mer_results, mer_targets, hfr_results)) %>% 
     names()
  
  # Bring together and derive correctness metrics for assessment
  df_qtr_cness <- df_datim_agg %>% 
    group_by_at(grp) %>% 
    mutate(correctness_ratio = ratio_calc(hfr_results, mer_results),
      correctness_distance = mer_results - hfr_results,
      target_ratio = ratio_calc(hfr_results, mer_targets),
      target_distance = mer_targets - hfr_results) %>% 
    ungroup() %>% 
    group_by(operatingunit, indicator, impflag_targets) %>% 
    mutate(hfr_results_ou = sum(hfr_results, na.rm = TRUE),
           mer_results_ou = sum(mer_results, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(ou_correctness_ratio = ratio_calc(hfr_results_ou, mer_results_ou)) %>% 
    group_by(operatingunit, indicator, impflag_targets) %>% 
    mutate(max_tmp = max(mer_results),
           max_tmp2 = max(hfr_results)) %>% 
    ungroup() %>% 
    mutate(ou_annotation = ifelse(mer_results == max_tmp, ou_correctness_ratio, NA_integer_))
  
  df_qtr_cness %>% 
    group_by(operatingunit, indicator, impflag_targets) %>% 
    summarise(mean = mean(ou_correctness_ratio)) %>% 
    spread(impflag_targets, mean) %>% 
    prinf()
    


# EXPORT ------------------------------------------------------------------

  #store file name
    filename_correctness <- paste0("HFR_Correctness_", quarter, "_",  format(Sys.Date(), "%Y%m%d"), ".csv")
  
  #save
    write_csv(df_qtr_cness, file.path(out_folder, filename_correctness), na = "")

  