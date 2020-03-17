## PROJECT:  Pump up the jam
## AUTHOR:   tessam | USAID
## LICENSE:  MIT
## PURPOSE:  Create correctness metrics and visuals
## DATE:     2020-03-12


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(Wavelength)
library(vroom)
library(tidytext)


# GLOBALS -----------------------------------------------------------------

  out_folder <- "Dataout"
  
  prinf <- function(df) {
    print(df, n = Inf)
  }

  # To avoid infinities and flag as NA
  ratio_calc <- function(x, y) {
    ifelse(y > 0.000, x / y, NA)
  }
  

# LOAD AND COMPLETENESS CALCULATIONS --------------------------------------
  
  df_datim_agg <- list.files(out_folder, "HFR_DATIM_FY20Q1_Agg_[[:digit:]]+\\.csv", full.names = TRUE) %>% 
    vroom(col_types = c(.default = "c"))  

  # Collapse HFR down to the quarter level, aggregating hfr entries by mech + site
  # TX_CURR and TX_MMD need to use max as they are cumulative indicators, rest can be summe

  # grouping variables  
   grp <- df_datim_agg %>%
     select(-c(mer_results, mer_targets, hfr_results)) %>% 
     names()
  
  # Bring together and derive correctness metrics for assessment
  df_q1_cness <- df_datim_agg %>% 
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
      max_tmp2 = max(hfr_results),
      ou_annotation = ifelse(mer_results == max_tmp & hfr_results == max_tmp2, ou_correctness_ratio, NA_integer_))
  
  df_q1_cness %>% 
    group_by(operatingunit, indicator, impflag_targets) %>% 
    summarise(mean = mean(ou_correctness_ratio)) %>% 
    spread(impflag_targets, mean) %>% 
    prinf()
    
  
  # Check a results for a country -- Botswana
 top_ous <- c("South Africa", "Kenya", "Mozambique", "Uganda",
   "Malawi", "Tanzania", "Zimbabwe", "Nigeria")
  
   df_q1_cness %>% 
    filter(!is.na(correctness_ratio) & indicator == "TX_CURR" & operatingunit %in% top_ous) %>% 
    mutate(site_sort = tidytext::reorder_within(orgunituid, correctness_ratio, correctness_distance),
      ou_order = fct_reorder(operatingunit, mer_targets, .fun = sum, .desc = TRUE),
      hv_flag = if_else(impflag_targets == 1, "High Volume", "Low Volume")) %>% 
    ggplot(aes(x = mer_results, y = hfr_results, colour = hv_flag, 
      size = mer_targets)) + 
    geom_point(aes(alpha = 0.80)) +
     geom_label(aes(label = scales::percent(round(ou_annotation, 2))), size = 4) +
    facet_wrap(ou_order ~ hv_flag, scales = "free",
      labeller = label_wrap_gen(multi_line = FALSE)) +
    theme_minimal() +
    scale_colour_manual(values = c("High Volume" = "#66c2a5", "Low Volume" = "#fc8d62")) +
    geom_abline(intercept = 0, slope = 1, colour = "#909090") +
     theme(legend.position = "none",
       legend.justification = "left",
       legend.key.size = unit(2, "cm"),
       strip.text = element_text(hjust = 0)) +
     scale_size(guide = "none") +
     labs(x = "MER results", y = "HFR results",
       title = "Mozambique",
       caption = "placeholder2",
       colour = "")
     
  
 #dump building block dfs
  #rm(list = ls(pattern = "*df_q1_"))
  
  
 
  

  
  # Did a site + mech report HFR values anywhere close to mer_results? (If they reported at all)
  

# PLOTTING ----------------------------------------------------------------



# CLEANUP -----------------------------------------------------------------

