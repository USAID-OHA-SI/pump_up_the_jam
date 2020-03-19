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
library(scales)
library(extrafont)


# GLOBALS -----------------------------------------------------------------

  out_folder <- "Dataout"
  viz_folder <- "Images"
  
  prinf <- function(df) {
    print(df, n = Inf)
  }

  # To avoid infinities and flag as NA
  ratio_calc <- function(x, y) {
    ifelse(y > 0.000, x / y, NA)
  }
  
  
  pal <- viridis_pal()(6) #%>% show_col()
  color_hv_sites <- pal[1]
  color_ref <- "#C8C8C8"
  color_all_sites <- "gray30" #"#D3D3D3"

# LOAD AND COMPLETENESS CALCULATIONS --------------------------------------
  
  df_datim_agg <- list.files(out_folder, "HFR_DATIM_FY20Q1_Agg_[[:digit:]]+\\.csv", full.names = TRUE) %>% 
    vroom()  

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
    

# CORRECTNESS PLOT FCN ----------------------------------------------------

  
 plot_cness <- function(df, ind, ous){
   
    df_viz <- df %>% 
      filter(!is.na(correctness_ratio) & indicator == {{ind}} & operatingunit %in% ous) %>% 
      mutate(site_sort = tidytext::reorder_within(orgunituid, correctness_ratio, correctness_distance),
             ou_order = fct_reorder(operatingunit, mer_targets, .fun = sum, .desc = TRUE),
             hv_flag = if_else(impflag_targets == 1, "High Volume", "Low Volume")) 
    
    df_viz %>% 
      ggplot(aes(x = mer_results, y = hfr_results, colour = hv_flag, 
                 size = mer_targets)) + 
      geom_point(aes(alpha = 0.80)) +
      geom_abline(intercept = 0, slope = 1, colour = "#909090") +
      geom_label(aes(label = scales::percent(round(ou_annotation, 2))), size = 4, 
                 hjust = 1, vjust = 1, fill = "white",
                 family = "Source Sans Pro") +
      facet_wrap(ou_order ~ hv_flag, scales = "free",
                 labeller = label_wrap_gen(multi_line = FALSE)) +
      theme_minimal() +
      scale_colour_manual(values = c("High Volume" = color_hv_sites, "Low Volume" = color_all_sites)) +
      scale_y_continuous(labels = comma) +
      scale_x_continuous(labels = comma) +
      theme(legend.position = "none",
            legend.justification = "left",
            legend.key.size = unit(2, "cm"),
            strip.text = element_text(hjust = 0)) +
      scale_size(guide = "none") +
      labs(x = "MER results", y = "HFR results",
           subtitle = paste({{ind}} ,"| FY20Q1 Site x Mechanism HFR Reporting Correctness"),
           caption = "Source: FY20Q1 MER + HFR",
           colour = "") +
      theme(text = element_text(family = "Source Sans Pro"),
            plot.title = element_text(face = "bold"),
            plot.caption = element_text(face = "plain", color = "gray30"))
 }

# PLOT --------------------------------------------------------------------

  
  # Check a results for a country -- Botswana
    top_ous <- c("South Africa", "Kenya", "Mozambique", "Uganda",
                 "Malawi", "Tanzania", "Zimbabwe", "Nigeria")
  
   
    plot_cness(df_q1_cness, "HTS_TST", top_ous) +
      labs(title = "SIMILAR TRENDS IN CORRECTNESS ACROSS SITE TYPES")
    
    ggsave(file.path(viz_folder,"HFR_Correctness_TX_CURR.png"), dpi = 300, 
           width = 10, height = 5.625, scale = 1.25)
    
    plot_cness(df_q1_cness, "TX_NEW", top_ous) +
      labs(title = "SIMILAR TRENDS IN CORRECTNESS ACROSS SITE TYPES")
    
    ggsave(file.path(viz_folder,"HFR_Correctness_TX_CURR.png"), dpi = 300, 
           width = 10, height = 5.625, scale = 1.25)
    
    plot_cness(df_q1_cness, "TX_CURR", top_ous) +
      labs(title = "SIMILAR TRENDS IN CORRECTNESS ACROSS SITE TYPES")
    
    ggsave(file.path(viz_folder,"HFR_Correctness_TX_CURR.png"), dpi = 300, 
           width = 10, height = 5.625, scale = 1.25)
   
   
