## PROJECT:  Pump up the jam
## AUTHOR:   achafetz | USAID
## LICENSE:  MIT
## PURPOSE:  TX_CURR visuals for Q2 review 
## NOTE:     data munging based on whats_trending/02
## DATE:     2020-06-23
## UPDATED:  2020-06-25


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(vroom)
library(Wavelength)
library(scales)
library(extrafont)
library(glitr)
library(COVIDutilities)
library(vroom)



# GLOBAL VARIABLES --------------------------------------------------------
  
  pandemic_date <- who_pandemic() %>% pull(date)

# IMPORT ------------------------------------------------------------------
  
  #TX CURR data from 31
    df_txcurr <- vroom("Dataout/HFR_FY20_TXCURR.csv")
  

# CLEAN UP ----------------------------------------------------------------

  
  #clean up
    df_tx <- df_tx %>% 
      filter(!is.na(operatingunit)) %>% 
      mutate(operatingunit = recode(operatingunit,
                                    "Democratic Republic of the Congo" = "DRC",
                                    "Dominican Republic" = "DR",
                                    "Western Hemisphere Region" = "WHR")) 
    
  #covert mech_code to character
    df_tx <- df_tx %>% 
      mutate(mech_code = as.character(mech_code))
  
  #align dates with hfr_pds
    df_pds <- hfr_identify_pds(2020) %>% 
      group_by(hfr_pd) %>% 
      summarise(hfr_pd_date_min = min(date),
                hfr_pd_date_max = max(date)) %>% 
      ungroup() 
    

# INTERPOLATE -------------------------------------------------------------

  #setup for interpolation: 
  #  replace 0's w/ NA; count reporting pds
    df_txcurr <- df_txcurr %>%
      mutate(hfr_results = na_if(hfr_results, 0)) %>% 
      group_by(mech_code, orgunituid) %>% 
      mutate(pds_reported = sum(!is.na(hfr_results))) %>% 
      ungroup()
    
  #id sites that need to be dropped (need min of 2pds for interpolation)
    df_nonipol_sites <- df_txcurr %>%
      filter(pds_reported < 2) %>% 
      mutate(hfr_results_ipol = hfr_results, 
             is_ipol = FALSE)
    
  #interpolate
    df_txcurr <- df_txcurr %>%
      filter(pds_reported >= 2) %>% 
      group_by(mech_code, orgunituid) %>% 
      mutate(hfr_results_ipol = approx(hfr_pd, hfr_results, hfr_pd)$y %>% round) %>% 
      ungroup() %>% 
      mutate(is_ipol = !is.na(hfr_results_ipol) & is.na(hfr_results))
    
  #apend non interpolated sites
    df_txcurr <- bind_rows(df_txcurr, df_nonipol_sites)

    

# FILTER TO COMPLETE COHORT -----------------------------------------------

  #identify the number of periods
    pds <- unique(df_txcurr$hfr_pd) %>% length()
    
  #aggregate values for complete cohort along with country total targets
    ctry_ipol <- df_txcurr %>% 
      filter(hfr_results_ipol > 0) %>% 
      group_by(orgunituid, mech_code) %>% 
      mutate(all_pds = n() == pds) %>% 
      ungroup() %>% 
      mutate(hfr_results_allpds_only = all_pds * hfr_results_ipol,
             mer_targets_allpds_only = all_pds * mer_targets,
             mer_targets_total = mer_targets) %>% 
      group_by(countryname, hfr_pd) %>% 
      summarise(across(c(hfr_results_allpds_only, mer_targets_allpds_only, mer_targets_total), 
                       sum, na.rm = TRUE)) %>% 
      ungroup() 
    
  #fix issue with aggregation (should all have same target values)
    ctry_ipol <- ctry_ipol %>% 
      filter(hfr_results_allpds_only > 0) %>% 
      group_by(countryname) %>% 
      mutate(across(c(mer_targets_allpds_only, mer_targets_total), max)) %>% 
      ungroup() %>% 
      mutate(comp_site_targ_share = mer_targets_allpds_only/mer_targets_total)
    

# PLOT --------------------------------------------------------------------

  #get df ready for plotting    
    ctry_ipol <- ctry_ipol %>% 
      mutate(countryname = recode(countryname, "Democratic Republic of the Congo" = "DRC"),
             ctry_share = paste0(countryname, " (", percent(comp_site_targ_share, 1), ")"))
    
    ctry_ipol <- left_join(ctry_ipol, df_pds)
    
  #plot
    ctry_ipol %>% 
      filter(comp_site_targ_share >= .75,
             countryname != "Thailand") %>%
      mutate(across(c(hfr_results_allpds_only, mer_targets_allpds_only), ~ . /1000)) %>% 
      ggplot(aes(hfr_pd_date_max, hfr_results_allpds_only, fill = hfr_pd_date_max >=pandemic_date)) +
      geom_col() +
      geom_hline(yintercept = 0, color = "gray30") +
      geom_hline(aes(yintercept = mer_targets_allpds_only), linetype = "dashed", color = "gray30") +
      geom_vline(xintercept = as.Date("2019-12-30")) +
      geom_vline(xintercept = as.Date("2020-03-22")) +
      facet_wrap(~fct_reorder(ctry_share, mer_targets_total, .desc = TRUE), scales = "free_y", nrow = 4) +
      scale_y_continuous(labels = comma_format(1)) +
      scale_x_date(date_breaks = "2 month", date_labels = "%b") +
      scale_fill_manual(values = c('gray70', "#84C3BE")) +
      labs(x = NULL, y = NULL,
           title = toupper("no clear drop off in arv services since covid"),
           subtitle = "only sites reporting every period | TX_CURR, thousands",
           caption = "data are interpolated; max TX_CURR for each pds was used where weekly provided
         source: HFR") +
      si_style_ygrid() +
      theme(legend.position = "none",
            axis.text.x = element_text(size = 7),
            axis.text.y = element_text(size = 7),
            strip.text = element_text(size = 8),
            panel.spacing = unit(1, "lines")
      )
    
  #save
    ggsave("comp_sites_txtrends.png", path = "Images", dpi = 330,
           width = 7.10, height = 4.15)
    
    