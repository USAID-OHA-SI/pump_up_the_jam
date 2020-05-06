## PROJECT:  Pump up the jam
## AUTHOR:   achafetz | USAID
## LICENSE:  MIT
## PURPOSE:  align FY20 HFR data
## DATE:     2020-05-05
## UPDATED:  


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(vroom)
library(here)
library(Wavelength)



# GLOBAL VARIABLES --------------------------------------------------------

datain <- "Data"
dataout <- "Dataout"


# IMPORT ------------------------------------------------------------------

  #Periods 2020.01 - 2020.03 via SQL View
    #https://drive.google.com/open?id=1fOaJBj0KwaeWPIhFF4y7afkRPvsq10B1
    path_sql <- here(datain, "HFR_SQLview_2020.01thru2020.03.zip")

    df_sql <- hfr_read(path_sql)

  #Periods 2020.04-2020.06 via standard output
    #https://drive.google.com/open?id=1mpGNxcCMpD_jbRGXe-nMhjDY_l9EzM1W
    path_twb <- here(datain, "HFR_2020.06_Tableau_20200427.zip")
  
    df_twb <- hfr_read(path_twb)
    

# MERGE -------------------------------------------------------------------

  #full FY20 TX_CURR dataset
    df_tx <- df_twb %>% 
        filter(indicator == "TX_CURR") %>% 
        bind_rows(df_sql)
     
    rm(df_sql, df_twb)
    

# CLEAN -------------------------------------------------------------------

  #remove SQL export row break
    df_tx <- df_tx %>% 
      mutate(primepartner = str_remove(primepartner, "\r$"))
  
 
  # aggregate (max) removing age/sex & date
    df_tx <- df_tx %>%
      group_by(operatingunit, countryname, snu1, psnu, orgunit, orgunituid,
               fy, hfr_pd,
               mech_code, mech_name, primepartner,
               indicator) %>% 
      summarise_at(vars(mer_targets, mer_results, hfr_results = val), max, na.rm = TRUE) %>% 
      ungroup()

  #fix -Inf issue
    df_tx <- df_tx %>% 
      mutate_at(vars(starts_with("mer")), ~ na_if(., -Inf))
  
  
  #replace //N with NA
    df_tx <- df_tx %>% 
      mutate_all(~ na_if(., "\\N")) %>% 
      mutate_all(~ na_if(., "NULL"))
    
  #pull mechlist from DATIM to assign orphan UIDs to OUs
    df_mechlist <-  pull_mech()
    
  #rename mechlist names for merging
    df_mechlist <- df_mechlist %>% 
      select(mech_code, 
             operatingunit_d = operatingunit,
             mech_name_d = mech_name,
             primepartner_d = primepartner)
    
  #merge melist on to fix orphaned UIDs
    df_tx <- df_tx %>% 
      left_join(df_mechlist, by = "mech_code") 
    
  #replace orphaned UIDs so they have an OU home
    df_tx <- df_tx %>% 
      mutate(operatingunit = ifelse(is.na(operatingunit), operatingunit_d, operatingunit),
             mech_name = ifelse(is.na(mech_name), mech_name_d, mech_name),
             primepartner_d = ifelse(is.na(primepartner), primepartner_d, primepartner_d)) %>% 
      select(-ends_with("_d"))
    
    

# EXPORT DATA -------------------------------------------------------------

  write.csv(df_tx, here(dataout, "HFR_FY20_TXCURR.csv"), na = "")    

# EXPLORE -----------------------------------------------------------------

    #site count
      df_site_cnt_mer <- df_tx %>% 
        filter(mer_targets > 0) %>% 
        distinct(operatingunit, orgunituid) %>% 
        count(operatingunit, name = "sites_mer")
      
      df_tx %>% 
        filter(hfr_results > 0) %>% 
        distinct(operatingunit, orgunituid, hfr_pd) %>% 
        count(operatingunit, hfr_pd, name = "sites_hfr") %>% 
        complete(hfr_pd, nesting(operatingunit), fill = list(n = 0)) %>% 
        full_join(df_site_cnt_mer) %>% 
        arrange(operatingunit, hfr_pd) %>% 
        mutate(sites_hfr = ifelse(is.na(sites_hfr), 0, sites_hfr),
               completeness = sites_hfr / sites_mer)
      
      df_tx_ou <- df_tx %>% 
        group_by(operatingunit, fy, hfr_pd) %>% 
        summarise_at(vars(hfr_results, mer_results, mer_targets), sum, na.rm = TRUE) %>% 
        ungroup() %>% 
        mutate_all(~ na_if(., -Inf))
        
      df_tx_ou  %>% 
        ggplot(aes(hfr_pd, hfr_results)) +
        geom_hline(aes(yintercept = mer_targets)) +
        geom_col() +
        facet_wrap(~operatingunit, scales = "free_y")
      
      
      df_tx_mech <- df_tx %>% 
        group_by(operatingunit, fy, hfr_pd, mech_code) %>% 
        summarise_at(vars(hfr_results, mer_results, mer_targets), sum, na.rm = TRUE) %>% 
        ungroup() %>% 
        mutate_all(~ na_if(., -Inf)) %>% 
        filter_at(vars(hfr_results, mer_results, mer_targets), any_vars(!is.na(.) & . != 0))
      
      df_tx_mech  %>% 
        ggplot(aes(hfr_pd, hfr_results)) +
        geom_hline(aes(yintercept = mer_targets)) +
        geom_col() +
        facet_wrap(~operatingunit + mech_code, scales = "free_y",  
                   labeller = label_wrap_gen(multi_line=FALSE))
      