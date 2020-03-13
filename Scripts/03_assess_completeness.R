## PROJECT:  Pump up the jam
## AUTHOR:   A.Chafetz, T.Essam | USAID
## LICENSE:  MIT
## PURPOSE:  calculate completeness of reporting
## DATE:     2020-03-11
## UPDATED:  


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(vroom)


# GLOBAL VARIABLES --------------------------------------------------------

out_folder <- "Dataout"


# IMPORT ------------------------------------------------------------------

  #load joint HFR + DATIM dataset for FY20Q1 (created in Scripts/assemble_data.R)
    df_joint <- list.files(out_folder, "HFR_DATIM_FY20Q1_[[:digit:]]+\\.csv", full.names = TRUE) %>% 
      vroom(col_types = c(.default = "c"))  
  
    
# MUNGE -------------------------------------------------------------------

  #change hfr reporting variable for clarity
    df_joint <- rename(df_joint, hfr_results = val)
    
  #remove data post Q1 for all but TX_CURR
    df_joint <- df_joint %>% 
      mutate(date = as_date(date)) %>% 
      filter(!(indicator != "TX_CURR" & date >= "2019-12-30")) 
    
    #check
      # df_joint %>%
      #   count(date, indicator) %>%
      #   spread(indicator, n)

  #reshape long to preserve NAs for non reporting
    df_joint_lng <- df_joint %>% 
      gather(type, value, hfr_results, mer_results, mer_targets, na.rm = TRUE) %>% 
      mutate(value = as.double(value))
    

# QUARTERLY AGGREGATION ---------------------------------------------------

  #create a FY20Q1 value for non-TX_CURR indicators
    df_q1_sum <- df_joint_lng %>% 
      filter(!indicator %in% c("TX_CURR", "TX_MMD"),
             type == "hfr_results") %>% 
      select(-date, -hfr_pd) %>% 
      group_by_if(is.character) %>% 
      summarise(value = sum(value, na.rm = TRUE)) %>% 
      ungroup()
    
  #create a FY20Q1 value for non-TX_CURR indicators
    df_q1_max <- df_joint_lng %>% 
      filter(indicator %in% c("TX_CURR", "TX_MMD") |
             type %in% c("mer_results", "mer_targets")) %>% 
      select(-date, -hfr_pd) %>% 
      group_by_if(is.character) %>% 
      summarise_if(is.numeric, max, na.rm = TRUE) %>% 
      ungroup()
  
  #join FY20Q1 dataset & respread
    df_q1 <- bind_rows(df_q1_sum, df_q1_max)  %>% 
      spread(type, value)
  
    rm(df_q1_max, df_q1_sum)    

# PERIOD AGGREGATION ------------------------------------------------------

    #create a FY20Q1 value for non-TX_CURR indicators
    df_pds_sum <- df_joint_lng %>% 
      filter(!indicator %in% c("TX_CURR", "TX_MMD"),
             type == "hfr_results") %>% 
      select(-date) %>% 
      group_by_if(is.character) %>% 
      summarise(value = sum(value, na.rm = TRUE)) %>% 
      ungroup()
    
    #create a FY20Q1 value for non-TX_CURR indicators
    df_pds_max <- df_joint_lng %>% 
      filter(indicator %in% c("TX_CURR", "TX_MMD") |
               type %in% c("mer_results", "mer_targets")) %>% 
      select(-date) %>% 
      group_by_if(is.character) %>% 
      summarise_if(is.numeric, max, na.rm = TRUE) %>% 
      ungroup()
    
    #join FY20Q1 dataset & respread
    df_pds <- bind_rows(df_pds_sum, df_pds_max)  %>% 
      spread(type, value)
    
    rm(df_pds_max, df_pds_sum)

# MERGE DATIM FLAGS -------------------------------------------------------

  #import volume weighting
    df_datim_wgts <- list.files(out_folder, "DATIM_FLAGS_[[:digit:]]+\\.csv", full.names = TRUE) %>% 
      vroom() %>% 
      mutate_at(vars(mech_code, fy), as.character)
    
  #drop vars before merging
    df_datim_wgts <- df_datim_wgts %>% 
      select(-c(mer_results, mer_targets, operatingunit))
    
  #join to quarterly aggregation
    df_q1 <- left_join(df_q1, df_datim_wgts)
    
  #join to period aggregation
    df_pds <- left_join(df_pds, df_datim_wgts)
    

# QUARTERLY COMPLETENESS --------------------------------------------------


  #create flags for whether site reported HFR and if site exists in DATIM
    df_q1 <- df_q1 %>% 
      mutate(has_hfr_reporting = !is.na(hfr_results),
             is_datim_site = mer_results > 0 | mer_targets > 0)
  
  #remove where HFR reporting against mech x site that does not have DATIM results/targets
    df_q1 <- df_q1 %>% 
      filter(!(has_hfr_reporting == TRUE & is_datim_site == FALSE))
    
  #mech x site completeness report
    df_completeness <- df_q1 %>% 
      filter(!(hfr_results == 0 & mer_results == 0)) %>% 
      group_by(operatingunit, indicator) %>% 
      summarise_at(vars(has_hfr_reporting, is_datim_site), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      mutate(completeness = case_when(is_datim_site >  0 ~ has_hfr_reporting / is_datim_site),
             site_type = "All")
             
      print(df_completeness, n = Inf)
    
  #mech x site completeness report for HV sites
    df_completeness_imp <- df_q1 %>% 
      filter(!(hfr_results == 0 & mer_results == 0),
             impflag_targets == 1) %>% 
      group_by(operatingunit, indicator) %>% 
      summarise_at(vars(has_hfr_reporting, is_datim_site), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      mutate(completeness = case_when(is_datim_site >  0 ~ has_hfr_reporting / is_datim_site),
             site_type = "Important (Target-based)")
             
      print(df_completeness_imp, n = Inf)
    
  #combine completeness for viz
      df_completeness_viz <- df_completeness %>% 
        bind_rows(df_completeness_imp) %>% 
        select(site_type, everything())

# PERIOD COMPLETENESS -----------------------------------------------------

    #create flags for whether site reported HFR and if site exists in DATIM
      df_pds <- df_pds %>% 
        mutate(has_hfr_reporting = !is.na(hfr_results),
               is_datim_site = mer_results > 0 | mer_targets > 0)
      
    #remove where HFR reporting against mech x site that does not have DATIM results/targets
      df_pds <- df_pds %>% 
        filter(!(has_hfr_reporting == TRUE & is_datim_site == FALSE))
      
    #adjust period
      df_pds <- df_pds %>% 
        mutate(hfr_pd = paste0(fy, ".", str_pad(hfr_pd, 2, pad = "0")))
      
    #mech x site completeness report
      df_completeness_pds <- df_pds %>% 
        filter(!(hfr_results == 0 & mer_results == 0)) %>% 
        group_by(operatingunit, hfr_pd, indicator) %>% 
        summarise_at(vars(has_hfr_reporting, is_datim_site), sum, na.rm = TRUE) %>% 
        ungroup() %>% 
        mutate(completeness = case_when(is_datim_site >  0 ~ has_hfr_reporting / is_datim_site),
               site_type = "All")
      
      print(df_completeness_pds, n = Inf)
      
    #mech x site completeness report for HV sites
      df_completeness_pds_imp <- df_pds %>% 
        filter(!(hfr_results == 0 & mer_results == 0),
               impflag_targets == 1) %>% 
        group_by(operatingunit, hfr_pd, indicator) %>% 
        summarise_at(vars(has_hfr_reporting, is_datim_site), sum, na.rm = TRUE) %>% 
        ungroup() %>% 
        mutate(completeness = case_when(is_datim_site >  0 ~ has_hfr_reporting / is_datim_site),
               site_type = "Important (Target-based)")
      
      print(df_completeness_imp, n = Inf)
      
      #combine completeness for viz
      df_completeness_pds_viz <- df_completeness_pds %>% 
        bind_rows(df_completeness_pds_imp) %>% 
        select(site_type, everything())       

# EXPORT ------------------------------------------------------------------
    
  #store file name
    filename <- paste0("HFR_DATIM_FY20Q1_Agg_", format(Sys.Date(), "%Y%m%d"), ".csv")
    filename_comp <- paste0("HFR_Completeness_", format(Sys.Date(), "%Y%m%d"), ".csv")
    filename_pds <- paste0("HFR_DATIM_FY20Q1_AggPds_", format(Sys.Date(), "%Y%m%d"), ".csv")
    filename_pds_comp <- paste0("HFR_Completeness_Pds_", format(Sys.Date(), "%Y%m%d"), ".csv")
    
  #save
    write_csv(df_q1, file.path(out_folder, filename), na = "")
    write_csv(df_completeness_viz, file.path(out_folder, filename_comp), na = "")
    # write_csv(df_pds, file.path(out_folder, filename_pds), na = "")
    write_csv(df_completeness_pds_viz, file.path(out_folder, filename_pds_comp), na = "")
      
    
    