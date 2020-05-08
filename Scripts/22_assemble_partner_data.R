## PROJECT:  Pump up the jam
## AUTHOR:   achafetz | USAID
## LICENSE:  MIT
## PURPOSE:  assemble MER and Partner HFR data
## DATE:     2020-05-06
## UPDATED:  2020-05-08


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(vroom)
library(lubridate)
library(Wavelength)
library(tidylog, warn.conflicts = FALSE)


# GLOBAL VARIABLES --------------------------------------------------------

  start_date <- "2020-01-20"
  weeks <- 12
  datim_folder <- "Data"
  processed_folder <- "Dataout/PartnerProcessed"
  out_folder <- "Dataout"


# APPEND MER RESULTS/TARGETS ----------------------------------------------
  
  
  #unzip Google Drive download of all MER FY20 data
    list.files(datim_folder, "DATIM", full.names = TRUE) %>% unzip(exdir = datim_folder)
  
  #store files paths as vecotre for import
    files <- list.files(datim_folder, "HFR_FY20Q1.*csv", full.names = TRUE)
    
  #import and bind together
    df_datim <- map_dfr(.x = files,
                        .f = ~readr::read_csv(.x))
    
  #fix TX_MMD "targets" --> set to Q1 results
    df_datim_mmd <- df_datim %>% 
      filter(indicator == "TX_CURR") %>% 
      mutate(indicator = "TX_MMD",
             mer_targets = mer_results,
             mer_results = NA)
    
    df_datim <- bind_rows(df_datim, df_datim_mmd)
    rm(df_datim_mmd)
    
  #aggregate to level of detail desired
    df_datim <- df_datim %>% 
      group_by(orgunituid, mech_code, fy, indicator) %>% 
      summarise_at(vars(starts_with("mer")), sum, na.rm = TRUE) %>% 
      ungroup()
    
  #identify dates to repeat df_datim
    dates <- as_date(start_date) %>% seq(by = 7, length.out = weeks)
  
  #repeat df_datim so there is a full target/result set for each week
    df_datim_rpt <- map_dfr(.x = dates,
                            .f = ~mutate(df_datim, date = .x))
  
  #change mech code to character
    df_datim_rpt <- mutate(df_datim_rpt, mech_code = as.character(mech_code))
    
  #remove files (just keep zipped folder)
    unlink(files)
    
  #remove extra objects
    rm(df_datim, dates, files)
    

# IMPORT + AGGREGATE HFR --------------------------------------------------

  #import partner data 
    df_hfr_ptnr <- list.files(processed_folder, full.names = TRUE) %>%
      hfr_read()
      
  #limit partner file to just HFR indicators
    df_hfr_ptnr <- df_hfr_ptnr %>% 
      filter(indicator %in% c("HTS_TST", "HTS_TST_POS",
                              "TX_NEW", "PrEP_NEW",
                              "VMMC_CIRC", "TX_CURR",
                              "TX_MMD"))
    
  #identify partner to limit country file
    partner_mechs <- unique(df_hfr_ptnr$mech_code)
    
  #grouping vars
    grp_vars <- c("orgunituid", "mech_code", "fy", "date", "indicator")

  #aggregate after removing extra
    df_hfr_ptnr <- df_hfr_ptnr %>% 
      group_by_at(vars(all_of(grp_vars))) %>% 
      summarise(hfr_results_ptnr = sum(val, na.rm = TRUE)) %>% 
      ungroup()
    
    partner_mechs <- unique(df_hfr_ptnr$mech_code)
    
  #import country data
    df_hfr_ctry <- file.path(datim_folder, "HFR_2020.07_Tableau_20200507.zip") %>% 
      hfr_read()
  
  #filter country submitted data
    df_hfr_ctry <- df_hfr_ctry %>% 
      filter(mech_code %in% partner_mechs)
    
  #aggregate after removing extra
    df_hfr_ctry <- df_hfr_ctry %>% 
      group_by_at(vars(all_of(grp_vars))) %>% 
      summarise(hfr_results_ctry = sum(val, na.rm = TRUE)) %>% 
      ungroup()
    
  #remove SQL export NAs
    df_hfr_ctry <- df_hfr_ctry %>% 
      mutate_if(is.character, ~na_if(., "\\N"))
    
    
# MERGE HFR + DATIM -------------------------------------------------------

    
  #merge both HFR datasets
    df_hfr <- df_hfr_ptnr %>% 
      full_join(df_hfr_ctry, by = grp_vars)
    
  #merge
    df_joint <- df_datim_rpt %>% 
      filter(mech_code %in% partner_mechs) %>% 
      full_join(df_hfr, by = grp_vars)
    
  #make sure all have the HFR PD (missing for DATIM)
    df_joint <- df_joint %>% 
      hfr_fix_date() %>% 
      hfr_assign_pds()
    
  #arrange var order
    df_joint <- df_joint %>% 
      select(fy, hfr_pd, date, everything())

# MERGE META --------------------------------------------------------------

  #import hierarchy
    df_hierarchy <- file.path(datim_folder, "HFR_FY20_GLOBAL_orghierarchy_20200306.csv") %>% 
      read_csv() %>% 
      select(-level)
    
  #merge hierarchy onto joint file
    df_joint <- left_join(df_joint, df_hierarchy, by = "orgunituid")
  
  #import mechanism info
    df_mech <- file.path(datim_folder, "HFR_FY20_GLOBAL_mechanisms_20200306.csv") %>% 
      read_csv(col_types = c(.default = "c")) %>% 
      select(-operatingunit)
    
  #merge mech info onto joint file
    df_joint <- left_join(df_joint, df_mech, by = "mech_code")
        
  #arrange var order
    df_joint <- df_joint %>% 
      select(operatingunit:facility, orgunit, orgunituid, latitude, longitude,
             fundingagency, mech_code, mech_name, primepartner,
             fy, hfr_pd, date, 
             indicator,
             hfr_results_ptnr, hfr_results_ctry, mer_results, mer_targets)

# EXPORT ------------------------------------------------------------------

  #store file name
    filename <- paste0("HFR_PARTNER_FY20PD0507_", format(Sys.Date(), "%Y%m%d"), ".csv")
  
  #save
    write_csv(df_joint, file.path(out_folder, filename), na = "")

