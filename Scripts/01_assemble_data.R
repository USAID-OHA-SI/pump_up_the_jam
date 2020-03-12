## PROJECT:  Pump up the jam
## AUTHOR:   jdavis, A.Chafetz | USAID
## LICENSE:  MIT
## PURPOSE:  assemble MER and HFR data
## DATE:     2020-03-10
## UPDATED:  2020-03-11


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(Wavelength)


# GLOBAL VARIABLES --------------------------------------------------------

  start_date <- "2019-09-30"
  weeks <- 16
  hfr_folder <- "Data"
  datim_folder <- "Data"
  out_folder <- "Dataout"


# APPEND MER RESULTS/TARGETS ----------------------------------------------
  
  
  #unzip Google Drive download of all MER FY20 data
    list.files(datim_folder, "DATIM", full.names = TRUE) %>% unzip(exdir = datim_folder)
  
  #store files paths as vecotre for import
    files <- list.files(datim_folder, "HFR_FY20Q1.*csv", full.names = TRUE)
    
  #import and bind together
    df_datim <- map_dfr(.x = files,
                        .f = ~readr::read_csv(.x))
    
  #fix TX_MMD "targets"
    df_datim_mmd <- df_datim %>% 
      filter(indicator == "TX_CURR") %>% 
      mutate(indicator = "TX_MMD",
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
  
  #remove files (just keep zipped folder)
    unlink(files)
    
  #remove extra objects
    rm(df_datim, dates, files)
    

# IMPORT + AGGREGATE HFR --------------------------------------------------

  #import
    df_hfr <- list.files(datim_folder, "inprocess", full.names = TRUE) %>% 
      read_csv(col_types = c(.default = "c"))
      
  #aggregate after removing extra
    df_hfr <- df_hfr %>% 
      mutate(val = as.numeric(val)) %>% 
      group_by(orgunituid, mech_code, fy, date, indicator) %>% 
      summarise_at(vars(val), sum, na.rm = TRUE) %>% 
      ungroup()
    
    
# MERGE HFR + DATIM -------------------------------------------------------

  #merge
    df_joint <- df_datim_rpt %>% 
      mutate_all(as.character) %>% 
      full_join(df_hfr)
    
  #make sure all have the HFR PD (missing for DATIM)
    df_joint <- df_joint %>% 
      mutate(date = as_date(date)) %>% 
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
    df_joint <- left_join(df_joint, df_hierarchy)
  
  #import mechanism info
    df_mech <- file.path(datim_folder, "HFR_FY20_GLOBAL_mechanisms_20200306.csv") %>% 
      read_csv(col_types = c(.default = "c")) %>% 
      select(-operatingunit)
    
  #merge mech info onto joint file
    df_joint <- left_join(df_joint, df_mech)
        
  #arrange var order
    df_joint <- df_joint %>% 
      select(operatingunit:facility, orgunit, orgunituid, latitude, longitude,
             fundingagency, mech_code, mech_name, primepartner,
             fy, hfr_pd, date, 
             indicator,
             val, mer_results, mer_targets)

# EXPORT ------------------------------------------------------------------

  #store file name
    filename <- paste0("HFR_DATIM_FY20Q1_", format(Sys.Date(), "%Y%m%d"), ".csv")
  
  #save
    write_csv(df_joint, file.path(out_folder, filename), na = "")

