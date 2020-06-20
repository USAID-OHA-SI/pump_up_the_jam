## PROJECT:  Pump up the jam
## AUTHOR:   T.Essam, A. Chafetz| USAID
## LICENSE:  MIT
## PURPOSE:  assemble MER and HFR data
## DATE:     2020-06-15


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(vroom)
library(lubridate)
library(Wavelength)
library(googledrive)
library(fs)
library(here)


# GLOBAL VARIABLES --------------------------------------------------------

  start_date <- "2019-09-30"
  weeks <- 32
  datim_folder <- "Data"
  out_folder <- "Dataout"
  quarter <- "Q2"

  #establish OAuth
  #drive_auth()

# APPEND MER RESULTS/TARGETS ----------------------------------------------
  
  #unzip Google Drive download of all MER FY20 data
    list.files(datim_folder, "DATIM", full.names = TRUE) %>% unzip(exdir = datim_folder)
  
  #store files paths as vecotre for import
    files <- list.files(datim_folder, "HFR_FY20Q2.*csv", full.names = TRUE, recursive = TRUE)
    
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
  
  #remove files (just keep zipped folder)
    #unlink(files)
    list.files(datim_folder, "3.1 DATIM", full.names = T) %>% unlink(., recursive = TRUE)

    
  #remove extra objects
    rm(df_datim, dates, files)
    

# IMPORT + AGGREGATE HFR --------------------------------------------------
# USING TABLEAU DATA + SERVER EXTRACTS
    
  # Load data from most recent to least recent; Filtering as needed
    hfr_pd678 <- vroom(here(datim_folder, "Viewforperiods678_06112020.csv"))
    
    hfr_pd5 <- vroom(here(datim_folder, "Viewforperiods567_05262020.csv")) %>% 
      filter(hfr_pd == 5)

    hfr_pd4 <- vroom(here(datim_folder, "Viewforperiods456_05262020.csv")) %>% 
      filter(hfr_pd == 4)
    
    hfr_pd123 <- vroom(here(datim_folder, "Viewforperiods123_05272020.csv")) 
    
    hfr_list <- ls(pattern = "hfr_pd")
    
    hfr_all <- bind_rows(hfr_pd678, hfr_pd5, hfr_pd4, hfr_pd123)

    
    # Check the data - check for non-numerical characters
    hfr_all %>% filter(!grepl('^[0-9]', val)) %>% count(val, hfr_pd) %>% arrange(hfr_pd)
    
    # Filtering these NA values out
    #hfr_all %>% filter(str_detect(val, "-")) %>% View()
    
    
  #     #import
  # hfr_list <- list.files(hfr_folder, "HFR_", full.names = TRUE, recursive = TRUE) 
  # 
  # # hfr_list <- list.files(hfr_folder, "2020.01", full.names = TRUE, recursive = TRUE)   
  # 
  # #
  # noisy_read <- function(df) {
  #   print(df)
  #   vroom(df, col_types = c(.default = "c"))
  # }
  #   
  # df_hfr <- map_dfr(.x = hfr_list,
  #                   .f = ~noisy_read(.))  
      
  #aggregate after removing extra
    df_hfr <- hfr_all %>% 
      filter(grepl('^[0-9]', val)) %>% 
      mutate(val = as.numeric(val)) %>% 
      group_by(orgunituid, mech_code, fy, date, indicator) %>% 
      summarise_at(vars(val), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      mutate_all(as.character)
    
  remove(list = ls(pattern = "hfr_p|hfr_all"))  
    
# MERGE HFR + DATIM -------------------------------------------------------

  # Check compatibility of variable types
  map(list(df_datim_rpt, df_hfr), str)
  
  #merge
    df_joint <- df_datim_rpt %>% 
      mutate_all(as.character) %>% 
      full_join(df_hfr)
    
  #make sure all have the HFR PD (missing for DATIM)
    df_joint <- df_joint %>% 
      hfr_fix_date() %>% 
      hfr_assign_pds()
    
  #arrange var order
    df_joint <- df_joint %>% 
      select(fy, hfr_pd, date, everything())

# MERGE META --------------------------------------------------------------

  #import hierarchy
    df_hierarchy <- file.path(datim_folder, "HFR_FY20_GLOBAL_orghierarchy_20200611.csv") %>% 
      read_csv() %>% 
      select(-level)
    
  #merge hierarchy onto joint file
    df_joint <- left_join(df_joint, df_hierarchy)
  
  #import mechanism info
    df_mech <- file.path(datim_folder, "HFR_FY20_GLOBAL_mechanisms_20200611.csv") %>% 
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
    filename <- paste0("HFR_DATIM_FY20", quarter, "_", format(Sys.Date(), "%Y%m%d"), ".csv")
  
  #save
    write_csv(df_joint, file.path(out_folder, filename), na = "")

