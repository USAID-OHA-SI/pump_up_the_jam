## PROJECT:  Pump up the jam
## AUTHOR:   jdavis, A.Chafetz | USAID
## LICENSE:  MIT
## PURPOSE:  assemble MER and HFR data
## DATE:     2020-03-10
## UPDATED:



# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(Wavelength)


# GLOBAL VARIABLES --------------------------------------------------------

  start_date <- "2019-09-30"
  weeks <- 16
  hfr_folder <- "data"
  datim_folder <- "data"
  out_folder <- "Dataout"


# APPEND MER RESULTS/TARGETS ----------------------------------------------
  
  
  #unzip Google Drive download of all MER FY20 data
    list.files(datim_folder, "DATIM", full.names = TRUE) %>% unzip(exdir = datim_folder)
  
  #store files paths as vecotre for import
    files <- list.files(datim_folder, "HFR_FY20.*csv", full.names = TRUE)
    
  #import and bind together
    df_datim <- map_dfr(.x = files,
                        .f = ~readr::read_csv(.x))
  
  #identify dates to repeat df_datim
    dates <- as_date(start_date) %>% seq(by = 7, length.out = weeks)
  
  #repeat df_datim so there is a full target/result set for each week
    df_datim_rpt <- map_dfr(.x = dates,
                            .f = ~mutate(df_datim, date = .x))
  
  #remove files (just keep zipped folder)
    unlink(files)
    
  #remove extra objects
    rm(df_datim, dates, files)
    
  

# IMPORT HFR --------------------------------------------------------------

  #import
    hfr <- list.files(datim_folder, "inprocess", full.names = TRUE) %>% 
      read_csv(col_types = c(.default = "c"))
  
  #remove mer_targets, updated from DATIM with results above
    hfr <- select(hfr, -mer_targets)


# APPEND HFR + DATIM ------------------------------------------------------

  #append
    df  <- df_datim_rpt %>% 
      mutate_all(as.character) %>% 
      bind_rows(hfr)
    
  #make sure all have the HFR PD (missing for DATIM)
    df <- df %>% 
      mutate(date = as_date(date)) %>% 
      hfr_assign_pds()
  
  #remove primeparnter and mech name to possible incompatibility
    df <- select(df, -c(primepartner, mech_name))
    
  #aggregate where possible
    grp <- setdiff(names(df), c("val", "mer_results", "mer_targets"))
    
    df_agg <- df %>% 
      mutate_at(vars(val, mer_results, mer_targets), as.double) %>% 
      group_by_at(grp) %>% 
      summarise_at(vars(val, mer_results, mer_targets), sum, na.rm = TRUE) %>% 
      ungroup()
    
  #arrange var order
    df_agg <- df_agg %>% 
      select(operatingunit, countryname, snu1, psnu, psnuuid, community, orgunit, orgunituid,
             fundingagency, mech_code,
             fy, hfr_pd, date, 
             indicator, sex, agecoarse, otherdisaggregate,
             val, mer_results, mer_targets
             )


# EXPORT ------------------------------------------------------------------

  #store file name
    filename <- paste0("merged_datim_hfr_", format(Sys.Date(), "%Y.%m.%d"), ".csv")
  
  #save
    write_csv(df_datim_rpt, file.path(out, filename))

