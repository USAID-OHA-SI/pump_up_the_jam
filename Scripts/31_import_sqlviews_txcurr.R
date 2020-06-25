## PROJECT:  Pump up the jam
## AUTHOR:   achafetz | USAID
## LICENSE:  MIT
## PURPOSE:  setup data for 
## NOTE:     data munging based on whats_trending/01
## DATE:     2020-06-23
## UPDATED:  2020-06-25


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(Wavelength)


# IMPORT ------------------------------------------------------------------

  #function
    import_sqlview <- function(file){
      df <- file %>% 
        hfr_read() %>% 
        filter(indicator == "TX_CURR")
      
      if(str_detect(file, "456"))
        df <- filter(df, hfr_pd != 6)
      
      return(df)
      
    }
  
  #data created in 01_align_tx
    df_tx <- list.files("Data", "Viewforperiods.*_06242020", full.names = TRUE) %>% 
      map_dfr(import_sqlview)
    

    
    
    
# CLEAN UP ----------------------------------------------------------------

  # aggregate age/sex total by date (sum)
    df_tx <- df_tx %>%
      group_by(operatingunit, countryname, orgunituid,
               fy, hfr_pd, date,
               mech_code,
               indicator) %>% 
      summarise_at(vars(mer_targets, mer_results, hfr_results = val), sum, na.rm = TRUE) %>%
      ungroup()
    
    #aggregate up to hfr period (max)
    df_tx <- df_tx %>% 
      group_by(operatingunit, countryname, orgunituid,
               fy, hfr_pd,
               mech_code, 
               indicator) %>% 
      summarise_at(vars(mer_targets, mer_results, hfr_results), max, na.rm = TRUE) %>% 
      ungroup()
    

# SAVE DATA ---------------------------------------------------------------

  #save
    write_csv(df_tx, "Dataout/HFR_FY20_TXCURR.csv", na = "")
    
