## PROJECT:  Pump up the jam
## AUTHOR:   A.Chafetz | USAID
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
    df_joint <- vroom(file.path(out_folder, "HFR_DATIM_FY20Q1_20200311.csv"))  
  

# MUNGE -------------------------------------------------------------------

  #fix mech code
    df_joint <- mutate(df_joint, mech_code = as.character(mech_code))
    
    
  #remove data post Q1 for all but TX_CURR
    df_joint <- df_joint %>% 
      filter(!(indicator != "TX_CURR" & date >= "2019-12-30")) 
    
  #check
    # df_joint %>% 
    #   count(date, indicator) %>% 
    #   spread(indicator, n)
    
    
  #adjust TX_CURR
    df_tx <- df_joint %>% 
      filter(indicator == "TX_CURR") %>% 
      select(-date) %>% 
      group_by_if(is.character) %>% 
      summarise_if(is.numeric, max, na.rm = TRUE) %>% 
      ungroup()
    
    df_joint <- df_joint %>% 
      filter(indicator != "TX_CURR") %>% 
      bind_rows(df_tx)
    
  #full period comp
    df_joint %>% 
      group_by()
    
    