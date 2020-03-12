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
    
  #change hfr reporting variable for clarity
    df_joint <- rename(df_joint, hfr_results = val)
    
  #remove data post Q1 for all but TX_CURR
    df_joint <- df_joint %>% 
      filter(!(indicator != "TX_CURR" & date >= "2019-12-30")) 
    
    #check
      # df_joint %>%
      #   count(date, indicator) %>%
      #   spread(indicator, n)
  
  #reshape long to preserve NAs for non reporting
    df_joint_lng <- df_joint %>% 
      gather(type, value, hfr_results, mer_results, mer_targets, na.rm = TRUE)
    
    
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
    
  
    
    