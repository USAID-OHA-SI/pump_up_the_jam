## PROJECT:  Pump up the jam
## AUTHOR:   achafetz | USAID
## LICENSE:  MIT
## PURPOSE:  process partner files
## DATE:     2020-05-06
## UPDATED:  

# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(Wavelength)



# SETUP FOLDERS AND FILES -------------------------------------------------


  #unpack partner submission to a folder
    sub_folder <- "PartnerSubmissions" %>% 
      file.path("Data", .)
    
    if(!dir.exists(sub_folder)){
      dir.create(sub_folder, showWarnings = FALSE)
      
      unzip("Data/HFR_Partner_2020.04through2020.07.zip",
            exdir = sub_folder)
    }
  
  
  #list all parnter submission files
    p_files <- list.files(sub_folder, recursive = TRUE, full.names = TRUE)
  
  #folder for submission output to go
    prod_folder <- "PartnerProcessed" %>% 
      file.path("Dataout", .)
    
    if(!dir.exists(prod_folder))
      dir.create(prod_folder, showWarnings = FALSE)
  

# FUNCTION - PROCESSING HFR -----------------------------------------------

  #function for identifying the period (from the folder name) to filter for processing
    process_hfr <- function(filepath, out = "Dataout/PartnerProcessed"){
      
      pd <- filepath %>% 
        str_extract("(?<=2020\\.)[:digit:]{2}") %>% 
        as.numeric()
      
      hfr_process_template(filepath, round_hfrdate = TRUE, hfr_pd_sel = pd,
                           folderpath_output = out)
    }
  

# PROCESS HFR DATA AND STORE ----------------------------------------------

  #process and store all the submissions
    walk(p_files, process_hfr)
  

# IDENTIFY PARTNERS X MECHANISMS ------------------------------------------

    
    flag_mechs <- function(filepath){
      file <- basename(filepath)
      pd <- filepath %>% 
        str_extract("(?<=2020\\.)[:digit:]{2}") %>% 
        as.numeric() 
      ptnr <- str_extract(filepath, "Pact|PATH|EGPAF|Jhpiego|PSI|FHI360|JSI|RTC")
      
      hfr_import(filepath) %>% 
        distinct(mech_code, operatingunit) %>% 
        mutate(period = pd,
               partner = ptnr)
      
    }
    
    df_ptnr_mechs <- map_dfr(p_files, flag_mechs)
    
    df_ptnr_mechs %>% 
      mutate(exists = "X") %>% 
      spread(period, exists) %>% 
      arrange(partner, mech_code) %>% 
      print(n = Inf)
    
    df_ptnr_mechs %>% 
      filter(!is.na(mech_code)) %>% 
      write_csv("Dataout/HFR_CentralPartnerMechs.csv", na = "")
    
