## PROJECT:  Pump up the jam
## AUTHOR:   jdavis | USAID
## LICENSE:  download pds1-4 hfr processed data, clean for current pd, stitch
## PURPOSE:  structure project folders
## DATE:     2020-03-13
## UPDATED:  To do: clean up and functionalize

# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(Wavelength)


# GLOBALS -----------------------------------------------------------------

pd1 <- "Data/old/2020.01"
pd2 <- "Data/old/2020.02"
pd3 <- "Data/old/2020.03"
pd4 <- "Data/old/2020.04"

date_pd1 <- c("2019-09-30", "2019-10-07", "2019-10-14", "2019-10-21")
out_folder <- "Dataout"

# check pds ---------------------------------------------------------------

# create date / pd lists

# pd1

  files <- list.files(pd1, "HFR", full.names = TRUE)

  hfr1 <- map_dfr(.x = files,
                .f = ~readr::read_csv(.x, col_types = c(.default = "c")))

  hfr1 <- hfr1 %>% 
    filter(hfr_pd == "1")

# pd2

  files <- list.files(pd2, "HFR", full.names = TRUE)

  hfr2 <- map_dfr(.x = files,
                .f = ~readr::read_csv(.x, col_types = c(.default = "c")))

  hfr2 <- hfr2 %>% 
    filter(hfr_pd == "2")
  
# pd3
  
  files <- list.files(pd3, "HFR", full.names = TRUE)
  
  hfr3 <- map_dfr(.x = files,
                  .f = ~readr::read_csv(.x, col_types = c(.default = "c")))
  
  hfr3 <- hfr3 %>% 
    filter(hfr_pd == "3")
  
# pd4
  
  files <- list.files(pd4, "HFR", full.names = TRUE)
  
  hfr4 <- map_dfr(.x = files,
                  .f = ~readr::read_csv(.x, col_types = c(.default = "c")))
  
  hfr4 <- hfr4 %>% 
    filter(hfr_pd == "4")
  
# bind
  
  df_hfr <- bind_rows(hfr1, hfr2, hfr3, hfr4) %>% 
    mutate(val = as.numeric(val))

  rm(hfr1, hfr2, hfr3, hfr4, pd1, pd2, pd3, pd4) 

# compare late fixes
  
  # where are the fixes
  late_data <- "Data/late"

  late_files <- list.files(late_data, "HFR", full.names = TRUE)

 # function to create fixed file with only appropriate pds
  
  fixie <- function(file) {
    
    #1 get mech names
    
      mech <- file %>% 
        basename() %>% 
        str_extract("[:digit:]{5,6}")
      
      #2 get intended pd #
      
      pd <- file %>% 
        basename() %>% 
        str_extract("(?<=\\.)[:digit:]{2}") %>% 
        str_remove("^0")
      
      # read in and filter for only data from submitted pd
      
      df <- readr::read_csv(file, col_types = c(.default = "c")) %>% 
      filter(mech_code %in% mech & hfr_pd %in% pd) %>% 
        mutate(val = as.numeric(val))
      
      return(df)
    
  }
  
  
  # read in fix file
  df_fix <- map_dfr(.x = late_files,
                    .f = ~fixie(.x)) 
    
  # filter master where there are files from fixed
  
  #late mechs
  mech <- late_files %>% 
    basename() %>% 
    str_extract("[:digit:]{5,6}")
  
  pd <- late_files %>% 
    basename() %>% 
    str_extract("(?<=\\.)[:digit:]{2}") %>% 
    str_remove("^0")
  
  
  # remove from master mech/pd combos that are present in the fixes
  df_hfr_sub <- df_hfr %>% 
    filter(!(hfr_pd %in% pd & mech_code %in% mech))
 
    ###
  
  df_master <- bind_rows(df_fix, df_hfr_sub)
  
  
  # write file
  
  filename <- paste0("hfr_pd1_pd4_clean_fixed", format(Sys.Date(), "%Y%m%d"), ".csv")
  write_csv(df_hfr, file.path(out_folder, filename))

  
  
  
  


