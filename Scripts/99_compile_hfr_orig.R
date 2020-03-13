## PROJECT:  Pump up the jam
## AUTHOR:   jdavis | USAID
## LICENSE:  download pds1-4 hfr processed data, clean for current pd, stitch
## PURPOSE:  structure project folders
## DATE:     2020-03-13
## UPDATED: 

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

  rm(hfr1, hfr2, hfr3, hfr4) 
  
# compare 'processed' folder with 'fixes/late submissions'
  # explore comparing the pd* lists with the late/fixes
  
  
  
  
  
  


