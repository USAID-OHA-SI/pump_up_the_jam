## PROJECT:  Pump up the jam
## AUTHOR:   T.Essam | USAID
## LICENSE:  MIT
## PURPOSE:  structure project folders & download data
## DATE:     2020-03-09
## UPDATED:  2020-6-25


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(glamr)
library(googledrive)

# Set global shortucts ----------------------------------------------------

  folder_setup()
  

# DOWNLOAD DATA -----------------------------------------------------------

  #OAuth
    drive_auth()
  
  #drive location
    fldr_id <- "18HBUdKSSk09oChhQanCOIbHK5IW8NE_K"

  #files
    files <- drive_ls(as_id(fldr_id), pattern = "View") %>% pull(name)

  #download
    walk(files, ~ import_drivefile(fldr_id, .x))


