## PROJECT:  Pump up the jam
## AUTHOR:   jdavis, achafetz | USAID
## LICENSE:  download pds1-4 hfr processed data, clean for current pd, stitch
## PURPOSE:  structure project folders
## DATE:     2020-03-13
## UPDATED:  2020-03-17

# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(vroom)


# GLOBALS -----------------------------------------------------------------

  zipfolder <- "Data/HFR_Processed.zip"
  unzipfolder <- "Data/HFR_Processed"

  data_folder <- "Data/HFR_Processed"
  out_folder <- "Data"



# REMOVE DUPLICATES -------------------------------------------------------

    
  #table of all files
    df_files <- unzip(zipfolder, list = TRUE) %>%
      pull(Name) %>%
      tibble(filepath = .)

  #extract info from filenames for identifying dups
    df_files <- df_files %>%
      mutate(ou = str_extract(filepath, "(?<=[:digit:]{2}_)[:upper:]{3}"),
             mech_code = str_extract(filepath, "[:digit:]{5,6}"),
             submission_pd = dirname(filepath),
             hfr_pd = str_extract(filepath, "(?<=\\.)[:digit:]{2}") %>%
               str_remove("^0"),
             date = str_extract(filepath, "[:digit:]{8}"))

  #flag dups, keep newest
    dups <- df_files %>%
      group_by(submission_pd, hfr_pd, ou, mech_code) %>%
      filter(n() > 1) %>%
      ungroup() %>%
      arrange(submission_pd, mech_code, date) %>%
      group_by(submission_pd, mech_code) %>%
      mutate(action = ifelse(date == max(date), "keep", "delete")) %>%
      ungroup() %>%
      print(n = Inf)

  #unzip files
    dir.create(unzipfolder)
    unzip(zipfolder, exdir = unzipfolder)

  #delete duplicates
    dups %>%
      filter(action == "delete") %>%
      pull(filepath) %>%
      paste0(unzipfolder, "/", .) %>%
      unlink()

  
# IMPORT FUNCTION ---------------------------------------------------------

  import_processed <- function(path){
    
    df <- vroom(path, col_types = c(.default = "c"))
    
    pd <- str_extract(path, "(?<=\\.)[:digit:]{2}") %>% str_remove("^0")
    
    df <- filter(df, hfr_pd == pd)
    
    invisible(df)
  }


# IMPORT HFR FILES --------------------------------------------------------

  #all files
    files <- list.files(data_folder, recursive = TRUE, full.names = TRUE) 
  
  #HFR orig submissions for Q1
    files_norm <- str_subset(files, pattern = "FIXES|2020.05", negate = TRUE)
  
  #import
    df_hfr <- map_dfr(files_norm, import_processed)


# IMPORT FIXES & REPLACE --------------------------------------------------

  #HFR fix/late files
    files_fixes <- files %>% 
      str_subset(pattern = "FIXES") %>% 
      str_subset(pattern = "2020.05", negate = TRUE)

  #import replacements
    df_fix <- map_dfr(files_fixes, import_processed)


  #create df for anti join to replace any existing
    df_replacements <- files_fixes %>% 
      tibble(filepath = .) %>% 
      mutate(mech_code = str_extract(filepath, "[:digit:]{5,6}"),
             hfr_pd = str_extract(filepath, "(?<=\\.)[:digit:]{2}") %>% 
               str_remove("^0"))

  #remove and add in replacements
    df_full <- df_hfr %>% 
      anti_join(df_replacements) %>% 
      bind_rows(df_fix)


# EXPORT ------------------------------------------------------------------


  filename <- paste0("hfr_pd1_pd4_clean_fixed_", format(Sys.Date(), "%Y%m%d"), ".csv")
  write_csv(df_full, file.path(out_folder, filename))

  
  unlink(unzipfolder, recursive = TRUE)
  
  


