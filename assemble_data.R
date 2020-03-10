## PROJECT:  Pump up the jam
## AUTHOR:   jdavis | USAID
## LICENSE:  MIT
## PURPOSE:  assemble MER and HFR data
## DATE:     2020-03-019
## UPDATED:

# things

start_date <- "2019-09-30"
weeks <- 12
hfr <- "C:/Users/Josh/Documents/data/fy20_q1_v1/hfr/HFR_Tableau_2020.03_20200122.csv"
datim_folder <- "C:/Users/Josh/Documents/data/fy20_q1_v1/hfr/mer"
out <- "C:/Users/Josh/Documents/data/fy20_q1_v1/hfr/out"
library(magrittr)
library(tidyverse)

# Step 1: stitch together MER data from API pull and create weeks

files <- list.files(datim_folder, full.names = TRUE)

df_datim <- purrr::map_dfr(.x = files,
                          .f = ~readr::read_csv(.x))

dates <- lubridate::as_date(start_date) %>% seq(by = 7, length.out = weeks)

df_datim_rpt <- purrr::map_dfr(.x = dates,
                               .f = ~dplyr::mutate(df_datim, date = .x))

rm(df_datim, dates)
  
# Step 2: read in HFR and parse

hfr <- readr::read_csv(hfr)

hfr <- hfr %>% 
  dplyr::select(-mer_results, -mer_targets)

# Step 3: merge MER targets and results back in

df  <- dplyr::left_join(hfr, df_datim_rpt)

# Step 4: write

filename <- paste0("merged_datim_hfr_", format(Sys.Date(), "%Y.%m.%d"), ".csv")

readr::write_csv(df_datim_rpt, file.path(out, filename))

