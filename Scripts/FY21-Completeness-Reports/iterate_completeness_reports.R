## PROJECT: pump_up_the_jam
## AUTHOR:  K. Srikanth | USAID
## PURPOSE: iterate over all the countries to run individual reports
## DATE:    2020-10-19


#Dependencies
library(tidyverse)
library(vroom)
library(glamr)
library(glue)
library(gophr)
library(scales)
library(lubridate)
library(skimr)
library(patchwork)
library(ggrepel)
library(glitr)
library(janitor)
library(extrafont)
library(ggtext)
library(gluedown)
library(knitr)
library(rmarkdown)
library(here)

#load your s3 creds
#setwd("C:/Users/STAR/Documents/Data")
load_secrets()

#inputs
prefix <- "HFR_Tableau" #other two options for file types - HFR_Submission, Detailed
outputfolder <- "C:/Users/STAR/Documents/Data"

#identify latest file
latest_file  <- glamr::s3_objects(
  bucket = "gov-usaid",
  prefix = glue::glue("ddc/uat/processed/hfr/outgoing/{prefix}")
) %>%
  dplyr::filter(last_modified == max(last_modified)) %>%
  dplyr::pull(key)

#download file
glamr::s3_download(
  bucket = "gov-usaid",
  object = latest_file,
  filepath = file.path(outputfolder, basename(latest_file))
)

#read in file
df <- vroom(file.path(outputfolder, basename(latest_file)))

# AGGREGATE AND MUNGE -----------------------------------------------------

#clean period name for viz later
if("hfr_pd" %in% names(df))
  df <- mutate(df, hfr_pd = paste0(fy, ".", str_pad(hfr_pd, 2, pad = "0")))

df %>% 
  glimpse()

# CREATE DF FOR OU ---------------------------------------------------
df_ag_ou <- df %>%
  mutate_at(c("mer_results", "val"), ~replace(., is.na(.), 0)) %>% 
  group_by(operatingunit, countryname, orgunituid,
           fy, hfr_pd, date,
           mech_code, expect_reporting,
           indicator) %>% 
  summarise_at(vars(mer_results, hfr_results = val), sum, na.rm = TRUE) %>%
  ungroup()

#specify the country parameter
country <- unique(df_ag_ou$operatingunit)

#create output folder for reports
reports <- tibble(
  output_file = stringr::str_c("output/", country, "-markdown-HFR-completeness.html"),
  params = map(country, ~list(country = .))
)

#check output folder
head(reports)

#create markdown folder under wd if it doesn't exist
if(dir.exists("markdown") == FALSE) {
  dir.create("markdown")
}

#delete old files
unlink("markdown/*")

#create reports
reports %>%
  pwalk(render, 
        input = here("Scripts/FY21-Completeness-Reports/FY21_Completeness_reports.Rmd"))

