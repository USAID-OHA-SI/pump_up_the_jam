# PURPOSE: Munge and Analysis of HFR Data
# AUTHOR: Tim Essam | SI
# LICENSE: MIT
# DATE: 2022-06-14
# NOTES: For HFR Assessment Report; 200 series in the script run sequence


# LOCALS & SETUP ============================================================================

# Libraries
library(glitr)
library(glamr)
library(gisr)
library(gophr)
library(tidyverse)
library(scales)
library(sf)
library(extrafont)
library(tidytext)
library(gt)
library(gtExtras)
library(paletteer)
library(googlesheets4)


# SI specific paths/functions  
load_secrets()
file_path <- return_latest(folderpath = "Data",
                           pattern = "HFR_Tableau_SQLview")

# Grab metadata
file.info(file_path)

# Used to reduce hfr universe to only expected reporting and then count  
  filter_count <- function(.data, ...){
    .data %>% 
      filter(expect_reporting == TRUE) %>% 
      distinct(...) %>% 
      nrow()
  }

  # Used to calculate share of sites within an indicator
  calc_share <- function(x){
    ifelse(!is.na(x), (x / sum(x, na.rm = T)), NA) %>% 
    scales::percent(., 1) %>% 
    replace_na(., "-")
  }

  # Calculate a running share for a column
  running_share <- function(x){
    ifelse(!is.na(x), (cumsum(x) / sum(x, na.rm = T)), NA)
  }


# Embiggen font size
  embiggen <- function(gt_obj){
    gt_obj %>% 
      gt::tab_options(
        source_notes.font.size = 10,
        table.font.size = 12,
        footnotes.font.size = 10,
        data_row.padding = gt::px(2)
        )
  }

# LOAD DATA ============================================================================  

  df <- vroom::vroom(file_path) %>% 
      mutate(mech_code = as.character(mech_code))
  
  
  mech_sheet_url <- "https://docs.google.com/spreadsheets/d/1agtg8ESXgkD-chh_9k1zlnhxaeT_R6OgjDpmxWdNhYc/edit#gid=0" 
  mech_df <- read_sheet(ss = mech_sheet_url)

  # Create a crosswalk to merge in clean'ish IP names
  mech_cw <- mech_df %>% 
    select(mech_code, ip_name_clean)

  # Merge the cleanish ip names in with df
  df <- df %>% 
    left_join(., mech_cw)

# MUNGE ============================================================================

  # How many ous does USAID PEPFAR serve?
  ou_count <- 
    pepfar_country_list %>% 
    distinct(operatingunit) %>% 
    nrow()

  # How many countries are supported across all these OUs?
  cntry_count <- 
    pepfar_country_list %>% 
    distinct(country) %>% 
    nrow()

  
  # How many unique mechanisms?
  mech_count <- df %>% 
    filter_count(mech_code)
  
  
  # How many IPs -- assuming IP clean names are correct
  ip_count <- df %>% 
    filter(expect_reporting == TRUE) %>% 
    distinct(ip_name_clean, mech_code) %>%
    count(ip_name_clean) %>% 
    arrange(desc(n)) %>% 
    nrow()
  
    # How many sites are IPs responsible for?
  df %>% 
    filter(expect_reporting == TRUE) %>% 
    distinct(ip_name_clean, orgunituid, operatingunit, countryname) %>% 
    count(ip_name_clean, countryname, operatingunit) %>% 
    arrange(desc(n)) %>% 
    write_csv("Dataout/hfr_ip_site_count.csv")
  
  ip_mechs <- 
    df %>% 
    filter(expect_reporting == TRUE) %>% 
    distinct(ip_name_clean, orgunituid, operatingunit) 
  
  
# Question: How many Mechs do we need to report to hit 80% of sites? 
  # ~ 66 / 261
# Question: How many Partners do we need?
  # 27 / 121
    
  df %>% 
    filter(expect_reporting == TRUE) %>% 
    distinct(ip_name_clean, orgunituid, mech_code, operatingunit) %>%
    count(ip_name_clean) %>% 
    arrange(desc(n)) %>% 
    mutate(share = n/sum(n), 
           running_sum = cumsum(share)) %>% 
    prinf()
  
  ip_mech_count <- 
    ip_mechs %>% 
    summarise(tot = sum(n)) %>% 
    pull()
  
  # Appears there are some mechs reporting at same sites, how many of these?
  df %>% 
    filter(expect_reporting == TRUE) %>% 
    distinct(ip_name_clean, orgunituid) %>% 
    group_by(orgunituid) %>% 
    mutate(count = n()) %>% 
    arrange(desc(count))
  
  df %>% filter(orgunituid =="mszioSxSYMT") %>% 
    count(ip_name_clean, mech_code, indicator) %>% prinf()
  
  # What is the share of indicators/sites a partner is responsible for?
  df %>% 
    distinct(ip_name_clean, orgunituid, indicator) %>% 
    count(indicator, ip_name_clean) %>% 
    pivot_wider(names_from = indicator,
                values_from = n) %>% 
    arrange(desc(HTS_TST))
  
  # # Heatmap of this
  #   ggplot(aes(x = operatingunit, y = ip_name_clean, fill = n)) +
  #   geom_tile() +
  #   facet_wrap(~operatingunit, scales = "free")
  
  
  # What is the total combinations of indicators reported?
  # TODO: Clean up the combos here -- some "Unknown Age" entries floating around
  # As well as some PrEP entries with other disaggregate fields (was data mis-entered?)
  df %>% 
    distinct(indicator, agecoarse, sex, otherdisaggregate) %>% 
    write_csv("Dataout/hfr_indic_flow.csv")
  
  # Total data points in a monthg
  df %>% 
    distinct(indicator, agecoarse, sex, otherdisaggregate, ip_name_clean, orgunituid)
  
  

# Summarize how sites that are expected to report are reporting at least 1 indicator by OU
site_count <- 
  df %>% 
  filter_count(orgunituid)


# Per OU, how many sites are reporting under each indicator?
# This can be toggled to include sex + age blowing up the numbers even more
ou_rpt_tble <- 
  df %>% 
  filter(expect_reporting == TRUE) %>% 
  distinct(operatingunit, indicator, orgunituid) %>% 
  #distinct(operatingunit, indicator, orgunituid, agecoarse, sex) %>% 
  count(indicator, operatingunit) %>% 
  pivot_wider(names_from = indicator, values_from = n) %>% 
  arrange(desc(HTS_TST))

ou_rpt_tble_sh <- 
  ou_rpt_tble %>% 
  mutate(across(where(is.numeric), .f = calc_share, .names = "{.col}_sh")) %>% 
  mutate(across(where(is.numeric), .f = running_share, .names = "{.col}_run_sh"))


# What is the running share of sites expected to report?
ou_rpt_tbl_sh

ou_rpt_tble_sh %>% 
  select(-contains("_run")) %>% 
  gt() %>%
  sub_missing(everything(), 
              missing_text = "-") %>% 
  grand_summary_rows(
    columns = where(is.integer), 
    fns = list(
      `ALL OUs` = ~sum(., na.rm = T)
    ),
    formatter = fmt_number,
    decimals = 0,
  ) %>% 
  fmt_number(columns = where(is.integer), sep_mark = ",", decimals = 0) %>% 
  gt_merge_stack(col1 = HTS_TST, col2 = HTS_TST_sh) %>% 
  gt_merge_stack(col1 = HTS_TST_POS, col2 = HTS_TST_POS_sh) %>% 
  gt_merge_stack(col1 = PrEP_NEW, col2 = PrEP_NEW_sh) %>% 
  gt_merge_stack(col1 = TX_CURR, col2 = TX_CURR_sh) %>% 
  gt_merge_stack(col1 = TX_MMD, col2 = TX_MMD_sh) %>% 
  gt_merge_stack(col1 = TX_NEW, col2 = TX_NEW_sh) %>% 
  gt_merge_stack(col1 = VMMC_CIRC, col2 = VMMC_CIRC_sh) %>% 
  embiggen() %>% 
  tab_header(title = glue::glue("SITES ELIGIBLE FOR REPORTING HFR INDICATORS"),
             subtitle = "Four operating units support over 50% of the total HFR sites")
  
  gtsave(., "Images/test.png")

# Just out of curiosity, how many indicators + sites do we expect in single period?
ou_rpt_tble_lng <- 
  ou_rpt_tble %>% 
  select(operatingunit, where(is.d)) %>% 
  pivot_longer(cols = where(is.integer), 
               values_to = "total",
               names_to = "indicator")

site_indic_ban <- 
  ou_rpt_tble_lng %>% 
  summarise(tot = sum(total, na.rm = T)) %>% 
  pull()

# What does a waffle graph look like of this?
library(waffle)

p <- ou_rpt_tble_lng %>% 
  filter(indicator == "HTS_TST_POS") %>% 
  ggplot(., aes(fill = operatingunit, values = total)) +
  geom_waffle(color = "white", size = .25, n_rows = 10, flip = TRUE) +
  facet_wrap(~operatingunit, nrow = 1, strip.position = "bottom") +
  theme(legend.position = "none")

# Question becomes: what is the best way to communicate this explosion?

