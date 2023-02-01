# PROJECT:  pump up the jam
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  treatment interuptions
# REF ID:   ce6b4a53 
# LICENSE:  MIT
# DATE:     2023-01-31
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(gagglr)
  library(glue)
  library(scales)
  library(extrafont)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(Wavelength)
  library(lubridate)
  library(RColorBrewer)

# GLOBAL VARIABLES --------------------------------------------------------
  
  ref_id <- "ce6b4a53" #id for adorning to plots, making it easier to find on GH
  
  source_date <- file.info(return_latest("Data", "GLOBAL_DATIM"))$ctime %>% format("%Y-%m-%d")

# IMPORT ------------------------------------------------------------------
  
  #https://drive.google.com/file/d/1wzCdAfMZ_VW6ekzr3TWcZYCP7-VzZFL4/view?usp=share_link
  df_hfr <- return_latest("Data", "Tableau") %>% 
    hfr_read() 
  
  df_mer <- return_latest("Data", "GLOBAL_DATIM") %>% 
    read_csv(col_types = list(fy = "i",
                              mer_results = "d",
                              .default = "c"
    )) 
  
  

# MUNGE -------------------------------------------------------------------

  #limit to TX_CURR and latest year
  df_hfr <- df_hfr %>% 
    filter(between(date, as.Date("2021-10-01"), as.Date("2022-09-01")),
           indicator == "TX_CURR")
  
  #aggregate to site level, id if reporting occurred (ie non-zero)
  df_hfr_agg <- df_hfr %>% 
    rename(hfr_results = val) %>% 
    mutate(has_hfr_reporting = !is.na(hfr_results)) %>% 
    group_by(countryname, date, orgunituid, mech_code, indicator) %>% 
    summarise(hfr_results = sum(hfr_results, na.rm = TRUE),
              # mer_results = sum(mer_results, n.rm = TRUE),
              has_hfr_reporting = max(has_hfr_reporting, na.rm = TRUE),
              expect_reporting = max(expect_reporting, na.rm = TRUE),
              .groups = "drop") %>% 
    mutate(across(c(has_hfr_reporting, expect_reporting), as.logical),
           # has_mer_reporting = !is.na(mer_results)
           hfr_results = ifelse(has_hfr_reporting == FALSE, NA, hfr_results)
    )
  
  
# DATE MAPPING ------------------------------------------------------------
  
  #mapping between quarter and date
  df_date_map <- df_mer %>% 
    distinct(period) %>% 
    mutate(date = map(period, convert_qtr_to_date)) %>% 
    unnest(date)
  
# MUNGE COMPLETENESS ------------------------------------------------------
  
  #aggregate mer to determine full set of site x im expected reporting by qtr
  df_mer_agg <- df_mer  %>%
    filter(indicator == "TX_CURR") %>% 
    left_join(df_date_map, by = "period") %>% 
    count(countryname, date, orgunituid, mech_code, indicator, 
          wt = mer_results, name = "mer_results")
  
  #replicate quarterly mer results for each month of qtr
  df_expect_reporting <- df_mer_agg %>% 
    bind_rows(mutate(df_mer_agg, date = date %m+% months(1))) %>% 
    bind_rows(mutate(df_mer_agg, date = date %m+% months(2))) %>% 
    arrange(countryname, indicator, date)

# COMBINE -----------------------------------------------------------------

  #limit HFR reporting to only where there are MER values reported
  df_comp <- df_expect_reporting %>% 
    tidylog::left_join(df_hfr_agg) %>% 
    mutate(has_hfr_reporting = ifelse(is.na(has_hfr_reporting), FALSE, has_hfr_reporting))
  
  # df_agg %>% 
  #   tidylog::anti_join(df_expect_reporting)

  #covert 0 to NA
  df_comp <- df_comp %>% 
    mutate(hfr_results = na_if(hfr_results, 0))
  
  #net_new
  df_comp <- df_comp %>% 
    select(-expect_reporting) %>% 
    group_by(orgunituid, mech_code, indicator) %>% 
    mutate(hfr_netnew = hfr_results - lag(hfr_results, order_by = date),
           hfr_delta = hfr_netnew / lag(hfr_results, order_by = date)) %>% 
    ungroup() %>% 
    relocate(has_hfr_reporting, .before = mer_results)

  df_comp %>% 
    filter(orgunituid == "CtzbCWmPy0K")
  
  
  df_tza_orgs <- df_mer %>%
    filter(countryname == "Tanzania") %>% 
    distinct(orgunituid, snu1, psnu, orgunit)
  
  
  v_snu_ordered <- df_tza %>% 
    filter(date == max(date)) %>% 
    count(snu1, wt = mer_results, sort = TRUE) %>% 
    pull(snu1)
  
  v_top_sites <- df_tza %>% 
    filter(date == max(date)) %>% 
    slice_max(order_by = mer_results, n = 20) %>% 
    pull(orgunituid)
  
  df_tza <- df_comp %>% 
    filter(countryname == "Tanzania") %>% 
    left_join(df_tza_orgs, by = "orgunituid")
  
  
  df_tza <- df_tza %>% 
    filter(date != "2021-10-01") %>% 
    mutate(date_viz = format(date, "%b %Y") %>% fct_inorder %>% fct_rev,
           snu1 = factor(snu1, v_snu_ordered),
           direction = ifelse(hfr_netnew <0, "neg", "pos"),
           largest_sites = orgunituid %in% v_top_sites)
           
 df_tza %>% 
    ggplot(aes(hfr_delta, date_viz, size = mer_results, color = hfr_delta)) +
    geom_point(position = position_jitter(height = .2, seed = 42), 
               na.rm = TRUE,
               alpha = .6) +
   # geom_point(data = filter(df_tza, largest_sites == TRUE), 
   #            position = position_jitter(height = .2, seed = 42),
   #            shape = 21, color = matterhorn, fill = NA,
   #            na.rm = TRUE,
   #            alpha = .6) +
    geom_vline(xintercept = 0) +
    facet_wrap(~snu1) +
    scale_x_continuous(label = percent,
                       limit=c(-2,2),
                       oob=squish) +
    scale_color_stepsn(colors = brewer.pal(5, "BrBG"),
                       limits = c(-2, 2),
                       breaks = c(-.25, -.05, .05, .25),
                       na.value = grey10k, 
                       labels = percent) +
    labs(y = NULL, x = "change from prior period(%)",
         size = "TX_CURR (MER)", color = NULL) +
    si_style_xgrid()

 
 df_tza_tot <- df_tza %>% 
   filter(!is.na(direction)) %>% 
   count(countryname, date_viz, direction, wt = hfr_netnew, name = "hfr_netnew") %>% 
   group_by(date_viz) %>% 
   mutate(hfr_netnew_tot = sum(hfr_netnew),
          hfr_netnew_tot = case_when(row_number() == 1 ~ hfr_netnew_tot)) %>% 
   ungroup()

 
 df_tza_tot %>% 
   filter(!is.na(direction)) %>% 
   ggplot(aes(hfr_netnew, date_viz, fill = direction)) +
   geom_col(na.rm = TRUE, alpha = .5) +
   geom_vline(xintercept = 0) +
   geom_errorbar(aes(xmin = hfr_netnew_tot, xmax = hfr_netnew_tot), color = "white", na.rm = TRUE) +
   scale_x_continuous(label = comma) +
   scale_fill_manual(values = c("neg" = brewer.pal(5, "BrBG")[1],
                                "pos" = brewer.pal(5, "BrBG")[5])) +
   labs(y = NULL, x = "change from prior period(%)",
        size = "TX_CURR (MER)", color = NULL) +
   si_style_xgrid() +
   theme(legend.position = "none")


  df_tza %>% 
    filter(orgunituid %in% v_top_sites) %>% 
    group_by(orgunit) %>% 
    mutate(fill_alpha = case_when((abs(lag(hfr_delta, order_by = date)) >= .05 |
                                   abs(hfr_delta) >= .05 |
                                   abs(lead(hfr_delta, order_by = date)) >= .05) ~ 1, 
                                  is.na(hfr_delta) ~ .05,
                                  TRUE ~ .5)) %>% 
    ungroup() %>% 
    mutate(fill_shp = ifelse(direction == "pos", 24, 23),
           month = format(date, "%b") %>% fct_inorder) %>% 
    ggplot(aes(month, orgunit, fill = hfr_delta)) +
    geom_tile(color = "white") +
    geom_text(aes(label = comma(hfr_results), alpha = fill_alpha),
              family = "Source Sans Pro", color = matterhorn) +
    geom_vline(xintercept = c(2.5, 5.5, 8.5), color = matterhorn) +
    scale_fill_stepsn(colors = brewer.pal(5, "BrBG"),
                       limits = c(-2, 2),
                       breaks = c(-.25, -.05, .05, .25),
                       na.value = grey10k, 
                       labels = percent) +
    scale_x_discrete(position = "top") +
    scale_alpha_identity() +
    labs(x = NULL, y = NULL,
         subtitle = "Are there any major interruptions in the largest sites?",
         caption = glue("Source: FY22 HFR Data [{source_date}]")) +
    si_style_nolines() +
    theme(legend.position = "none")
  
  
  df_tza %>% 
    filter(orgunituid %in% v_top_sites) %>% 
    mutate(fill_shp = ifelse(direction == "pos", 24, 25)) %>% 
    mutate(month = format(date, "%b") %>% fct_inorder) %>% 
    ggplot(aes(month, orgunit, fill = hfr_delta)) +
    geom_vline(xintercept = c(2.5, 5.5, 8.5), color = matterhorn) +
    geom_point(aes(shape = fill_shp), color = "white", size = 4) +
    # geom_tile(color = "white") +
    # geom_text(aes(label = comma(hfr_results)),
    #           family = "Source Sans Pro", color = matterhorn) +
    scale_fill_stepsn(colors = brewer.pal(5, "BrBG"),
                      limits = c(-2, 2),
                      breaks = c(-.25, -.05, .05, .25),
                      na.value = grey10k,
                      labels = percent) +
    scale_shape_identity() +
    scale_x_discrete(position = "top") +
    labs(x = NULL, y = NULL) +
    si_style_nolines()
  