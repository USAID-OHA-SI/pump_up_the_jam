# PROJECT:  pump up the jam
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  review completeness for FY21
# REF ID:   335f52c3 
# LICENSE:  MIT
# DATE:     2022-10-31
# UPDATED:  2022-11-03

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
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  ref_id <- "335f52c3"

# IMPORT ------------------------------------------------------------------
  
  df_hfr <- return_latest("Data", "Tableau") %>% 
    hfr_read() %>% 
    filter(between(date, as.Date("2020-10-01"), as.Date("2021-09-01")))

# MUNGE -------------------------------------------------------------------

  
  df_agg <- df_hfr %>% 
    rename(hfr_results = val) %>% 
    filter(expect_reporting == TRUE) %>% 
    mutate(has_hfr_reporting = !is.na(hfr_results)) %>% 
    group_by(countryname, date, orgunituid, mech_code, indicator) %>% 
    summarise(hfr_results = sum(hfr_results, na.rm = TRUE),
              has_hfr_reporting = max(has_hfr_reporting, na.rm = TRUE),
              expect_reporting = max(expect_reporting, na.rm = TRUE),
              .groups = "drop") %>% 
    mutate(across(c(has_hfr_reporting, expect_reporting), as.logical))
  
  df_agg <- df_agg %>% 
    group_by(countryname, date) %>% 
    summarise(has_hfr_reporting = sum(has_hfr_reporting),
              site_mech_ind_combos = n(),
              .groups = "drop") %>% 
    mutate(completeness = has_hfr_reporting /site_mech_ind_combos)

  df_viz <- df_agg %>% 
    mutate(month = format(date, "%b") %>% fct_inorder,
           countryname = recode(countryname,
                                "Democratic Republic of the Congo" = "DRC",
                                "Papua New Guinea" = "PNG",
                                "Dominican Republic" = "DR")) 
    
  
  
  mean(df_viz$completeness, na.rm = TRUE)
  
  df_viz %>% 
    mutate(completeness = na_if(completeness, 0)) %>% 
    ggplot(aes(month, fct_reorder(countryname, site_mech_ind_combos, max),
               fill = completeness)) +
    geom_tile(color = "white") +
    geom_text(aes(label = percent(completeness, 1)),
              family = "Source Sans Pro", color = "white", size = 6/.pt) +
    scale_x_discrete(expand = c(.005, .005), position = "top") +
    scale_y_discrete(expand = c(.005, .005)) +
    # scale_fill_viridis_c(option = "C", direction = -1) +
    scale_fill_si("denims", reverse = FALSE, na.value = "white",
                  label = percent) +
    labs(x = NULL, y = NULL, fill = "Completeness",
         title = glue("The average reporting completess across countries for FY21 was {percent(mean(df_viz$completeness, na.rm = TRUE))}") %>% toupper,
         subtitle = "Reporting completeness for all indicators, mechanisms, and sites",
         caption = glue("Source: HFR Tableau Output FY21 | Ref ID: {ref_id}")) +
    si_style_nolines() +
    theme(legend.position = "none",
          axis.text.y = element_text(size = 6))
  
  si_save("Images/FY21_HFR_completeness_by_cntry.png",
          width = 9.27)  
  
  df_avg <- df_viz %>% 
    group_by(date) %>% 
    summarise(completeness = sum(has_hfr_reporting, na.rm = TRUE) / sum(site_mech_ind_combos, na.rm = TRUE),
              .groups = "drop") %>% 
    mutate(month = format(date, "%b") %>% fct_inorder )
    
  df_viz %>% 
    ggplot(aes(month, completeness,
               color = completeness)) +
    geom_point(aes(size = site_mech_ind_combos), alpha = .2,
               position = position_jitter(width = .1, height = .01, seed = 42)) +
    # geom_point(data = df_avg, size = 12, shape = 15) +
    geom_point(data = df_avg, size = 15, shape = 18) +
    geom_text(data = df_avg, color = "white", size = 11/.pt,
               aes(label = percent(completeness, 1))) +
    scale_y_continuous(label = percent) +
    scale_color_si("denims", reverse = TRUE, na.value = "white",
                  label = percent) +
    coord_cartesian(clip = "off") +
    expand_limits(y = 1) +
    labs(x = NULL, y = NULL, fill = "Completeness",
         title = glue("The average reporting completess across countries for FY21 was {percent(mean(df_viz$completeness, na.rm = TRUE))}") %>% toupper,
         subtitle = "Circles represent each country's submission completeness, proportionally sized to the number of expected reporting points",
         caption = glue("Source: HFR Tableau Output FY21 | Ref ID: {ref_id}")) +
    si_style_ygrid() +
    theme(legend.position = "none")

  si_save("Images/FY21_HFR_completeness.png",
          width = 9.27)  

  