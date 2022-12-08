# PROJECT:  pump up the jam
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  review completeness
# REF ID:   335f52c3 
# LICENSE:  MIT
# DATE:     2022-10-31
# UPDATED:  2022-12-08

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
  
  #https://drive.google.com/file/d/1wzCdAfMZ_VW6ekzr3TWcZYCP7-VzZFL4/view?usp=share_link
  df_hfr <- return_latest("Data", "Tableau") %>% 
    hfr_read() 

# MUNGE -------------------------------------------------------------------

  df_hfr <- df_hfr %>% 
    filter(between(date, as.Date("2021-10-01"), as.Date("2022-09-01")))
  
  df_agg <- df_hfr %>% 
    rename(hfr_results = val) %>% 
    filter(expect_reporting == TRUE) %>% 
    mutate(has_hfr_reporting = !is.na(hfr_results)) %>% 
    group_by(countryname, date, orgunituid, mech_code, indicator) %>% 
    summarise(hfr_results = sum(hfr_results, na.rm = TRUE),
              mer_results = sum(mer_results, n.rm = TRUE),
              has_hfr_reporting = max(has_hfr_reporting, na.rm = TRUE),
              expect_reporting = max(expect_reporting, na.rm = TRUE),
              .groups = "drop") %>% 
    mutate(across(c(has_hfr_reporting, expect_reporting), as.logical),
           has_mer_reporting = !is.na(mer_results))

# MUNGE COMPLETENESS ------------------------------------------------------

  df_comp <- df_agg %>% 
    group_by(countryname, date) %>% 
    summarise(has_hfr_reporting = sum(has_hfr_reporting),
              site_mech_ind_combos = n(),
              .groups = "drop") %>% 
    mutate(completeness = has_hfr_reporting /site_mech_ind_combos)
  
  df_comp_viz <- df_comp %>% 
    mutate(month = format(date, "%b") %>% fct_inorder,
           countryname = recode(countryname,
                                "Democratic Republic of the Congo" = "DRC",
                                "Papua New Guinea" = "PNG",
                                "Dominican Republic" = "DR")) 
  

# MUNGE CORRECTNESS -------------------------------------------------------

  df_corr <- df_agg %>% 
    filter(indicator %ni% c("TX_CURR", "TX_MMD")) %>% 
    bind_rows(df_agg %>% 
                filter(indicator %in% c("TX_CURR", "TX_MMD"),
                       date == max(date))) %>% 
    select(-starts_with("mer")) %>% 
    mutate(fy = unique(df_hfr$fy)) %>% 
    # filter(has_mer_reporting == TRUE) %>% 
    count(countryname, fy, indicator, wt = hfr_results, name = "hfr_results")

  df_targets_ou <- df_hfr %>% 
    filter(date == max(date),
           indicator != "TX_MMD") %>% 
    group_by(countryname, indicator) %>% 
    summarise(across(starts_with("mer"), sum, na.rm = TRUE),
              .groups = "drop") %>% 
    filter(!(mer_targets == 0 & mer_results == 0))
  
  df_targets_ou <- df_targets_ou %>% 
    bind_rows(df_targets_ou %>% 
                filter(indicator == "TX_CURR") %>% 
                mutate(indicator = "TX_MMD"))
  
  df_comp_ou_full_fy <- df_agg %>% 
    group_by(countryname, indicator) %>% 
    summarise(has_hfr_reporting = sum(has_hfr_reporting),
              site_mech_ind_combos = n(),
              .groups = "drop") %>% 
    mutate(completeness = has_hfr_reporting /site_mech_ind_combos)
  
  df_corr_combo <- df_corr %>% 
    full_join(df_targets_ou, by = c("countryname", "indicator")) %>% 
    relocate(mer_targets, .after = -1) %>% 
    mutate(correctness = hfr_results / mer_results) %>% 
    full_join(df_comp_ou_full_fy, by = c("countryname", "indicator"))
  
  
# VIZ - COMPLETENESS: COUNTRY HEATMAP -------------------------------------

  curr_fy <- unique(df_hfr$fy) %>% str_replace("20", "FY")
  
  df_comp_viz %>% 
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
         title = glue("The average reporting completeness across countries for {curr_fy} was {percent(mean(df_comp_viz$completeness, na.rm = TRUE))}") %>% toupper,
         subtitle = "Reporting completeness for all indicators, mechanisms, and sites",
         caption = glue("Source: HFR Tableau Output {curr_fy} | Ref ID: {ref_id}")) +
    si_style_nolines() +
    theme(legend.position = "none",
          axis.text.y = element_text(size = 6))
  
  si_save(glue("Images/{curr_fy}_HFR_completeness_by_cntry.png"),
          width = 9.27)  
  

# VIZ - COMPLETENESS: COUNTRY SCATTER -------------------------------------

  df_avg <- df_comp_viz %>% 
    group_by(date) %>% 
    summarise(completeness = sum(has_hfr_reporting, na.rm = TRUE) / sum(site_mech_ind_combos, na.rm = TRUE),
              .groups = "drop") %>% 
    mutate(month = format(date, "%b") %>% fct_inorder )
    
  df_comp_viz %>% 
    ggplot(aes(month, completeness,
               color = completeness)) +
    geom_point(aes(size = site_mech_ind_combos), alpha = .2,
               position = position_jitter(width = .1, height = .01, seed = 42)) +
    # geom_point(data = df_avg, size = 15, shape = 18) +
    # geom_errorbar(data = df_avg, 
    #               aes(ymin = completeness, ymax = completeness),
    #               width = .5, alpha = .7, color = matterhorn) +
    geom_point(data = df_avg, size = 15, alpha = .4) +
    geom_text(data = df_avg, color = "white", size = 13/.pt,
               aes(label = percent(completeness, 1)),
              family = "Source Sans Pro SemiBold") +
    scale_y_continuous(label = percent) +
    scale_color_si("denims", reverse = FALSE, na.value = "white",
                  label = percent) +
    coord_cartesian(clip = "off") +
    expand_limits(y = 1) +
    labs(x = NULL, y = NULL, fill = "Completeness",
         title = glue("The average reporting completeness across countries for {curr_fy} was {percent(mean(df_comp_viz$completeness, na.rm = TRUE))}") %>% toupper,
         subtitle = "Circles represent each country's submission completeness, proportionally sized to the number of expected reporting points",
         caption = glue("Source: HFR Tableau Output {curr_fy} | Ref ID: {ref_id}")) +
    si_style_ygrid() +
    theme(legend.position = "none")

  si_save(glue("Images/{curr_fy}_HFR_completeness3.png"),
          width = 9.27)  


# VIZ - CORRECTNESS -------------------------------------------------------


  df_corr_combo %>%
    filter(indicator %ni% c("HTS_TST", "TX_MMD")) %>% 
    ggplot(aes(mer_results, hfr_results, color = completeness)) +
    geom_blank(aes(hfr_results, mer_results)) +
    geom_abline(slope = 1, linetype = "dashed") +
    geom_point(alpha = .4) +
    facet_wrap(~indicator, scales = "free") +
    scale_x_continuous(label = label_number(scale_cut = cut_short_scale())) +
    scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
    # scale_x_log10(label = label_number(scale_cut = cut_short_scale())) +
    # scale_y_log10(label = label_number(scale_cut = cut_short_scale())) +
    scale_color_si("denims", reverse = FALSE, na.value = "white",
                   label = percent) +
    labs(x = "MER", y = "HFR", color = "Completeness",
         title = glue("") %>% toupper,
         subtitle = "Circles represent each country's fiscal year HFR results against MER results, colored by reporting completeness across the whole year",
         caption = glue("Source: HFR Tableau Output {curr_fy} | Ref ID: {ref_id}")) +
    si_style() +
    theme(aspect.ratio = 1)
  
  