# PROJECT:  pump up the jam
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  review completeness
# REF ID:   335f52c3 
# LICENSE:  MIT
# DATE:     2022-10-31
# UPDATED:  2022-12-12

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
  library(rcartocolor)
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  ref_id <- "335f52c3"

  pal <- carto_pal(n = 5, name = "SunsetDark") %>% rev()
  
# IMPORT ------------------------------------------------------------------
  
  #https://drive.google.com/file/d/1wzCdAfMZ_VW6ekzr3TWcZYCP7-VzZFL4/view?usp=share_link
  df_hfr <- return_latest("Data", "Tableau") %>% 
    hfr_read() 

# MUNGE -------------------------------------------------------------------

  #limit to latest full FY
  df_hfr <- df_hfr %>% 
    filter(between(date, as.Date("2021-10-01"), as.Date("2022-09-01")))
  
  #clean up country names for viz
  df_hfr <- df_hfr %>% 
    mutate(countryname = recode(countryname,
                                "Democratic Republic of the Congo" = "DRC",
                                "Papua New Guinea" = "PNG",
                                "Dominican Republic" = "DR"))
  
  #aggregate to site level, id if reporting occured (ie non-zero)
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

  #aggregate site counts and reporting up to country level for completeness
  df_comp <- df_agg %>% 
    group_by(countryname, date) %>% 
    summarise(has_hfr_reporting = sum(has_hfr_reporting),
              site_mech_ind_combos = n(),
              .groups = "drop") %>% 
    mutate(completeness = has_hfr_reporting /site_mech_ind_combos)
  
  #order months
  df_comp_viz <- df_comp %>% 
    mutate(month = format(date, "%b") %>% fct_inorder) 
  

# MUNGE CORRECTNESS -------------------------------------------------------

  #sum non-snapshot indicators and pull the last obs for TX_CURR/MMD
  df_corr <- df_agg %>% 
    filter(indicator %ni% c("TX_CURR", "TX_MMD")) %>% 
    bind_rows(df_agg %>% 
                filter(indicator %in% c("TX_CURR", "TX_MMD"),
                       hfr_results > 0) %>% 
                group_by(orgunituid, mech_code, indicator) %>% 
                filter(date == max(date)) %>% 
                ungroup()) %>% 
    select(-starts_with("mer")) %>% 
    mutate(fy = unique(df_hfr$fy)) %>% 
    # filter(has_mer_reporting == TRUE) %>% 
    count(countryname, fy, indicator, wt = hfr_results, name = "hfr_results")

  #aggregate fiscal year results and targets by country
  df_targets_ou <- df_hfr %>% 
    filter(date == max(date),
           indicator != "TX_MMD") %>% 
    group_by(fy, countryname, indicator) %>% 
    summarise(across(starts_with("mer"), sum, na.rm = TRUE),
              .groups = "drop") %>% 
    filter(!(mer_targets == 0 & mer_results == 0))
  
  #add MMD targets by duplicating TX_CURR & renaming
  df_targets_ou <- df_targets_ou %>% 
    bind_rows(df_targets_ou %>% 
                filter(indicator == "TX_CURR") %>% 
                mutate(indicator = "TX_MMD"))
  
  #country level completeness for comparision in plots
  df_comp_ou_full_fy <- df_agg %>% 
    group_by(countryname, indicator) %>% 
    summarise(has_hfr_reporting = sum(has_hfr_reporting),
              site_mech_ind_combos = n(),
              .groups = "drop") %>% 
    mutate(completeness = has_hfr_reporting /site_mech_ind_combos)
  
  #bind correctness, targets, and completeness together
  df_corr_combo <- df_corr %>% 
    full_join(df_targets_ou, by = c("fy", "countryname", "indicator")) %>% 
    filter(mer_results != 0) %>% 
    mutate(hfr_results = ifelse(is.na(hfr_results), 0, hfr_results)) %>% 
    relocate(mer_targets, .after = -1) %>% 
    mutate(correctness = hfr_results / mer_results) %>% 
    full_join(df_comp_ou_full_fy, by = c("countryname", "indicator")) %>% 
    arrange(countryname, indicator)
  
  #relative size of country's results (by indicator) for plotting
  df_corr_viz <- df_corr_combo %>% 
    group_by(indicator) %>% 
    mutate(rel_ind_results_size = mer_results / sum(mer_results, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(weight_order = case_when(indicator == "TX_CURR" ~ mer_results),
           fill_val = case_when(correctness > 2 ~ 1,
                                correctness > 1 ~ correctness - 1, 
                                TRUE ~ 1 - correctness),
           fill_color = case_when(fill_val <= .2 ~ pal[1],
                                  fill_val <= .4 ~ pal[2],
                                  fill_val <= .6 ~ pal[3],
                                  fill_val <= .8 ~ pal[4],
                                  fill_val >  .8 ~ pal[5]) )
  
  
# VIZ - COMPLETENESS: COUNTRY HEATMAP -------------------------------------

  #curr FY for plot source info
  curr_fy <- unique(df_hfr$fy) %>% str_replace("20", "FY")
  
  #plot completeness heatmap table (OUxPeriodxCompleteness) 
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

  #create avg completeness for all USAID 
  df_avg <- df_comp_viz %>% 
    group_by(date) %>% 
    summarise(completeness = sum(has_hfr_reporting, na.rm = TRUE) / sum(site_mech_ind_combos, na.rm = TRUE),
              .groups = "drop") %>% 
    mutate(month = format(date, "%b") %>% fct_inorder )
    
  #plot distribution of completeness overlaid by agency value
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


  df_corr_viz %>%
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
  
  df_corr_viz %>% 
    filter(indicator %ni% c("HTS_TST", "TX_MMD")) %>%
    mutate(indicator = factor(indicator, c("HTS_TST_POS", "TX_NEW",
                                           "TX_CURR", "PrEP_NEW", "VMMC_CIRC"))) %>% 
    ggplot(aes(correctness, fct_reorder(countryname, weight_order, sum, na.rm = TRUE), color = fill_color)) +
    annotate("rect",
             xmin = .8, xmax = 1.2,
             ymin = -Inf, ymax = Inf,
             fill = "#EBEBEB", alpha = .4) +
    geom_vline(aes(xintercept = 1), color = matterhorn, linetype = "dotted") +
    geom_segment(aes(x = correctness, xend = 1, yend = countryname), 
                 linewidth = 1.3, na.rm = TRUE) +
    geom_point(aes(size = rel_ind_results_size), color = "white", na.rm = TRUE) +
    geom_point(aes(size = rel_ind_results_size), alpha = .8, na.rm = TRUE) +
    facet_grid(~indicator) +
    scale_x_continuous(label = percent,
                       position = "top",
                       limit = c(0, 2),
                       breaks = seq(0, 2, by = .5),
                       oob = oob_squish) +
    scale_color_identity() +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = NULL,
         title = "Outside of TX_CURR, OU-level annual HFR values do not closely track to MER" %>% toupper,
         subtitle = "How close are HFR reported values to MER annual values in DATIM?",
         caption =glue("Note: Countries ordered by MER TX_CURR results
                       Source: HFR Tableau Output {curr_fy} | Ref ID: {ref_id}") ) +
    si_style_xgrid() +
    theme(legend.position = "off",
          strip.placement = "outside",
          strip.text = element_text(hjust = .5, face = "bold"))

  
  