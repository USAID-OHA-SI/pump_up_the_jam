# PROJECT:  pump up the jam
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  review completeness + correctness trends
# REF ID:   f6592c5c  
# LICENSE:  MIT
# DATE:     2023-01-17
# UPDATED:  2022-12-12

# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(gagglr)
  library(lubridate)
  library(glue)
  library(scales)
  library(extrafont)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(Wavelength)
  library(rcartocolor)
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  ref_id <- "f6592c5c"

  pal <- carto_pal(n = 5, name = "SunsetDark") %>% rev()
  

# PULL QUARTERLY MER RESULTS ----------------------------------------------

  # #period
  # pd <- as.Date("2021-10-01") %>% 
  #   seq.Date(length.out = 4, by = "3 months") %>% 
  #   quarter(with_year = TRUE) %>% 
  #   str_replace("\\.", "Q") %>% 
  #   paste(collapse = ";")
  # 
  # #map pull over each OU
  # df_mer <- purrr::map_dfr(pepfar_country_list$country,
  #                          ~pull_mer(ou_name = .x, 
  #                                    fy_pd = pd,
  #                                    username = datim_user(),
  #                                    password = datim_pwd()))
  # 
  # #check
  # # df_mer %>% 
  # #   count(period, indicator, wt = mer_results) %>% 
  # #   pivot_wider(names_from = period, values_from = n)
  # 
  # 
  # df_mer %>% 
  #   write_csv(glue("Data/HFR_FY22_GLOBAL_DATIM_{str_remove_all(Sys.Date(), '-')}.csv"), na = "")
  
# IMPORT ------------------------------------------------------------------
  
  #https://drive.google.com/file/d/1wzCdAfMZ_VW6ekzr3TWcZYCP7-VzZFL4/view?usp=share_link
  df_hfr <- return_latest("Data", "Tableau") %>% 
    hfr_read() 
  
  df_mer <- return_latest("Data", "GLOBAL_DATIM") %>% 
    read_csv(col_types = list(fy = "i",
                              mer_results = "d",
                              .default = "c"
                              )) 

  source_date <- file.info(return_latest("Data", "GLOBAL_DATIM"))$ctime %>% format("%Y-%m-%d")
  
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
  
  #aggregate to site level, id if reporting occurred (ie non-zero)
  df_agg <- df_hfr %>% 
    rename(hfr_results = val) %>% 
    # filter(expect_reporting == TRUE) %>% 
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
    mutate(countryname = recode(countryname,
                                "Democratic Republic of the Congo" = "DRC",
                                "Papua New Guinea" = "PNG",
                                "Dominican Republic" = "DR")) %>% 
    left_join(df_date_map, by = "period") %>% 
    count(countryname, date, orgunituid, mech_code, indicator, 
          wt = mer_results, name = "mer_results")
  
  #create baseline MMD since not in DATIM pull
  df_mer_agg <- df_mer_agg %>% 
    bind_rows(df_mer_agg %>% 
                filter(indicator == "TX_CURR") %>% 
                mutate(indicator = "TX_MMD"))
  
  #replicate quarterly mer results for each month of qtr
  df_expect_reporting <- df_mer_agg %>% 
    bind_rows(mutate(df_mer_agg, date = date %m+% months(1))) %>% 
    bind_rows(mutate(df_mer_agg, date = date %m+% months(2))) %>% 
    arrange(countryname, indicator, date)
  
  #limit HFR reporting to only where there are MER values reported
  df_comp <- df_expect_reporting %>% 
    tidylog::left_join(df_agg) %>% 
    mutate(has_hfr_reporting = ifelse(is.na(has_hfr_reporting), FALSE, has_hfr_reporting))
  
  # df_agg %>% 
  #   tidylog::anti_join(df_expect_reporting)
  
  #aggregate site counts and reporting up to country level for completeness
  df_comp <- df_comp %>% 
    group_by(countryname, date, indicator) %>% 
    summarise(mer_results = sum(mer_results, na.rm = TRUE),
              hfr_results = sum(hfr_results, na.rm = TRUE),
              has_hfr_reporting = sum(has_hfr_reporting),
              site_mech_ind_combos = n(),
              .groups = "drop") %>% 
    mutate(completeness = has_hfr_reporting /site_mech_ind_combos) %>% 
    arrange(countryname, indicator, date)
  

# MUNGE MER FOR STEPS -----------------------------------------------------

  df_mer_qtr <- df_mer %>% 
    left_join(df_date_map, by = "period") %>%  
    mutate(countryname = recode(countryname,
                                "Democratic Republic of the Congo" = "DRC",
                                "Papua New Guinea" = "PNG",
                                "Dominican Republic" = "DR")) %>% 
    count(countryname, date, indicator, wt = mer_results, name = "mer_results")
  


# LARGEST TX COUNTRIES ----------------------------------------------------

  v_lrg_cntry <- df_mer %>% 
    mutate(countryname = recode(countryname,
                                "Democratic Republic of the Congo" = "DRC",
                                "Papua New Guinea" = "PNG",
                                "Dominican Republic" = "DR")) %>% 
    filter(indicator == "TX_CURR",
           period == max(period)) %>% 
    count(countryname, indicator, period, wt = mer_results, sort = TRUE) %>% 
    slice_head(n = 12) %>% 
    pull(countryname)
  
# MUNGE VIZ DF ------------------------------------------------------------

  df_viz <- df_comp %>% 
    filter(indicator == "TX_CURR",
           countryname %in% v_lrg_cntry) %>% 
    mutate(countryname = factor(countryname, v_lrg_cntry),
           y_comp = 0,
           hfr_results = ifelse(is.na(hfr_results), 0, hfr_results))
  
  df_viz_step <- df_mer_qtr %>% 
    bind_rows(df_mer_qtr %>% 
                filter(date == max(date)) %>% 
                mutate(date = date %m+% months(3) %m-% days(5))) %>% 
    filter(indicator == "TX_CURR",
           countryname %in% v_lrg_cntry) %>% 
    mutate(countryname = factor(countryname, v_lrg_cntry),
           date = date %m-% days(15)) %>% 
    arrange(countryname)
  
# VIZ ---------------------------------------------------------------------

  df_viz %>% 
    ggplot(aes(date, hfr_results, group = countryname)) +
    # geom_blank(aes(y = 6e5)) +
    annotate(geom = 'rect',
             xmin = as.Date("2021-09-15"),
             xmax = as.Date("2021-12-15"),
             ymin = -Inf,
             ymax = Inf,
             fill = matterhorn,
             alpha = .1) +
    annotate(geom = 'rect',
             xmin = as.Date("2022-03-15"),
             xmax = as.Date("2022-06-15"),
             ymin = -Inf,
             ymax = Inf,
             fill = matterhorn,
             alpha = .1) +
    geom_hline(yintercept = 0, color = matterhorn) +
    geom_step(data =  df_viz_step,
              aes(x = date, y = mer_results), color = moody_blue, linewidth = 1.1) +
    geom_path(color = denim, na.rm = TRUE, linewidth = .9) +
    geom_point(shape = 21, fill = denim_light, color = denim, size = 1.5, stroke = 1,
               na.rm = TRUE) +
    geom_point(aes(y = y_comp, color = completeness), shape = 15, size = 4) +
    # geom_text(aes(y = y_comp, label = percent(completeness, 1)),
    geom_text(aes(y = y_comp, label = round(completeness*100, 0)),
              family = "Source Sans Pro", size = 6/.pt, color = "white") +
    # geom_rug(aes(color = completeness), sides="b", na.rm = TRUE,) +
    # facet_wrap(~countryname) +
    facet_wrap(~countryname, scales = "free_y") +
    scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
    scale_x_date(breaks = "1 month", date_labels = "%b") +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = NULL,
         title = "Large or inconsistent gaps in HFR reporting make interpretation/action challenging" %>% toupper,
         subtitle = glue("TX_CURR | Trends in <span style = 'color:{denim};'>**monthly HFR data**</span> against <span style = 'color:{moody_blue};'>**quarterly MER data**</span> contextualized with HFR reporting completeness (%)"),
         caption = glue("Note: Completeness = Site x IM x Indicator HFR reporting at site / MER reporting at site
                        Source: HFR FY22 + DATIM [{source_date}]| Ref ID: {ref_id}")) +
    scale_color_stepsn(colors = rev(pal),
                       limits = c(0, 1),
                       breaks = seq(0, 1, 0.2),
                       na.value = grey10k, 
                       labels = percent) +
    si_style_nolines(facet_space = 0.75) +
    theme(legend.position = "none",
          plot.subtitle = element_markdown())

  si_save("Graphics/FY22-HFR_comp-corr_top-tx.svg")  

  
  
# VIZ - TX_NEW ------------------------------------------------------------

  df_viz_txnew <- df_comp %>% 
    filter(indicator == "TX_NEW",
           countryname %in% v_lrg_cntry) %>% 
    mutate(countryname = factor(countryname, v_lrg_cntry),
           y_comp = 0,
           hfr_results = ifelse(is.na(hfr_results), 0, hfr_results))
  
  df_viz_txnew <- df_viz_txnew %>% 
    group_by(countryname, indicator) %>% 
    mutate(hfr_results_cum = cumsum(hfr_results)) %>% 
    ungroup()
    
  df_viz_step_txnew <- df_mer_qtr %>% 
    bind_rows(df_mer_qtr %>%
                filter(date == max(date)) %>%
                mutate(date = date %m+% months(3) %m-% days(5),
                       mer_results = 0)) %>%
    filter(indicator == "TX_NEW",
           countryname %in% v_lrg_cntry) %>% 
    mutate(countryname = factor(countryname, v_lrg_cntry),
           date = date %m-% days(15)) %>% 
    arrange(countryname, date) %>% 
    group_by(countryname, indicator) %>% 
    mutate(mer_results_cum = cumsum(mer_results)) %>% 
    ungroup()
  
  df_viz_txnew %>% 
    ggplot(aes(date, hfr_results_cum, group = countryname)) +
    # geom_blank(aes(y = 6e5)) +
    annotate(geom = 'rect',
             xmin = as.Date("2021-09-15"),
             xmax = as.Date("2021-12-15"),
             ymin = -Inf,
             ymax = Inf,
             fill = matterhorn,
             alpha = .1) +
    annotate(geom = 'rect',
             xmin = as.Date("2022-03-15"),
             xmax = as.Date("2022-06-15"),
             ymin = -Inf,
             ymax = Inf,
             fill = matterhorn,
             alpha = .1) +
    geom_hline(yintercept = 0, color = matterhorn) +
    geom_step(data =  df_viz_step_txnew,
              aes(x = date, y = mer_results_cum), color = moody_blue, linewidth = 1.1) +
    geom_col(aes(y = hfr_results), fill = matterhorn, alpha = .2, linewidth = .2) +
    geom_path(color = denim, na.rm = TRUE, size = .9) +
    geom_point(shape = 21, fill = denim_light, color = denim, size = 1.5, stroke = 1,
               na.rm = TRUE) +
    geom_point(aes(y = y_comp, color = completeness), shape = 15, size = 4) +
    # geom_text(aes(y = y_comp, label = percent(completeness, 1)),
    geom_text(aes(y = y_comp, label = round(completeness*100, 0)),
              family = "Source Sans Pro", size = 6/.pt, color = "white") +
    # geom_rug(aes(color = completeness), sides="b", na.rm = TRUE,) +
    # facet_wrap(~countryname) +
    facet_wrap(~countryname, scales = "free_y") +
    scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
    scale_x_date(breaks = "1 month", date_labels = "%b") +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = NULL,
         title = "Large or inconsistent gaps in HFR reporting make interpretation/action challenging" %>% toupper,
         subtitle = glue("TX_NEW | Trends in <span style = 'color:{denim};'>**monthly HFR data**</span> against <span style = 'color:{moody_blue};'>**quarterly MER data**</span> contextualized with HFR reporting completeness (%)"),
         caption = glue("Note: Completeness = Site x IM x Indicator HFR reporting at site / MER reporting at site
                        Source: HFR FY22 + DATIM [{source_date}]| Ref ID: {ref_id}")) +
    scale_color_stepsn(colors = rev(pal),
                       limits = c(0, 1),
                       breaks = seq(0, 1, 0.2),
                       na.value = grey10k, 
                       labels = percent) +
    si_style_nolines(facet_space = 0.75) +
    theme(legend.position = "none",
          plot.subtitle = element_markdown())
  
  si_save("Graphics/FY22-HFR_comp-corr_top-txnew.svg")  
  