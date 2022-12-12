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
  library(rcartocolor)
  

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
  
  
  # What if we visualize this with 12 dots that are filled based on 5 colors that bin reporting?
  # Let's also bin the OUs into high, medium, and low site counts to create some facets we can use to break up the viz
  # Checked using ntile(site_mech_ind_combos, 3) and determined reasonable cuts to be 200 sites, 200-1000, 1000+
  # Can we add in a new row that would show the average completeness for the year by ou? Month = Ave
  # Looked into row binding using group_modify(~add_rows(.x, .after = Inf)) but would pry take as many lines
  # to create cumulative totals as just creating a new df and binding it on
  df_comp_viz_cat <- 
    df_comp_viz %>% 
    mutate(site_size = case_when(
      between(site_mech_ind_combos, 0, 200) ~ "low",
      between(site_mech_ind_combos, 200, 1000) ~ "medium",
      TRUE ~ "high"
    ),
      site_size = fct_relevel(site_size, c("high", "medium", "low"))
    ) 
  
  df_comp_viz_cat %>% 
    filter(date == max(date)) %>% 
    arrange(site_size, desc(site_mech_ind_combos)) %>% 
    prinf()
  
  
  # Create a month order vector so you don't have to type the darned things, moving All to the end
  month_order <- c(levels(df_comp_viz_cat$month), "All")
  
  # Create a new dataframe that contains the annual ave completeness
  # Bind this on to the viz_cat frame so we can plot annual completeness
  df_comp_annual <- 
    df_comp_viz_cat %>% 
    group_by(countryname, site_size) %>% 
    summarise(across(where(is.integer), sum, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(completeness = has_hfr_reporting / site_mech_ind_combos,
           month = "All", 
           missing_sites = site_mech_ind_combos-has_hfr_reporting)
  
  
  # Grab 5 colors from rcartocolor DarkSunset
  pal <- carto_pal(n = 5, name = "SunsetDark") 
  
  # Need to add a max site # b/c for some OUs the max # changes across year
  # For the max sites, we need the second highest max value b/c the max is the total count across the year
  df_comp_viz_cat %>% 
    bind_rows(df_comp_annual) %>% 
    mutate(month = fct_relevel(month, month_order)) %>% 
    group_by(countryname) %>% 
    mutate(max_sites = max(site_mech_ind_combos[-which.max(site_mech_ind_combos)])) %>% 
    fill(., max_sites, .direction = "down") %>%  
    ungroup() %>% 
    mutate(completeness = na_if(completeness, 0),
           ylabel = fct_reorder(paste0(countryname, " (", comma(max_sites), ")"), site_mech_ind_combos, max)) %>%
    ggplot(aes(x = month, y = ylabel, fill = completeness)) +
    geom_tile(color = "white") +
    geom_text(aes(label = percent(completeness, 1)),
              family = "Source Sans Pro", color = "white", size = 7/.pt) +
    facet_wrap(~site_size, scales = "free_y") +
    scale_fill_stepsn(colours = pal,
                      limits = c(0, 1),
                      breaks = seq(0, 1, 0.2),
                      na.value = grey10k, 
                      labels = percent) +
    scale_x_discrete(position = "top", 
                     breaks = c("Oct", "Jan", "Apr", "Jul", "All")) +
    si_style_nolines(facet_space = 0.75) +
    si_legend_fill() +
    theme(strip.placement = "outside", 
          legend.position = "none") +
    labs(x = NULL, y = NULL, fill = "Completeness",
         title = glue("The average reporting completeness across countries for {curr_fy} was {percent(mean(df_comp_viz$completeness, na.rm = TRUE))}") %>% toupper,
         subtitle = "Reporting completeness for all indicators, mechanisms, and sites",
         caption = glue("Source: HFR Tableau Output {curr_fy} | Ref ID: {ref_id}")) 
  
    si_save("Graphics/HFR_completeness_by_country.svg", scale = 1.25)
  


  # What is the reporting rate by size of OUs
  # Could add these to the viz if we wanted? may be too much
    df_comp_viz_cat %>% 
      group_by(site_size) %>% 
      summarise(comp_rate = percent(sum(has_hfr_reporting, na.rm = T)/sum(site_mech_ind_combos, na.rm = T)))
    
  # Do we see a COP drop? not really  
    df_comp_viz_cat %>% 
      group_by(site_size, month) %>% 
      summarise(comp_rate = percent(sum(has_hfr_reporting, na.rm = T)/sum(site_mech_ind_combos, na.rm = T))) %>% 
      spread(month, comp_rate)
    
 # Create a histogram of the completeness that will serve as the legend 
  df_comp_viz_cat %>%
    mutate(hist_fill = case_when(
      between(completeness, 0.0001, .2) ~ pal[1],
      between(completeness, 0.2, .4) ~ pal[2],
      between(completeness, 0.4, .6) ~ pal[3],
      between(completeness, 0.6, .80) ~ pal[4],
      between(completeness, 0.80, 1) ~ pal[5],
      completeness == 0 ~ grey10k
    ))%>% 
    ggplot(aes(x = completeness)) +
    geom_vline(xintercept = mean(df_comp_viz_cat$completeness, na.rm = TRUE), linewidth = 0.75, color = grey90k) +
    geom_histogram(aes(fill = hist_fill), bins = 100, color = "white") +
    geom_hline(yintercept = seq(0, 84, 1), color = "white") +
    scale_fill_identity() +
    si_style_xline() +
    coord_cartesian(expand = F) +
    scale_x_continuous(labels = percent, breaks = seq(0, 1, .2)) +
    labs(x = NULL, y = NULL)
  
  # Need 7.9043 by 2.7341 to fit in the designated AI space
  si_save("Graphics/HFR_completeness_histogram.svg", width = 7.9043, height = 2.7341)

  
  
  df_comp_viz_cat %>%
    filter(site_size == "large") %>% 
    group_by(countryname) %>% 
    mutate(max_sites = max(site_mech_ind_combos)) %>% 
    ungroup() %>% 
    mutate(completeness = na_if(completeness, 0),
           ylabel = fct_reorder(paste0(countryname, " (", comma(max_sites), ")"), site_mech_ind_combos, max, .desc = T)) %>%
    ggplot(aes(x = month, y = completeness)) +
    geom_col(aes(y = 1), fill = grey20k, width = 0.5) +
    geom_point(aes(fill = completeness), width = 0.5) +
    facet_wrap(~ylabel, ncol = 2) +
    scale_fill_stepsn(colours = rcartocolor::carto_pal(n = 5, name = "SunsetDark"),
                      limits = c(0, 1),
                      breaks = seq(0, 1, 0.2),
                      na.value = grey10k, 
                      labels = percent) +
    si_style_nolines(facet_space = 0.2) +
    si_legend_fill() +
    scale_y_continuous(labels = percent) +
    scale_x_discrete(breaks = c("Oct", "Jan", "Apr", "Jul")) +
    theme(axis.text.y = element_blank())


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
    #scale_x_continuous(label = label_number(scale_cut = cut_short_scale())) +
    #scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
    scale_x_log10(label = label_number(scale_cut = cut_short_scale())) +
    scale_y_log10(label = label_number(scale_cut = cut_short_scale())) +
    scale_color_si("denims", reverse = FALSE, na.value = "white",
                   label = percent) +
    labs(x = "MER", y = "HFR", color = "Completeness",
         title = glue("") %>% toupper,
         subtitle = "Circles represent each country's fiscal year HFR results against MER results, colored by reporting completeness across the whole year",
         caption = glue("Source: HFR Tableau Output {curr_fy} | Ref ID: {ref_id}")) +
    si_style() +
    theme(aspect.ratio = 1)
  
  