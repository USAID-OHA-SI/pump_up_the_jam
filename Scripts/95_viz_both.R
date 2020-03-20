## PROJECT:  Pump up the jam
## AUTHOR:   A.Chafetz, T.Essam | USAID
## LICENSE:  MIT
## PURPOSE:  viz completeness/correctness
## DATE:     2020-03-13
## UPDATED:  2020-03-16


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(vroom)
library(scales)
library(extrafont)
library(ggtext)


# GLOBAL VARIABLES --------------------------------------------------------

out_folder <- "Dataout"
viz_folder <- "Images"

ind_sel  <- c("HTS_TST", "TX_NEW", "TX_CURR")

pal <- viridis_pal()(6) #%>% show_col()
color_hv_sites <- pal[1]
color_ref <- "gray30" #"#C8C8C8"
color_all_sites <- "#D3D3D3"


# IMPORT ------------------------------------------------------------------


df_q1 <- list.files(out_folder, "HFR_DATIM_FY20Q1_Agg_[[:digit:]]+\\.csv", full.names = TRUE) %>% 
  vroom()



df_q1 %>% 
  filter(indicator %in% ind_sel,
         !(has_hfr_reporting == TRUE & is_datim_site == FALSE)) %>% 
  group_by(operatingunit, indicator) %>% 
  summarise_at(vars(hfr_results, mer_results, mer_targets, site_count = is_datim_site), sum, na.rm = TRUE) %>% 
  ungroup() %>% 
  mutate

#completeness
  df_cc <- df_q1 %>%
    filter(indicator %in% ind_sel) %>% 
    filter_at(vars(hfr_results, mer_results, mer_targets), any_vars(.!=0)) %>% 
    group_by(operatingunit, indicator) %>% 
    summarise_at(vars(hfr_results, mer_results, mer_targets, has_hfr_reporting, is_datim_site), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    mutate(completeness_value = case_when(is_datim_site >  0 ~ has_hfr_reporting / is_datim_site),
           completeness_dist = abs(1-completeness_value),
           correctness_value = hfr_results/mer_results,
           correctness_dist = abs(1-correctness_value))
  
  
  df_cc <- df_cc %>% 
    select(operatingunit, indicator, n_sites = is_datim_site, completeness_value:correctness_dist) %>% 
    gather(type, value, -operatingunit, -indicator, -n_sites) %>% 
    separate(type, c("measure", "type")) %>% 
    spread(type, value)
  
  df_cc <- df_cc %>% 
    mutate(operatingunit = case_when(operatingunit == "Democratic Republic of the Congo" ~ "DRC",
                                     operatingunit == "Dominican Republic" ~ "DR",
                                     operatingunit == "Western Hemisphere Region" ~ "WH Region",
                                     TRUE ~ operatingunit))
  
  #order
  df_cc <- df_cc %>% 
    group_by(operatingunit) %>% 
    mutate(sort_max = ifelse(indicator == "HTS_TST", n_sites, NA_real_)) %>%
    fill(sort_max, .direction = "updown") %>%
    ungroup() %>%
    mutate(indicator = factor(indicator, ind_sel),
           operatingunit = fct_reorder(operatingunit, sort_max))


# PLOT --------------------------------------------------------------------

  
  df_cc %>% 
    ggplot(aes(measure, operatingunit, size = n_sites, color = dist)) +
    geom_point() +
    geom_text(aes(label = percent(value, 1)), na.rm = TRUE,
              family = "Source Sans Pro", size = 3, 
              #color = "gray20", 
              hjust = -.7) +
    facet_wrap(~indicator) +
    scale_size(guide = "none") +
    scale_x_discrete(position = "top") +
    scale_color_viridis_c(option = "A", labels = percent, end = 0.9, alpha = 0.85) +
    labs(x = NULL, y = NULL,
         color = "Reporting completeness/correctness ",
         title = "COMPLETENESS/CORRECTNESS ACROSS ALL SITES",
         subtitle = "FY20Q1 Site x Mechanism HFR Reporting Completeness and Correctness",
         caption = "Source: FY20Q1 MER + HFR") +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          text = element_text(family = "Source Sans Pro"),
          strip.placement = "outside",
          legend.position = "top",
          legend.justification = c(0, 0))
  
    
  
  df_cc %>%
    gather(type, value, dist, value) %>% 
    spread(measure, value) %>% 
    filter(type == "value") %>% 
    ggplot(aes(correctness, completeness, size = n_sites)) +
    geom_point(color = color_all_sites, na.rm = TRUE) +
    geom_abline(intercept = 0, slope = 1, colour = "#909090") +
    facet_wrap(~indicator) +
    labs(title = "BETTER COMPLETENESS AND CORRECTNESS FOR TX_CURR",
         subtitle = "FY20Q1 Site x Mechanism HFR Reporting Correctness/Completeness",
         caption = "Source: FY20Q1 MER + HFR") +
    scale_y_continuous(label = percent) +
    scale_x_continuous(label = percent) +
    scale_size(guide = "none") +
    theme_minimal() +
    theme(strip.text = element_text(hjust = 0))
  