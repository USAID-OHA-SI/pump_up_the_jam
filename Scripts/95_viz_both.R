## PROJECT:  Pump up the jam
## AUTHOR:   A.Chafetz, T.Essam | USAID
## LICENSE:  MIT
## PURPOSE:  viz completeness/correctness
## DATE:     2020-03-13
## UPDATED:  2020-03-20


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

  #import
    df_q1 <- list.files(out_folder, "HFR_DATIM_FY20Q1_Agg_[[:digit:]]+\\.csv", full.names = TRUE) %>% 
      vroom()


# MUNGE -------------------------------------------------------------------

  #calc OU level completeness and correctnessess (and their distance from 100% for color) 
    df_cc <- df_q1 %>%
      filter(indicator %in% ind_sel,
             !(has_hfr_reporting == TRUE & is_datim_site == FALSE)) %>% 
      filter_at(vars(hfr_results, mer_results, mer_targets), any_vars(.!=0)) %>% 
      group_by(operatingunit, indicator) %>% 
      summarise_at(vars(hfr_results, mer_results, mer_targets, has_hfr_reporting, is_datim_site), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      mutate(completeness_value = case_when(is_datim_site >  0 ~ has_hfr_reporting / is_datim_site),
             completeness_dist = abs(1-completeness_value),
             correctness_value = hfr_results/mer_results,
             correctness_dist = abs(1-correctness_value))
  
  #reshape to get completeness/correctness in one col for plotting and value/dist as their own cols
    df_cc <- df_cc %>% 
      select(operatingunit, indicator, n_sites = is_datim_site, completeness_value:correctness_dist) %>% 
      gather(type, value, -operatingunit, -indicator, -n_sites) %>% 
      separate(type, c("measure", "type")) %>% 
      spread(type, value)
    
  #clean up variable country
    df_cc <- df_cc %>% 
      mutate(operatingunit = case_when(operatingunit == "Democratic Republic of the Congo" ~ "DRC",
                                       operatingunit == "Dominican Republic" ~ "DR",
                                       operatingunit == "Western Hemisphere Region" ~ "WH Region",
                                       TRUE ~ operatingunit))
  
  #order OUs by HTS site count
    df_cc <- df_cc %>% 
      group_by(operatingunit) %>% 
      mutate(sort_max = ifelse(indicator == "HTS_TST", n_sites, NA_real_)) %>%
      fill(sort_max, .direction = "updown") %>%
      ungroup() %>%
      mutate(indicator = factor(indicator, ind_sel),
             operatingunit = fct_reorder(operatingunit, sort_max))


# PLOT --------------------------------------------------------------------

  
  #dot comparison of completeness + correctness by OU
    df_cc %>% 
      ggplot(aes(measure, operatingunit, size = n_sites, color = dist)) +
      geom_point() +
      geom_text(aes(label = percent(value, 1)), na.rm = TRUE,
                family = "Source Sans Pro", size = 3, 
                #color = "gray20", 
                hjust = -.7) +
      facet_wrap(~indicator) +
      scale_size_continuous(labels = comma) +
      scale_x_discrete(position = "top") +
      scale_color_viridis_c(option = "A", labels = percent, end = 0.9, alpha = 0.85,
                            guide = "none") +
      labs(x = NULL, y = NULL,
           #color = "Reporting completeness/correctness distance from 100% complete/correct",
           size = "number of sites",
           title = "COMPLETENESS/CORRECTNESS ACROSS ALL SITES",
           subtitle = "FY20Q1 Site x Mechanism HFR Reporting Completeness and Correctness",
           caption = "Source: FY20Q1 MER + HFR") +
      theme_minimal() +
      guides(size = guide_legend(reverse=T))+
      theme(panel.grid = element_blank(),
            text = element_text(family = "Source Sans Pro"),
            strip.placement = "outside",
            legend.position = "right",
            #legend.justification = c(0, 0)
            )
    
    ggsave(file.path(viz_folder,"HFR_CC_Comparison.png"), dpi = 300, 
           width = 10, height = 5.625)  
  

    #scatter plots of correctness + completeness by OU (single plot)
    df_cc %>%
      gather(type, value, dist, value) %>% 
      spread(measure, value) %>% 
      filter(type == "value") %>% 
      ggplot(aes(completeness, correctness, size = n_sites, color = indicator)) +
      geom_hline(aes(yintercept = 0), color = "#909090") +
      geom_vline(aes(xintercept = 0), color = "#909090") +
      geom_segment(aes(x = 0, y = 1, xend = 1, yend = 1), size = .5, linetype = "dashed", color = "#909090") +
      geom_segment(aes(x = 1, y = 0, xend = 1, yend = 1), size = .5, linetype = "dashed", color = "#909090") +
      geom_point(alpha = .6, #color = color_all_sites,
                 na.rm = TRUE) +
      geom_abline(intercept = 0, slope = 1, color = "#909090") +
      expand_limits(x = 1.7, y = 1.7) +
      labs(title = "COMPLETENESS v CORRECTNESS",
           subtitle = "FY20Q1 Site x Mechanism HFR Reporting Correctness/Completeness",
           color = NULL,
           caption = "Source: FY20Q1 MER + HFR") +
      scale_color_viridis_d() +
      scale_y_continuous(label = percent) +
      scale_x_continuous(label = percent) +
      scale_size(guide = "none") +
      theme_minimal() +
      theme(strip.text = element_text(hjust = 0),
            legend.position = "top")
    
    ggsave(file.path(viz_folder,"HFR_CC_Scatter.png"), dpi = 300, 
           width = 5.625, height = 5.625)  
    