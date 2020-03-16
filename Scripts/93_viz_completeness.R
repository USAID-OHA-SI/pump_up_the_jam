## PROJECT:  Pump up the jam
## AUTHOR:   A.Chafetz, T.Essam | USAID
## LICENSE:  MIT
## PURPOSE:  visualizing completeness of reporting
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
color_ref <- pal[4]
color_all_sites <- "#D3D3D3"

# IMPORT ------------------------------------------------------------------

  #import
    df_completeness_viz <- list.files(out_folder, "HFR_Completeness_[[:digit:]]+\\.csv", full.names = TRUE) %>% 
      vroom()
  
    df_completeness_pds_viz <- list.files(out_folder, "HFR_Completeness_Pds_[[:digit:]]+\\.csv", full.names = TRUE) %>% 
      vroom()

# MUNGE -------------------------------------------------------------------

  #clean up OU names
    df_completeness_viz <- df_completeness_viz %>% 
      mutate(operatingunit = case_when(operatingunit == "Democratic Republic of the Congo" ~ "DRC",
                                       operatingunit == "Dominican Republic" ~ "DR",
                                       operatingunit == "Western Hemisphere Region" ~ "WH Region",
                                       TRUE ~ operatingunit))
  
  #create a global reference line (OU averages - All sites)
    df_ref_completeness_avg <- df_completeness_viz %>%
        filter(site_type == "All") %>% 
        group_by(indicator) %>%
        summarise(completeness_global_ou_avg = mean(completeness, na.rm = TRUE)) %>% 
        ungroup()
    
  #create a global reference line (global totals - All sites)
    df_ref_completeness <- df_completeness_viz %>%
      filter(site_type == "All") %>% 
      group_by(indicator) %>%
      summarise_at(vars(has_hfr_reporting, is_datim_site), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      mutate(completeness_global = has_hfr_reporting / is_datim_site) %>% 
      select(indicator, completeness_global)
    
  #merge
    df_completeness_viz <- df_completeness_viz %>% 
      left_join(df_ref_completeness_avg) %>% 
      left_join(df_ref_completeness)
    
    
  #clean up OU names
    df_completeness_pds_viz <- df_completeness_pds_viz %>% 
      mutate(operatingunit = case_when(operatingunit == "Democratic Republic of the Congo" ~ "DRC",
                                       operatingunit == "Dominican Republic" ~ "DR",
                                       operatingunit == "Western Hemisphere Region" ~ "WH Region",
                                       TRUE ~ operatingunit))
    
  #classes for completeness
    df_completeness_pds_viz <- df_completeness_pds_viz %>% 
      mutate(class = case_when(completeness < .6 ~ "low",
                               completeness < .9 ~ "med",
                               TRUE              ~ "okay"))

# PLOT --------------------------------------------------------------------
  

  theme_set(theme_minimal(base_family = "Source Sans Pro"))
  
  my_title <- "<b><span style = 'color:#440154;'>HIGH VOLUME SITES</span></b> ARE REPORTING AT HIGHER LEVELS THAN OTHER SITES</span></b>"
  
  df_completeness_viz %>%
    filter(indicator %in% ind_sel,
           site_type != "Low Volume (Target)") %>%
    group_by(operatingunit) %>%
    mutate(sort_max = if_else(site_type =="High Volume (Target)" & indicator == "HTS_TST", completeness, NA_real_)) %>%
    fill(sort_max, .direction = "updown") %>%
    ungroup() %>%
    mutate(indicator = factor(indicator, ind_sel),
           ou_sort = fct_reorder(operatingunit, sort_max)) %>%
    ggplot(aes(ou_sort, completeness, group = operatingunit, color = site_type)) +
    geom_hline(aes(yintercept = 0), color = "gray40") +
    geom_hline(aes(yintercept = 1), color = "gray40") + #, linetype = "dashed"
    geom_hline(aes(yintercept = completeness_global), color = color_ref, linetype = "dashed", size = .5) +
    geom_path(size = .6, color = 'gray70') +
    geom_point(size = 4) +
    scale_y_continuous(labels = percent) +
    scale_color_manual(values = c(color_all_sites, color_hv_sites)) +
    coord_flip() +
    facet_grid(~ indicator) +
    labs(title = my_title,
         subtitle = "FY20Q1 Site x Mechanism HFR Reporting Completeness",
         y = NULL, x = NULL, color = "Site Type",
         caption = "Notes:
         (a) Completeness derived by comparing HFR reporting against sites with DATIM results/targets
         (b) The green dashed line denoted global completeness of reporting
         Source: FY20Q1 MER + HFR") +
    theme_minimal() +
    theme(strip.text = element_text(hjust = 0),
          legend.position = "top",
          legend.justification = c(0, 0),
          plot.title = element_markdown(hjust = 0, size = 14, face = "bold", color = "gray30"),
          plot.caption = element_text(color = "gray30", size = 8))
  
  ggsave(file.path(viz_folder,"HFR_Completeness.png"), dpi = 300, 
         width = 10, height = 5.625)
  
  
  df_completeness_pds_viz %>%
    filter(indicator %in% ind_sel,
           site_type != "Low Volume (Target)") %>%
    group_by(operatingunit) %>%
    mutate(sort_max = ifelse(site_type =="High Volume (Target)" & indicator == "HTS_TST", completeness, NA_real_)) %>%
    fill(sort_max, .direction = "updown") %>%
    ungroup() %>%
    mutate(indicator = factor(indicator, ind_sel),
           ou_sort = fct_reorder(operatingunit, sort_max),
           rank = percent_rank(completeness),
    ) %>%
    ggplot(aes(hfr_pd, ou_sort, fill = completeness)) +
    geom_tile(color = "white", size = 0.25) +
    geom_text(aes(label = ifelse(rank < 0.50, percent(completeness), NA_real_)),
              size = 2.5, colour = "gray30", na.rm = TRUE) +
    scale_fill_viridis_c(option = "A", direction = -1, labels = percent, end = 0.9, alpha = 0.85) +
    facet_wrap(rev(site_type) ~ indicator, nrow = 1) +
    labs(title = "FOCUSING ON IMPOTANT SITES PROVIDES BETTER REPORTING COMPLETENESS",
         subtitle = "FY20Q1 Site x Mechanism HFR Reporting Completeness by Period",
         y = NULL, x = NULL, color = "Site Type",
         caption = "Note: Completeness derived by comparing HFR reporting against sites with DATIM results/targets
         Source: FY20Q1 MER + HFR",
         fill = "Reporting completeness (100% = all sites reporting) ") +
    theme_minimal() + coord_fixed(ratio = .007) +
    theme(legend.position = "top",
          legend.justification = c(0, 0),
          panel.grid = element_blank(),
          axis.text.x = element_text(size = 7),
          strip.text = element_text(hjust = 0))

  ggsave(file.path(viz_folder,"HFR_Completeness_Pd.png"), dpi = 300, 
         width = 10, height = 5.625)
  
  
  
  
  
  
  df_completeness_pds_viz %>%
    filter(indicator %in% ind_sel,
           site_type != "Low Volume (Target)") %>%
    group_by(operatingunit) %>%
    mutate(sort_max = ifelse(site_type =="High Volume (Target)" & indicator == "HTS_TST", completeness, NA_real_)) %>%
    fill(sort_max, .direction = "updown") %>%
    ungroup() %>%
    mutate(indicator = factor(indicator, ind_sel),
           ou_sort = fct_reorder(operatingunit, sort_max),
           rank = percent_rank(completeness),
           hfr_pd = as.character(hfr_pd),
           grp = paste(site_type, indicator)
    ) %>%
    ggplot(aes(hfr_pd, completeness, group = grp, color = completeness)) +
    geom_hline(aes(yintercept = 0), color = "gray30") +
    geom_smooth(color = color_ref, linetype = "dashed") +
    geom_jitter(size = 2, width = .2) +
    scale_y_continuous(labels = percent) +
    scale_colour_viridis_c(option = "A", direction = -1, labels = percent, end = 0.9, alpha = 0.85) +
    facet_grid(rev(site_type) ~ indicator, switch = "y") +
    labs(title = "PERIOD COMPLETENESS POOR ACROSS ALL SITE TYPES",
         subtitle = "FY20Q1 OU HFR Reporting Completeness by Period",
         y = NULL, x = NULL, 
         caption = "Notes: 
         (a) Completeness derived by comparing HFR reporting against sites with DATIM results/targets
         (b) Each dot represents an OU's Site x Mechanism completeness for each period
         Source: FY20Q1 MER + HFR",
         color = "Reporting completeness (100% = all sites reporting) ") +
    theme_minimal() + 
    #coord_fixed(ratio = .007) +
    theme(legend.position = "top",
          legend.justification = c(0, 0),
          axis.text.x = element_text(size = 7),
          strip.text = element_text(face = "bold"),
          strip.placement = "outside")
  
  
  ggsave(file.path(viz_folder,"HFR_Completeness_Pd_Trends.png"), dpi = 300, 
         width = 10, height = 5.625)
  
  