## PROJECT:  Pump up the jam
## AUTHOR:   A.Chafetz, T.Essam | USAID
## LICENSE:  MIT
## PURPOSE:  visualizing completeness of reporting
## DATE:     2020-03-13
## UPDATED:  


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(vroom)
library(scales)
library(extrafont)

# GLOBAL VARIABLES --------------------------------------------------------

out_folder <- "Dataout"
viz_folder <- "Images"

ind_sel  <- c("HTS_TST", "TX_NEW", "TX_CURR")

color_hv_sites <- viridis_pal()(1)
color_all_sites <- "#D3D3D3"

# IMPORT ------------------------------------------------------------------


  df_completeness_viz <- list.files(out_folder, "HFR_Completeness_[[:digit:]]+\\.csv", full.names = TRUE) %>% 
    vroom()

  df_completeness_pds_viz <- list.files(out_folder, "HFR_Completeness_Pds_[[:digit:]]+\\.csv", full.names = TRUE) %>% 
    vroom()

# MUNGE -------------------------------------------------------------------

#arrange
  df_completeness_viz <- df_completeness_viz %>% 
    mutate(operatingunit = case_when(operatingunit == "Democratic Republic of the Congo" ~ "DRC",
                                     operatingunit == "Dominican Republic" ~ "DR",
                                     operatingunit == "Western Hemisphere Region" ~ "WH Region",
                                     TRUE ~ operatingunit))
  
  df_completeness_pds_viz <- df_completeness_pds_viz %>% 
    mutate(operatingunit = case_when(operatingunit == "Democratic Republic of the Congo" ~ "DRC",
                                     operatingunit == "Dominican Republic" ~ "DR",
                                     operatingunit == "Western Hemisphere Region" ~ "WH Region",
                                     TRUE ~ operatingunit))
  
  
  df_completeness_pds_viz <- df_completeness_pds_viz %>% 
    mutate(class = case_when(completeness < .6 ~ "low",
                             completeness < .9 ~ "med",
                             TRUE              ~ "okay"))

# PLOT --------------------------------------------------------------------
  

  df_completeness_viz %>% 
    filter(indicator %in% ind_sel,
           site_type != "Low Volume (Target)") %>% 
    mutate(indicator = factor(indicator, ind_sel)) %>% 
    ggplot(aes(fct_reorder(operatingunit, is_datim_site, .fun = sum), completeness, group = operatingunit, color = site_type)) +
    geom_hline(aes(yintercept = 0), color = "gray40") +
    geom_hline(aes(yintercept = 1), color = "gray40", linetype = "dashed") +
    geom_path(size = .6, color = 'gray70') +
    geom_point(size = 4) +
    scale_y_continuous(labels = percent) +
    scale_color_manual(values = c(color_all_sites, color_hv_sites)) +
    coord_flip() +
    facet_grid(~ indicator) +
    labs(title = "FOCUSING ON IMPORTANT SITES PROVIDES BETTER REPORTING COMPLETESS",
         subtitle = "FY20Q1 Site x Mechanism HFR Reporting Completeness", 
         y = NULL, x = NULL, color = "Site Type",
         caption = "Notes: 
         (a) Completeness derived by comparing HFR reporting against sites with DATIM results/targets
         (b) OUs sorted by the number of sites x mechanism pairs 
         Source: FY20Q1 MER + HFR") +
    theme_minimal() +
    theme(text = element_text(family = "Calibri Light"),
          plot.title = element_text(family = "Calibri", face = "bold"),
          strip.text = element_text(family = "Calibri", face = "bold"),
          #panel.grid.major.y = element_line(color = "gray50"),
          legend.position = "bottom",
          plot.caption = element_text(color = "gray30"))
  
  ggsave(file.path(viz_folder,"HFR_Completeness.png"), dpi = 300, 
         width = 10, height = 5.625)
  
  
  df_completeness_pds_viz %>% 
    filter(indicator %in% ind_sel,
           site_type != "Low Volume (Target)") %>% 
    mutate(indicator = factor(indicator, ind_sel)) %>% 
    ggplot(aes(hfr_pd, fct_reorder(operatingunit, is_datim_site, .fun = sum), fill = class, color = class)) +
    geom_tile(color = "white") +
    geom_text(aes(label = percent(completeness)), 
              size = 2.5, family = "Calibri Light") +
    scale_color_manual(values = c("gray90", "gray30","gray30")) +
    scale_fill_brewer(palette = "OrRd", direction = -1) +
    facet_grid(site_type ~ indicator, switch = "y") +
    labs(title = "FOCUSING ON IMPORTANT SITES PROVIDES BETTER REPORTING COMPLETESS",
         subtitle = "FY20Q1 Site x Mechanism HFR Reporting Completeness by Period", 
         y = NULL, x = NULL, color = "Site Type",
         caption = "Note: Completeness derived by comparing HFR reporting against sites with DATIM results/targets
         Source: FY20Q1 MER + HFR") +
    theme_minimal() +
    theme(text = element_text(family = "Calibri Light"),
          axis.text.y = element_text(size = 7),
          plot.title = element_text(family = "Calibri", face = "bold"),
          strip.text = element_text(family = "Calibri", face = "bold"),
          strip.placement = "outside",
          panel.grid = element_blank(),
          legend.position = "none",
          plot.caption = element_text(color = "gray30"))

  ggsave(file.path(viz_folder,"HFR_Completeness_Pd.png"), dpi = 300, 
         width = 10, height = 5.625)
  