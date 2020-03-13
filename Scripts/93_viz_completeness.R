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

ind_sel  <- c("HTS_TST", "TX_NEW", "TX_CURR")

# IMPORT ------------------------------------------------------------------


  df_completeness_viz <- list.files(out_folder, "HFR_Completeness_[[:digit:]]+\\.csv", full.names = TRUE) %>% 
    vroom()


# MUNGE -------------------------------------------------------------------

#arrange
  df_completeness_viz <- df_completeness_viz %>% 
    mutate(operatingunit = case_when(operatingunit == "Democratic Republic of the Congo" ~ "DRC",
                                     operatingunit == "Dominican Republic" ~ "DR",
                                     operatingunit == "Western Hemisphere Region" ~ "WH Region",
                                     TRUE ~ operatingunit))

# PLOT --------------------------------------------------------------------
  

  df_completeness_viz %>% 
    filter(indicator %in% ind_sel) %>% 
    mutate(indicator = factor(indicator, ind_sel)) %>% 
    ggplot(aes(fct_reorder(operatingunit, is_datim_site, .fun = sum), completeness, group = operatingunit, color = site_type)) +
    geom_hline(aes(yintercept = 1), color = "gray40", linetype = "dashed") +
    geom_path(size = .9, color = 'gray60') +
    geom_point(size = 7) +
    scale_y_continuous(labels = percent) +
    scale_color_viridis_d(option = "C", end = .5) +
    coord_flip() +
    facet_grid(~ indicator) +
    labs(title = "FOCUSING ON IMPORTANT SITES PROVIDES BETTER REPORTING COMPLETESS",
         subtitle = "FY20Q1 Site x Mechanism HFR Reporting Completeness", 
         y = NULL, x = NULL, color = "Site Type",
         caption = "Note: Completeness derived by comparing HFR reporting against sites with DATIM results/targets
         Source: FY20Q1 MER + HFR") +
    theme_minimal() +
    theme(text = element_text(family = "Calibri Light"),
          plot.title = element_text(family = "Calibri", face = "bold"),
          strip.text = element_text(family = "Calibri", face = "bold"),
          #panel.grid.major.y = element_line(color = "gray50"),
          legend.position = "bottom",
          plot.caption = element_text(color = "gray30"))
