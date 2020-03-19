## PROJECT:  Pump up the jam
## AUTHOR:   tessam  achafetz| USAID
## LICENSE:  MIT
## PURPOSE:  visualize correctness
## DATE:     2020-03-19


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(vroom)
library(tidytext)
library(scales)
library(extrafont)
library(ggrepel)


# GLOBALS -----------------------------------------------------------------

  out_folder <- "Dataout"
  viz_folder <- "Images"
  
  
  pal <- viridis_pal()(6) #%>% show_col()
  color_hv_sites <- pal[1]
  color_ref <- "#C8C8C8"
  color_all_sites <- "gray30" #"#D3D3D3"


# IMPORT ------------------------------------------------------------------

  #import
    df_completeness_viz <- list.files(out_folder, "HFR_Correctness_[[:digit:]]+\\.csv", full.names = TRUE) %>% 
      vroom()


# CORRECTNESS PLOT FCN ----------------------------------------------------


  plot_cness <- function(df, ind, ous){
    
    df_viz <- df %>% 
      filter(!is.na(correctness_ratio) & indicator == {{ind}} & operatingunit %in% ous) %>% 
      mutate(site_sort = tidytext::reorder_within(orgunituid, correctness_ratio, correctness_distance),
             ou_order = fct_reorder(operatingunit, mer_targets, .fun = sum, .desc = TRUE),
             hv_flag = if_else(impflag_targets == 1, "High Volume", "Low Volume")) 
    
    df_viz %>% 
      ggplot(aes(x = mer_results, y = hfr_results, colour = hv_flag, 
                 size = mer_targets)) + 
      geom_point(aes(alpha = 0.80)) +
      geom_abline(intercept = 0, slope = 1, colour = "#909090") +
      geom_label_repel(aes(y = mer_results, label = percent(ou_annotation, 1)), size = 4, 
                 #hjust = 1, vjust = 1, 
                 fill = "white", na.rm = TRUE,
                 family = "Source Sans Pro") +
      facet_wrap(ou_order ~ hv_flag, scales = "free",
                 labeller = label_wrap_gen(multi_line = FALSE)) +
      theme_minimal() +
      scale_colour_manual(values = c("High Volume" = color_hv_sites, "Low Volume" = color_all_sites)) +
      scale_y_continuous(labels = comma) +
      scale_x_continuous(labels = comma) +
      theme(legend.position = "none",
            legend.justification = "left",
            legend.key.size = unit(2, "cm"),
            strip.text = element_text(hjust = 0)) +
      scale_size(guide = "none") +
      labs(x = "MER results", y = "HFR results",
           subtitle = paste({{ind}} ,"| FY20Q1 Site x Mechanism HFR Reporting Correctness"),
           caption = "Source: FY20Q1 MER + HFR",
           colour = "") +
      theme(text = element_text(family = "Source Sans Pro"),
            plot.title = element_text(face = "bold"),
            plot.caption = element_text(face = "plain", color = "gray30"))
  }

# PLOT --------------------------------------------------------------------

  #select OUs
    top_ous <- c("South Africa", "Kenya", "Mozambique", "Uganda",
                 "Malawi", "Tanzania", "Zimbabwe", "Nigeria")


  #plot and save for HTS_TST, TX_NEW, TX_CURR
    plot_cness(df_q1_cness, "HTS_TST", top_ous) +
      labs(title = "SIMILAR TRENDS IN CORRECTNESS ACROSS SITE TYPES")
    
      ggsave(file.path(viz_folder,"HFR_Correctness_HTS_TST.png"), dpi = 300, 
             width = 10, height = 5.625, scale = 1.25)
      
    plot_cness(df_q1_cness, "TX_NEW", top_ous) +
      labs(title = "SIMILAR TRENDS IN CORRECTNESS ACROSS SITE TYPES")
    
      ggsave(file.path(viz_folder,"HFR_Correctness_TX_NEW.png"), dpi = 300, 
             width = 10, height = 5.625, scale = 1.25)
    
    plot_cness(df_q1_cness, "TX_CURR", top_ous) +
      labs(title = "SIMILAR TRENDS IN CORRECTNESS ACROSS SITE TYPES")
    
      ggsave(file.path(viz_folder,"HFR_Correctness_TX_CURR.png"), dpi = 300, 
             width = 10, height = 5.625, scale = 1.25)



