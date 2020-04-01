## PROJECT:  Pump up the jam
## AUTHOR:   A.Chafetz, T.Essam | USAID
## LICENSE:  MIT
## PURPOSE:  viz completeness/correctness for FY20Q1 review
## DATE:     2020-03-27
## UPDATED:  2020-04-01


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
  
  
  color_ou_avg <- "#0067B9" #USAID Med Blue
  color_mechs <- "gray30" #"#C8C8C8"

  color_bar <- "#A7C6ED" #USAID Light Blue
  color_bar2 <- "#74a9cf"
  color_target <- color_ou_avg
  
  blues <- c('#a6bddb','#74a9cf','#3690c0','#0570b0','#045a8d','#023858')
  blues_c <- colorRampPalette(blues)(30)
  blues_d <- colorRampPalette(blues)(5)
  
  cb_bl <- c("#C7E9B4", "#7FCDBB", "#41B6C4", "#1D91C0", "#225EA8", "#0C2C84")
  cb_bl_c <- colorRampPalette(RColorBrewer::brewer.pal(9,"PuBu")[2:9])(30)



  

# IMPORT ------------------------------------------------------------------

  #import
    df_q1 <- list.files(out_folder, "HFR_DATIM_FY20Q1_Agg_[[:digit:]]+\\.csv", full.names = TRUE) %>% 
      vroom()
    
    df_wks_comp_gap <- list.files(out_folder, "HFR_OU_Wk_GapTarget_CompleteOnly_[[:digit:]]+\\.csv", full.names = TRUE) %>% 
      vroom()
    


# MUNGE -------------------------------------------------------------------

## Completeness/Correctness
    
  #calc OU level completeness and correctnessess (and their distance from 100% for color) 
    df_cc_site <- df_q1 %>%
      filter(!(has_hfr_reporting == TRUE & is_datim_site == FALSE)) %>% 
      filter_at(vars(hfr_results, mer_results, mer_targets), any_vars(.!=0)) %>% 
      group_by(operatingunit, mech_code, indicator) %>%
      summarise_at(vars(hfr_results, mer_results, mer_targets, has_hfr_reporting, is_datim_site), sum, na.rm = TRUE) %>%
      ungroup() %>%
      mutate(completeness_value = case_when(is_datim_site >  0 ~ has_hfr_reporting / is_datim_site),
             completeness_dist = abs(1-completeness_value),
             correctness_value = hfr_results/mer_results,
             correctness_dist = abs(1-correctness_value))
    
  #reshape to get completeness/correctness in one col for plotting and value/dist as their own cols
    df_cc_site <- df_cc_site %>% 
      select(operatingunit, mech_code, indicator, completeness_value:correctness_dist) %>% 
      gather(type, value, -operatingunit,  -mech_code, -indicator) %>% 
      separate(type, c("measure", "type")) %>% 
      spread(type, value)
  
  
  #OU completeness for each indicator
    df_cc_ou <- df_q1 %>%
      filter(!(has_hfr_reporting == TRUE & is_datim_site == FALSE)) %>% 
      filter_at(vars(hfr_results, mer_results, mer_targets), any_vars(.!=0)) %>% 
      group_by(operatingunit, indicator) %>%
      summarise_at(vars(hfr_results, mer_results, mer_targets, has_hfr_reporting, is_datim_site), sum, na.rm = TRUE) %>%
      ungroup() %>%
      mutate(completeness_value = case_when(is_datim_site >  0 ~ has_hfr_reporting / is_datim_site),
             completeness_dist = abs(1-completeness_value),
             correctness_value = hfr_results/mer_results,
             correctness_dist = abs(1-correctness_value))
    
    #reshape to get completeness/correctness in one col for plotting and value/dist as their own cols
    df_cc_ou <- df_cc_ou %>% 
      select(operatingunit, indicator, completeness_value:correctness_dist) %>% 
      gather(type, value, -operatingunit, -indicator) %>% 
      separate(type, c("measure", "type")) %>% 
      spread(type, value) %>% 
      rename_at(vars(dist, value), ~ paste0("ou_", .))

    
    df_cc_site <- bind_rows(df_cc_site, df_cc_ou)
  
  #clean up variable country
    df_cc_site <- df_cc_site %>% 
      mutate(operatingunit = case_when(operatingunit == "Democratic Republic of the Congo" ~ "DRC",
                                       operatingunit == "Dominican Republic" ~ "DR",
                                       operatingunit == "Western Hemisphere Region" ~ "WH Region",
                                       TRUE ~ operatingunit))
  
## Gap target
    
  #clean up
    df_gap <- df_wks_comp_gap %>% 
      filter(!indicator %in% c("TX_CURR", "TX_MMD")) %>% 
      mutate(hfr_pd = as.character(hfr_pd),
             operatingunit = case_when(operatingunit == "Democratic Republic of the Congo" ~ "DRC",
                                              operatingunit == "Dominican Republic" ~ "DR",
                                              operatingunit == "Western Hemisphere Region" ~ "WH Region",
                                              TRUE ~ operatingunit))
    df_gap_pd <- df_wks_comp_gap %>% 
      filter(indicator %in% c("TX_CURR", "TX_MMD")) %>% 
      group_by(operatingunit, indicator, hfr_pd) %>% 
      #summarise_at(vars(hfr_results, mer_targets, gap_target), max, na.rm = TRUE) %>% 
      summarise(date = min(date, na.rm = TRUE),
                hfr_results = max(hfr_results, na.rm = TRUE), 
                mer_targets = max(mer_targets, na.rm = TRUE), 
                gap_target = max(gap_target, na.rm = TRUE)) %>% 
      ungroup() %>% 
      mutate(hfr_pd = as.character(hfr_pd),
             operatingunit = case_when(operatingunit == "Democratic Republic of the Congo" ~ "DRC",
                                       operatingunit == "Dominican Republic" ~ "DR",
                                       operatingunit == "Western Hemisphere Region" ~ "WH Region",
                                       TRUE ~ operatingunit))
    
    df_gap <- bind_rows(df_gap, df_gap_pd)


# CC PLOT FUNCTION --------------------------------------------------------

    plot_cc <- function(ind, path_out = NULL){
      
      viz <- df_cc_site %>% 
        filter(indicator == {{ind}}) %>% 
        mutate(ou_comp = case_when(measure == "completeness" ~ ou_value),
               value_plot = ifelse(value > 1.5, 1.5, value)) %>% 
        ggplot(aes(value_plot, fct_reorder(operatingunit, ou_comp, sum, na.rm = TRUE))) +
        geom_rect(aes(xmin = 1, xmax = Inf, ymin = -Inf, ymax = Inf),
          fill = "#f6f6f6", alpha = 0.1) + 
        geom_vline(aes(xintercept = 0), color = "gray60") +
        geom_vline(aes(xintercept = 1), color = "gray60") +
        #geom_jitter(size = 3, color = color_mechs, alpha = .1, height = .1, na.rm = TRUE) +
        geom_jitter(aes(color = if_else(value_plot <= 1, value_plot, 1.9 - value_plot)), 
          alpha = 0.2, size = 3, height = .1, na.rm = TRUE) +
        geom_point(aes(ou_value, fill = if_else(ou_value <= 1, ou_value, 1.9 - ou_value)), 
          shape = 21, colour = "white", size = 6.5, na.rm = TRUE) +
        #geom_point(aes(ou_value), color = color_ou_avg, size = 6.5, na.rm = TRUE) +
        geom_text(aes(if_else(ou_value > 0.80, ou_value, NA_real_), label = percent(ou_value, 1)), 
          na.rm = TRUE, size = 2, colour = "white") +
        scale_x_continuous(label = percent) +
          scale_fill_gradientn(colours = cb_bl_c) +
        scale_color_gradientn(colours = cb_bl_c) +
        facet_wrap(~ measure, scales = "free_x") +
        labs(x = NULL, y = NULL,
             title = paste("Comparing", {{ind}}, "HFR Completeness and Correctness"),
             caption =  "Note: Correctness capped at 150%
           Source: FY20Q1 MER + HFR") +
        theme(plot.title = element_text(face = "bold"),
              strip.text = element_text(face = "bold"),
              #panel.border = element_rect(color = "gray30", fill = NA),
              plot.caption = element_text(color = "gray30", face = "plain"),
              legend.position = "none",
          strip.text.x = element_text(hjust = 0),
          axis.line = element_blank(),
          panel.grid.minor.x = element_blank(), 
          panel.border = element_blank(),
          panel.background = element_blank(),
          panel.spacing = unit(2, "lines")
          )
      
      if(!is.null(path_out)){
        ggsave(file.path(path_out, paste0("Q1Review_CC_", {{ind}}, ".png")), viz, 
               dpi = 330, width = 10, height = 5.66)
      } else {
        return(viz)
      }
      
    }    
    
    

# GAP TRENDS PLOT FUNCTION ------------------------------------------------

  plot_gap <- function(ind, path_out = NULL){
  
    viz <- df_gap %>% 
      filter(indicator == {{ind}}) %>% 
      ggplot(aes(date, hfr_results)) +
      geom_col(aes(date, gap_target), fill = "#f3f3f3") +
      geom_col(aes(fill = hfr_pd)) +
      # geom_col(fill = color_bar2) +
      geom_hline(aes(yintercept = 0), color = "gray30") +
      geom_hline(aes(yintercept = gap_target), color = "#F5A65D", size = 0.75) +
      facet_wrap(~ fct_reorder(operatingunit, mer_targets, .desc = TRUE), scale = "free_y") +
      scale_y_continuous(label = comma) +
      scale_x_date()+
      scale_fill_manual(values = blues_d) +
      labs(x = NULL, y = NULL,
           title = paste({{ind}}, "| HFR Weekly Results Against Target", 
            ifelse({{ind}} %in% c("TX_CURR", "TX_MMD"), "", "Gap")),
           caption =  paste0(ifelse({{ind}} %in% c("TX_CURR", "TX_MMD"), 
                "Note: Target = FY Target", "Note: Gap Target = FY Target / 52"),
           "\nSource: FY20Q1 MER + HFR")) +
      theme(plot.title = element_text(face = "bold"),
            strip.text = element_text(face = "bold", hjust = 0),
            plot.caption = element_text(color = "gray30", face = "plain"),
            panel.grid = element_blank(),
            legend.position = "none"
        
        )
    
    if(!is.null(path_out)){
      ggsave(file.path(path_out, paste0("Q1Review_Gap_", {{ind}}, ".png")), viz, 
             dpi = 330, width = 10, height = 5.66)
    } else {
      return(viz)
    }
    
    }
    
    
        
# PLOT --------------------------------------------------------------------

  theme_set(theme_minimal(base_family = "Source Sans Pro"))


  #test
    plot_cc("HTS_TST")
  

  #output for each ind
    unique(df_cc_site$indicator) %>% 
      setdiff("TX_NET_NEW") %>% 
      walk(plot_cc, viz_folder)


  #test
    plot_gap("HTS_TST_POS")
    
  #output for each ind
    walk(unique(df_gap$indicator),
         plot_gap, viz_folder)
    