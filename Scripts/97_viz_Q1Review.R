## PROJECT:  Pump up the jam
## AUTHOR:   A.Chafetz, T.Essam | USAID
## LICENSE:  MIT
## PURPOSE:  viz completeness/correctness for FY20Q1 review
## DATE:     2020-03-27
## UPDATED:  


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
  color_target <- color_ou_avg

# IMPORT ------------------------------------------------------------------

  #import
    df_q1 <- list.files(out_folder, "HFR_DATIM_FY20Q1_Agg_[[:digit:]]+\\.csv", full.names = TRUE) %>% 
      vroom()
  
    df_pds_comp_gap <- list.files(out_folder, "HFR_OU_Pd_GapTarget_CompleteOnly_[[:digit:]]+\\.csv", full.names = TRUE) %>% 
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
  
  
  #calc OU avg for eac indicator
    df_cc_ou_avg <- df_cc_site %>% 
      group_by(operatingunit, indicator, measure) %>% 
      summarise(avg_ou = mean(value, na.rm = TRUE)) %>% 
      ungroup() %>% 
      filter(is.finite(avg_ou))
    
    df_cc_site <- bind_rows(df_cc_site, df_cc_ou_avg)
  
  #clean up variable country
    df_cc_site <- df_cc_site %>% 
      mutate(operatingunit = case_when(operatingunit == "Democratic Republic of the Congo" ~ "DRC",
                                       operatingunit == "Dominican Republic" ~ "DR",
                                       operatingunit == "Western Hemisphere Region" ~ "WH Region",
                                       TRUE ~ operatingunit))
  
## Gap target
    
  #clean up
    df_gap <- df_pds_comp_gap %>% 
      mutate(hfr_pd = as.character(hfr_pd),
             operatingunit = case_when(operatingunit == "Democratic Republic of the Congo" ~ "DRC",
                                              operatingunit == "Dominican Republic" ~ "DR",
                                              operatingunit == "Western Hemisphere Region" ~ "WH Region",
                                              TRUE ~ operatingunit))


# CC PLOT FUNCTION --------------------------------------------------------

    plot_cc <- function(ind, path_out = NULL){
      
      viz <- df_cc_site %>% 
        filter(indicator == {{ind}}) %>% 
        mutate(ou_comp = case_when(measure == "completeness" ~ avg_ou),
               value_plot = ifelse(value > 1.5, 1.5, value)) %>% 
        ggplot(aes(value_plot, fct_reorder(operatingunit, ou_comp, sum, na.rm = TRUE))) +
        geom_vline(aes(xintercept = 0), color = "gray60") +
        geom_vline(aes(xintercept = 1), color = "gray60") +
        geom_jitter(size = 3, color = color_mechs, alpha = .2, height = .1, na.rm = TRUE) +
        geom_point(aes(avg_ou), color = color_ou_avg, size = 6.5, na.rm = TRUE) +
        geom_text(aes(avg_ou, label = percent(avg_ou, 1)), size = 2, color = "white", na.rm = TRUE) +
        scale_x_continuous(label = percent, limits = c(0, 1.5)) +
        facet_grid(~ measure) +
        labs(x = NULL, y = NULL,
             title = paste("Comparing", {{ind}}, "HFR Completeness and Correctness"),
             caption =  "Note: Correctness capped at 150%
           Source: FY20Q1 MER + HFR") +
        theme(plot.title = element_text(face = "bold"),
              strip.text = element_text(face = "bold"),
              panel.border = element_rect(color = "gray30", fill = NA),
              plot.caption = element_text(color = "gray30", face = "plain"))
      
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
      ggplot(aes(hfr_pd, hfr_results)) +
      geom_col(fill = color_bar) +
      geom_hline(aes(yintercept = 0), color = "gray30") +
      geom_hline(aes(yintercept = gap_target), color = color_target, size = 1.2) +
      facet_wrap(~ fct_reorder(operatingunit, mer_targets, .desc = TRUE), scale = "free_y") +
      scale_y_continuous(label = comma) +
      labs(x = NULL, y = NULL,
           title = paste({{ind}}, "| HFR Results Against Period Gap Target"),
           caption =  "Note: Gap Target = FY Target / 13 
           Source: FY20Q1 MER + HFR") +
      theme(plot.title = element_text(face = "bold"),
            strip.text = element_text(face = "bold"),
            plot.caption = element_text(color = "gray30", face = "plain"),
            panel.grid = element_blank())
    
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
    plot_gap("HTS_TST")
    
  #output for each ind
    walk(unique(df_gap$indicator),
         plot_gap, viz_folder)
    