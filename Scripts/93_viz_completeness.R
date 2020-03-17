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
color_ref <- "#C8C8C8"
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
    
    
  # Run lowess regression on each indicator + site type with time as the xvar.
  # Saving results so we can make an arrow at the end of the fitted line with geom_segement
  # NOTE: Didn't work out as expect, simply adding a circle at the end of fitted line
  # Functions to allow for filtering on the data frames going into the flow process
    
    heatmap_prep <- function(df) {
      df <-
        df_completeness_pds_viz %>%
        filter(
          indicator %in% ind_sel,
          site_type != "Low Volume (Target)"
        ) %>%
        group_by(operatingunit) %>%
        mutate(sort_max = ifelse(site_type == "High Volume (Target)" & indicator == "HTS_TST", completeness, NA_real_)) %>%
        fill(sort_max, .direction = "updown") %>%
        ungroup() %>%
        mutate(
          indicator = factor(indicator, ind_sel),
          ou_sort = fct_reorder(operatingunit, sort_max),
          rank = percent_rank(completeness)
        )
      
      return(df)
    }
    
    prep_fitline <- function(df) {
      
      # Convert to numeric so it can be used in the local regression as a covariate
      model_data <- df %>% mutate(timevar = as.numeric(hfr_pd))
      
      models <- model_data  %>%
        tidyr::nest(-site_type, - indicator) %>%
        mutate(
          fit = purrr:: map(data, ~ loess(completeness ~ timevar, weights = mer_targets, span = 0.75, data = .)),
          # Extract out fitted models
          fitted = purrr::map(fit, `[[`, "fitted")
        )
      
      df_results <- models %>%
        dplyr::select(-fit) %>%
        tidyr::unnest(cols = c(data, fitted))
      
      return(df_results)
    }
      
    collapse_results <- function(df) {
    
      # Collapsing down results to get the placement of the final point for the dot at end of line
      # Called in the ggplot function
      df_results_collapse <- df %>% 
        filter(indicator %in% ind_sel,
          site_type != "Low Volume (Target)") %>% 
        group_by(indicator, site_type, hfr_pd) %>% 
        summarise(arrows = mean(fitted)) %>% 
        filter(hfr_pd == max(hfr_pd)) %>% 
        mutate(grp = paste(site_type, indicator),
          hfr_pd = as.character(hfr_pd)) %>% 
        ungroup()
      
      return(df_results_collapse)
    }
    
    sparkline_prep <- function(df) {
    
      df_hmap_bottom <- df %>%
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
      ) 
  }
   
  # Prep data frames needed for sparklines; Collapse function called within ggplot function 
    df_heatmap <- heatmap_prep(df_completeness_pds_viz)
    
    df_results_nonzero_sparkline <- prep_fitline(df_completeness_pds_viz %>% filter(completeness > 0)) %>%
      sparkline_prep()

    df_results_sparkline <- prep_fitline(df_completeness_pds_viz) %>% 
      sparkline_prep()

     
  
  
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
         width = 10, height = 5.625, scale = 1.25)
  
  

  
  # Heatmap of completeness with fixed coordinates to get squares   
  df_heatmap %>% 
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
    theme_minimal() + 
      coord_fixed(ratio = .007) +
    theme(legend.position = "top",
          legend.justification = c(0, 0),
          panel.grid = element_blank(),
          axis.text.x = element_text(size = 7),
          strip.text = element_text(hjust = 0))

  ggsave(file.path(viz_folder,"HFR_Completeness_Pd.png"), dpi = 300, 
         width = 18, height = 10)

  
  # Need two plots here, one w/ filtering out 0s one without
  # Functionalize plot
  
    sparkline_plot <- function(df) {
    # df = data frame to plot the sparkline
    # df_dot - data frame used to create the dot at end
    
    df %>% 
      ggplot(aes(hfr_pd, completeness, group = grp)) +
      geom_hline(aes(yintercept = 0), color = "gray30") +
      geom_jitter(size = 2, width = .2, alpha = 0.60, colour = "#D0D0D0") +
      geom_smooth(aes(weight = mer_targets), se = FALSE, color = "#303030") +
      geom_point(data = collapse_results(df) ,
        aes(x = hfr_pd, y = arrows, fill = arrows), shape = 21, size = 2.5) +
      scale_y_continuous(labels = percent) +
      scale_fill_viridis_c(option = "A", direction = -1, labels = percent, end = 0.9) +
      facet_wrap(site_type ~ indicator, nrow = 1) +
      #facet_grid(rev(site_type) ~ indicator, switch = "y") +
      labs(y = NULL, x = NULL,
        caption = "Notes:
         (a) Completeness derived by comparing HFR reporting against sites with DATIM results/targets
         (b) Each dot represents an OU's Site x Mechanism completeness for each period
         (c) MER targets included as weighting in polynomial fit line
         Source: FY20Q1 MER + HFR",
        color = "Reporting completeness (100% = all sites reporting) ") +
      theme_minimal() + 
      theme(legend.position = "none",
        legend.justification = c(0, 0),
        axis.text.x = element_text(size = 7),
        strip.text = element_text(hjust = 0),
        strip.placement = "outside")
  }
  
  # Return the two different plots
  spk_plot_nonzero <- sparkline_plot(df_results_nonzero_sparkline) + 
    labs(title = "FOCUSING ON IMPORTANT SITES PROVIDES BETTER REPORTING COMPLETENESS",
      subtitle = "FY20Q1 Site x Mechanism HFR Reporting Completeness by Period")
  
  ggsave(file.path(viz_folder,"HFR_Completeness_Pd_Trends_nonzero.png"), 
    plot = spk_plot_nonzero,
    dpi = 300, 
    width = 10, height = 5.625, scale = 1.25)
  
  spk_plot_all <- sparkline_plot(df_results_sparkline) + 
    labs(title = "FOCUSING ON ALL SITES PROVIDES INFERIOR REPORTING COMPLETENESS",
      subtitle = "FY20Q1 Site x Mechanism HFR Reporting Completeness by Period")
  
  ggsave(file.path(viz_folder,"HFR_Completeness_Pd_Trends_all.png"), 
    plot = spk_plot_all,
    dpi = 300, 
    width = 10, height = 5.625, scale = 1.25)
  


  