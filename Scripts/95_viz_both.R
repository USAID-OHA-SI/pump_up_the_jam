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
  library(tidytext)
  library(extrafont)


# GLOBAL VARIABLES --------------------------------------------------------

  out_folder <- "Dataout"
  viz_folder <- "Images"
  
  ind_sel  <- c("HTS_TST", "TX_NEW", "TX_CURR")
  
  pal <- viridis_pal()(6) #%>% show_col()
  color_hv_sites <- pal[1]
  color_ref <- "gray30" #"#C8C8C8"
  color_all_sites <- "#D3D3D3"

  cc_thresh = 0.80
  quarter <- "Q2"


# IMPORT ------------------------------------------------------------------

  #import
  file_name <- paste0("HFR_DATIM_FY20", quarter, "_Agg_[[:digit:]]+\\.csv") 
    df_qtr <- list.files(out_folder, file_name, full.names = TRUE) %>% 
      vroom()


# MUNGE -------------------------------------------------------------------

  #calc OU level completeness and correctnessess (and their distance from 100% for color) 
    df_cc <- df_qtr %>%
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
             operatingunit = fct_reorder(operatingunit, sort_max)) %>% 
    group_by(indicator, measure) %>% 
      mutate(indicator_ave = mean(value, na.rm = TRUE)) %>% 
      ungroup() %>% 
      arrange(operatingunit, indicator) %>% 
      mutate(ou_id = group_indices(., operatingunit)) %>% 
    ungroup() %>% 
      group_by(operatingunit, indicator) %>% 
      mutate(comp_value = ifelse(measure == "completeness", value, NA_real_)) %>% 
      fill(comp_value, .direction = "updown") %>% 
      mutate(corr_value = ifelse(measure == "correctness", value, NA_real_)) %>% 
      fill(corr_value, .direction = "updown") %>% 
    ungroup() %>% 
      mutate(ou_corr_sort = reorder_within(operatingunit, comp_value, indicator, .desc = TRUE),
        color_flag = if_else(comp_value >= cc_thresh & corr_value >=  cc_thresh, 1, 0))
      

    #Global data frame w/ correct aggregates  
    
    df_cc_glob <- df_qtr %>%
      filter(indicator %in% ind_sel,
             !(has_hfr_reporting == TRUE & is_datim_site == FALSE)) %>% 
      filter_at(vars(hfr_results, mer_results, mer_targets), any_vars(.!=0)) %>% 
      group_by(indicator) %>% 
      summarise_at(vars(hfr_results, mer_results, mer_targets, has_hfr_reporting, is_datim_site), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      mutate(completeness_value = case_when(is_datim_site >  0 ~ has_hfr_reporting / is_datim_site),
             completeness_dist = abs(1-completeness_value),
             correctness_value = hfr_results/mer_results,
             correctness_dist = abs(1-correctness_value))
    
    #reshape to get completeness/correctness in one col for plotting and value/dist as their own cols
    df_cc_glob <- df_cc_glob %>% 
      select(indicator, n_sites = is_datim_site, completeness_value:correctness_dist) %>% 
      gather(type, value, -indicator, -n_sites) %>% 
      separate(type, c("measure", "type")) %>% 
      spread(type, value) %>% 
      mutate(indicator = factor(indicator, ind_sel))

# PLOT --------------------------------------------------------------------

  theme_set(theme_minimal(base_family = "Source Sans Pro"))
    
  #dot comparison of completeness + correctness by O
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
           subtitle = paste0("FY20", quarter, " Site x Mechanism HFR Reporting Completeness and Correctness"),
           caption = paste0("Source: FY20", quarter, "MER + HFR")) +
      theme_minimal() +
      guides(size = guide_legend(reverse=T))+
      theme(panel.grid = element_blank(),
            text = element_text(family = "Source Sans Pro"),
            strip.placement = "outside",
            legend.position = "right",
            #legend.justification = c(0, 0)
            )
    
    
    #dot comparison of completeness + correctness by O
    df_cc %>% 
      ggplot(aes(measure, ou_corr_sort)) +
      geom_tile(aes(fill = factor(color_flag)), colour = "#404040") +
      #geom_tile(aes(fill = factor(ifelse(value >= cc_thresh, 1, 0))), colour = "#404040") +
      geom_text(aes(label = percent(value, 1)), na.rm = TRUE,
        family = "Source Sans Pro", size = 3,) +
      facet_wrap(~indicator, scales = "free_y") +
      scale_size_continuous(labels = comma) +
      scale_y_reordered() +
      scale_x_discrete(position = "top") +
      scale_fill_manual(values = c("0" = "#f4f4f4", "1" = "#c2a5cf")) +
      labs(x = NULL, y = NULL,
        #color = "Reporting completeness/correctness distance from 100% complete/correct",
        size = "number of sites",
        title = "FEW SITES REPORT HIGH LEVELS (>80%) OF BOTH COMPLETENESS/CORRECTNESS",
        subtitle = paste0("FY20", quarter, " Site x Mechanism HFR Reporting Completeness and Correctness"),
        caption = paste0("Source: FY20", quarter, "MER + HFR")) +
      theme_minimal() +
      guides(size = guide_legend(reverse=T))+
      theme(panel.grid = element_blank(),
        text = element_text(family = "Source Sans Pro"),
        strip.placement = "outside",
        legend.position = "none",
        strip.text = element_text(hjust = 0)
        #legend.justification = c(0, 0)
      )
    
    ggsave(file.path(viz_folder,"HFR_CC_table_Comparison.png"), dpi = 300, 
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
           subtitle = paste0("FY20", quarter, " Site x Mechanism HFR Reporting Correctness/Completeness"),
           color = NULL,
           caption = paste0("Source: FY20", quarter, "MER + HFR")) +
      scale_color_viridis_d() +
      scale_y_continuous(label = percent) +
      scale_x_continuous(label = percent) +
      scale_size(guide = "none") +
      theme_minimal() +
      theme(strip.text = element_text(hjust = 0),
            legend.position = "top")
    
    ggsave(file.path(viz_folder,"HFR_CC_Scatter.png"), dpi = 300, 
           width = 5.625, height = 5.625)  
    

# TIM'S SUGGESTIONS - NOT SURE WHERE THEY GO ------------------------------
   
     # What if we mimic the J2SR plots? 
    # Steps, create a separate data frame for the country of focus & indicator averages
    # Flag observations above 100%, these should get colored differently
    # Create custom color ramp that maxs at 100, then reflects previous side
    
    purples <- c("#edf8fb","#bfd3e6","#9ebcda","#8c96c6","#8c6bb1","#88419d","#6e016b", 
      "#88419d", "#8c6bb1", "#8c96c6", "#9ebcda", "#bfd3e6", "#edf8fb")
    colorRampPalette(purples)(30) %>% scales::show_col(labels = FALSE)
    
    
    #TODO: FIX FACET LABELS OR RE WORK AS FACET_GRID?
    
    hfr_cc_scplot <- function(df, ou_code = 15) {
      
      df_ou <- df %>% filter(ou_id == ou_code)
      ou_name <- df_ou %>% distinct(as.character(operatingunit)) %>% pull()
      
      # define max value of colors
      ou_max <- df %>% summarise(max = max(abs(value), na.rm = TRUE)) %>% pull()
      
      df %>%
        filter(ou_id != ou_code) %>%
        ggplot(aes(x = value, y = measure)) +
        geom_vline(data = df_ou, aes(xintercept = indicator_ave), colour = "#909090", linetype = "dotted") +
        geom_point(size = 4, alpha = 0.70, shape = 21, colour = color_all_sites, stroke = 1, fill = color_all_sites) +
        # geom_point(data = df_ou, aes(fill = (value <= 1)), size = 6, shape = 21, stroke = 0.5, colour = "#303030") +
        geom_point(data = df_ou, aes(fill = value), size = 6, shape = 21, stroke = 0.5, colour = "#303030") +
        facet_wrap(indicator ~ measure, nrow = 3, scales = "free_y") +
        geom_text(
          data = df_ou, aes(label = percent(value, accuracy = 1)),
          vjust = -1.25, hjust = 0.4
        ) +
        scale_fill_gradientn(
          colours = colorRampPalette(purples)(30),
          limits = c(0, 2)
        ) +
        # scale_fill_manual(values = c("FALSE" = "#a180a9", "TRUE" = color_hv_sites)) +
        scale_x_continuous(labels = percent_format(accuracy = 1), breaks = c(seq(0, 1, 0.2))) +
        theme_minimal() +
        theme(
          legend.position = "none",
          text = element_text(family = "Source Sans Pro"),
          panel.grid = element_blank(),
          strip.text = element_text(hjust = 0),
          panel.spacing = unit(3, "lines")
        ) +
        labs(
          x = NULL, y = NULL,
          title = str_c(stringr::str_to_upper(ou_name))
        )
    }
    
    # List to loop over of operating unit ids
    ou_list <- df_cc %>% distinct(ou_id) %>% pull()
    
    plots <- map(.x = ou_list, .f = ~ hfr_cc_scplot(df_cc, ou_code = .x))
    plots[7][[1]]
    
    #TODO: CREATE GLOBAL GRAPHIC WITH RESULTS AGGREGATED TO THE APPROPRIATE LEVEL
    
    # To functionalize spread (inspired by happy butt day)
    # TODO: MOVE TO GLOBALS?
    
    spread_scat <- function(df) {
      df_spread <- df %>%
        select(-indicator_ave, -ou_id) %>%
        gather(type, value, dist, value) %>%
        spread(measure, value) %>%
        filter(type == "value")
      return(df_spread)
    }
    
    # Function to create country specific dot plots
    hfr_cc_scatter <- function(df, ou_code = 15) {
      df_scatter <- df %>%
        filter(ou_id != ou_code) %>%
        spread_scat()
      
      df_ou <- df %>%
        filter(ou_id == ou_code) %>%
        spread_scat()
      
      ou_name <- df_ou %>%
        distinct(as.character(operatingunit)) %>%
        pull()
      
      df_scatter %>%
        ggplot(aes(correctness, completeness, size = n_sites)) +
        geom_point(fill = color_all_sites, na.rm = TRUE, alpha = 0.75, 
          shape = 21, colour = "white") +
        geom_point(
          data = df_ou, aes(size = n_sites),
          fill = color_hv_sites, shape = 21, colour = "303030", stroke = 0.25
        ) +
        geom_abline(intercept = 0, slope = 1, colour = "#D3D3D3") +
        facet_wrap(~indicator) +
        labs(
          title = str_c(stringr::str_to_upper(ou_name)),
          subtitle = paste0("FY20", quarter, " Site x Mechanism HFR Reporting Correctness/Completeness"),
          caption = paste0("Source: FY20", quarter, "MER + HFR")
        ) +
        scale_y_continuous(label = percent, limits = c(0, 1.5)) +
        scale_x_continuous(label = percent, limits =c(0, 1.5)) +
        coord_fixed() +
        scale_size(guide = "none") +
        theme_minimal() +
        theme(strip.text = element_text(hjust = 0))
    }
    
    hfr_cc_scatter(df_cc, ou_code = 10)
    hfr_scatter_plots <- map(.x = ou_list, .f = ~ hfr_cc_scatter(df_cc, ou_code = .x))
    hfr_scatter_plots[20][[1]]
    
    
    #TODO: DETERMINE BEST LAYOUT FOR THESE. MIGRATE DATA FRAMES OVER TO TABLEAU FOR TEMPLATING.
    
  

# INTRO SLIDE PLOT --------------------------------------------------------
  df_cc_glob_flt <-  df_cc_glob %>% filter(measure == "completeness")
     
    df_cc %>% 
      filter(measure == "completeness") %>% 
      ggplot(aes(value, fct_rev(measure))) + 
      geom_vline(aes(xintercept = 1), color = "gray60", linetype = "dashed") +
      geom_jitter(shape = 21, alpha = .5, height = .1, size = 3,
                  color = "white", fill = "#595959", na.rm = TRUE) +
      geom_point(data = df_cc_glob_flt, aes(value, fct_rev(measure)), size = 7, color = "#0067b9") +
      geom_text(data = df_cc_glob_flt, aes(label = percent(value, 1)),
                size = 2.6, color = "white",
                family = "Source Sans Pro") +
      facet_grid(indicator ~ ., switch = "y") +
      scale_x_continuous(labels = percent) +
      scale_color_manual(values = c("#8C88BF", "#730E75"), guide = FALSE) +
      labs(x = NULL, y = NULL,
           title = "HFR COMPLETENESS BETTER TREATMENT REPORTING \nTHAN TESTING IN FY20Q2",
           caption = "Source: FY20Q2 MER + HFR") +
      theme(strip.placement = "outside",
            strip.text = element_text(face = "bold"),
            panel.border = element_rect(color = "gray60", fill = NA),
            panel.grid.major.y = element_blank(),
            plot.title = element_text(face = "bold"),
            plot.caption = element_text(color = "gray30", face = "plain"),
            strip.text.y.left = element_text(angle = 0), 
            axis.text.y = element_blank())
    
    ggsave(file.path(viz_folder,"HFR_CC_Intro.png"), dpi = 300, 
           width = 7.24, height = 4.23)  
    

  
    