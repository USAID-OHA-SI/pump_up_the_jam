## PROJECT:  Pump up the jam
## AUTHOR:   achafetz | USAID
## LICENSE:  MIT
## PURPOSE:  compare MER to Partner HFR
## DATE:     2020-05-06
## UPDATED:  2020-05-09


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(vroom)
library(lubridate)
library(Wavelength)
library(glitr)
library(scales)
library(extrafont)
library(ICPIutilities)
library(RColorBrewer)
library(patchwork)

# GLOBAL VARIABLES --------------------------------------------------------

  data_folder <- "Data"
  out_folder <- "Dataout"
  
  ind_order <- c("HTS_TST", "HTS_TST_POS", "TX_NEW", "TX_CURR", "TX_MMD",
                 "VMMC_CIRC", "PrEP_NEW")

  # paste(ind_order, "=", viridis_pal(direction = -1, end = .85)(7))
  pal <- c("HTS_TST" = "#9AD93CFF", "HTS_TST_POS" = "#47C16EFF", 
           "TX_NEW" = "#1FA188FF", "TX_CURR" = "#277E8EFF", 
           "TX_MMD" = "#375B8DFF", "VMMC_CIRC" = "#46327EFF", 
           "PrEP_NEW" = "#440154FF")

  
  # paste((1:12), '=', viridis_pal(direction = -1)(12))
  heatmap_pal <- c("1"  = "#FDE725FF", "2"  = "#C2DF23FF", "3"  = "#85D54AFF",
                   "4"  = "#51C56AFF", "5"  = "#2BB07FFF", "6"  = "#1E9B8AFF", 
                   "7"  = "#25858EFF", "8"  = "#2D708EFF", "9"  = "#38598CFF", 
                   "10" = "#433E85FF", "11" = "#482173FF", "12" = "#440154FF")
  
# IMPORT ------------------------------------------------------------------

  #read partner data
    df_hfr <- list.files(out_folder, "HFR_PARTNER", full.names = TRUE) %>% 
      hfr_read() %>% 
      mutate_at(vars(starts_with("hfr_results")), as.double)
    

# AGGREGATE ---------------------------------------------------------------
  
  #aggregateion function
    pd_agg <- function(df, rm_time_components = NULL){
      
      #remove time components for aggregation
      if(!is.null(rm_time_components))
        df <- select(df, -all_of(rm_time_components))
      
      #remove MMD disaggs, just keeping total
      df <- df %>% 
        filter(is.na(otherdisaggregate)) %>% 
        select(-otherdisaggregate)
      
      #reshape
      df <- df %>% 
        gather(type, value, hfr_results_ctry, hfr_results_ptnr, 
               mer_results, mer_targets, na.rm = TRUE) 
      
      #create a period value for non-TX_CURR indicators
      df_pd_sum <- df %>% 
        filter(!indicator %in% c("TX_CURR", "TX_MMD"),
               str_detect(type, "hfr_results")) %>% 
        group_by_if(is.character) %>% 
        summarise(value = sum(value, na.rm = TRUE)) %>% 
        ungroup()
      
      #create a period value for non-TX_CURR indicators
      df_pd_max <- df %>% 
        filter(indicator %in% c("TX_CURR", "TX_MMD") |
                 type %in% c("mer_results", "mer_targets")) %>% 
        group_by_if(is.character) %>% 
        summarise_if(is.numeric, max, na.rm = TRUE) %>% 
        ungroup()
      
      #join period dataset & respread
      df_pd <- bind_rows(df_pd_sum, df_pd_max)  %>% 
        spread(type, value, fill = 0)   #treat 0's as NA -> convert all NAs to 0
      
      return(df_pd)
      
    }
  
  #aggreggate to HFR period
   df_hfr_pdagg <- df_hfr %>% 
     unite(hfr_pd, fy, hfr_pd, sep = ".0") %>% 
     pd_agg("date") 
   
  #filter out full blank lines
   df_hfr_pdagg <- df_hfr_pdagg %>% 
     filter_at(vars(starts_with("hfr_results"), starts_with("mer_")), any_vars(. != 0))
   

# MERGING & VIZ SETUP -----------------------------------------------------

 #order indicators
   df_hfr_pdagg <- df_hfr_pdagg %>% 
     mutate(indicator = factor(indicator, ind_order))
   
  #create a merged iso and mech name for plot
   df_hfr_pdagg <- df_hfr_pdagg %>% 
     mutate(iso_mech = paste(iso_ou, mech_code),
            iso_mech = str_remove(iso_mech, "^NA "))
     

# FUNCTION - PLOT COMPARISON ----------------------------------------------


   plot_comparion <- function(ptnr, pd, out_path = NULL){
     
     plot <- df_hfr_pdagg %>%
       filter(hfr_pd == pd,
              sub_partner == ptnr) %>% 
       ggplot(aes(hfr_results_ptnr, hfr_results_ctry, color = indicator)) +
       geom_hline(aes(yintercept = 0)) +
       geom_vline(aes(xintercept = 0)) +
       geom_abline(aes(slope = 1, intercept = 0)) +
       geom_point(size = 2, alpha = .4) +
       scale_y_continuous(labels = comma_format(1)) +
       scale_x_continuous(labels = comma_format(1)) +
       facet_wrap(iso_mech ~ indicator, scales = "free", labeller = label_wrap_gen(multi_line=FALSE)) +
       scale_color_manual(values = pal) +
       labs(x = "Partner (HQ) Submisions", y = "USAID Country Team Submissions",
            title = paste(ptnr, "HFR", pd),
            subtitle = paste("comparsion of partner submission to USAID field missions")) +
       si_style() +
       theme(strip.placement = "outside",
             plot.title = element_text(face = "bold", size = 14),
             strip.text = element_text(face = "bold", size = 8),
             strip.text.y = element_text(vjust = .5),
             axis.text = element_text(size = 8),
             legend.position = "none")
     
     if(!is.null(out_path)){
       filename <- paste0("HFR_Comparison_", ptnr, "_", pd, ".png")
       ggsave(filename, path = out_path,
              width = 10, height = 5.625, dpi = 300)
     }
     
      return(plot)
   }
   
   

# PLOT COMPARISON SCATTER PLOTS -------------------------------------------

  #distinct list of partners & periods   
    ptnr_tbl <- df_hfr_pdagg %>% 
     distinct(sub_partner, hfr_pd) %>% 
     arrange(sub_partner)
   
   partners <- unique(ptnr_tbl$sub_partner)
   
  #test
   plot_comparion(ptnr_tbl$sub_partner[1], ptnr_tbl$hfr_pd[1])
  
  #scatter plots for each partner and pd
    walk2(ptnr_tbl$sub_partner, ptnr_tbl$hfr_pd, 
          plot_comparion, out_path ="Images")
   

# CALCULATE COMPLETENESS --------------------------------------------------

   #create flags for whether site reported HFR and if site exists in DATIM
     df_comp <- df_hfr_pdagg %>% 
       mutate(has_hfr_reporting_ptnr = hfr_results_ptnr > 0 ,
              has_hfr_reporting_ctry = hfr_results_ctry > 0 ,
              is_datim_site = mer_results > 0 | mer_targets > 0)
     
   #aggregate to country x mech level
     df_comp <- df_comp %>% 
       group_by(mech_code, sub_partner, hfr_pd, indicator, mech_name, primepartner,
                operatingunit, iso_ou, iso_mech) %>% 
       summarise_at(vars(has_hfr_reporting_ptnr, has_hfr_reporting_ctry, is_datim_site, mer_targets), sum, na.rm = TRUE) %>% 
       ungroup()
     
  #calculate completeness
     df_comp <- df_comp %>% 
       mutate(completeness_ptnr = has_hfr_reporting_ptnr / is_datim_site,
              completeness_ctry = has_hfr_reporting_ctry / is_datim_site) %>% 
       mutate_at(vars(starts_with("completeness")), ~ ifelse(is.nan(.) | is.infinite(.), NA, .))
     
  #bands
     df_comp <- df_comp %>% 
       mutate(completeness_band = case_when(completeness_ptnr ==0 ~ 1,
                                            completeness_ptnr <= 1 ~ round(completeness_ptnr/.1, 0),
                                            !is.na(completeness_ptnr) ~ 12),
              completeness_band = as.character(completeness_band))
       

# FUNCTION - PLOT COMPLETENESS --------------------------------------------


  plot_completeness <- function(ptnr, out_path = NULL){
    plot <- df_comp %>% 
      filter(sub_partner == ptnr) %>% 
      ggplot(aes(hfr_pd, fct_reorder(iso_mech, mer_targets, sum), fill = completeness_band)) +
      geom_tile(color = "white", size = 0.25) +
      geom_text(aes(label = percent(completeness_ptnr),
                    color = ifelse(completeness_band <= 1, "dark", "light")),
                size = 2.5, na.rm = TRUE) +
      scale_fill_manual(values = heatmap_pal, na.value = "gray80") +
      scale_color_manual(values = c("dark" = "gray30", "light" = "white"), guide = FALSE) +
      facet_wrap(~ indicator, nrow = 1) +
      labs(title = paste(toupper(ptnr), "COMPLETENESS TRENDS ACROSS ALL SITES"),
           subtitle = "Site x Mechanism HFR Reporting Completeness by Period",
           y = NULL, x = NULL, color = "Site Type",
           caption = "Note: Completeness derived by comparing HFR reporting against sites with DATIM results/targets
          Gray shaded boxes represent submissions that have no match in DATIM",
           fill = "Reporting completeness (100% = all sites reporting) ") +
      theme_minimal() + 
      coord_fixed(ratio = 1) +
      theme(legend.position = "none",
            legend.justification = c(0, 0),
            panel.grid = element_blank(),
            axis.text.x = element_text(size = 7),
            strip.text = element_text(hjust = 0))
    
    if(!is.null(out_path)){
      filename <- paste0("HFR_Completeness_", ptnr, ".png")
      ggsave(filename, path = out_path,
             width = 10, height = 5.625, dpi = 300)
    }
    
    return(plot)
    
  } 
   
     

# PLOT COMPLETENESS -------------------------------------------------------

    
    #test
     plot_completeness(partners[1], "Images")
     
    #scatter plots for each partner and pd
     walk(partners, plot_completeness, out_path ="Images")
     
     


# STRUCTURE TRENDS REVIEW -------------------------------------------------

     
     wkly_ind <- c("HTS_TST_POS", "TX_NEW", "PrEP_NEW", "VMMC_CIRC")
     
     trends_grp <- c("sub_partner", "date", "indicator", "iso_mech")
     trends_grp_pd <- c("sub_partner", "hfr_pd", "indicator", "iso_mech")
     
  #trends in weekly indicators by date and mech
     df_trends_viz <- df_hfr %>% 
       filter(indicator %in% wkly_ind,
              hfr_results_ptnr > 0) %>%
       mutate(iso_mech = paste(iso_ou, mech_code),
              iso_mech = str_remove(iso_mech, "^NA ")) %>% 
       group_by_at(vars(all_of(trends_grp))) %>% 
       summarise_if(is_double, sum, na.rm = TRUE) %>% 
       ungroup() %>% 
       mutate(indicator = factor(indicator, wkly_ind)) %>%
       group_by(indicator, iso_mech) %>% 
       mutate(point = case_when(date %in% c(max(date), min(date)) ~ hfr_results_ptnr)) %>% 
       ungroup() %>% 
       complete(date, nesting(sub_partner, indicator)) %>% 
       arrange(sub_partner, iso_mech, indicator, date)
     
  #identify start dates for HFR periods
     pd_dates <- df_hfr %>% 
       distinct(hfr_pd, date) %>% 
       group_by(hfr_pd) %>% 
       summarise(date = min(date)) %>% 
       ungroup()
     
    #trends in TX_CURR and MMD (3+ mo)
     df_trends_txcurr <- df_hfr %>% 
       filter(indicator == "TX_CURR" | 
                (indicator == "TX_MMD" & otherdisaggregate == "+3 months")) %>%
       mutate(iso_mech = paste(iso_ou, mech_code),
              iso_mech = str_remove(iso_mech, "^NA ")) %>%
       group_by_at(vars(all_of(trends_grp), hfr_pd)) %>% 
       summarise_if(is_double, sum, na.rm = TRUE) %>% 
       ungroup() %>%
       select(-date) %>% 
       group_by_at(vars(all_of(trends_grp_pd))) %>% 
       summarise_if(is_double, max, na.rm = TRUE) %>% 
       ungroup() %>%
       select(-c(starts_with("mer_"), hfr_results_ctry)) %>% 
       filter(hfr_results_ptnr > 0) %>%
       left_join(pd_dates, by = "hfr_pd") %>% 
       spread(indicator, hfr_results_ptnr, fill = 0) %>% 
       mutate(`+3 Months MMD (% of TX_CURR)` = TX_MMD / TX_CURR,
              hfr_pd = paste0("2020.0", hfr_pd)) %>% 
       gather(indicator, hfr_results_ptnr, -sub_partner, -hfr_pd, -iso_mech, -date) %>% 
       group_by(indicator, iso_mech) %>% 
       mutate(point = case_when(hfr_pd %in% c(max(hfr_pd), min(hfr_pd)) ~ hfr_results_ptnr)) %>% 
       ungroup() %>%
       complete(hfr_pd, nesting(sub_partner, indicator)) %>% 
       arrange(sub_partner, iso_mech, indicator, hfr_pd)
     
# FUNCTION - PLOT TRENDS --------------------------------------------------

     plot_wklytrends <- function(ptnr, out_path = NULL) {
       
       plot1 <- df_trends_viz %>% 
         filter(sub_partner == ptnr,
                indicator %in% c("HTS_TST_POS", "TX_NEW")) %>% 
         ggplot(aes(date, hfr_results_ptnr)) +
         geom_point(aes(y = point), color = "gray80", na.rm = TRUE) +
         geom_path(aes(group = iso_mech), na.rm = TRUE, size = .9, color = "gray80") +
         geom_smooth(method = "loess", formula = "y ~ x", se = FALSE, na.rm = TRUE, size = 1.5, color = "#440154FF") +
         geom_hline(aes(yintercept = 0)) +
         facet_wrap(~indicator, nrow = 1) +
         labs(x = NULL, y = NULL) +
         scale_y_continuous(labels = comma) +
         scale_x_date(breaks = as.Date(c("2020-01-20", "2020-02-17", "2020-03-16")),
                      date_labels = "%b %d") +
         si_style_ygrid() +
         theme(legend.position = "none")
       
       plot2 <- df_trends_viz %>% 
         filter(sub_partner == ptnr,
                indicator %in% c("PrEP_NEW", "VMMC_CIRC")) %>% 
         ggplot(aes(date, hfr_results_ptnr)) +
         geom_point(aes(y = point), color = "gray80", na.rm = TRUE) +
         geom_path(aes(group = iso_mech), na.rm = TRUE, size = .9, color = "gray80") +
         geom_smooth(method = "loess", formula = "y ~ x", se = FALSE, na.rm = TRUE, size = 1.5, color = "#440154FF") +
         geom_hline(aes(yintercept = 0)) +
         facet_wrap(~indicator, scales = "free_y", nrow = 1) +
         labs(x = NULL, y = NULL) +
         scale_y_continuous(labels = comma) +
         scale_x_date(breaks = as.Date(c("2020-01-20", "2020-02-17", "2020-03-16")),
                      date_labels = "%b %d") +
         si_style_ygrid() +
         theme(legend.position = "none")
       
       plot <- plot1 + plot2
       
       if(!is.null(out_path)){
         filename <- paste0("HFR_WklyTrends_", ptnr, ".png")
         ggsave(filename, path = out_path,
                width = 10, height = 3, dpi = 300)
       }
       
       return(plot)
     }
     
     
     plot_pdtrends <- function(ptnr, out_path = NULL){
       
       trends_mmd <- df_trends_txcurr %>% 
         filter(sub_partner == ptnr,
                indicator == "+3 Months MMD (% of TX_CURR)") %>% 
         ggplot(aes(date, hfr_results_ptnr)) +
         geom_point(aes(y = point), color = "gray80", na.rm = TRUE) +
         geom_path(aes(group = iso_mech), na.rm = TRUE, size = .9, color = "gray80") +
         geom_smooth(se = FALSE, na.rm = TRUE, size = 1.5, color = "#440154FF") +
         geom_hline(aes(yintercept = 0)) +
         facet_wrap(~indicator, nrow = 1) +
         labs(x = NULL, y = NULL) +
         expand_limits(y = 1) +
         scale_y_continuous(labels = percent_format(1)) +
         scale_x_date(breaks = as.Date(c("2020-01-20", "2020-02-17", "2020-03-16")),
                      date_labels = c("2020.05", "2020.06", "2020.07")) +
         si_style_ygrid() +
         theme(legend.position = "none")
       
       trends_tx <- df_trends_txcurr %>% 
         filter(sub_partner == ptnr,
                indicator == "TX_CURR") %>% 
         ggplot(aes(date, hfr_results_ptnr)) +
         geom_point(aes(y = point), color = "gray80", na.rm = TRUE) +
         geom_path(aes(group = iso_mech), na.rm = TRUE, size = .9, color = "gray80") +
         geom_smooth(se = FALSE, na.rm = TRUE, size = 1.5, color = "#440154FF") +
         geom_hline(aes(yintercept = 0)) +
         facet_wrap(~indicator, nrow = 1) +
         labs(x = NULL, y = NULL) +
         expand_limits(y = 1) +
         scale_y_continuous(labels = comma) +
         scale_x_date(breaks = as.Date(c("2020-01-20", "2020-02-17", "2020-03-16")),
                      date_labels = c("2020.05", "2020.06", "2020.07")) +
         si_style_ygrid() +
         theme(legend.position = "none")
       
       plot <- trends_tx + trends_mmd
       
       if(!is.null(out_path)){
         filename <- paste0("HFR_PdTrends_", ptnr, ".png")
         ggsave(filename, path = out_path,
                width = 6, height = 3, dpi = 300)
       }
       
       return(plot)
       
     }
     

# PLOT TRENDS -------------------------------------------------------------

  #test
   plot_wklytrends(partners[1])
   
  #export plots
   walk(partners, plot_wklytrends, out_path = "Images")
     
  #test
   plot_pdtrends(partners[1])
   
  #export plots
   walk(partners, plot_pdtrends, out_path = "Images")     
   