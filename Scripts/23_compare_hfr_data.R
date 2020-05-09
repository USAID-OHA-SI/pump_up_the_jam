## PROJECT:  Pump up the jam
## AUTHOR:   achafetz | USAID
## LICENSE:  MIT
## PURPOSE:  compare MER to Parnter HFR
## DATE:     2020-05-06
## UPDATED:  2020-05-08


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

# GLOBAL VARIABLES --------------------------------------------------------

  data_folder <- "Data"
  out_folder <- "Dataout"
  
  # paste(c("HTS_TST", "HTS_TST_POS", "TX_NEW", "TX_CURR", "TX_MMD", "VMMC_CIRC", "PrEP_NEW"), "=",
  #       RColorBrewer::brewer.pal(7, "Dark2"))

  pal <- c("HTS_TST" = "#1B9E77", "HTS_TST_POS" = "#D95F02", "TX_NEW" = "#7570B3", 
           "TX_CURR" = "#E7298A", "TX_MMD" = "#66A61E", "VMMC_CIRC" = "#E6AB02", 
           "PrEP_NEW" = "#A6761D")
  
  # paste((1:12), '=', viridis_pal(direction = -1)(12))
  heatmap_pal <- c("1"  = "#FDE725FF",
                   "2"  = "#C2DF23FF",
                   "3"  = "#85D54AFF",
                   "4"  = "#51C56AFF",
                   "5"  = "#2BB07FFF",
                   "6"  = "#1E9B8AFF",
                   "7"  = "#25858EFF",
                   "8"  = "#2D708EFF",
                   "9"  = "#38598CFF",
                   "10" = "#433E85FF",
                   "11" = "#482173FF",
                   "12" = "#440154FF")


  
# IMPORT ------------------------------------------------------------------

  #read partner data
    df_hfr <- list.files(out_folder, "HFR_PARTNER", full.names = TRUE) %>% 
      hfr_read() %>% 
      mutate_at(vars(starts_with("hfr_results")), as.double)
  
  #list of mechs by each partner
    df_ptnr_mechs <- read_csv("Dataout/HFR_CentralPartnerMechs.csv", 
                              col_types = c(.default = "c"))
    

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

  #merge on partner name for grouping
   df_hfr_pdagg <- df_ptnr_mechs %>%
     select(mech_code, sub_partner = partner) %>% 
     distinct() %>% 
     right_join(df_hfr_pdagg, by = "mech_code")
   
  #map on iso code (from Wavelength)
   df_hfr_pdagg <- df_hfr_pdagg %>% 
     left_join(iso_map, by = c("countryname" = "operatingunit")) %>% 
     select(-regional)
   
  #order indicators
   df_hfr_pdagg <- df_hfr_pdagg %>% 
     mutate(indicator = factor(indicator, c("HTS_TST", "HTS_TST_POS",
                                            "TX_NEW", "TX_CURR", "TX_MMD",
                                            "VMMC_CIRC", "PrEP_NEW")))
  #create a merged iso and mech name for plot
   df_hfr_pdagg <- df_hfr_pdagg %>% 
     mutate(iso_mech = paste(iso, mech_code),
            iso_mech = str_remove(iso_mech, "^NA "))
     

# PLOT FUNCTION -----------------------------------------------------------

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
       # facet_grid(iso_mech ~ indicator, switch = "y") +
       facet_wrap(iso_mech ~ indicator, scales = "free", labeller = label_wrap_gen(multi_line=FALSE)) +
       scale_color_brewer(type = "qual", palette = 2) +
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
    ptnr_tbl <- distinct(df_hfr_pdagg, sub_partner, hfr_pd)
   
  #test
   # plot_comparion(ptnr_tbl$sub_partner[1], ptnr_tbl$hfr_pd[1], "Images")
  
  #scatter plots for each partner and pd
    walk2(ptnr_tbl$sub_partner, ptnr_tbl$hfr_pd, 
          plot_comparion, out_path ="Images")
   

# CALCULATE COMPLETENESS --------------------------------------------------

   #create flags for whether site reported HFR and if site exists in DATIM
     df_comp <- df_hfr_pdagg %>% 
       mutate(has_hfr_reporting_ptnr = hfr_results_ptnr > 0 ,
              has_hfr_reporting_ctry = hfr_results_ctry > 0 ,
              is_datim_site = mer_results > 0 | mer_targets > 0)
   
   #remove where no DATIM results/targets and there is some value in the row
     # df_comp <- df_comp %>% 
     #   filter(!(has_hfr_reporting_ptnr == TRUE & is_datim_site == FALSE)) %>% 
     #   filter_at(vars(mer_results, mer_targets), any_vars(.!=0))
     
   #aggregate to country x mech level
     df_comp <- df_comp %>% 
       group_by(mech_code, sub_partner, hfr_pd, indicator, mech_name, primepartner,
                countryname, iso, iso_mech) %>% 
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
       
# PLOT COMPLETENESS -------------------------------------------------------

   
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
   
     
     ptnrs <- unique(df_comp$sub_partner)
     
    #test
     plot_completeness(ptnrs[4])
     
    #scatter plots for each partner and pd
     walk2(ptnr_tbl$sub_partner, ptnr_tbl$hfr_pd, 
           plot_comparion, out_path ="Images")
   