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

# GLOBAL VARIABLES --------------------------------------------------------

  data_folder <- "Data"
  out_folder <- "Dataout"


# IMPORT ------------------------------------------------------------------

  #read partner data
    df_full <- list.files(out_folder, "HFR_PARTNER", full.names = TRUE) %>% 
      hfr_read()
    

# RESHAPE -----------------------------------------------------------------

  
  #reshape
    df_full_lng <- df_full %>% 
      gather(type, value, hfr_results_ctry, hfr_results_ptnr, 
             mer_results, mer_targets, na.rm = TRUE) 
  

# AGGREGATE ---------------------------------------------------------------


  #aggregateion function
    pd_agg <- function(df, rm_time_components = NULL){
      
      #remove time componets for aggregation
      if(!is.null(rm_time_components))
        df <- select(df, -all_of(rm_time_components))
      
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
   df_agg <- df_full_lng %>% 
     mutate(hfr_pd = (2020 + (hfr_pd/100)) %>% as.character(.)) %>% 
     select(orgunituid, mech_code, hfr_pd, date, indicator, type, value) %>% 
     pd_agg("date") 
   
  #filter out blank lines
   df_agg <- df_agg %>% 
     filter_at(vars(starts_with("hfr_results")), any_vars(. != 0))
   

# MERGING & VIZ SETUP -----------------------------------------------------

  #merge on partner name for grouping
   df_agg_ptnr <- df_ptnr_mechs %>%
     select(mech_code, partner) %>% 
     distinct() %>% 
     right_join(df_agg, by = "mech_code")
   
  #mapping table for mech meta data & join
    mapping <- df_ctry %>% 
     distinct(mech_code, mech_name, primepartner, countryname) %>% 
     filter(!is.na(countryname)) %>% 
     distinct(mech_code, .keep_all = TRUE) %>% 
     left_join(iso_map, by = c("countryname" = "operatingunit")) %>% 
     select(-regional)
   
    df_agg_ptnr <- left_join(df_agg_ptnr, mapping, by = "mech_code")
   
  #order indicators
   df_agg_ptnr <- df_agg_ptnr %>% 
     mutate(indicator = factor(indicator, c("HTS_TST", "HTS_TST_POS",
                                            "TX_NEW", "TX_CURR", "TX_MMD",
                                            "VMMC_CIRC", "PrEP_NEW")))
  #create a merged iso and mech name for plot
   df_agg_ptnr <- df_agg_ptnr %>% 
     mutate(iso_mech = paste(iso, mech_code))
     

# PLOT FUNCTION -----------------------------------------------------------

   plot_comparion <- function(ptnr, pd, out_path = NULL){
     
     plot <- df_agg_ptnr %>%
       filter(hfr_pd == "2020.06",
              partner == "Jhpiego") %>% 
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
    ptnr_tbl <- distinct(df_agg_ptnr, partner, hfr_pd)
   
  #test
   # plot_comparion(ptnr_tbl$partner[1], ptnr_tbl$hfr_pd[1], "Images")
  
  #scatter plots for each partner and pd
    walk2(ptnr_tbl$partner, ptnr_tbl$hfr_pd, 
          plot_comparion, out_path ="Images")
   

# CALCULATE COMPLETENESS --------------------------------------------------

   #create flags for whether site reported HFR and if site exists in DATIM
     df_comp <- df_agg_ptnr %>% 
       mutate(has_hfr_reporting_ptnr = hfr_results_ptnr > 0 ,
              has_hfr_reporting_ctry = hfr_results_ctry > 0 ,
              is_datim_site = mer_results > 0 | mer_targets > 0)
   
   #remove where no DATIM results/targets and there is some value in the row
     # df_comp <- df_comp %>% 
     #   filter(!(has_hfr_reporting_ptnr == TRUE & is_datim_site == FALSE)) %>% 
     #   filter_at(vars(mer_results, mer_targets), any_vars(.!=0))
     
   #aggregate to country x mech level
     df_comp <- df_comp %>% 
       group_by(mech_code, partner, hfr_pd, indicator, mech_name, primepartner,
                countryname, iso, iso_mech) %>% 
       summarise_at(vars(has_hfr_reporting_ptnr, has_hfr_reporting_ctry, is_datim_site, mer_targets), sum, na.rm = TRUE) %>% 
       ungroup()
     
  #calculate completeness
     df_comp <- df_comp %>% 
       mutate(completeness_ptnr = has_hfr_reporting_ptnr / is_datim_site,
              completeness_ctry = has_hfr_reporting_ctry / is_datim_site)
   

# PLOT COMPLETENESS -------------------------------------------------------

   df_comp %>% 
     filter(partner == "Jhpiego") %>% 
     ggplot(aes(hfr_pd, iso_mech, fill = completeness)) +
     geom_tile(color = "white", size = 0.25) +
     geom_text(aes(label = percent(completeness)),
               size = 2.5, colour = "gray30", na.rm = TRUE) +
     scale_fill_viridis_c(option = "A", direction = -1, labels = percent, end = 0.9, alpha = 0.85) +
     facet_wrap(~ indicator, nrow = 1) +
     labs(title = "COMPLETENESS TRENDS ACROSS ALL SITES",
          subtitle = "FY20Q1 Site x Mechanism HFR Reporting Completeness by Period",
          y = NULL, x = NULL, color = "Site Type",
          caption = "Note: Completeness derived by comparing HFR reporting against sites with DATIM results/targets
         Source: FY20Q1 MER + HFR",
          fill = "Reporting completeness (100% = all sites reporting) ") +
     theme_minimal() + 
     coord_fixed(ratio = 1) +
     theme(legend.position = "top",
           legend.justification = c(0, 0),
           panel.grid = element_blank(),
           axis.text.x = element_text(size = 7),
           strip.text = element_text(hjust = 0))
   