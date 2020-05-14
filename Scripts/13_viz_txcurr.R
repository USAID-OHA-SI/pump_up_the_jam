## PROJECT:  Pump up the jam
## AUTHOR:   achafetz | USAID
## LICENSE:  MIT
## PURPOSE:  review and visualize TX_CURR HFR data
## DATE:     2020-05-13
## UPDATED:  


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(vroom)
library(here)
library(Wavelength)
library(scales)
library(extrafont)
library(glitr)
library(patchwork)




# GLOBAL VARIABLES --------------------------------------------------------

  datain <- "Data"
  dataout <- "Dataout"
  
  # paste((1:12), '=', viridis_pal(direction = -1)(12))
  heatmap_pal <- c("1"  = "#FDE725FF", "2"  = "#C2DF23FF", "3"  = "#85D54AFF",
                   "4"  = "#51C56AFF", "5"  = "#2BB07FFF", "6"  = "#1E9B8AFF", 
                   "7"  = "#25858EFF", "8"  = "#2D708EFF", "9"  = "#38598CFF", 
                   "10" = "#433E85FF", "11" = "#482173FF", "12" = "#440154FF")


# IMPORT ------------------------------------------------------------------

  #data created in 12_align_tx
    df_tx <- read_csv(here(dataout, "HFR_FY20_TXCURR.csv"))



# CLEAN UP ----------------------------------------------------------------

  #clean up
    df_tx <- df_tx %>% 
      filter(!is.na(operatingunit)) %>% 
      mutate(operatingunit = recode(operatingunit,
                                    "Democratic Republic of the Congo" = "DRC",
                                    "Dominican Republic" = "DR",
                                    "Western Hemisphere Region" = "WHR"),
             hfr_pd = (fy + hfr_pd/100) %>% as.character) 
    
  #create flags for whether site reported HFR and if site exists in DATIM
    df_tx <- df_tx %>% 
      mutate(has_hfr_reporting = hfr_results > 0 ,
             is_datim_site = mer_results > 0 | mer_targets > 0)
    
# CALCULATE COMPLETENESS --------------------------------------------------
    
    #aggregate to country
      df_comp <- df_tx %>% 
        group_by(hfr_pd, indicator, operatingunit) %>% 
        summarise_at(vars(has_hfr_reporting, is_datim_site, mer_targets), sum, na.rm = TRUE) %>% 
        ungroup()
    
    #calculate completeness
      df_comp <- df_comp %>% 
        mutate(completeness = has_hfr_reporting / is_datim_site,
               completeness = ifelse(is.nan(completeness) | is.infinite(completeness), NA, completeness),
               completeness_band = case_when(completeness < .05 ~ 1,
                                             completeness <= 1 ~ round(completeness/.1, 0),
                                             !is.na(completeness) ~ 12) %>% as.character)

    #site count
      df_comp <- df_comp %>% 
        mutate(ou_sitecount = paste0(operatingunit, " (", comma(is_datim_site), ")"))
      
    #filter
      df_comp <- df_comp %>% 
        filter_at(vars(has_hfr_reporting, is_datim_site, mer_targets), any_vars(. != 0 | is.na(.)))
    
    
  viz_comp <- df_comp %>% 
    ggplot(aes(hfr_pd, fct_reorder(ou_sitecount, mer_targets, sum), fill = completeness_band)) +
    geom_tile(color = "white", size = 0.25) +
    geom_text(aes(label = percent(completeness),
                  color = ifelse(completeness_band <= 1, "dark", "light")),
              size = 2.5, na.rm = TRUE) +
    scale_fill_manual(values = heatmap_pal, na.value = "gray80") +
    scale_color_manual(values = c("dark" = "gray30", "light" = "white"), guide = FALSE) +
    labs(subtitle = "HFR Site Completeness",
         y = NULL, x = NULL, color = "Site Type",
         caption = "Note: Completeness derived by comparing HFR reporting against sites with DATIM results/targets") +
    theme_minimal() + 
    #coord_fixed(ratio = .2) +
    theme(legend.position = "none",
          legend.justification = c(0, 0),
          panel.grid = element_blank(),
          text = element_text(family = "Source Sans Pro"))
      
  
  viz_targ <- df_comp %>% 
    filter(hfr_pd == "2020.01") %>% 
    ggplot(aes(mer_targets, fct_reorder(operatingunit, mer_targets, sum))) +
    geom_blank(aes(mer_targets * 1.2)) +
    geom_col(fill = heatmap_pal[10]) +
    geom_text(aes(label = comma(mer_targets)), family = "Source Sans Pro",
              color = "gray50", hjust = -.2) +
    labs(subtitle = "MER Targets (USAID)",
         x = NULL, y = NULL) +
    scale_x_continuous(expand = c(0.005, 0.005)) +
    scale_y_discrete(expand = c(0.005, 0.005)) +
    si_style_nolines() +
    theme(axis.text.x=element_text(color = "white"))
    
  viz_comp + viz_targ + 
    plot_annotation(title = "FY20 HFR TX_CURR COMPLETENESS AND MER TARGETS") &
    theme(plot.title = element_text(family = "Source Sans Pro", face = "bold"))
  
  ggsave("HFR_TX_Comp.png", path = "Images", width = 10, height = 5.625, dpi = 300)
  

# OU X MECH TRENDS --------------------------------------------------------

df_trends <-  df_tx %>% 
    group_by(operatingunit, mech_code, primepartner, indicator, hfr_pd) %>% 
    summarise_at(vars(mer_targets, mer_results, hfr_results, has_hfr_reporting, is_datim_site), sum, na.rm = TRUE) %>% 
    ungroup() %>%
    mutate_at(vars(hfr_results, mer_targets, mer_results), ~ na_if(., 0)) %>% 
    arrange(operatingunit, mech_code, hfr_pd) %>% 
    mutate(mech_partner = paste(mech_code, primepartner))
  
  #calculate completeness
  df_trends <- df_trends %>% 
    mutate(completeness = has_hfr_reporting / is_datim_site,
           completeness = ifelse(is.nan(completeness) | is.infinite(completeness), NA, completeness),
           completeness_band = case_when(completeness < .05 ~ 1,
                                         completeness <= 1 ~ round(completeness/.1, 0),
                                         !is.na(completeness) ~ 12) #%>% as.character
           )
  
    

  plot_trends <- function(ou_sel, output_path = NULL){
    
    df_trends %>% 
      filter(operatingunit == ou_sel) %>% 
      ggplot(aes(hfr_pd, hfr_results)) +
      geom_blank(aes(y = hfr_results * 1.2)) +
      geom_hline(aes(yintercept = 0), color = "gray30", na.rm = TRUE) +
      geom_col(fill = "gray80", na.rm = TRUE) +
      geom_point(aes(y = 0, color = completeness_band), size = 6) +
      geom_hline(aes(yintercept = mer_results), linetype = "dashed", color = "gray20", na.rm = TRUE) +
      facet_wrap(~ fct_reorder(mech_partner, mer_targets, sum, .desc = TRUE), scales = "free_y") +
      scale_y_continuous(label = comma, expand = c(-0, 0)) +
      scale_color_viridis_c(limits = c(0, 12), label = NULL, direction = -1) +
      labs(x = NULL, y = NULL, fill = "Completeness (0% - 100%)",
           title = paste("FY20", toupper(ou_sel), "TX_CURR MECHANISM TRENDS"),
           caption = "notes: mechanisms ordered by MER targets
         dotted line identifies FY20Q1 reported value") +
      si_style_ygrid() +
      theme(strip.text = element_text(face = "bold"),
            legend.title = element_text(family = "Source Sans Pro", color = "gray30"))
    
    if(!is.null(output_path)){
      file <- paste0("HFR_TX_Trends", ou_sel, ".png")
      ggsave(file, path = "Images", width = 10, height = 5.625, dpi = 300)
    }
  }
  
  
  walk(unique(df_trends$operatingunit), plot_trends, output_path = "Images")
  
  
  

# CONSISTENT REPORTING ----------------------------------------------------

  pds <- unique(df_tx$hfr_pd) %>% length()
  
  df_complete_orgunits <- df_tx %>% 
    filter(hfr_results > 0) %>% 
    group_by(orgunituid, mech_code) %>% 
    filter(n() == pds) %>% 
    ungroup() %>% 
    distinct(operatingunit, orgunituid) %>% 
    count(operatingunit, name = "complete_sites")
  
  df_complete_share <- df_tx %>% 
    distinct(operatingunit, orgunituid) %>% 
    count(operatingunit, name = "all_sites") %>% 
    full_join(df_complete_orgunits) %>% 
    mutate(complete_sites = ifelse(is.na(complete_sites), 0, complete_sites),
           share = complete_sites / all_sites,
           ou_count = paste0(operatingunit, " (", complete_sites, "/",all_sites, ")"))

  df_complete_share %>% 
    ggplot(aes(share, fct_reorder(ou_count, share, sum))) +
    geom_col(aes(x = 1), fill = "gray90") +
    geom_col(fill = heatmap_pal[5]) +
    geom_vline(xintercept = seq(from = 0, to = 1, by = .1), color = "white") +
    geom_text(aes(label = percent(share, 1)),
              hjust = -.1, family = "Source Sans Pro", color = "gray30") +
    labs(x = NULL, y = NULL,
         title = paste("SHARE OF SITES REPORTING IN ALL", pds, "PERIODS")) +
    scale_x_continuous(labels = percent, expand = c(0.005, 0.005)) +
    scale_y_discrete(expand = c(0.005, 0.005)) +
    si_style_nolines() +
    theme(axis.text.x = element_blank())
  
  ggsave("HFR_TX_SitesAllPds.png", path = "Images", width = 10, height = 5.625, dpi = 300)
  

# GROWTH TRENDS FOR CONSISTENT SITES --------------------------------------

  
  df_tx_comp <- df_tx %>% 
    filter(hfr_results > 0) %>% 
    group_by(orgunituid, mech_code) %>% 
    filter(n() == pds) %>% 
    ungroup()
  
  
  df_tx_comp <- df_tx_comp %>% 
    group_by(operatingunit, hfr_pd) %>% 
    summarise(hfr_results  = sum(hfr_results, na.rm = TRUE), 
              mer_targets = sum(mer_targets, na.rm = TRUE), 
              n = n()
              ) %>% 
    ungroup() %>% 
    group_by(operatingunit) %>% 
    mutate(growth = (hfr_results - lag(hfr_results, order_by = hfr_pd)) / lag(hfr_results, order_by = hfr_pd)) %>% 
    ungroup() %>% 
    filter(hfr_pd != "2020.01") %>% 
    mutate(ou_count = paste0(operatingunit, " (", n, ")"))

  df_tx_comp %>% 
    ggplot(aes(hfr_pd, growth, group = ou_count)) +
    geom_hline(yintercept = 0) +
    geom_area(alpha = .2, fill = heatmap_pal[5], color = heatmap_pal[5]) +
    #geom_path(size = .9) +
    geom_point(size = 5, color = heatmap_pal[5]) +
    facet_wrap(~ fct_reorder(ou_count, mer_targets, sum, .desc = TRUE), scales = "free_y") +
    scale_y_continuous(labels = percent) +
    labs(x = NULL, y = NULL,
         title = "TX_CURR GROWTH",
         subtitle =  "only sites that report every period") +
    si_style() +
    theme(strip.text = element_text(face = "bold"))
  
  ggsave("HFR_TX_Growth_SitesAllPds.png", path = "Images", width = 10, height = 5.625, dpi = 300)
  
  
  
  
  