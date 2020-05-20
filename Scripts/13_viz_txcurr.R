## PROJECT:  Pump up the jam
## AUTHOR:   achafetz | USAID
## LICENSE:  MIT
## PURPOSE:  review and visualize TX_CURR HFR data
## DATE:     2020-05-13
## UPDATED:  2020-05-20


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(vroom)
library(here)
library(Wavelength)
library(scales)
library(extrafont)
library(glitr)
library(patchwork)
library(RColorBrewer)




# GLOBAL VARIABLES --------------------------------------------------------

  datain <- "Data"
  dataout <- "Dataout"
  
  # paste((1:12), '=', viridis_pal(direction = -1)(12))
  heatmap_pal <- c("1"  = "#FDE725FF", "2"  = "#C2DF23FF", "3"  = "#85D54AFF",
                   "4"  = "#51C56AFF", "5"  = "#2BB07FFF", "6"  = "#1E9B8AFF", 
                   "7"  = "#25858EFF", "8"  = "#2D708EFF", "9"  = "#38598CFF", 
                   "10" = "#433E85FF", "11" = "#482173FF", "12" = "#440154FF")
  
  posneg_pal <- brewer.pal(3, "BrBG")


# IMPORT ------------------------------------------------------------------

  #data created in 12_align_tx
    df_tx <- vroom(here(dataout, "HFR_FY20_TXCURR.csv"))



# CLEAN UP ----------------------------------------------------------------

  #clean up
    df_tx <- df_tx %>% 
      filter(!is.na(operatingunit)) %>% 
      mutate(operatingunit = recode(operatingunit,
                                    "Democratic Republic of the Congo" = "DRC",
                                    "Dominican Republic" = "DR",
                                    "Western Hemisphere Region" = "WHR")) 
  
  #align dates with hfr_pds
    df_pds <- hfr_identify_pds(2020) %>% 
      group_by(hfr_pd) %>% 
      summarise(hfr_pd_date_min = min(date),
                hfr_pd_date_max = max(date)) %>% 
      ungroup() 
      # mutate(hfr_pd = (2020 + hfr_pd/100) %>% as.character)
      
  
  #extract mmd
    df_mmd <- df_tx %>% 
      select(-mer_results, -mer_targets) %>% 
      spread(indicator, hfr_results) %>% 
      rename_all(tolower) %>% 
      rowwise() %>% 
      mutate(tx_mmd.unkwn = tx_curr - sum(tx_mmd.u3, tx_mmd.35, tx_mmd.o6, na.rm = TRUE)) 
    
  #filter out sites where there is more than 100% reporting on MMD
    df_mmd <- df_mmd %>% 
      filter(tx_curr > 0,
             tx_mmd.unkwn >= 0)
    
  #remove mmd
    df_txcurr <- filter(df_tx, indicator == "TX_CURR")
    
  #create flags for whether site reported HFR and if site exists in DATIM
    df_txcurr <- df_txcurr %>% 
      mutate_at(vars(mer_results, mer_targets), ~ ifelse(is.na(.), 0, .)) %>% 
      mutate(has_hfr_reporting = hfr_results > 0 ,
             is_datim_site = mer_results > 0 | mer_targets > 0)
    
  #keep only DATIM sites
    df_txcurr <- df_txcurr %>% 
      filter(is_datim_site == TRUE)
    
    

# INTERPOLATE -------------------------------------------------------------

  #setup for interpolation: 
  #  replace 0's w/ NA; count reporting pds
    df_txcurr <- df_txcurr %>%
      mutate(hfr_results = na_if(hfr_results, 0)) %>% 
      group_by(mech_code, orgunituid) %>% 
      mutate(pds_reported = sum(!is.na(hfr_results))) %>% 
      ungroup()
    
  #id sites that need to be dropped (need min of 2pds for interpolation)
    df_nonipol_sites <- df_txcurr %>%
      filter(pds_reported < 2) %>% 
      mutate(is_ipol = FALSE)
    
  #interpolate
    df_txcurr <- df_txcurr %>%
      filter(pds_reported >= 2) %>% 
      group_by(mech_code, orgunituid) %>% 
      mutate(hfr_results_ipol = approx(hfr_pd, hfr_results, hfr_pd)$y %>% round) %>% 
      ungroup() %>% 
      mutate(is_ipol = !is.na(hfr_results_ipol) & is.na(hfr_results))
    
  #apend non interpolated sites
    df_txcurr <- bind_rows(df_txcurr, df_nonipol_sites)
    

# EXTRAPOLATE -------------------------------------------------------------

  #TBD
    
# CALCULATE COMPLETENESS --------------------------------------------------
    
    #aggregate to country
      df_comp <- df_txcurr %>% 
        group_by(hfr_pd, indicator, operatingunit) %>% 
        summarise_at(vars(has_hfr_reporting, is_datim_site, mer_targets), sum, na.rm = TRUE) %>% 
        ungroup()
    
    #calculate completeness
      df_comp <- df_comp %>% 
        mutate(completeness = has_hfr_reporting / is_datim_site,
               completeness = ifelse(is.nan(completeness) | is.infinite(completeness), NA, completeness),
               completeness_band = case_when(completeness < .05 ~ 1,
                                             completeness <= 1 ~ round(completeness/.1, 0),
                                             !is.na(completeness) ~ 12))

    #site count
      df_comp <- df_comp %>% 
        mutate(ou_sitecount = paste0(operatingunit, " (", comma(is_datim_site), ")"))
      
    #filter
      df_comp <- df_comp %>% 
        filter_at(vars(has_hfr_reporting, is_datim_site, mer_targets), any_vars(. != 0 | is.na(.)))
    
    #clean up period
      df_comp <- df_comp %>% 
        left_join(df_pds) %>% 
        mutate(date_lab = paste0(format.Date(hfr_pd_date_max, "%b %d"), "\n(",
                                 str_pad(hfr_pd, 2, pad = "0"), ")"),
               date_lab = fct_reorder(date_lab, hfr_pd_date_max))
      
    #viz completeness
      viz_comp <- df_comp %>% 
        ggplot(aes(date_lab, fct_reorder(ou_sitecount, mer_targets, sum), fill = completeness_band)) +
        geom_tile(color = "white", size = 0.25) +
        geom_text(aes(label = percent(completeness),
                      color = ifelse(completeness_band <= 1, "dark", "light")),
                  size = 2.5, na.rm = TRUE) +
        scale_fill_viridis_c(limits = c(1, 12), label = NULL, direction = -1, na.value = "gray80") +
        scale_color_manual(values = c("dark" = "gray30", "light" = "white"), guide = FALSE) +
        labs(subtitle = "HFR Site Completeness",
             y = NULL, x = NULL, color = "Site Type",
             caption = "Note: Completeness derived by comparing HFR reporting against sites with DATIM results/targets") +
        theme_minimal() + 
        theme(legend.position = "none",
              legend.justification = c(0, 0),
              panel.grid = element_blank(),
              text = element_text(family = "Source Sans Pro"),
              plot.caption = element_text(color = "gray30"))
      
    #viz mer targets
      viz_targ <- df_comp %>% 
        filter(hfr_pd == max(hfr_pd)) %>% 
        mutate(trgt_lab = case_when(mer_targets > 1000000 ~ paste0(round(mer_targets/1000000, 1), "m"),
                                    mer_targets > 10000 ~ paste0(round(mer_targets/1000, 0), "k"),
                                    mer_targets == 0 ~ "0",
                                    TRUE ~ paste0(round(mer_targets/1000, 1), "k"))) %>% 
        ggplot(aes(mer_targets, fct_reorder(operatingunit, mer_targets, sum))) +
        geom_blank(aes(mer_targets * 1.2)) +
        geom_col(fill = heatmap_pal[10]) +
        geom_text(aes(label = trgt_lab), family = "Source Sans Pro",
                  color = "gray50", hjust = -.2) +
        labs(subtitle = "MER Targets (USAID)",
             x = NULL, y = NULL) +
        scale_x_continuous(expand = c(0.005, 0.005)) +
        scale_y_discrete(expand = c(0.005, 0.005)) +
        si_style_nolines() +
        theme(axis.text.x=element_text(color = "white"))
      
    #combine viz
      viz_comp + viz_targ + 
        plot_annotation(title = "FY20 HFR TX_CURR COMPLETENESS AND MER TARGETS") &
        theme(plot.title = element_text(family = "Source Sans Pro", face = "bold"))
      
      ggsave("HFR_TX_Comp.png", path = "Images", width = 10, height = 5.625, dpi = 300)
  

# SITE COUNT PER PERIOD ---------------------------------------------------

      df_rpt_sites <- df_txcurr %>% 
        group_by(operatingunit, hfr_pd) %>% 
        summarise_at(vars(mer_targets, has_hfr_reporting, is_datim_site), sum, na.rm = TRUE) %>% 
        ungroup() %>% 
        mutate(no_reporting = has_hfr_reporting- is_datim_site,
               share_reporting = has_hfr_reporting/is_datim_site,
               share_noreporting = share_reporting-1,
               ou_sitecount = paste0(operatingunit, " (", comma(is_datim_site), ")")) %>% 
        left_join(df_pds) %>% 
        mutate(date_lab = paste0(format.Date(hfr_pd_date_max, "%b %d"), "\n(",
                                 str_pad(hfr_pd, 2, pad = "0"), ")"),
               date_lab = fct_reorder(date_lab, hfr_pd_date_max))
      
      df_rpt_sites %>% 
        ggplot(aes(date_lab)) +
        geom_col(aes(y = share_reporting), fill = heatmap_pal[10]) +
        geom_col(aes(y = share_noreporting), fill = heatmap_pal[6]) +
        geom_hline(yintercept = 0) +
        facet_wrap( ~ fct_reorder(ou_sitecount, mer_targets, .desc = TRUE)) +
        scale_y_continuous(label = percent) +
        si_style() 
      
      
      df_rpt_sites %>% 
        ggplot(aes(date_lab, share_reporting)) +
        geom_col(fill = heatmap_pal[10]) +
        geom_col(aes(y = 1), fill = NA, color = heatmap_pal[10]) +
        geom_hline(yintercept = 0) +
        geom_text(aes(y = 1.15, label = comma(has_hfr_reporting)), color = "gray30",
                  family = "Source Sans Pro") +
        expand_limits(y = 1.3) +
        facet_wrap( ~ fct_reorder(ou_sitecount, mer_targets, .desc = TRUE)) +
        scale_y_continuous(label = percent) +
        labs(x = NULL, y = "share of sites reporting",
             title = "SITES REPORTING EACH PERIOD BY OPERATING UNIT",
             caption = "Note: Completeness derived by comparing HFR reporting against sites with DATIM results/targets"
        ) +
        si_style_nolines() +
        theme(plot.caption = element_text(color = "gray30"))      
      
# CONSISTENT REPORTING ----------------------------------------------------
      
    #identify the number of periods
      pds <- unique(df_txcurr$hfr_pd) %>% length()
      
    #identify which site x mechs had reporting every period 
      df_complete_orgunits <- df_txcurr %>% 
        filter(hfr_results > 0) %>% 
        group_by(orgunituid, mech_code) %>% 
        filter(n() == pds) %>% 
        ungroup() %>% 
        distinct(operatingunit, orgunituid) %>% 
        count(operatingunit, name = "complete_sites")
    
    #identify which site x mechs had reporting/interpoled data every period 
      df_complete_ipol_orgunits <- df_txcurr %>% 
        filter(hfr_results_ipol > 0) %>% 
        group_by(orgunituid, mech_code) %>% 
        filter(n() == pds) %>% 
        ungroup() %>% 
        distinct(operatingunit, orgunituid) %>% 
        count(operatingunit, name = "complete_sites_ipol")
      
    #get a share of sites reporting every pd
      df_complete_share <- df_txcurr %>% 
        distinct(operatingunit, orgunituid) %>% 
        count(operatingunit, name = "all_sites") %>% 
        full_join(df_complete_orgunits, by = "operatingunit") %>% 
        full_join(df_complete_ipol_orgunits, by = "operatingunit") %>% 
        mutate(complete_sites = ifelse(is.na(complete_sites), 0, complete_sites),
               complete_sites_ipol = ifelse(is.na(complete_sites_ipol), 0, complete_sites_ipol),
               share = complete_sites / all_sites,
               share_ipol = complete_sites_ipol / all_sites,
               ou_count = paste0(operatingunit, " (", comma(complete_sites), "/", comma(all_sites), ")"),
               ou_count_ipol = paste0(operatingunit, " (", comma(complete_sites_ipol), "/", comma(all_sites), ")"))
      
    #viz, completeness w/ original data
      df_complete_share %>% 
        ggplot(aes(share, fct_reorder(ou_count, share, sum))) +
        geom_col(fill = heatmap_pal[10], width = .8) +
        geom_vline(xintercept = seq(from = 0, to = 1, by = .1), color = "white") +
        geom_col(aes(x = 1), fill = NA, width = .8, color = heatmap_pal[10]) +
        geom_text(aes(label = percent(share, 1)),
                  hjust = -.1, family = "Source Sans Pro", color = "gray30") +
        labs(x = NULL, y = NULL,
             title = paste("SHARE OF SITES BY OU REPORTING IN ALL", pds, "PERIODS")) +
        scale_x_continuous(labels = percent, expand = c(0.005, 0.005)) +
        scale_y_discrete(expand = c(0.005, 0.005)) +
        si_style_nolines() +
        theme(axis.text.x = element_blank())
      
      ggsave("HFR_TX_SitesAllPds.png", path = "Images", width = 10, height = 5.625, dpi = 300)
      
    #viz, completeness w/ interpolation
      df_complete_share %>% 
        ggplot(aes(share, fct_reorder(ou_count_ipol, share, sum))) +
        geom_col(aes(share_ipol), fill = heatmap_pal[7], width = .8) +
        geom_col(fill = heatmap_pal[10], width = .8) +
        geom_vline(xintercept = seq(from = 0, to = 1, by = .1), color = "white") +
        geom_col(aes(x = 1), fill = NA, width = .8, color = heatmap_pal[10]) +
        geom_text(aes(share_ipol, label = percent(share_ipol, 1)),
                  hjust = -.1, family = "Source Sans Pro", color = "gray30") +
        labs(x = NULL, y = NULL,
             title = paste("SHARE OF SITES BY OU REPORTING IN ALL", pds, "PERIODS WHEN INTERPOLATING")) +
        scale_x_continuous(labels = percent, expand = c(0.005, 0.005)) +
        scale_y_discrete(expand = c(0.005, 0.005)) +
        si_style_nolines() +
        theme(axis.text.x = element_blank())

      ggsave("HFR_TX_SitesAllPds_Ipol.png", path = "Images", width = 10, height = 5.625, dpi = 300)
      
# GROWTH TRENDS FOR CONSISTENT SITES --------------------------------------
      
    #filter to where reporting is greater than 0 and for all pds
      df_txcurr_comp <- df_txcurr %>% 
        filter(hfr_results > 0) %>% 
        group_by(orgunituid, mech_code) %>% 
        filter(n() == pds) %>% 
        ungroup()
      
    #aggregate to OU level and create growth metric
      df_txcurr_comp <- df_txcurr_comp %>% 
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
        mutate(ou_count = paste0(operatingunit, " (", n, ")"),
               hfr_pd = str_sub(hfr_pd, start = -2))
    
    #viz
      df_txcurr_comp %>% 
        ggplot(aes(hfr_pd, growth, group = ou_count)) +
        geom_col(aes(fill = growth > 0)) +
        geom_hline(yintercept = 0, color = "gray40") +
        facet_wrap(~ fct_reorder(ou_count, mer_targets, sum, .desc = TRUE), scales = "free_y") +
        scale_y_continuous(labels = percent_format(.1)) +
        scale_fill_manual(values = c(posneg_pal[1], posneg_pal[3])) +
        labs(x = NULL, y = NULL,
             title = "TX_CURR GROWTH",
             subtitle =  "only sites that report every period") +
        si_style_ygrid() +
        theme(strip.text = element_text(face = "bold"),
              legend.position = "none")
      
      
      ggsave("HFR_TX_Growth_SitesAllPds.png", path = "Images", width = 10, height = 5.625, dpi = 300)
      
# OU X MECH TRENDS --------------------------------------------------------

  #aggregate to ou level
    df_trends <-  df_txcurr %>% 
        group_by(operatingunit, mech_code, primepartner, indicator, hfr_pd) %>% 
        summarise_at(vars(mer_targets, mer_results, hfr_results, hfr_results_ipol, has_hfr_reporting, is_datim_site), sum, na.rm = TRUE) %>% 
        ungroup() %>%
        mutate_at(vars(hfr_results, hfr_results_ipol, mer_targets, mer_results), ~ na_if(., 0)) %>% 
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
  #clean up pd
    df_trends <- df_trends %>% 
      mutate(hfr_pd = as.integer(hfr_pd)) %>% 
      left_join(df_pds, by = "hfr_pd")
    
  #setup range
    df_trends <- df_trends %>% 
      group_by(mech_code) %>% 
      mutate(max_range = max(hfr_results, mer_results, 50/1.2, na.rm = TRUE) * 1.2) %>% 
      ungroup()
    
  #plot function for trends
    plot_trends <- function(ou_sel, output_path = NULL){
      
      plot <- df_trends %>% 
        filter(operatingunit == ou_sel) %>% 
        ggplot(aes(hfr_pd, hfr_results)) +
        geom_blank(aes(y = max_range)) +
        geom_hline(aes(yintercept = 0), color = "gray30", na.rm = TRUE) +
        geom_col(fill = heatmap_pal[9], na.rm = TRUE) +
        geom_label(aes(y = 0, label = percent(completeness, 1)), vjust = -.2,
                   label.size = 0,
                   family = "Source Sans Pro", color = heatmap_pal[9]) +
        geom_hline(aes(yintercept = mer_results), linetype = "dashed", color = "gray20", na.rm = TRUE) +
        facet_wrap(~ fct_reorder(mech_partner, mer_targets, sum, .desc = TRUE), scales = "free_y") +
        scale_y_continuous(label = comma, expand = c(-0, 0)) +
        labs(x = NULL, y = NULL, color = "Completeness (0% - 100%)",
             title = paste("FY20", toupper(ou_sel), "TX_CURR MECHANISM TRENDS"),
             subtitle = "site completeness indicated at column base",
             caption = "notes: mechanisms ordered by MER targets
              dotted line identifies FY20Q1 reported value
              periods with no columns or line represent mechanisms with targets but no MER or HFR results") +
        si_style_ygrid() +
        theme(strip.text = element_text(face = "bold"),
              legend.title = element_text(family = "Source Sans Pro", color = "gray30"))
      
      if(!is.null(output_path)){
        file <- paste0("HFR_TX_Trends", ou_sel, ".png")
        ggsave(file, path = "Images", width = 10, height = 5.625, dpi = 300)
      } else {
        return(plot)
      }
    }
  
    #plot for each OU
      walk(unique(df_trends$operatingunit), plot_trends, output_path = "Images")
  
  
  


    
    
  
  