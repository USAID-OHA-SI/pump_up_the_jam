  ## PROJECT:  Pump up the jam
  ## AUTHOR:   T.Essam, A.Chafetz | USAID
  ## LICENSE:  MIT
  ## PURPOSE:  flag DATIM sites with targets and results
  ## DATE:     2020-03-12
  
  
  # DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(lubridate)
  library(Wavelength)
  library(extrafont)
  
  
# GLOBALS -----------------------------------------------------------------
  
  
  
    hfr_folder <- "Data"
    datim_folder <- "Data/DATIM"
    out_folder <- "Dataout"
    viz_folder <- "Images"
    quarter <- "Q3"
    
    # thresholds for the targets and results cutoffs
    targets_thresh <- 0.80
    results_thresh <- 0.80
    
    # Below thresholds are to allow for inclusion of large sites in an ou w/ few overall sites
    site_thresh_targets <- 0.20
    site_thresh_results <- 0.20
  
  
# FLAG SITES AS IMPORTANT USING MER RESULTS/TARGETS  ----------------------------------------------
  
  
  # unzip Google Drive download of all MER FY20 data
    list.files(datim_folder, "DATIM", full.names = TRUE) %>% unzip(exdir = datim_folder)
  
  # store files paths as vecotre for import
    files <- list.files(datim_folder, "HFR_FY20Q3.*csv", full.names = TRUE, recursive = TRUE)
  
  # import and bind together
    df_datim <- map_dfr(
      .x = files,
      .f = ~ readr::read_csv(.x)
    )
    
    #fix TX_MMD "targets" --> set to Q1 results
    df_datim_mmd <- df_datim %>% 
      filter(indicator == "TX_CURR") %>% 
      mutate(indicator = "TX_MMD",
             mer_targets = mer_results,
             mer_results = NA)
    
    df_datim <- bind_rows(df_datim, df_datim_mmd)
    rm(df_datim_mmd)
    
    
  # remove files (just keep zipped folder)
    list.files(datim_folder, "3.1 DATIM", full.names = T) %>% unlink(., recursive = TRUE)
  
  # aggregate to level of detail desired
  # flag sites with results but no targets, flag number of mechs per site
    df_datim <- df_datim %>%
      group_by(orgunituid, mech_code, fy, indicator, operatingunit) %>%
      summarise_at(vars(starts_with("mer")), sum, na.rm = TRUE) %>%
      ungroup() %>%
      group_by(orgunituid, indicator) %>%
      add_tally(name = "mechs_per_site") %>%
      ungroup() %>%
      mutate(results_no_targets = if_else(mer_results >= 0 & mer_targets == 0, 1, 0)) %>%
      group_by(operatingunit, indicator) %>%
      mutate(
        ou_targets = sum(mer_targets, na.rm = TRUE),
        ou_results = sum(mer_results, na.rm = TRUE),
        site_targets_sh = mer_targets / ou_targets,
        site_results_sh = mer_results / ou_results,
    
        # Flagging sites that are more than 20% of total in case there are 2 or 3 sites (Angola)
        flag_targets_sh = if_else(site_targets_sh >= site_thresh_targets, 1, 0),
        flag_results_sh = if_else(site_results_sh >= site_thresh_results, 1, 0)
      ) %>%
      add_tally(name = "ou_total_sites") %>%
      ungroup() %>%
      arrange(operatingunit, indicator, orgunituid, mech_code)
  
  # calculate site weights - need to know 1) site targets/results in aggregate
  # ou targets/results in aggregate to get shares --> importance weights
  
    df_datim_wgts <-
      df_datim %>%
      arrange(operatingunit, indicator, desc(site_targets_sh)) %>%
      group_by(operatingunit, indicator) %>%
      mutate(
        run_sum_targets = cumsum(site_targets_sh),
        lag_run_sum_targets = lag(run_sum_targets, n = 1, order_by = desc(site_targets_sh)),
        impflag_targets = case_when(
          run_sum_targets <= targets_thresh | lag_run_sum_targets <=  targets_thresh ~ 1,
          flag_targets_sh == 1 ~ 1, # grab Angola type site distributions here
          TRUE ~ 0,
        ),
        impflag_targets_count = sum(impflag_targets),
        impflag_targets_sh = (impflag_targets_count / ou_total_sites)
      ) %>%
      ungroup() %>%
      # Now with results
      arrange(operatingunit, indicator, desc(site_results_sh)) %>%
      group_by(operatingunit, indicator) %>%
      mutate(
        run_sum_results = cumsum(site_results_sh),
        lag_run_sum_results = lag(run_sum_results, n = 1, order_by = desc(site_results_sh)),
        impflag_results = case_when(
          run_sum_results <= results_thresh | lag_run_sum_results <= targets_thresh ~ 1,
          flag_results_sh == 1 ~ 1, # grab Angola type site distributions here
          TRUE ~ 0,
        ),
        impflag_results_count = sum(impflag_results),
        impflag_results_sh = (impflag_results_count / ou_total_sites)
      ) %>%
      ungroup() %>%
      # flag VIP sites
      mutate(impflag_both = if_else(impflag_results == 1 & impflag_targets == 1, 1, 0))
    
# CLEAN UP AND SAVE -------------------------------------------------------
    
    # remove extra objects
    rm(df_datim, files)
    
    
    # store file name
    filename <- paste0("DATIM_FLAGS_", format(Sys.Date(), "%Y%m%d"), ".csv")
    
    # save
    write_csv(df_datim_wgts, file.path(out_folder, filename), na = "")    
    

# SUMMARY PLOTS (OPTIONAL) ------------------------------------------------
  
  # Look at where a site is important to both target and results
    df_datim_wgts %>%
      group_by(operatingunit, indicator) %>%
      summarise(
        n = mean(ou_total_sites),
        res = sum(impflag_results),
        tgts = sum(impflag_targets),
        both = sum(impflag_both)
      ) %>%
      print(n = Inf)
  
  # plot results for summary graphic
    df_datim_wgts %>%
      group_by(operatingunit, indicator) %>%
      summarise(
        n = mean(ou_total_sites),
        results_flags = sum(impflag_results),
        targets_flags = sum(impflag_targets),
        both_flags = sum(impflag_both)
      ) %>%
      ungroup() %>%
      gather(flagtype, value, results_flags:both_flags) %>%
      mutate(
        ou_order = fct_reorder(operatingunit, value, .fun = max, .desc = TRUE),
        ind_order = fct_reorder(indicator, value),
        logval = log(value)
      ) %>%
      ggplot(., aes(ind_order, flagtype, fill = logval)) +
      geom_tile(colour = "white") +
      geom_text(aes(label = ifelse(value != 0, value, NA)), color = "#303030") +
      facet_wrap(~ou_order) +
      coord_flip() +
      scale_fill_viridis_c(option = "A", begin = 0.5, na.value = "#f1f1f1", direction = -1) +
      theme_minimal() +
      theme(
        strip.text = element_text(hjust = 0),
        legend.position = "none"
      ) +
      labs(
        x = "", y = "", title = "Summary of important sites for targets and results",
        caption = str_c("Created on ", format(Sys.Date(), "%Y%m%d"))
      )
  
 # Summary graphic explaining tagging process
    df_datim_wgts %>% 
      filter(operatingunit == "Botswana" & indicator == "TX_CURR") %>% 
      mutate(sort_order = str_c(mech_code, "+", orgunituid),
        sort_order = fct_reorder(sort_order, site_targets_sh)) %>% 
      ggplot(aes(x = site_targets_sh, y = indicator, fill = factor(impflag_targets),
        colour = sort_order)) + 
      geom_bar(stat = "identity", colour = "white") +
      geom_vline(xintercept = 0.80, colour = "#505050") +
      theme(legend.position = "none",
        text = element_text(family = "Source Sans Pro"),
        panel.grid = element_blank(),
        axis.text.y = element_blank()) +
      scale_fill_manual(values = c("1" ="#B07AA1", "0" ="#B8B8B8")) +
      scale_x_continuous(breaks = c(seq(0, 1, 0.1)), 
        labels = scales::percent_format(accuracy = 1)) +
      labs(x = NULL, y = NULL)
  
  ggsave(file.path(viz_folder, "BWA_imp_flag_graphic.png"),
    plot = last_plot(), 
    height = 0.96,
    width = 9.62,
    units = "in",
    dpi = "retina")
  

   df_datim_wgts %>% 
    filter(operatingunit == "Botswana" & indicator == "TX_CURR") %>% 
   ggplot(aes(x = ou_targets, y = indicator)) + geom_col(fill = "#B8B8B8") +
   theme(legend.position = "none",
     text = element_text(family = "Source Sans Pro", face = "Light"),
     panel.grid = element_blank(),
     axis.text = element_blank()) +
    labs(x = NULL, y = NULL) +
    geom_text(aes(label = str_c(ou_targets, " targets")), size = 6, hjust = 0,
      colour = "white")

  ggsave(file.path(viz_folder, "BWA_imp_flag_graphic_targets.png"),
    plot = last_plot(), 
    height = 0.96,
    width = 9.62,
    units = "in",
    dpi = "retina")
    
 

