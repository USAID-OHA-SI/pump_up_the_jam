## PROJECT:  Pump up the jam
## AUTHOR:   T.Essam, A.Chafetz | USAID
## LICENSE:  MIT
## PURPOSE:  flag DATIM sites with targets and results
## DATE:     2020-03-12


# DEPENDENCIES ------------------------------------------------------------

  library(tidyverse)
  library(lubridate)
  library(Wavelength)


# GLOBALS -----------------------------------------------------------------


  
  hfr_folder <- "Data"
  datim_folder <- "Data"
  out_folder <- "Dataout"
  
  #thresholds for the targets and results cutoffs    
  targets_thresh = 0.81
  results_thresh  = 0.81
  
  # Below thresholds are to allow for inclusion of large sites in an ou w/ few overall sites
  site_thresh_targets <- 0.20
  site_thresh_results <- 0.20
  
  
# FLAG SITES AS IMPORTANT USING MER RESULTS/TARGETS  ----------------------------------------------
  
  
  #unzip Google Drive download of all MER FY20 data
  list.files(datim_folder, "DATIM", full.names = TRUE) %>% unzip(exdir = datim_folder)
  
  #store files paths as vecotre for import
  files <- list.files(datim_folder, "HFR_FY20Q1.*csv", full.names = TRUE)
  
  #import and bind together
  df_datim <- map_dfr(.x = files,
    .f = ~readr::read_csv(.x))
  
  #remove files (just keep zipped folder)
    unlink(files)
  
  #aggregate to level of detail desired
  #flag sites with results but no targets, flag number of mechs per site
  df_datim <- df_datim %>% 
    group_by(orgunituid, mech_code, fy, indicator, operatingunit) %>% 
    summarise_at(vars(starts_with("mer")), sum, na.rm = TRUE) %>%
    ungroup() %>% 
    group_by(orgunituid, indicator) %>% 
    add_tally(name = "mechs_per_site") %>% 
    ungroup() %>% 
    mutate(results_no_targets = if_else(mer_results >=0 & mer_targets == 0, 1, 0)) %>% 
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
  

  
  # TODO: Functionalize below
  #calculate site weights - need to know 1) site targets/results in aggregate
  # ou targets/results in aggregate to get shares --> importance weights
  
  df_datim_wgts <- 
    df_datim %>% 
    arrange(operatingunit, indicator, desc(site_targets_sh)) %>% 
    group_by(operatingunit, indicator) %>% 
    mutate(
      run_sum_targets = cumsum(site_targets_sh),
      impflag_targets = case_when(
        run_sum_targets <= targets_thresh ~ 1,
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
      impflag_results = case_when(
        run_sum_results <= results_thresh ~ 1,
        flag_results_sh == 1 ~ 1, # grab Angola type site distributions here
        TRUE ~ 0,
      ),
      impflag_results_count = sum(impflag_results),
      impflag_results_sh = (impflag_results_count / ou_total_sites)
    ) %>% 
    ungroup() %>% 
    #flag VIP sites
    mutate(impflag_both = if_else(impflag_results == 1 & impflag_targets == 1, 1, 0))
  
  # Look at where a site is important to both target and results
  df_datim_wgts %>% 
    group_by(operatingunit, indicator) %>% 
    summarise(n = mean(ou_total_sites),
      res = sum(impflag_results),
      tgts = sum(impflag_targets),
      both = sum(impflag_both)) %>%
    print(n = Inf)
  
  # plot results for summary graphic
  df_datim_wgts %>% 
    group_by(operatingunit, indicator) %>% 
    summarise(n = mean(ou_total_sites),
      results_flags = sum(impflag_results),
      targets_flags = sum(impflag_targets),
      both_flags = sum(impflag_both)) %>%
    ungroup() %>% 
    gather(flagtype, value, results_flags:both_flags) %>% 
    mutate(ou_order = fct_reorder(operatingunit, value, .fun = max, .desc = TRUE),
      ind_order = fct_reorder(indicator, value),
      logval = log(value)) %>% 
    ggplot(., aes(ind_order, flagtype, fill = logval)) +
    geom_tile(colour = "white") + geom_text(aes(label = ifelse(value != 0, value, NA)), color = "#303030") +
    facet_wrap(~ou_order) + coord_flip() +
    scale_fill_viridis_c(option = "A", begin = 0.5, na.value = "#f1f1f1", direction = -1) +
    theme_minimal() +
    theme(strip.text = element_text(hjust = 0),
      legend.position = "none") +
    labs(x = "", y = "", title = "Summary of important sites for targets and results",
      caption = str_c("Created on ", format(Sys.Date(), "%Y%m%d")))
  
  #remove files (just keep zipped folder)
  unlink(files)
  
  #remove extra objects
  rm(df_datim, files)
  
  
  #store file name
  filename <- paste0("DATIM_FLAGS_", format(Sys.Date(), "%Y%m%d"), ".csv")
  
  #save
  write_csv(df_datim_wgts, file.path(out_folder, filename), na = "")

