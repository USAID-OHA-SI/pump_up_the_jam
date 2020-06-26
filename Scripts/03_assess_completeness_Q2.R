## PROJECT:  Pump up the jam
## AUTHOR:   A.Chafetz, T.Essam | USAID
## LICENSE:  MIT
## PURPOSE:  calculate completeness of reporting
## DATE:     2020-03-11
## UPDATED:  2020-06-20


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(vroom)


# GLOBAL VARIABLES --------------------------------------------------------

out_folder <- "Dataout"
quarter <- "Q2"


# IMPORT ------------------------------------------------------------------

  #load joint HFR + DATIM dataset for FY20Q2 (created in Scripts/assemble_data_Q2.R)
    df_joint <- list.files(out_folder, "HFR_DATIM_FY20Q2_[[:digit:]]+\\.csv", full.names = TRUE) %>% 
      vroom(col_types = c(.default = "c"))  
  
    
# MUNGE -------------------------------------------------------------------

  #change hfr reporting variable for clarity
    df_joint <- rename(df_joint, hfr_results = val)
    
#TODO: @ache - what are doing here for Q2? -> given the ask (and ease), let just make about Q2

  #filter dates to keep within bounds of Q2 (full pd indicators use all weeks of pd) 
    df_joint <- df_joint %>% 
      mutate(date = as_date(date)) %>% 
      filter((!indicator %in% c("TX_CURR", "TX_MMD") &
             between(date, as.Date("2019-12-30"), as.Date("2020-05-04"))) |
             (indicator %in% c("TX_CURR", "TX_MMD") &
                between(date, as.Date("2019-12-23"), as.Date("2020-05-04")))) 
    #check
      # df_joint %>%
      #   count(date, indicator) %>%
      #   spread(indicator, n)

  #reshape long to preserve NAs for non reporting
  #TODO: @acheftz - if we are preserving NAs why are they removed in gather?
    df_joint_lng <- df_joint %>% 
      gather(type, value, hfr_results, mer_results, mer_targets, na.rm = TRUE) %>% 
      mutate(value = as.double(value))
 
    rm(df_joint)
    
# AGGREGATION + MUNGING FUNCTIONS BY PD TYPE ------------------------------

  #aggregate by period type
    
    pd_agg <- function(df, rm_time_components){
      
      #remove time componets for aggregation
        df <- df %>% 
          select(-all_of(rm_time_components))
        
      #create a period value for non-TX_CURR indicators
        df_pd_sum <- df %>% 
          filter(!indicator %in% c("TX_CURR", "TX_MMD"),
                 type == "hfr_results") %>% 
          group_by_if(is.character) %>% 
          summarise(value = sum(value, na.rm = TRUE)) %>% 
          ungroup()
        
      #create a period value for TX_CURR indicators
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
    
    
  #merge on flags (02)
    
    merge_flags <- function(df){
      
      #import volume weighting
        df_datim_wgts <- list.files(out_folder, "DATIM_FLAGS_[[:digit:]]+\\.csv", full.names = TRUE) %>% 
          vroom(col_types = c(.default = "d",
                              orgunituid = "c",
                              indicator = "c",
                              operatingunit = "c",
                              mech_code = "c",
                              fy = "c"))
      
      #drop vars before merging
        df_datim_wgts <- df_datim_wgts %>% 
          select(-c(mer_results, mer_targets, operatingunit))
      
      #join to pd aggregation
        df_wgts <- left_join(df, df_datim_wgts, 
                             by = c("orgunituid", "mech_code", "fy", "indicator"))
        
        return(df_wgts)
        
    }
    
    
  #create completeness
    
    gen_completeness <- function(df, out_folder = NULL){
      
      #create flags for whether site reported HFR and if site exists in DATIM
        df <- df %>% 
          mutate(has_hfr_reporting = hfr_results > 0 ,
                 is_datim_site = mer_results > 0 | mer_targets > 0)
      
      #remove where no DATIM results/targets and there is some value in the row
        df <- df %>% 
          filter(!(has_hfr_reporting == TRUE & is_datim_site == FALSE)) %>% 
          filter_at(vars(hfr_results, mer_results, mer_targets), any_vars(.!=0))
      
      #clean period name for viz later
        if("hfr_pd" %in% names(df))
          df <- mutate(df, hfr_pd = paste0(fy, ".", str_pad(hfr_pd, 2, pad = "0")))
        
      #export #1
        type <- case_when("date" %in% names(df) ~ "Wks",
                         "hfr_pd" %in% names(df) ~ "Pds",
                         TRUE ~ "")
        filename <- paste0("HFR_DATIM_FY20", quarter, "_Agg", type, "_", format(Sys.Date(), "%Y%m%d"), ".csv")
        if(!is.null(out_folder))
          write_csv(df, file.path(out_folder, filename), na = "")
      
      #identify pd grouping vars
        pd_grp <- intersect(c("hfr_pd", "date"), names(df))
        grp_vars <- c("operatingunit", "indicator", "site_type", pd_grp)
      
      #aggregate across all sites
        df_agg <- df %>% 
          mutate(site_type = "All") %>% 
          group_by_at(grp_vars) %>% 
          summarise_at(vars(has_hfr_reporting, is_datim_site, mer_targets), sum, na.rm = TRUE) %>% 
          ungroup()
      
      #aggregate by site volume
        df_agg_vol <- df %>% 
          mutate(site_type = case_when(impflag_targets == 1 ~ "High Volume (Target)",
                                       impflag_targets == 0 ~ "Low Volume (Target)")) %>% 
          group_by_at(grp_vars) %>%
          summarise_at(vars(has_hfr_reporting, is_datim_site, mer_targets), sum, na.rm = TRUE) %>% 
          ungroup() 
        
      #combine
        df_agg <- df_agg %>% 
          bind_rows(df_agg_vol) %>% 
          select(site_type, everything()) 
        
      #create completeness
        df_comp <- df_agg %>% 
          mutate(completeness = case_when(is_datim_site >  0 ~ has_hfr_reporting / is_datim_site))
        
      #export #2
        type <- ifelse(length(type) > 1, paste0("_", type), type)
        comp_filename <- paste0("HFR_Completeness_", quarter, "_", type, "_",format(Sys.Date(), "%Y%m%d"), ".csv")
        if(!is.null(out_folder))
          write_csv(df_comp, file.path(out_folder, comp_filename), na = "")
        
        return(df_comp)
        
    }


# GENERATE VARIOUS DATASETS -----------------------------------------------

    
  #full quarter
    df_qtr <- df_joint_lng %>% 
      pd_agg(c("hfr_pd","date")) %>% 
      merge_flags() %>% 
      gen_completeness(out_folder)  
    
  #by pd
    df_pds <- df_joint_lng %>% 
      pd_agg(c("date")) %>% 
      merge_flags() %>% 
      gen_completeness(out_folder)  
    
  #by week
    df_wks <- df_joint_lng %>% 
      spread(type, value, fill = 0) %>% 
      merge_flags() %>% 
      gen_completeness(out_folder)
    

 

# EXPORT ------------------------------------------------------------------
    
  #already completed in gen_completeness
    
    