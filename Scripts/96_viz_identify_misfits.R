## PROJECT:  Pump up the jam
## AUTHOR:   A.Chafetz, T.Essam | USAID
## LICENSE:  MIT
## PURPOSE:  vaudit plots to identify mechs w/ improper HFR submissions
## DATE:     2020-03-23

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
  color_ref <- "gray30" #"#C8C8C8"
  color_all_sites <- "#D3D3D3"
  
  # To avoid infinities and flag as NA
    ratio_calc <- function(x, y) {
      ifelse(y > 0.000, x / y, NA)
    }
  
    prinf <- function(df) {
      print(df, n = Inf)
    }


# IMPORT ------------------------------------------------------------------

  #import
  df_q1 <- list.files(out_folder, "HFR_DATIM_FY20Q1_Agg_[[:digit:]]+\\.csv", full.names = TRUE) %>% 
    vroom()


# GGPLOTS -----------------------------------------------------------------

  # Function to create a scatter plot of hfr to mer results on a scatter
  # flag points that are +/- 10% points from hfr_mer/mer_results ratio
    ou_auditplot <- function(df, indicator = "TX_CURR") {
        
        indic <- df %>% count(indicator) %>% pull(indicator)
        
        df %>% 
          filter(indicator == indicator) %>% 
          mutate(
            hfr_ratio = ratio_calc(hfr_results, mer_results),
            hfr_flag = if_else(hfr_ratio <= 0.90 | hfr_ratio >= 1.1, 1, 0),
            ou_order = fct_reorder(operatingunit, mer_targets, .fun = sum, .desc = TRUE)
            ) %>% 
          { 
            ggplot() +
              geom_point(data = dplyr::filter(., hfr_flag == 0), 
                aes(x = mer_results, y = hfr_results, size = mer_targets, fill = factor(hfr_flag)),
                shape = 21, colour = "white", stroke = 0.25, alpha = 0.60) +
              geom_point(data = dplyr::filter(., hfr_flag == 1), 
                aes(x = mer_results, y = hfr_results, size = mer_targets, fill = factor(hfr_flag)),
                shape = 21, colour = "white", stroke = 0.25, alpha = 0.60) +
          facet_wrap(~ou_order, scales = "free") +
          scale_fill_manual(values = c("0" = "#909090", "1" = "#dd1c77")) +
          theme_minimal() +
              theme(strip.text = element_text(hjust = 0),
                legend.position = "none") +
          geom_abline(intercept = 0, slope = 1, colour = "#909090") +
              labs(x = "MER results", y = "HFR results",
                title = str_c(indicator, ": HFR DATA DOES NOT MATCH TO MER RESULTS DATA FOR NUMEROUS COUNTRIES IN FY20 Q1"),
                caption = "Source: FY20Q1 MER + HFR")
          }
      }
        
    ou_auditplot(df_q1)
    
    ind_sel  <- c("HTS_TST", "TX_NEW", "TX_CURR")
    plots_audit <- map(ind_sel, ~ou_auditplot(df_q1, indicator = .x))
    
  

# TABLES ------------------------------------------------------------------

    
  # Listing out each OU to show how misfits there are in each OU by indicator    
    df_audit <- df_q1 %>% 
      mutate(
        hfr_ratio = ratio_calc(hfr_results, mer_results),
        hfr_flag = if_else(hfr_ratio <= 0.90 | hfr_ratio >= 1.1, 1, 0),
        ou_order = fct_reorder(operatingunit, mer_targets, .fun = sum, .desc = TRUE)
      ) 
    
    df_audit %>% 
      filter(indicator %in% ind_sel) %>% 
      group_by(operatingunit, indicator) %>% 
      summarise(count = sum(hfr_flag, na.rm = TRUE),
        rows = n()) %>% 
      group_by(operatingunit) %>% 
      mutate(total_flags = sum(count)) %>% 
      ungroup() %>% 
      mutate(indicator_flag_rate = count / rows) %>% 
      arrange(desc(total_flags)) %>% 
      prinf() 
