## PROJECT:  Pump up the jam
## AUTHOR:   A.Chafetz, T.Essam | USAID
## LICENSE:  MIT
## PURPOSE:  calculate completeness of reporting
## DATE:     2020-03-11
## UPDATED:  2020-06-20


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(glamr)
library(vroom)
library(scales)
library(extrafont)
library(ggtext)
library(extrafont)
library(Wavelength)
library(glamr)
library(glitr)
library(COVIDutilities)
library(ISOcodes)



# GLOBAL VARIABLES --------------------------------------------------------

    
    out_folder <- "Dataout"
    viz_folder <- "Images"
    
    ind_sel  <- c("HTS_TST", "TX_NEW", "TX_CURR")
    
    pal <- viridis_pal()(6) #%>% show_col()
    color_hv_sites <- pal[1]
    color_ref <- "gray30" #"#C8C8C8"
    color_all_sites <- "#D3D3D3"
    period <- "Q3"
    
    
    #Stringency Index API url - start/end date 
    ox_start <- "2020-01-01"
    ox_end <- today()
    url_ox <- paste("https://covidtrackerapi.bsg.ox.ac.uk/api/v2/stringency/date-range",
                    ox_start, ox_end, sep = "/")
    rm(ox_end, ox_start)
    
    start_date <- "2019-12-30"
    weeks <- 40
    myuser <- ""

# IMPORT ------------------------------------------------------------------

    #load joint HFR + DATIM dataset for FY20Q3 (created in Scripts/assemble_data_Q2.R)
    df_joint <- list.files(out_folder, paste0("HFR_DATIM_FY20", quarter, "_[[:digit:]]+\\.csv"), 
                           full.names = TRUE) %>% 
        vroom(col_types = c(.default = "c"))  


# MUNGE -------------------------------------------------------------------

#change hfr reporting variable for clarity
    df_joint <- rename(df_joint, hfr_results = val)
    
    #TODO: @ache - what are doing here for Q2? -> given the ask (and ease), let just make about Q2
    
    #filter dates to keep within bounds of Q3 (full pd indicators use all weeks of pd) 
    df_joint <- 
        df_joint %>% 
        mutate(date = as_date(date)) %>% 
        filter((!indicator %in% c("TX_CURR", "TX_MMD") &
                    between(date, as.Date("2020-01-20"), as.Date("2020-07-27"))) |
                   (indicator %in% c("TX_CURR", "TX_MMD") &
                        between(date, as.Date("2020-01-20"), as.Date("2020-07-27"))),
               indicator == "HTS_TST") %>% 
        mutate(across(contains("results"), as.numeric),
               pd = as.numeric(hfr_pd),
               tgts = as.numeric(mer_targets)) 
    
    top_sites <- df_joint %>% 
        group_by(operatingunit, indicator) %>% 
        mutate(top_site = rank(desc(tgts), ties.method = "first")) %>% 
        filter(top_site == 1) %>% pull(orgunituid)
    
    ou_list <- df_joint %>% filter(orgunituid %in% top_sites) %>% 
        pull(operatingunit) %>% unique()



    top_sites_df <- 
        df_joint %>% 
    filter(orgunituid %in% top_sites,
           str_detect(operatingunit, "Region", negate = T)) 

    
    

# COVID DATA PULLS --------------------------------------------------------
    
    #COVID Restrictions (HDX)
    df_gov_measures <- extract_excel_data(hdx_govmes_url, 
                                          hdx_govmes_linkid, 
                                          "Database", 'xlsx')
    #Government Response (Oxford - https://covidtracker.bsg.ox.ac.uk/about-api)
    json <- url_ox %>%
        jsonlite::fromJSON(flatten = TRUE)
    
    #COVID cases (JHU)
    df_covid <- pull_jhu_covid()
    
    # MUNGE OXFORD DATA -------------------------------------------------------
    
    #covert from json to dataframe
    df_stringency <- json %>%
        unlist() %>%
        enframe()
    
    #clean up table
    df_stringency <- df_stringency %>% 
        rowwise() %>%
        mutate(
            parts = length(unlist(str_split(name, "[.]"))),
            tbl = first(unlist(str_split(name, "[.]"))),
            tbl = gsub("\\d", "", tbl)
        ) %>%
        filter(parts == 4) %>%    # Keep the data, section with the longest parts
        separate(name,
                 into = c("name", "date", "iso", "variable"),
                 sep = "[.]") %>%                   # Separate column into multiple parts
        select(date:value) %>%               # Get rid of extra columns
        filter(date != value, iso != value) %>%     # Exclude repetition
        mutate(date = ymd(date), value = as.numeric(value)) %>% 
        spread(variable, value) %>% 
        select(-contains("legacy"))
    
    #filter to PEPFAR countries
    df_stringency <- df_stringency %>% 
        filter(iso %in% iso_map$iso)
    
    rm(json)
    
    # COVID CALENDAR ----------------------------------------------------------
    
    covid <- who_pandemic() %>% pull(date)
    
    # Aling HFR to COVID stingency so we can merge by date and OU
    df_stringency_date <- 
        ISO_3166_1 %>% 
        select(Name, iso = Alpha_3) %>%
        mutate(Name = recode(Name, 
                             "Congo, The Democratic Republic of the" = "Congo (Kinshasa)",
                             "Myanmar" = "Burma",
                             "C?te d'Ivoire" = "Cote d'Ivoire",
                             "Lao People's Democratic Republic" = "Laos",
                             "Tanzania, United Republic of" = "Tanzania",
                             "Viet Nam" = "Vietnam")) %>% 
        left_join(df_stringency) %>% 
        rename(operatingunit = Name) %>% 
        mutate(operatingunit = recode(operatingunit, 
                                      "Congo (Kinshasa)" = "Democratic Republic of the Congo")) %>% 
        filter(iso %in% iso_map$iso) %>% 
        mutate(covid_val = -0.15)
    
    # Adding custom colors to stringency index for plotting. Collapsing down to week to aling with HFR
    # so plots line up.
    df_stringency_wkly <- 
        df_stringency_date %>% 
        group_by(date = floor_date(date, "week", week_start = getOption("lubridate.week.start", 1)),
                 iso, operatingunit, covid_val) %>%
        summarise(stringency = mean(stringency_actual, na.rm = TRUE)) %>% 
        ungroup() %>% 
        filter(!is.na(date)) %>% 
        mutate(bins = case_when(is.na(stringency)  ~ "NA",
                                stringency < 1     ~ "<1",
                                stringency < 25    ~ "1-24",
                                stringency < 50    ~ "25-49",
                                stringency < 75    ~ "50-74",
                                stringency < 85    ~ "75-84",
                                TRUE               ~ "85-100"),
               color = case_when(is.na(stringency) ~ "#D9CDC3",
                                 stringency < 1    ~ "#D3E8F0",
                                 stringency < 25   ~ "#FAE1AF",
                                 stringency < 50   ~ "#FDAC7A",
                                 stringency < 75   ~ "#F6736B",
                                 stringency < 85   ~ "#DA3C6A",
                                 TRUE              ~ "#A90773"
               )) %>% 
        mutate(bins = factor(bins, c("NA","<1", "1-24", "25-49", "50-74", "75-84", "85-100")),
               color = factor(color, c("#D9CDC3", "#D3E8F0","#FAE1AF", "#FDAC7A", "#F6736B", "#DA3C6A", "#A90773")))
    
    
    # Need to expand the completeness data to extend through the max of the covid data
    max_date_covid <- df_stringency_wkly$date %>% max()
    max_date_hfr <- df_comp_wks_viz$date %>% max()
    min_date_hfr <- df_comp_wks_viz$date %>% min() # Min date distored by TX_CURR and TX_MMD
    expand_dates <- as_tibble(seq.Date(as.Date("2019-12-30"), by = "week", length.out = 42)) %>% 
        rename(date = value)
    

    
    # Merge with completeness data
    df_comp_covid <- df_stringency_wkly %>% 
        left_join(., top_sites_df) %>% 
        arrange(operatingunit, date) %>% 
        mutate(operatingunit = if_else(operatingunit == "Democratic Republic of the Congo", "DRC", operatingunit))

    
    df_comp_covid %>% View()
        filter(operatingunit %in% ou_list, operatingunit != "Uganda") %>% 
        ggplot(aes(x = date, y = hfr_results)) +
        annotate("rect", xmin = as.Date("2020-01-01"), xmax = as.Date("2020-04-01"), ymin = 0, ymax = 1, 
                 fill = grey10k, alpha = 0.5) +
        annotate("rect", xmin = as.Date("2020-07-01"), xmax = as.Date("2020-08-25"), ymin = 0, ymax = 1, 
                 fill = grey10k, alpha = 0.5) +
        geom_col(aes(y = -100, fill = (color)), alpha = 1) +
        geom_col(fill = "#84c3be") +
        geom_segment(aes(y = 0, yend = 1, x = covid, xend = covid), colour = grey20k, size = 1, alpha = 0.80) +
        scale_fill_identity() +
        facet_wrap(~operatingunit) +
        scale_x_date(date_labels = "%b", date_breaks = "2 months", 
                     limits = as.Date(c('2020-01-01','2020-08-25'))) +
        si_style_ygrid() 
       
        
        
        
        # 
        # #geom_col(aes(y = ymax), fill = grey10k, alpha = 0.4) +
        # geom_segment(aes(y = 0, yend = 1, x = covid, xend = covid), colour = grey20k, size = 1, alpha = 0.80) +
        # geom_col(fill = "#84c3be") +
        # geom_hline(yintercept = 0, size = 2, colour = "white") +
        # geom_hline(yintercept = 0, size = 0.25, colour = grey90k, fill = grey70k) +
        # facet_wrap(~ou_order, nrow = 3) +
        # scale_y_continuous(labels = percent, limits = c(-.15, 1)) +
        # scale_x_date(date_labels = "%b", date_breaks = "2 months", 
        #              limits = as.Date(c('2020-01-01','2020-08-25')))+
        # scale_fill_identity() +
        # #scale_fill_gradientn(colours = RColorBrewer::brewer.pal(7, 'OrRd'), na.value = "white") +
        # si_style_ygrid() +
        # labs(x = NULL, y = NULL, title = paste0("WEEKLY ", indicator, ": HFR REPORTING REMAINS LOW ACROSS VARYING STRINGENCY MEASURES"),
        #      caption = "Source: HFR Data + stringecy index from Blavatnik School of Government at Oxford University") +
        # theme(legend.position = "none")
    
        