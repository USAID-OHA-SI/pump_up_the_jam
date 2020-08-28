## PROJECT:  Pump up the jam
## AUTHOR:   A.Chafetz, T.Essam | USAID
## LICENSE:  MIT
## PURPOSE:  visualizing completeness of reporting
## DATE:     2020-03-13
## UPDATED:  2020-03-16


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(vroom)
library(scales)
library(extrafont)
library(ggtext)
library(extrafont)
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
  period <- "Q2"

  #quarter starts
  qtrs <- as_date(c("2019-10-01", "2020-01-01", "2020-04-01", "2020-07-01", "2020-09-30"))
  
  #review slide background
  bckgrnd <- "#cfcdc9"
  
  
#Stringency Index API url - start/end date 
  ox_start <- "2020-01-01"
  ox_end <- today()
  url_ox <- paste("https://covidtrackerapi.bsg.ox.ac.uk/api/v2/stringency/date-range",
                  ox_start, ox_end, sep = "/")
  rm(ox_end, ox_start)
  
  start_date <- "2019-12-30"
  weeks <- 48
  
  # DATIM base
  baseurl <- "https://final.datim.org/"
  
# IMPORT ------------------------------------------------------------------

  #myuser <- "" #do not save
  iso_map <- identify_levels(username = myuser, password = mypwd(myuser)) %>% 
    rename(iso = countryname_iso)
  
  #TX_CURR data (DATIM)
  url_tx <- paste0(baseurl,
                   "api/29/analytics.json?",
                   "dimension=RUkVjD3BsS1:PE5QVF0w4xj&", #Top Level 
                   "dimension=IeMmjHyBUpi:Jh0jDM5yQ2E;W8imnja2Owd&",
                   "dimension=pe:THIS_FINANCIAL_YEAR&",  #period
                   "dimension=LxhLO68FcXm:MvszPTQrUhy&", #Technical Area: TX_CURR
                   "dimension=bw8KHXzxd9i:NLV6dy7BE2O&", #Funding Agency: USAID
                   "dimension=ou:LEVEL-4;ybg3MO3hcf4&", 
                   "displayProperty=SHORTNAME&skipMeta=false&hierarchyMeta=true")
  df_tx <- extract_datim(url_tx, myuser, mypwd(myuser))
  
# COVID CALENDAR ----------------------------------------------------------
  
  covid <- who_pandemic() %>% pull(date)
  
  fy20_dates <- seq.Date(as_date("2019-10-01"), as_date("2020-09-30"), length.out = 365)
  
  df <- tibble(date = fy20_dates) %>% 
    mutate(value = ifelse(date < "2020-07-01", 1, 0),
           post_who = date > covid)
  
  df %>% 
    ggplot(aes(date, value, fill = post_who)) +
    geom_area() +
    geom_vline(xintercept = covid, size = 2) +
    geom_vline(xintercept = qtrs, color = grey10k, size = 1) +
    scale_x_date(date_labels = "%b") +
    scale_fill_manual(values = c("gray60", USAID_red)) +
    labs(x = NULL, y = NULL,
         subtitle = "FY20 Calendar") +
    si_style_nolines() +
    theme(axis.text.y = element_blank()) +
    theme(legend.position = "none",
          plot.background = element_rect(fill = bckgrnd, colour = NA))  
  
# MUNGE TX DATA -----------------------------------------------------------
  
  #create country names
  df_tx <-df_tx %>% 
    mutate(countryname = ifelse(str_detect(orglvl_3, "Region"), 
                                `Organisation unit`, orglvl_3))
  
  #limit vars, sum to OU level, and spread by results/targets
  df_tx <- df_tx %>% 
    select(period = Period,
           indicator = `Technical Area`,
           type = `Targets / Results`, 
           fundingagency = `Funding Agency`,
           countryname,
           value = Value) %>% 
    group_by_if(is.character) %>% 
    summarise(value = sum(value, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(type = str_remove(type, "MER ") %>% tolower) %>% 
    spread(type, value) 
  
  #add ISO code
  df_tx <- ISO_3166_1 %>% 
    select(Name, iso = Alpha_3) %>%
    mutate(Name = recode(Name, 
                         "Congo, The Democratic Republic of the" = "Democratic Republic of the Congo",
                         "Myanmar" = "Burma",
                         "C?te d'Ivoire" = "Cote d'Ivoire",
                         "Lao People's Democratic Republic" = "Laos",
                         "Tanzania, United Republic of" = "Tanzania",
                         "Viet Nam" = "Vietnam")) %>% 
    left_join(df_tx, ., by = c("countryname" = "Name")) %>% 
    relocate(iso, .after = countryname)
  
  #largest treatment programs
  top_tx <- df_tx %>% 
    filter(countryname != "South Africa") %>% 
    slice_max(n = 12, order_by = targets) %>% 
    pull(countryname) 
  
  top_tx_iso <- df_tx %>% 
    filter(countryname != "South Africa") %>% 
    slice_max(n = 12, order_by = targets) %>% 
    pull(iso)    
  

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

  df_covid <- ISO_3166_1 %>% 
    select(Name, iso = Alpha_3) %>%
    mutate(Name = recode(Name, 
                         "Congo, The Democratic Republic of the" = "Congo (Kinshasa)",
                         "Myanmar" = "Burma",
                         "C?te d'Ivoire" = "Cote d'Ivoire",
                         "Lao People's Democratic Republic" = "Laos",
                         "Tanzania, United Republic of" = "Tanzania",
                         "Viet Nam" = "Vietnam")) %>% 
    left_join(df_covid, ., by = c("countryname" = "Name")) %>% 
    mutate(countryname = recode(countryname, 
                                "Congo (Kinshasa)" = "Democratic Republic of the Congo"))
  
  #filter to just PEPFAR countries
  df_covid_pepfar <- df_covid %>% 
    filter(iso %in% iso_map$iso)
  
  df_covid_pepfar <- df_covid_pepfar %>%
    filter(cases >= 10)
  
  df_covid_pepfar_top <- df_covid_pepfar %>% 
    filter(countryname %in% top_tx) %>%
    group_by(countryname) %>% 
    mutate(lab = case_when(date == max(date) ~ iso)) %>% 
    ungroup()
  
  

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
  
# ALIGNMENT ----------------------------------------------------------
  
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
  df_stringency <- 
    df_stringency_date %>% 
    # group_by(date = floor_date(date, "week", week_start = getOption("lubridate.week.start", 1)),
    #          iso, operatingunit, covid_val) %>%
    # summarise(stringency = mean(stringency_actual, na.rm = TRUE)) %>% 
    # ungroup() %>% 
    # filter(!is.na(date)) %>% 
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
  expand_dates <- as_tibble(seq.Date(as.Date("2019-12-30"), by = "week", length.out = 33)) %>% 
    rename(date = value)

  # Need to collapse covid_daily data down to weekly time-step
  
  
  
  

# Join COVID and STringency data together
  df_covid_stringe <- df_stringency %>% left_join(df_covid, by = c("iso", "date"))
  

  df_covid_stringe %>% 
    filter(iso %in% top_tx_iso) %>% 
    ggplot(aes(date, )) +
    geom_line() +
    facet_wrap(~iso) +
    si_style() 
  
  select_ous <- c("Kenya", "Nigeria", "Zambia", "Zimbabwe")
  single_ou <- c("Zambia")
  
  df_covid_stringe %>%
    filter(operatingunit %in%  "Zambia") %>% 
    mutate(sort_var = fct_reorder(operatingunit, daily_cases, .desc = T)) %>% 
    ggplot(aes(x = date), group = operatingunit) +
    geom_vline(xintercept = as.Date("2020-04-01"), size = 0.5, color = grey20k) +
    geom_vline(xintercept = as.Date("2020-07-01"), size = 0.5, color = grey20k) +
    #geom_line(aes(y = if_else(cases> 1, daily_cases, NA_real_))) + 
    geom_area((aes(y = if_else(cases> 0, daily_cases, NA_real_))), fill = grey40k, alpha = 0.85) +
    geom_hline(yintercept = -2, size = 3, color = "white") +
    geom_col(aes(y = -50, fill = (color)), alpha = 1) +
    facet_wrap(~paste0(sort_var, "\n")) +
    scale_fill_identity() +
    scale_x_date(limits = as.Date(c('2020-01-01','2020-08-26')),
                 date_labels = "%b", date_breaks = "1 months") +
    #scale_y_log10() +
    si_style_ygrid() +
    labs(title = "DAILY COVID-19 CASES CONTINUE TO RISE INTO Q4",
         caption = "Source: JHU COVID-19 feed + stringecy index from Blavatnik School of Government at Oxford University",
         x =NULL, y=NULL)
    
  # Zambia Specific plots for POART Q3
  library(zoo) # for rolling mean
  
  df_covid_stringe %>%
    filter(operatingunit %in% select_ous) %>% 
    arrange(date) %>% 
    group_by(operatingunit) %>% 
    mutate(sort_var = fct_reorder(operatingunit, daily_cases, .desc = T),
           seven_day = zoo::rollmean(daily_cases, 7, fill = NA, align = c("right")),
           fourteen_day = zoo::rollmean(daily_cases, 14, fill = NA, align = c("right"))) %>% 
    ungroup() %>% 
    ggplot(aes(x = date), group = operatingunit) +
    geom_rect(aes(xmin = as.Date("2020-03-01"), xmax = as.Date("2020-04-01"), ymin = 0, ymax = Inf), 
              fill = grey10k, alpha = 0.05) +
    geom_rect(aes(xmin = as.Date("2020-07-01"), xmax = as.Date("2020-08-26"), ymin = 0, ymax = Inf), 
              fill = grey10k, alpha = 0.05) +
    geom_vline(xintercept = as.Date("2020-04-01"), size = 0.5, color = grey20k) +
    geom_vline(xintercept = as.Date("2020-07-01"), size = 0.5, color = grey20k)+
    #geom_line(aes(y = if_else(cases> 1, daily_cases, NA_real_))) + 
    geom_col((aes(y = if_else(cases> 0, daily_cases, NA_real_))), fill = grey20k, alpha = 0.85) +
    geom_area(aes(y = fourteen_day), fill = "#f7c5c4", alpha = 0.75) +
    geom_line(aes(y = fourteen_day), color = "#d73636", size = 1) +
    # geom_line(aes(y = zoo::rollmean(daily_cases, 14, fill = grey20k, align = c("right")))) +
    #geom_hline(yintercept = -5, size = 2, color = "white") +
    geom_col(aes(y = -50, fill = (color)), alpha = 1) +
    geom_col(aes(y = -10), fill = "white") +
    facet_wrap(~paste0(sort_var, "\n")) +
    scale_fill_identity() +
    scale_x_date(limits = as.Date(c('2020-03-01','2020-08-26')),
                 date_labels = "%b", date_breaks = "1 months") +
    #scale_y_log10() +
    si_style_ygrid() +
    labs(title = "DAILY COVID-19 CASES CONTINUE TO RISE INTO Q4",
         caption = "Source: JHU COVID-19 feed + stringecy index from Blavatnik School of Government at Oxford University",
         x =NULL, y=NULL)
  
  
  
  ggsave(file.path(viz_folder, "Q4_COVID_rise.png"),
         plot = last_plot(), 
         width = 10, height = 5.625, dpi = "retina")
  
  
  
  
  df_covid_stringe %>% 
    ggplot() +
    geom_col(aes(y = covid_val, fill = (color)), alpha = 1) +
    geom_col(aes(y = ymax), fill = grey10k, alpha = 0.4) +
    geom_segment(aes(y = 0, yend = 1, x = covid, xend = covid), colour = grey10k, size = 2, alpha = 0.80) +
    geom_col(fill = "#84c3be") +
    geom_errorbar(aes(x = date, ymin = comp_trunc, ymax = comp_trunc), size = 0.5, width = 5, colour = "#27b7fb") +
    geom_hline(yintercept = 0, size = 2, colour = "white") +
    geom_hline(yintercept = 0, size = 0.25, colour = grey90k, fill = grey70k) +
    facet_wrap(~ou_order) +
    scale_y_continuous(labels = percent, limits = c(-.15, 1)) +
    scale_x_date(date_labels = "%b", date_breaks = "1 months")+
    scale_fill_identity() +
    #scale_fill_gradientn(colours = RColorBrewer::brewer.pal(7, 'OrRd'), na.value = "white") +
    si_style_ygrid() +
    labs(x = NULL, y = NULL, title = "WEEKLY TX_NEW COMPLETENESS RATES SORTED BY LARGEST OUS\n",
         caption = "HFR Data + stringecy index from Blavatnik School of Government at Oxford University") +
    theme(legend.position = "none")
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  df_stringency %>% 
    ggplot(aes(date, fct_reorder(countryname, targets), fill = color)) +
    geom_tile(color = "white") +
    geom_vline(xintercept = qtrs,  color = "gray20", size = 1.2) +
    scale_fill_identity(guide = "legend", labels = rep("", 7)) +
    scale_x_date(expand = c(0.005, 0.005), position = "top") +
    scale_y_discrete(expand = c(.005, .005)) +
    labs(x = NULL, y = NULL,
         caption = "source: stringecy index from Blavatnik School of Government at Oxford University
           color scheme developed by Financial Times") + 
    si_style_nolines() +
    guides(fill = guide_legend(title = "stringency index",
                               nrow = 1)) +
    theme(legend.spacing.x = unit(0, 'cm'),
          legend.title = element_text(color = "gray30", family = "Source Sans Pro"),
          legend.position = "none",
          axis.text.y = element_text(size = 7),
          plot.background = element_rect(fill = bckgrnd, color = bckgrnd)
    )
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  top_ous <- c("South Africa", "Nigeria", "Mozambique", "Tanzania", "Zimbabwe", 
               "Uganda", "Nigeria", "Kenya", "Malawi", "Zambia", "DRC",
               "Eswatini", "Lesotho")
  

