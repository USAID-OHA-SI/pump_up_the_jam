## TEST HEX MAP

library(tidyverse)
library(vroom)
library(rnaturalearth)
library(sf)

dataout <- "Dataout"

ctry_sel <- "Nigeria"

##df_txcurr carried over from 13_viz_txcurr.R
# df_repgap <- df_txcurr %>% 
#   filter(hfr_pd >=4) 
# 
# 
# df_repgap <- df_repgap %>% 
#   mutate(type = ifelse(hfr_pd >= 7, "post", "pre")) %>% 
#   group_by(operatingunit, countryname, orgunituid,
#            mech_code, type) %>% 
#   summarise(has_hfr_reporting = sum(has_hfr_reporting, na.rm = TRUE),
#             is_datim_site = sum(is_datim_site, na.rm = TRUE)) %>% 
#   ungroup()
# 
# 
# df_repgap <- df_repgap %>% 
#   left_join(iso_map, by = c("countryname" = "operatingunit")) %>% 
#   select(-regional)
# 
# df_repgap <- df_orgheirarchy %>% 
#   select(orgunituid, latitude, longitude) %>% 
#   left_join(df_repgap, .)
# 
# write_csv(df_repgap, file.path(dataout, "HFR_TXCURR_munged.csv"), na = "")

df_repgap <- vroom(file.path(dataout, "HFR_TXCURR_munged.zip"))

#country border
  ctry_adm0 <- ne_countries(country = ctry_sel, scale = 'medium', returnclass = 'sf') %>% 
    st_geometry(NULL) %>% 
    st_transform(crs = st_crs(3857)) %>% 
    st_as_sf()

#create hex grid for country
  ctry_hex <- ctry_adm0 %>% 
    st_make_grid(what = 'polygons', cellsize = 30000, square = F) %>% 
    st_as_sf() 

#clip hexes to country border
  ctry_hex <- ctry_hex %>% 
    st_intersection(ctry_adm0) 

#create id for merging
  ctry_hex <- ctry_hex %>% 
    mutate(id = row_number())

#limit dataset to country, ensure coords exist & transform from decimal degrees to projection in meters
  df_mapdata <- df_repgap %>% 
    filter(countryname == ctry_sel) %>% 
    filter_at(vars(latitude, longitude), any_vars(!is.na(.))) %>% 
    st_as_sf(coords = c("latitude", "longitude"),
             crs = st_crs(4326)) %>% 
    st_transform(crs = st_crs(3857))
  
#bind hex ids onto data for join post aggregation
  df_mapdata <- st_join(df_mapdata, ctry_hex, join = st_intersects)
  
#how many sites didn't map to a bin?
  df_mapdata %>% 
    select(-geometry) %>% 
    as_tibble() %>% 
    distinct(orgunituid, id) %>% 
    count(is.na(id))

#remove geometry and aggregate to calc hex completeness
  df_mapdata <- df_mapdata %>% 
    select(-geometry) %>% 
    as_tibble() %>% 
    group_by(countryname, iso, id, type) %>% 
    summarise_at(vars(has_hfr_reporting, is_datim_site), sum, na.rm = TRUE) %>% 
    ungroup()

#create completeness pre/post and reshape to get one variable
  df_mapdata <- df_mapdata %>% 
    mutate(reporting_rate = has_hfr_reporting/is_datim_site,
           status = case_when(reporting_rate <= .25 ~ paste0(type, "-low"),
                              reporting_rate < .75 ~ paste0(type, "-med"),
                              TRUE ~ paste0(type, "-high"))) %>% 
    select(-has_hfr_reporting, -is_datim_site, -reporting_rate)  %>% 
    spread(type, status) %>% 
    unite(position, c(pre, post), sep = ", ")


#fill colors for bivarate plot
  bivar_map <- c("pre-high, post-high" = "#e8e8e8",
                 "pre-low, post-high" = "#5ac8c8",
                 "pre-med, post-high" = "#ace4e4",
                 "pre-high, post-low" = "#be64ac",
                 "pre-low, post-low" = "#3b4994",
                 "pre-med, post-low" = "#8c62aa",
                 "pre-high, post-med" = "#dfb0d6",
                 "pre-low, post-med" = "#5698b9",
                 "pre-med, post-med" = "#a5add3")


#join aggregated data to hex
  df_mapdata <- left_join(ctry_hex, df_mapdata)

#map
  df_mapdata %>% 
    ggplot() +
    geom_sf(aes(fill = position)) +
    scale_fill_manual(values = bivar_map) +
    theme_void()

