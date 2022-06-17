
tmp <- data.frame(x = 1:1e2, z = 1:1e2)
x <- seq(1, 100, 1)
z <- seq(1, 300, 1)

tmp <- expand.grid(x = x, z = z) %>% as_tibble()

# Create flags for what to visualize
tmp <- tmp %>% 
  mutate(cntry_count = ifelse(z == 1 & x <= cntry_count, 1, 0),
         ou_count = ifelse(z == 1 & x <= ou_count, 1, 0))




# VIZ ---------------------------------------------------------------------

  # Helper functions
  plot_tiles <- function(hfr_stat){
      geom_tile(data = . %>% 
                slice_head(n = {{hfr_stat}}), 
              fill = scooter_med, size = 0.01, color = "white")
  }

  annot_text <- function(annot_label) {
    annotate("text", x = 150, y = 50, label = annot_label, size = 40/.pt,
             family = "Source Sans Pro")
  }


  # Base grid of 10k squares
base <- 
  tmp %>% 
  ggplot(aes(x = z, y = x)) +
  geom_tile(fill = grey30k, size = 0.01, color = "white") +
  si_style_void() +
  coord_fixed(ratio = 1) +
  labs(x = NULL, y = NULL) +
  theme(plot.background = element_rect(fill = grey10k)) 

base + 
  annot_text(glue::glue("30K tiles"))

base +
  plot_tiles(ou_count) +
  annot_text(glue::glue("{ou_count}\nOperating Units"))


base +
  plot_tiles(cntry_count) +
  annot_text(glue::glue("{cntry_count}\nCountries"))

base + 
  plot_tiles(mech_count) +
  annot_text(glue::glue("{mech_count}\nMechanisms"))

base + 
  plot_tiles(ip_count) +
  annot_text(glue::glue("{ip_count}\nImplementers"))

base + 
  plot_tiles(ip_mech_count) +
  annot_text(glue::glue("{comma(ip_mech_count)}\nSites Reporting"))



# MUNGE AND PLAY

# Want to get a dataframe of all OUs + MECHS + SITES + PRIME PARTNER
ip_mechs <- 
  df %>% 
  filter(expect_reporting == TRUE) %>% 
  distinct(ip_name_clean, orgunituid, operatingunit, mech_code, indicator) %>% 
  count(operatingunit, ip_name_clean, mech_code, indicator)

ip_mechs %>% 
  filter(operatingunit %in% c("Zambia")) %>% 
  mutate(ip_order = fct_reorder(ip_name_clean, n, .desc = T)) %>% 
  ggplot(aes(fill = ip_name_clean, values = n)) +
  waffle::geom_waffle(color = "white", size = .25, n_rows = 10, flip = TRUE) +
  facet_wrap(~indicator, nrow = 1, strip.position = "bottom",
             labeller = label_wrap_gen(10)) +
  # facet_wrap(~ip_order, nrow = 1, strip.position = "bottom",
  #            labeller = label_wrap_gen(10)) +
  scale_x_discrete() + 
  scale_y_continuous(labels = function(x) x * 10, # make this multiplyer the same as n_rows
                     expand = c(0,0)) +
  si_style(facet_space = 0.4) 

 



si_save("Images/tmp.svg")  
