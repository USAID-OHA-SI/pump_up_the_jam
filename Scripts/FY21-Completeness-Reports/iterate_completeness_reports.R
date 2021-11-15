library(tidyverse)
library(rmarkdown)
library(here)

country <- unique(df_comp_ou$operatingunit)

reports <- tibble(
  output_file = stringr::str_c("output/", country, "-markdown-HFR-completeness.html"),
  params = map(country, ~list(country = .))
)

head(reports)

#create markdown folder under wd if it doesn't exist
if(dir.exists("markdown") == FALSE) {
  dir.create("markdown")
}

#delete old files
unlink("markdown/*")

#create reports
reports %>%
  pwalk(render, 
        input = here("reporting_completeness_markdown.Rmd"))


reports %>%
  pwalk(rmarkdown::render, input = "rmarkdown-completeness-reports.Rmd")
