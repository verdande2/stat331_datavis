# Load packages
library(tidyverse)
library(rmarkdown)


render(
     input = "AutomatedReport.Rmd",
     output_file = "Alabama.html",
     params = list(state = "Alabama")
)

render(
     input = "AutomatedReport.Rmd",
     output_file = "Alaska.html",
     params = list(state = "Alaska")
)

render(
     input = "AutomatedReport.Rmd",
     output_file = "Arizona.html",
     params = list(state = "Arizona")
)


## Create a vector of all states and the District of Columbia
#state <- tibble(state.name) %>%
#     rbind("District of Columbia") %>%
#     pull(state.name)

## Create a tibble with information on the:
## input R Markdown document
## output HTML file
## parameters needed to knit the document
#reports <- tibble(
#     input = "AutomatedReport.Rmd",
#     output_file = str_glue("{state}.html"),
#     params = map(state, ~ list(state = .))
#)

## Generate all of our reports
#pwalk(reports, render)