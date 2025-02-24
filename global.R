# Packages ----------------------------------------------------------------

# Packages for Interactive Web application
library(shiny)
library(bslib)
library(shinycssloaders)

# Packages for Data Cleaning/Processing
library(dplyr)
library(readr)
library(purrr)
library(stringr)
library(tidyr)

# Packages for Charts
library(ggplot2)
library(plotly)
library(scales)
library(htmlwidgets)

# Packages for Map Creation
library(sf)
library(leaflet)

# Packages for Tables
library(DT)

# Inputs ---------------------------------------------------------------
wgs84 <- 4326
load_clr <- "#91268F"

emphasis_order <- c("Birth", "Marriage", "Youth", "Total Population")
comparison_order <- c("Above National Average", "Below National Average", "Washington State")

# Data via RDS files ------------------------------------------------------
census_data <- readRDS("data/federal_emphasis_areas_data.rds") |> mutate(emphasis_area = str_replace_all(emphasis_area, "Fertility", "Birth"))
census_yr <- unique(census_data$year)

top_30_states <- census_data |> 
  filter(geography_type=="Statewide" & emphasis_area=="Total Population") |> 
  select("state", "rate") |>
  slice_max(rate, n=30) |>
  select("state") |>
  pull()

top_30_msa <- census_data |> 
  filter(geography_type=="Metro Region" & emphasis_area=="Total Population") |> 
  select("state", "rate") |>
  slice_max(rate, n=30) |>
  select("state") |>
  pull()

top_30 <- c(top_30_states, top_30_msa)

state_mapping_data <- readRDS("data/state_mapping_data.rds")
msa_mapping_data <- readRDS("data/msa_mapping_data.rds") |> distinct() |> slice_max(population, n=50)

national_youth <- census_data |> filter(state=="United States" & emphasis_area == "Youth") |> select(rate) |> pull()
national_birth <- census_data |> filter(state=="United States" & emphasis_area == "Birth") |> select(rate) |> pull()
national_marriage <- census_data |> filter(state=="United States" & emphasis_area == "Marriage") |> select(rate) |> pull()

census_data <- census_data |>
  mutate(rate = replace_na(rate, 0)) |>
  mutate(comparison = case_when(
    emphasis_area == "Youth" & rate < national_youth ~ "Below National Average",
    emphasis_area == "Youth" & rate >= national_youth ~ "Above National Average",
    emphasis_area == "Marriage" & rate < national_marriage ~ "Below National Average",
    emphasis_area == "Marriage" & rate >= national_marriage ~ "Above National Average",
    emphasis_area == "Birth" & rate < national_birth ~ "Below National Average",
    emphasis_area == "Birth" & rate >= national_birth ~ "Above National Average")) |>
  mutate(comparison = case_when(
    state == "Washington" ~ "Washington State",
    state != "Washington" ~ comparison)) |>
  mutate(emphasis_area = factor(emphasis_area, levels = emphasis_order)) |>
  mutate(comparison = factor(comparison, levels = comparison_order))

# Run Modules Files ---------------------------------------------------------------------------
module_files <- list.files('modules', full.names = TRUE)
sapply(module_files, source)

# Page Information --------------------------------------------------------
page_text <- read_csv("data/page_text.csv", show_col_types = FALSE)

# Values for Drop Downs ---------------------------------------------------

emphasis_areas_list <- census_data |> filter(emphasis_area != "Total Population") |> arrange(emphasis_area) |> select("emphasis_area") |> distinct() |> pull()
transit_links <- c("Community Transit" = "https://www.communitytransit.org/",
                   "Everett Transit" = "https://everetttransit.org/",
                   "King County Metro" = "https://kingcounty.gov/en/dept/metro",
                   "Kitsap Transit" = "https://www.kitsaptransit.com/",
                   "Pierce Transit" = "https://www.piercetransit.org/",
                   "Pierce County Ferry" = "https://www.piercecountywa.gov/1793/Ferry",
                   "Sound Transit" = "https://www.soundtransit.org/",
                   "Washington State Ferries" = "https://wsdot.wa.gov/travel/washington-state-ferries",
                   "Transit Planning at PSRC" = "https://www.psrc.org/our-work/transit"
)

links_withtags <- withTags(
  map2(transit_links[1:8], names(transit_links)[1:8], 
       ~div(class = "links-container", tags$a(class = "links", href = .x, .y, tabindex="0", target = "_blank")))
)

psrc_mission <- "Our mission is to advance solutions to achieve a thriving, racially equitable, and sustainable central Puget Sound region through leadership, visionary planning, and collaboration."
