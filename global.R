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
emphasis_areas <- c("Birth", "Marriage", "Youth")

# Data via RDS files ------------------------------------------------------
state_data <- readRDS("data/us_states.rds") 
congressional_data <- readRDS("data/wa_congressional.rds") 
county_data <- readRDS("data/wa_counties.rds") 
city_data <- readRDS("data/wa_cities.rds") 
tract_data <- readRDS("data/wa_tracts.rds")
msa_data <- readRDS("data/us_msa.rds")

national_data <- readRDS("data/national.rds")

national_youth <- national_data |> 
  filter(emphasis_area == "Youth") |> 
  select(rate) |> 
  pull()

national_birth <- national_data |> 
  filter(emphasis_area == "Birth") |> 
  select(rate) |> 
  pull()

national_marriage <- national_data |> 
  filter(emphasis_area == "Marriage") |> 
  select(rate) |> 
  pull()

census_yr <- unique(state_data$year)

source_info <- read_csv("data/source_information.csv", show_col_types = FALSE)

# Run Modules Files ---------------------------------------------------------------------------
module_files <- list.files('modules', full.names = TRUE)
sapply(module_files, source)

# Page Information --------------------------------------------------------
page_text <- read_csv("data/page_text.csv", show_col_types = FALSE)
psrc_mission <- "Our mission is to advance solutions to achieve a thriving, racially equitable, and sustainable central Puget Sound region through leadership, visionary planning, and collaboration."
