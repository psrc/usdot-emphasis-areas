# Libraries ---------------------------------------------------------------
library(dplyr)
library(stringr)
library(tidyr)
library(tidycensus)
library(sf)

# Variables ---------------------------------------------------------------
wgs84 <- 4326
spn <- 2285

acs_yr <- 2023
acs_type <- "acs5"

bar_items <- 25

marriage_tbl <- "S1201"
fertility_tbl <- "B13016"
age_tbl <- "B01001"

youth_variables <- c("B01001_003","B01001_004","B01001_005","B01001_006","B01001_027","B01001_028","B01001_029","B01001_030")
us_states_file <- "C:/Users/chelmann/Puget Sound Regional Council/2026-2050 RTP Trends - General/Federal/data/cb_2018_us_state_500k.shp"
us_territories <- c("60", "69", "66", "72", "78")
us_msa_file <- "C:/Users/chelmann/Puget Sound Regional Council/2026-2050 RTP Trends - General/Federal/data/tl_2024_us_cbsa.shp"

# Variable Labels ---------------------------------------------------------
acs_detailed_labels <- load_variables(year = acs_yr, dataset = acs_type, cache = TRUE) |> select(variable = "name", "label", "concept")
acs_subject_labels <- load_variables(year = acs_yr, dataset = paste0(acs_type,"/subject"), cache = TRUE) |> select(variable = "name", "label", "concept")

# National Youth, Fertility & Marriage Rates --------------------------
print(str_glue("Working on National Youth, Fertility & Marriage rates for {acs_yr}"))

d <- get_acs(geography = "us", variables =  "B01001_001", year = acs_yr, survey = acs_type)
d <- left_join(d, acs_detailed_labels, by=c("variable")) |>
  mutate(geography_type = "National", emphasis_area="Total Population") |>
  select(state="NAME", "geography_type", "emphasis_area", rate="estimate")
total_population <- d
rm(d)

d <- get_acs(geography = "us", table = age_tbl, year = acs_yr, survey = acs_type)
d <- left_join(d, acs_detailed_labels, by=c("variable"))
t1 <- d |> filter(variable %in% youth_variables) |> select(state="NAME", "estimate") |> group_by(state) |> summarise(youth = sum(estimate)) |> as_tibble()
t2 <- d |> filter(variable == "B01001_001") |> select(state="NAME", total="estimate")
youth_rate <- left_join(t1, t2, by="state") |> mutate(rate = (youth/total), geography_type = "National", emphasis_area="Youth") |> select("state", "geography_type", "emphasis_area", "rate") |> arrange(desc(rate))
rm(t1, t2, d)

d <- get_acs(geography = "us", table = marriage_tbl, year = acs_yr, survey = acs_type)
d <- left_join(d, acs_subject_labels, by=c("variable"))
marriage_rate <- d |> filter(variable == "S1201_C02_001") |> mutate(geography_type = "National", emphasis_area="Marriage", estimate = estimate/100) |> select(state="NAME", "geography_type", "emphasis_area", rate="estimate") |> arrange(desc(rate))
rm(d)

d <- get_acs(geography = "us", table = fertility_tbl, year = acs_yr, survey = acs_type)
d <- left_join(d, acs_detailed_labels, by=c("variable"))
t1 <- d |> filter(variable == "B13016_002") |> select(state="NAME", fertility="estimate")
t2 <- d |> filter(variable == "B13016_001") |> select(state="NAME", total="estimate")
fertility_rate <- left_join(t1, t2, by="state") |> mutate(rate = (fertility/total), geography_type = "National", emphasis_area="Fertility") |> select("state", "geography_type", "emphasis_area", "rate") |> arrange(desc(rate))
rm(t1, t2, d)

# MSA Youth, Fertility & Marriage Rates --------------------------
print(str_glue("Working on MSA Youth, Fertility & Marriage rates for {acs_yr}"))

d <- get_acs(geography = "cbsa", variables =  "B01001_001", year = acs_yr, survey = acs_type) |> filter(str_detect(NAME, "Metro Area"))
d <- left_join(d, acs_detailed_labels, by=c("variable")) |>
  mutate(geography_type = "Metro Region", emphasis_area="Total Population") |>
  select(state="NAME", "geography_type", "emphasis_area", rate="estimate")
total_population <- bind_rows(total_population, d)
rm(d)

d <- get_acs(geography = "cbsa", table = age_tbl, year = acs_yr, survey = acs_type) |> filter(str_detect(NAME, "Metro Area"))
d <- left_join(d, acs_detailed_labels, by=c("variable"))
t1 <- d |> filter(variable %in% youth_variables) |> select(state="NAME", "estimate") |> group_by(state) |> summarise(youth = sum(estimate)) |> as_tibble()
t2 <- d |> filter(variable == "B01001_001") |> select(state="NAME", total="estimate")
t3 <- left_join(t1, t2, by="state") |> mutate(rate = (youth/total), geography_type = "Metro Region", emphasis_area="Youth") |> select("state", "geography_type", "emphasis_area", "rate") |> arrange(desc(rate))
youth_rate <- bind_rows(youth_rate, t3)
rm(t1, t2, t3, d)

d <- get_acs(geography = "cbsa", table = marriage_tbl, year = acs_yr, survey = acs_type) |> filter(str_detect(NAME, "Metro Area"))
d <- left_join(d, acs_subject_labels, by=c("variable"))
d <- d |> filter(variable == "S1201_C02_001") |> mutate(geography_type = "Metro Region", emphasis_area="Marriage", estimate = estimate/100) |> select(state="NAME", "geography_type", "emphasis_area", rate="estimate") |> arrange(desc(rate))
marriage_rate <- bind_rows(marriage_rate, d)
rm(d)

d <- get_acs(geography = "cbsa", table = fertility_tbl, year = acs_yr, survey = acs_type) |> filter(str_detect(NAME, "Metro Area"))
d <- left_join(d, acs_detailed_labels, by=c("variable"))
t1 <- d |> filter(variable == "B13016_002") |> select(state="NAME", fertility="estimate")
t2 <- d |> filter(variable == "B13016_001") |> select(state="NAME", total="estimate")
t3 <- left_join(t1, t2, by="state") |> mutate(rate = (fertility/total), geography_type = "Metro Region", emphasis_area="Fertility") |> select("state", "geography_type", "emphasis_area", "rate") |> arrange(desc(rate))
fertility_rate <- bind_rows(fertility_rate, t3)
rm(t1, t2, t3, d)

# State Youth, Fertility & Marriage Rates --------------------------
print(str_glue("Working on Statewide Youth, Fertility & Marriage rates for {acs_yr}"))

d <- get_acs(geography = "state", variables =  "B01001_001", year = acs_yr, survey = acs_type)
d <- left_join(d, acs_detailed_labels, by=c("variable")) |>
  mutate(geography_type = "Statewide", emphasis_area="Total Population") |>
  select(state="NAME", "geography_type", "emphasis_area", rate="estimate")
total_population <- bind_rows(total_population, d)
rm(d)

d <- get_acs(geography = "state", table = age_tbl, year = acs_yr, survey = acs_type)
d <- left_join(d, acs_detailed_labels, by=c("variable"))
t1 <- d |> filter(variable %in% youth_variables) |> select(state="NAME", "estimate") |> group_by(state) |> summarise(youth = sum(estimate)) |> as_tibble()
t2 <- d |> filter(variable == "B01001_001") |> select(state="NAME", total="estimate")
t3 <- left_join(t1, t2, by="state") |> mutate(rate = (youth/total), geography_type = "Statewide", emphasis_area="Youth") |> select("state", "geography_type", "emphasis_area", "rate") |> arrange(desc(rate))
youth_rate <- bind_rows(youth_rate, t3)
rm(t1, t2, t3, d)

d <- get_acs(geography = "state", table = marriage_tbl, year = acs_yr, survey = acs_type)
d <- left_join(d, acs_subject_labels, by=c("variable"))
d <- d |> filter(variable == "S1201_C02_001") |> mutate(geography_type = "Statewide", emphasis_area="Marriage", estimate = estimate/100) |> select(state="NAME", "geography_type", "emphasis_area", rate="estimate") |> arrange(desc(rate))
marriage_rate <- bind_rows(marriage_rate, d)
rm(d)

d <- get_acs(geography = "state", table = fertility_tbl, year = acs_yr, survey = acs_type)
d <- left_join(d, acs_detailed_labels, by=c("variable"))
t1 <- d |> filter(variable == "B13016_002") |> select(state="NAME", fertility="estimate")
t2 <- d |> filter(variable == "B13016_001") |> select(state="NAME", total="estimate")
t3 <- left_join(t1, t2, by="state") |> mutate(rate = (fertility/total), geography_type = "Statewide", emphasis_area="Fertility") |> select("state", "geography_type", "emphasis_area", "rate") |> arrange(desc(rate))
fertility_rate <- bind_rows(fertility_rate, t3)
rm(t1, t2, t3, d)

# WA Congressional District Youth, Fertility & Marriage Rates --------------------------
print(str_glue("Working on WA Congressional District Youth, Fertility & Marriage rates for {acs_yr}"))

d <- get_acs(geography = "congressional district", state = "WA", table = age_tbl, year = acs_yr, survey = acs_type)
d <- left_join(d, acs_detailed_labels, by=c("variable"))
t1 <- d |> filter(variable %in% youth_variables) |> select(state="NAME", "estimate") |> group_by(state) |> summarise(youth = sum(estimate)) |> as_tibble()
t2 <- d |> filter(variable == "B01001_001") |> select(state="NAME", total="estimate")
t3 <- left_join(t1, t2, by="state") |> mutate(rate = (youth/total), geography_type = "WA Congressional District", emphasis_area="Youth") |> select("state", "geography_type", "emphasis_area", "rate") |> arrange(desc(rate))
youth_rate <- bind_rows(youth_rate, t3)
rm(t1, t2, t3, d)

d <- get_acs(geography = "congressional district", state = "WA", table = marriage_tbl, year = acs_yr, survey = acs_type)
d <- left_join(d, acs_subject_labels, by=c("variable"))
d <- d |> filter(variable == "S1201_C02_001") |> mutate(geography_type = "WA Congressional District", emphasis_area="Marriage", estimate = estimate/100) |> select(state="NAME", "geography_type", "emphasis_area", rate="estimate") |> arrange(desc(rate))
marriage_rate <- bind_rows(marriage_rate, d)
rm(d)

d <- get_acs(geography = "congressional district", state = "WA", table = fertility_tbl, year = acs_yr, survey = acs_type)
d <- left_join(d, acs_detailed_labels, by=c("variable"))
t1 <- d |> filter(variable == "B13016_002") |> select(state="NAME", fertility="estimate")
t2 <- d |> filter(variable == "B13016_001") |> select(state="NAME", total="estimate")
t3 <- left_join(t1, t2, by="state") |> mutate(rate = (fertility/total), geography_type = "WA Congressional District", emphasis_area="Fertility") |> select("state", "geography_type", "emphasis_area", "rate") |> arrange(desc(rate))
fertility_rate <- bind_rows(fertility_rate, t3)
rm(t1, t2, t3, d)

# WA County Youth, Fertility & Marriage Rates --------------------------
print(str_glue("Working on WA County Youth, Fertility & Marriage rates for {acs_yr}"))

d <- get_acs(geography = "county", state = "WA", table = age_tbl, year = acs_yr, survey = acs_type)
d <- left_join(d, acs_detailed_labels, by=c("variable"))
t1 <- d |> filter(variable %in% youth_variables) |> select(state="NAME", "estimate") |> group_by(state) |> summarise(youth = sum(estimate)) |> as_tibble()
t2 <- d |> filter(variable == "B01001_001") |> select(state="NAME", total="estimate")
t3 <- left_join(t1, t2, by="state") |> mutate(rate = (youth/total), geography_type = "WA County", emphasis_area="Youth") |> select("state", "geography_type", "emphasis_area", "rate") |> arrange(desc(rate))
youth_rate <- bind_rows(youth_rate, t3)
rm(t1, t2, t3, d)

d <- get_acs(geography = "county", state = "WA", table = marriage_tbl, year = acs_yr, survey = acs_type)
d <- left_join(d, acs_subject_labels, by=c("variable"))
d <- d |> filter(variable == "S1201_C02_001") |> mutate(geography_type = "WA County", emphasis_area="Marriage", estimate = estimate/100) |> select(state="NAME", "geography_type", "emphasis_area", rate="estimate") |> arrange(desc(rate))
marriage_rate <- bind_rows(marriage_rate, d)
rm(d)

d <- get_acs(geography = "county", state = "WA", table = fertility_tbl, year = acs_yr, survey = acs_type)
d <- left_join(d, acs_detailed_labels, by=c("variable"))
t1 <- d |> filter(variable == "B13016_002") |> select(state="NAME", fertility="estimate")
t2 <- d |> filter(variable == "B13016_001") |> select(state="NAME", total="estimate")
t3 <- left_join(t1, t2, by="state") |> mutate(rate = (fertility/total), geography_type = "WA County", emphasis_area="Fertility") |> select("state", "geography_type", "emphasis_area", "rate") |> arrange(desc(rate))
fertility_rate <- bind_rows(fertility_rate, t3)
rm(t1, t2, t3, d)

# WA Cities Youth, Fertility & Marriage Rates --------------------------
print(str_glue("Working on WA Cities Youth, Fertility & Marriage rates for {acs_yr}"))

d <- get_acs(geography = "place", state = "WA", table = age_tbl, year = acs_yr, survey = acs_type) |> filter(!(str_detect(NAME, "CDP")))
d <- left_join(d, acs_detailed_labels, by=c("variable"))
t1 <- d |> filter(variable %in% youth_variables) |> select(state="NAME", "estimate") |> group_by(state) |> summarise(youth = sum(estimate)) |> as_tibble()
t2 <- d |> filter(variable == "B01001_001") |> select(state="NAME", total="estimate")
t3 <- left_join(t1, t2, by="state") |> mutate(rate = (youth/total), geography_type = "WA Cities & Towns", emphasis_area="Youth") |> select("state", "geography_type", "emphasis_area", "rate") |> arrange(desc(rate))
youth_rate <- bind_rows(youth_rate, t3)
rm(t1, t2, t3, d)

d <- get_acs(geography = "place", state = "WA", table = marriage_tbl, year = acs_yr, survey = acs_type) |> filter(!(str_detect(NAME, "CDP")))
d <- left_join(d, acs_subject_labels, by=c("variable"))
d <- d |> filter(variable == "S1201_C02_001") |> mutate(geography_type = "WA Cities & Towns", emphasis_area="Marriage", estimate = estimate/100) |> select(state="NAME", "geography_type", "emphasis_area", rate="estimate") |> arrange(desc(rate))
marriage_rate <- bind_rows(marriage_rate, d)
rm(d)

d <- get_acs(geography = "county", state = "WA", table = fertility_tbl, year = acs_yr, survey = acs_type) |> filter(!(str_detect(NAME, "CDP")))
d <- left_join(d, acs_detailed_labels, by=c("variable"))
t1 <- d |> filter(variable == "B13016_002") |> select(state="NAME", fertility="estimate")
t2 <- d |> filter(variable == "B13016_001") |> select(state="NAME", total="estimate")
t3 <- left_join(t1, t2, by="state") |> mutate(rate = (fertility/total), geography_type = "WA Cities & Towns", emphasis_area="Fertility") |> select("state", "geography_type", "emphasis_area", "rate") |> arrange(desc(rate))
fertility_rate <- bind_rows(fertility_rate, t3)
rm(t1, t2, t3, d)

# WA Tracts Youth, Fertility & Marriage Rates --------------------------
print(str_glue("Working on WA Tracts Youth, Fertility & Marriage rates for {acs_yr}"))

d <- get_acs(geography = "tract", state = "WA", table = age_tbl, year = acs_yr, survey = acs_type)
d <- left_join(d, acs_detailed_labels, by=c("variable"))
t1 <- d |> filter(variable %in% youth_variables) |> select(state="NAME", "estimate") |> group_by(state) |> summarise(youth = sum(estimate)) |> as_tibble()
t2 <- d |> filter(variable == "B01001_001") |> select(state="NAME", total="estimate")
t3 <- left_join(t1, t2, by="state") |> mutate(rate = (youth/total), geography_type = "WA Census Tracts", emphasis_area="Youth") |> select("state", "geography_type", "emphasis_area", "rate") |> arrange(desc(rate))
youth_rate <- bind_rows(youth_rate, t3)
rm(t1, t2, t3, d)

d <- get_acs(geography = "tract", state = "WA", table = marriage_tbl, year = acs_yr, survey = acs_type)
d <- left_join(d, acs_subject_labels, by=c("variable"))
d <- d |> filter(variable == "S1201_C02_001") |> mutate(geography_type = "WA Census Tracts", emphasis_area="Marriage", estimate = estimate/100) |> select(state="NAME", "geography_type", "emphasis_area", rate="estimate") |> arrange(desc(rate))
marriage_rate <- bind_rows(marriage_rate, d)
rm(d)

d <- get_acs(geography = "tract", state = "WA", table = fertility_tbl, year = acs_yr, survey = acs_type)
d <- left_join(d, acs_detailed_labels, by=c("variable"))
t1 <- d |> filter(variable == "B13016_002") |> select(state="NAME", fertility="estimate")
t2 <- d |> filter(variable == "B13016_001") |> select(state="NAME", total="estimate")
t3 <- left_join(t1, t2, by="state") |> mutate(rate = (fertility/total), geography_type = "WA Census Tracts", emphasis_area="Fertility") |> select("state", "geography_type", "emphasis_area", "rate") |> arrange(desc(rate))
fertility_rate <- bind_rows(fertility_rate, t3)
rm(t1, t2, t3, d)

# Combined data ----------------------------------------------
usdot_emphasis_areas <- bind_rows(total_population, youth_rate, marriage_rate, fertility_rate) |> 
  mutate(year = acs_yr) |>
  mutate(emphasis_area = str_replace_all(emphasis_area, "Fertility", "Birth")) |>
  mutate(rate = replace_na(rate, 0))

rm(total_population, youth_rate, marriage_rate, fertility_rate)

# National ----------------------------------------------------------------
national <- usdot_emphasis_areas |> 
  filter(geography_type=="National")

saveRDS(national, "data/national.rds")

# US States --------------------------------------------------------------
us_states <- read_sf(us_states_file) |> 
  st_transform(crs = wgs84) |> 
  filter(!(GEOID %in% us_territories)) |> 
  select(fips="GEOID", state="NAME", abbv="STUSPS")

# Total Population
t <- usdot_emphasis_areas |> 
  filter(geography_type=="Statewide" & emphasis_area == "Total Population") |> 
  select("state", population="rate") |>
  slice_max(population, n=bar_items) |>
  select("state") |>
  pull()
  
m <- usdot_emphasis_areas |> 
  filter(geography_type=="Statewide" & emphasis_area == "Total Population") |> 
  select("state", population="rate") |> 
  mutate(population_comparison = case_when(
    state %in% t ~ "In largest areas",
    !(state %in% t) ~ "Not in largest areas"))

us_states <- left_join(us_states, m, by="state")

# Marriage
n <- usdot_emphasis_areas |> 
  filter(geography_type=="National" & emphasis_area == "Marriage") |> 
  select("rate") |> 
  pull()

m <- usdot_emphasis_areas |> 
  filter(geography_type=="Statewide" & emphasis_area == "Marriage") |> 
  select("state", marriage="rate") |> 
  mutate(marriage_comparison = case_when(
    marriage < n ~ "Below National Average",
    marriage >= n ~ "Above National Average"))

us_states <- left_join(us_states, m, by="state")

# Youth
n <- usdot_emphasis_areas |> 
  filter(geography_type=="National" & emphasis_area == "Youth") |> 
  select("rate") |> 
  pull()

m <- usdot_emphasis_areas |> 
  filter(geography_type=="Statewide" & emphasis_area == "Youth") |> 
  select("state", youth="rate") |> 
  mutate(youth_comparison = case_when(
    youth < n ~ "Below National Average",
    youth >= n ~ "Above National Average"))

us_states <- left_join(us_states, m, by="state")

# Birth
n <- usdot_emphasis_areas |> 
  filter(geography_type=="National" & emphasis_area == "Birth") |> 
  select("rate") |> 
  pull()

m <- usdot_emphasis_areas |> 
  filter(geography_type=="Statewide" & emphasis_area == "Birth") |> 
  select("state", birth="rate") |> 
  mutate(birth_comparison = case_when(
    birth < n ~ "Below National Average",
    birth >= n ~ "Above National Average"))

us_states <- left_join(us_states, m, by="state")

us_states <- us_states |>
  mutate(year = acs_yr) |>
  select("state", "population", "population_comparison", 
         "marriage", "marriage_comparison", 
         "youth", "youth_comparison",
         "birth", "birth_comparison",
         "year") |>
  arrange(state)
  
saveRDS(us_states, "data/us_states.rds")
rm(t, m, n)

# Shapefiles - MSA --------------------------------------------------------
us_msa <- read_sf(us_msa_file) |> 
  st_transform(crs = wgs84) |> 
  filter(LSAD == "M1" & !(str_detect(NAME, "PR"))) |> 
  select(fips="GEOID", state="NAMELSAD")

# Total Population
top_30 <- total_population |> 
  filter(geography_type=="Metro Region") |> 
  select("state", population="rate") |>
  slice_max(population, n=30) |>
  select("state") |>
  pull()

m <- total_population |> 
  filter(geography_type=="Metro Region") |> 
  select("state", population="rate") |> 
  mutate(population_comparison = case_when(
    state %in% top_30 ~ "In largest areas",
    !(state %in% top_30) ~ "Not in largest areas"))

us_msa <- left_join(us_msa, m, by="state")

# Marriage
n <- marriage_rate |> filter(geography_type=="National") |> select("rate") |> pull()
m <- marriage_rate |> 
  filter(geography_type=="Metro Region") |> 
  select("state", marriage="rate") |> 
  mutate(marriage_comparison = case_when(
    marriage < n ~ "Below National Average",
    marriage >= n ~ "Above National Average"))
us_msa <- left_join(us_msa, m, by="state")

# Youth
n <- youth_rate |> filter(geography_type=="National") |> select("rate") |> pull()
m <- youth_rate |> 
  filter(geography_type=="Metro Region") |> 
  select("state", youth="rate") |> 
  mutate(youth_comparison = case_when(
    youth < n ~ "Below National Average",
    youth >= n ~ "Above National Average"))
us_msa <- left_join(us_msa, m, by="state")

# Birth
n <- fertility_rate |> filter(geography_type=="National") |> select("rate") |> pull()
m <- fertility_rate |> 
  filter(geography_type=="Metro Region") |> 
  select("state", birth="rate") |> 
  mutate(birth_comparison = case_when(
    birth < n ~ "Below National Average",
    birth >= n ~ "Above National Average"))
us_msa <- left_join(us_msa, m, by="state")

us_msa <- us_msa |>
  slice_max(population, n=30)

saveRDS(us_msa, "data/msa_mapping_data.rds")

