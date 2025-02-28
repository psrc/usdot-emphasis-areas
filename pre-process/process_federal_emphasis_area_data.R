# Libraries ---------------------------------------------------------------
library(dplyr)
library(readr)
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
wa_congressional_file <- "C:/Users/chelmann/Puget Sound Regional Council/2026-2050 RTP Trends - General/Federal/data/Washington_State_Congressional_Districts_2022.shp"
wa_county_file <- "C:/Users/chelmann/Puget Sound Regional Council/2026-2050 RTP Trends - General/Federal/data/WA_County_Boundaries.shp"
wa_city_file <- "C:/Users/chelmann/Puget Sound Regional Council/2026-2050 RTP Trends - General/Federal/data/WSDOT_-_City_Limits.shp"
wa_tract_file <- "C:/Users/chelmann/Puget Sound Regional Council/2026-2050 RTP Trends - General/Federal/data/OFM_SAEP_Census_Tracts.shp"
msa_comparison_file <- "C:/Users/chelmann/Puget Sound Regional Council/2026-2050 RTP Trends - General/Federal/data/msa_comparisons.csv"

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

d <- get_acs(geography = "congressional district", state = "WA", variables =  "B01001_001", year = acs_yr, survey = acs_type)
d <- left_join(d, acs_detailed_labels, by=c("variable")) |>
  mutate(geography_type = "WA Congressional District", emphasis_area="Total Population") |>
  select(state="NAME", "geography_type", "emphasis_area", rate="estimate") |>
  mutate(state = str_remove_all(state, " \\(118th Congress\\), Washington"))
total_population <- bind_rows(total_population, d)
rm(d)

d <- get_acs(geography = "congressional district", state = "WA", table = age_tbl, year = acs_yr, survey = acs_type)
d <- left_join(d, acs_detailed_labels, by=c("variable"))
t1 <- d |> 
  filter(variable %in% youth_variables) |> 
  select(state="NAME", "estimate") |> 
  group_by(state) |> 
  summarise(youth = sum(estimate)) |> 
  as_tibble()
t2 <- d |> 
  filter(variable == "B01001_001") |> 
  select(state="NAME", total="estimate")
t3 <- left_join(t1, t2, by="state") |> 
  mutate(rate = (youth/total), geography_type = "WA Congressional District", emphasis_area="Youth") |> 
  select("state", "geography_type", "emphasis_area", "rate") |> 
  mutate(state = str_remove_all(state, " \\(118th Congress\\), Washington")) |>
  arrange(desc(rate))
youth_rate <- bind_rows(youth_rate, t3)
rm(t1, t2, t3, d)

d <- get_acs(geography = "congressional district", state = "WA", table = marriage_tbl, year = acs_yr, survey = acs_type)
d <- left_join(d, acs_subject_labels, by=c("variable"))
d <- d |> 
  filter(variable == "S1201_C02_001") |> 
  mutate(geography_type = "WA Congressional District", emphasis_area="Marriage", estimate = estimate/100) |> 
  select(state="NAME", "geography_type", "emphasis_area", rate="estimate") |> 
  mutate(state = str_remove_all(state, " \\(118th Congress\\), Washington")) |>
  arrange(desc(rate))
marriage_rate <- bind_rows(marriage_rate, d)
rm(d)

d <- get_acs(geography = "congressional district", state = "WA", table = fertility_tbl, year = acs_yr, survey = acs_type)
d <- left_join(d, acs_detailed_labels, by=c("variable"))
t1 <- d |> 
  filter(variable == "B13016_002") |> 
  select(state="NAME", fertility="estimate")
t2 <- d |> 
  filter(variable == "B13016_001") |> 
  select(state="NAME", total="estimate")
t3 <- left_join(t1, t2, by="state") |> 
  mutate(rate = (fertility/total), geography_type = "WA Congressional District", emphasis_area="Fertility") |> 
  select("state", "geography_type", "emphasis_area", "rate") |> 
  mutate(state = str_remove_all(state, " \\(118th Congress\\), Washington")) |>
  arrange(desc(rate))
fertility_rate <- bind_rows(fertility_rate, t3)
rm(t1, t2, t3, d)

# WA County Youth, Fertility & Marriage Rates --------------------------
print(str_glue("Working on WA County Youth, Fertility & Marriage rates for {acs_yr}"))

d <- get_acs(geography = "county", state = "WA", variables =  "B01001_001", year = acs_yr, survey = acs_type)
d <- left_join(d, acs_detailed_labels, by=c("variable")) |>
  mutate(geography_type = "WA County", emphasis_area="Total Population") |>
  select(state="NAME", "geography_type", "emphasis_area", rate="estimate") |>
  mutate(state = str_remove_all(state, ", Washington"))
total_population <- bind_rows(total_population, d)
rm(d)

d <- get_acs(geography = "county", state = "WA", table = age_tbl, year = acs_yr, survey = acs_type)
d <- left_join(d, acs_detailed_labels, by=c("variable"))
t1 <- d |> 
  filter(variable %in% youth_variables) |> 
  select(state="NAME", "estimate") |> 
  group_by(state) |> 
  summarise(youth = sum(estimate)) |> 
  as_tibble()
t2 <- d |> 
  filter(variable == "B01001_001") |> 
  select(state="NAME", total="estimate")
t3 <- left_join(t1, t2, by="state") |> 
  mutate(rate = (youth/total), geography_type = "WA County", emphasis_area="Youth") |> 
  select("state", "geography_type", "emphasis_area", "rate") |> arrange(desc(rate)) |>
  mutate(state = str_remove_all(state, ", Washington"))
youth_rate <- bind_rows(youth_rate, t3)
rm(t1, t2, t3, d)

d <- get_acs(geography = "county", state = "WA", table = marriage_tbl, year = acs_yr, survey = acs_type)
d <- left_join(d, acs_subject_labels, by=c("variable"))
d <- d |> 
  filter(variable == "S1201_C02_001") |> 
  mutate(geography_type = "WA County", emphasis_area="Marriage", estimate = estimate/100) |> 
  select(state="NAME", "geography_type", "emphasis_area", rate="estimate") |> 
  mutate(state = str_remove_all(state, ", Washington")) |>
  arrange(desc(rate))
marriage_rate <- bind_rows(marriage_rate, d)
rm(d)

d <- get_acs(geography = "county", state = "WA", table = fertility_tbl, year = acs_yr, survey = acs_type)
d <- left_join(d, acs_detailed_labels, by=c("variable"))
t1 <- d |> 
  filter(variable == "B13016_002") |> 
  select(state="NAME", fertility="estimate")
t2 <- d |> 
  filter(variable == "B13016_001") |> 
  select(state="NAME", total="estimate")
t3 <- left_join(t1, t2, by="state") |> 
  mutate(rate = (fertility/total), geography_type = "WA County", emphasis_area="Fertility") |> 
  select("state", "geography_type", "emphasis_area", "rate") |> 
  mutate(state = str_remove_all(state, ", Washington")) |>
  arrange(desc(rate))
fertility_rate <- bind_rows(fertility_rate, t3)
rm(t1, t2, t3, d)

# WA Cities Youth, Fertility & Marriage Rates --------------------------
print(str_glue("Working on WA Cities Youth, Fertility & Marriage rates for {acs_yr}"))

d <- get_acs(geography = "place", state = "WA", variables =  "B01001_001", year = acs_yr, survey = acs_type) |> filter(!(str_detect(NAME, "CDP")))
d <- left_join(d, acs_detailed_labels, by=c("variable")) |>
  mutate(geography_type = "WA Cities & Towns", emphasis_area="Total Population") |>
  select(state="NAME", "geography_type", "emphasis_area", rate="estimate") |>
  mutate(state = str_remove_all(state, " city, Washington")) |>
  mutate(state = str_remove_all(state, " town, Washington")) 
total_population <- bind_rows(total_population, d)
rm(d)

d <- get_acs(geography = "place", state = "WA", table = age_tbl, year = acs_yr, survey = acs_type) |> filter(!(str_detect(NAME, "CDP")))
d <- left_join(d, acs_detailed_labels, by=c("variable"))
t1 <- d |> 
  filter(variable %in% youth_variables) |> 
  select(state="NAME", "estimate") |> 
  group_by(state) |> 
  summarise(youth = sum(estimate)) |> 
  as_tibble()
t2 <- d |> 
  filter(variable == "B01001_001") |> 
  select(state="NAME", total="estimate")
t3 <- left_join(t1, t2, by="state") |> 
  mutate(rate = (youth/total), geography_type = "WA Cities & Towns", emphasis_area="Youth") |> 
  select("state", "geography_type", "emphasis_area", "rate") |> 
  mutate(state = str_remove_all(state, " city, Washington")) |>
  mutate(state = str_remove_all(state, " town, Washington")) |>
  arrange(desc(rate))
youth_rate <- bind_rows(youth_rate, t3)
rm(t1, t2, t3, d)

d <- get_acs(geography = "place", state = "WA", table = marriage_tbl, year = acs_yr, survey = acs_type) |> filter(!(str_detect(NAME, "CDP")))
d <- left_join(d, acs_subject_labels, by=c("variable"))
d <- d |> 
  filter(variable == "S1201_C02_001") |> 
  mutate(geography_type = "WA Cities & Towns", emphasis_area="Marriage", estimate = estimate/100) |> 
  select(state="NAME", "geography_type", "emphasis_area", rate="estimate") |> 
  mutate(state = str_remove_all(state, " city, Washington")) |>
  mutate(state = str_remove_all(state, " town, Washington")) |>
  arrange(desc(rate))
marriage_rate <- bind_rows(marriage_rate, d)
rm(d)

d <- get_acs(geography = "place", state = "WA", table = fertility_tbl, year = acs_yr, survey = acs_type) |> filter(!(str_detect(NAME, "CDP")))
d <- left_join(d, acs_detailed_labels, by=c("variable"))
t1 <- d |> 
  filter(variable == "B13016_002") |> 
  select(state="NAME", fertility="estimate")
t2 <- d |> 
  filter(variable == "B13016_001") |> 
  select(state="NAME", total="estimate")
t3 <- left_join(t1, t2, by="state") |> 
  mutate(rate = (fertility/total), geography_type = "WA Cities & Towns", emphasis_area="Fertility") |> 
  select("state", "geography_type", "emphasis_area", "rate") |> 
  mutate(state = str_remove_all(state, " city, Washington")) |>
  mutate(state = str_remove_all(state, " town, Washington")) |>
  arrange(desc(rate))
fertility_rate <- bind_rows(fertility_rate, t3)
rm(t1, t2, t3, d)

# WA Tracts Youth, Fertility & Marriage Rates --------------------------
print(str_glue("Working on WA Tracts Youth, Fertility & Marriage rates for {acs_yr}"))

d <- get_acs(geography = "tract", state = "WA", variables =  "B01001_001", year = acs_yr, survey = acs_type)
d <- left_join(d, acs_detailed_labels, by=c("variable")) |>
  mutate(geography_type = "WA Census Tracts", emphasis_area="Total Population") |>
  select(state="NAME", "geography_type", "emphasis_area", rate="estimate") |>
  mutate(state = str_remove_all(state, " city, Washington")) |>
  mutate(state = str_remove_all(state, " town, Washington")) 
total_population <- bind_rows(total_population, d)
rm(d)

d <- get_acs(geography = "tract", state = "WA", table = age_tbl, year = acs_yr, survey = acs_type)
d <- left_join(d, acs_detailed_labels, by=c("variable"))
t1 <- d |> 
  filter(variable %in% youth_variables) |> 
  select(state="NAME", "estimate") |> 
  group_by(state) |> 
  summarise(youth = sum(estimate)) |> 
  as_tibble()
t2 <- d |> 
  filter(variable == "B01001_001") |> 
  select(state="NAME", total="estimate")
t3 <- left_join(t1, t2, by="state") |> 
  mutate(rate = (youth/total), geography_type = "WA Census Tracts", emphasis_area="Youth") |> 
  select("state", "geography_type", "emphasis_area", "rate") |> 
  arrange(desc(rate))
youth_rate <- bind_rows(youth_rate, t3)
rm(t1, t2, t3, d)

d <- get_acs(geography = "tract", state = "WA", table = marriage_tbl, year = acs_yr, survey = acs_type)
d <- left_join(d, acs_subject_labels, by=c("variable"))
d <- d |> 
  filter(variable == "S1201_C02_001") |> 
  mutate(geography_type = "WA Census Tracts", emphasis_area="Marriage", estimate = estimate/100) |> 
  select(state="NAME", "geography_type", "emphasis_area", rate="estimate") |> 
  arrange(desc(rate))
marriage_rate <- bind_rows(marriage_rate, d)
rm(d)

d <- get_acs(geography = "tract", state = "WA", table = fertility_tbl, year = acs_yr, survey = acs_type)
d <- left_join(d, acs_detailed_labels, by=c("variable"))
t1 <- d |> 
  filter(variable == "B13016_002") |> 
  select(state="NAME", fertility="estimate")
t2 <- d |> 
  filter(variable == "B13016_001") |> 
  select(state="NAME", total="estimate")
t3 <- left_join(t1, t2, by="state") |> 
  mutate(rate = (fertility/total), geography_type = "WA Census Tracts", emphasis_area="Fertility") |> 
  select("state", "geography_type", "emphasis_area", "rate") |> 
  arrange(desc(rate))
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

# Congressional Districts -------------------------------------------------
wa_congressional <- read_sf(wa_congressional_file) |> 
  st_transform(crs = wgs84) |> 
  mutate(state = paste0("Congressional District ", DISTRICTN)) |>
  select("state")

# Total Population
m <- usdot_emphasis_areas |> 
  filter(geography_type=="WA Congressional District" & emphasis_area == "Total Population") |> 
  select("state", population="rate") |> 
  mutate(population_comparison = "In largest areas")

wa_congressional <- left_join(wa_congressional, m, by="state")

# Marriage
n <- usdot_emphasis_areas |> 
  filter(geography_type=="National" & emphasis_area == "Marriage") |> 
  select("rate") |> 
  pull()

m <- usdot_emphasis_areas |> 
  filter(geography_type=="WA Congressional District" & emphasis_area == "Marriage") |> 
  select("state", marriage="rate") |> 
  mutate(marriage_comparison = case_when(
    marriage < n ~ "Below National Average",
    marriage >= n ~ "Above National Average"))

wa_congressional <- left_join(wa_congressional, m, by="state")

# Youth
n <- usdot_emphasis_areas |> 
  filter(geography_type=="National" & emphasis_area == "Youth") |> 
  select("rate") |> 
  pull()

m <- usdot_emphasis_areas |> 
  filter(geography_type=="WA Congressional District" & emphasis_area == "Youth") |> 
  select("state", youth="rate") |> 
  mutate(youth_comparison = case_when(
    youth < n ~ "Below National Average",
    youth >= n ~ "Above National Average"))

wa_congressional <- left_join(wa_congressional, m, by="state")

# Birth
n <- usdot_emphasis_areas |> 
  filter(geography_type=="National" & emphasis_area == "Birth") |> 
  select("rate") |> 
  pull()

m <- usdot_emphasis_areas |> 
  filter(geography_type=="WA Congressional District" & emphasis_area == "Birth") |> 
  select("state", birth="rate") |> 
  mutate(birth_comparison = case_when(
    birth < n ~ "Below National Average",
    birth >= n ~ "Above National Average"))

wa_congressional <- left_join(wa_congressional, m, by="state")

wa_congressional <- wa_congressional |>
  mutate(year = acs_yr) |>
  select("state", "population", "population_comparison", 
         "marriage", "marriage_comparison", 
         "youth", "youth_comparison",
         "birth", "birth_comparison",
         "year") |>
  arrange(state)

saveRDS(wa_congressional, "data/wa_congressional.rds")
rm(m, n)

# Washington Counties -----------------------------------------------------
wa_counties <- read_sf(wa_county_file) |> 
  st_transform(crs = wgs84) |> 
  select(state = "JURISDIC_3")

# Total Population
m <- usdot_emphasis_areas |> 
  filter(geography_type=="WA County" & emphasis_area == "Total Population") |> 
  select("state", population="rate") |> 
  mutate(population_comparison = "In largest areas")

wa_counties <- left_join(wa_counties, m, by="state")

# Marriage
n <- usdot_emphasis_areas |> 
  filter(geography_type=="National" & emphasis_area == "Marriage") |> 
  select("rate") |> 
  pull()

m <- usdot_emphasis_areas |> 
  filter(geography_type=="WA County" & emphasis_area == "Marriage") |> 
  select("state", marriage="rate") |> 
  mutate(marriage_comparison = case_when(
    marriage < n ~ "Below National Average",
    marriage >= n ~ "Above National Average"))

wa_counties <- left_join(wa_counties, m, by="state")

# Youth
n <- usdot_emphasis_areas |> 
  filter(geography_type=="National" & emphasis_area == "Youth") |> 
  select("rate") |> 
  pull()

m <- usdot_emphasis_areas |> 
  filter(geography_type=="WA County" & emphasis_area == "Youth") |> 
  select("state", youth="rate") |> 
  mutate(youth_comparison = case_when(
    youth < n ~ "Below National Average",
    youth >= n ~ "Above National Average"))

wa_counties <- left_join(wa_counties, m, by="state")

# Birth
n <- usdot_emphasis_areas |> 
  filter(geography_type=="National" & emphasis_area == "Birth") |> 
  select("rate") |> 
  pull()

m <- usdot_emphasis_areas |> 
  filter(geography_type=="WA County" & emphasis_area == "Birth") |> 
  select("state", birth="rate") |> 
  mutate(birth_comparison = case_when(
    birth < n ~ "Below National Average",
    birth >= n ~ "Above National Average"))

wa_counties <- left_join(wa_counties, m, by="state")

wa_counties <- wa_counties |>
  mutate(year = acs_yr) |>
  select("state", "population", "population_comparison", 
         "marriage", "marriage_comparison", 
         "youth", "youth_comparison",
         "birth", "birth_comparison",
         "year") |>
  arrange(state)

saveRDS(wa_counties, "data/wa_counties.rds")
rm(m, n)

# Washington Cities -----------------------------------------------------
wa_cities <- read_sf(wa_city_file) |> 
  st_transform(crs = wgs84) |> 
  filter(CountyFIPS %in% c("53033", "53035", "53053", "53061")) |>
  select(state = "CityName")

# Total Population
m <- usdot_emphasis_areas |> 
  filter(geography_type=="WA Cities & Towns" & emphasis_area == "Total Population") |> 
  select("state", population="rate") |> 
  mutate(population_comparison = "In largest areas")

wa_cities <- left_join(wa_cities, m, by="state")

# Marriage
n <- usdot_emphasis_areas |> 
  filter(geography_type=="National" & emphasis_area == "Marriage") |> 
  select("rate") |> 
  pull()

m <- usdot_emphasis_areas |> 
  filter(geography_type=="WA Cities & Towns" & emphasis_area == "Marriage") |> 
  select("state", marriage="rate") |> 
  mutate(marriage_comparison = case_when(
    marriage < n ~ "Below National Average",
    marriage >= n ~ "Above National Average"))

wa_cities <- left_join(wa_cities, m, by="state")

# Youth
n <- usdot_emphasis_areas |> 
  filter(geography_type=="National" & emphasis_area == "Youth") |> 
  select("rate") |> 
  pull()

m <- usdot_emphasis_areas |> 
  filter(geography_type=="WA Cities & Towns" & emphasis_area == "Youth") |> 
  select("state", youth="rate") |> 
  mutate(youth_comparison = case_when(
    youth < n ~ "Below National Average",
    youth >= n ~ "Above National Average"))

wa_cities <- left_join(wa_cities, m, by="state")

# Birth
n <- usdot_emphasis_areas |> 
  filter(geography_type=="National" & emphasis_area == "Birth") |> 
  select("rate") |> 
  pull()

m <- usdot_emphasis_areas |> 
  filter(geography_type=="WA Cities & Towns" & emphasis_area == "Birth") |> 
  select("state", birth="rate") |> 
  mutate(birth_comparison = case_when(
    birth < n ~ "Below National Average",
    birth >= n ~ "Above National Average"))

wa_cities <- left_join(wa_cities, m, by="state")

wa_cities <- wa_cities |>
  mutate(year = acs_yr) |>
  select("state", "population", "population_comparison", 
         "marriage", "marriage_comparison", 
         "youth", "youth_comparison",
         "birth", "birth_comparison",
         "year") |>
  arrange(state)

saveRDS(wa_cities, "data/wa_cities.rds")
rm(m, n)

# Washington Census Tracts -----------------------------------------------------
wa_tracts <- read_sf(wa_tract_file) |> 
  st_transform(crs = wgs84) |> 
  filter(COUNTYFP %in% c("033", "035", "053", "061")) |>
  mutate(state = paste0(NAMELSAD, " ", NAME, "; ", COUNTYNAME, " County; Washington")) |>
  select("state")

# Total Population
m <- usdot_emphasis_areas |> 
  filter(geography_type=="WA Census Tracts" & emphasis_area == "Total Population") |> 
  select("state", population="rate") |> 
  mutate(population_comparison = "In largest areas")

wa_tracts <- left_join(wa_tracts, m, by="state")

# Marriage
n <- usdot_emphasis_areas |> 
  filter(geography_type=="National" & emphasis_area == "Marriage") |> 
  select("rate") |> 
  pull()

m <- usdot_emphasis_areas |> 
  filter(geography_type=="WA Census Tracts" & emphasis_area == "Marriage") |> 
  select("state", marriage="rate") |> 
  mutate(marriage_comparison = case_when(
    marriage < n ~ "Below National Average",
    marriage >= n ~ "Above National Average"))

wa_tracts <- left_join(wa_tracts, m, by="state")

# Youth
n <- usdot_emphasis_areas |> 
  filter(geography_type=="National" & emphasis_area == "Youth") |> 
  select("rate") |> 
  pull()

m <- usdot_emphasis_areas |> 
  filter(geography_type=="WA Census Tracts" & emphasis_area == "Youth") |> 
  select("state", youth="rate") |> 
  mutate(youth_comparison = case_when(
    youth < n ~ "Below National Average",
    youth >= n ~ "Above National Average"))

wa_tracts <- left_join(wa_tracts, m, by="state")

# Birth
n <- usdot_emphasis_areas |> 
  filter(geography_type=="National" & emphasis_area == "Birth") |> 
  select("rate") |> 
  pull()

m <- usdot_emphasis_areas |> 
  filter(geography_type=="WA Census Tracts" & emphasis_area == "Birth") |> 
  select("state", birth="rate") |> 
  mutate(birth_comparison = case_when(
    birth < n ~ "Below National Average",
    birth >= n ~ "Above National Average"))

wa_tracts <- left_join(wa_tracts, m, by="state")

wa_tracts <- wa_tracts |>
  mutate(year = acs_yr) |>
  select("state", "population", "population_comparison", 
         "marriage", "marriage_comparison", 
         "youth", "youth_comparison",
         "birth", "birth_comparison",
         "year") |>
  arrange(state)

saveRDS(wa_tracts, "data/wa_tracts.rds")
rm(m, n)

# Metro Areas --------------------------------------------------------
msa_comparisons <- read_csv(msa_comparison_file, show_col_types = FALSE)
msa_ids <- msa_comparisons |> select("fips") |> pull()
msa_names <- msa_comparisons |> select("fips", "state", "name") |> mutate(fips = as.character(fips))

us_msa <- read_sf(us_msa_file) |> 
  st_transform(crs = wgs84) |> 
  select(fips="GEOID") |>
  filter(fips %in% msa_ids) 

us_msa <- left_join(us_msa, msa_names, by="fips")
  
# Total Population
m <- usdot_emphasis_areas |> 
  filter(geography_type=="Metro Region" & emphasis_area == "Total Population") |> 
  select("state", population="rate") |> 
  mutate(population_comparison = "In largest areas")

us_msa <- left_join(us_msa, m, by="state")

# Marriage
n <- usdot_emphasis_areas |> 
  filter(geography_type=="National" & emphasis_area == "Marriage") |> 
  select("rate") |> 
  pull()

m <- usdot_emphasis_areas |> 
  filter(geography_type=="Metro Region" & emphasis_area == "Marriage") |> 
  select("state", marriage="rate") |> 
  mutate(marriage_comparison = case_when(
    marriage < n ~ "Below National Average",
    marriage >= n ~ "Above National Average"))

us_msa <- left_join(us_msa, m, by="state")

# Youth
n <- usdot_emphasis_areas |> 
  filter(geography_type=="National" & emphasis_area == "Youth") |> 
  select("rate") |> 
  pull()

m <- usdot_emphasis_areas |> 
  filter(geography_type=="Metro Region" & emphasis_area == "Youth") |> 
  select("state", youth="rate") |> 
  mutate(youth_comparison = case_when(
    youth < n ~ "Below National Average",
    youth >= n ~ "Above National Average"))

us_msa <- left_join(us_msa, m, by="state")

# Birth
n <- usdot_emphasis_areas |> 
  filter(geography_type=="National" & emphasis_area == "Birth") |> 
  select("rate") |> 
  pull()

m <- usdot_emphasis_areas |> 
  filter(geography_type=="Metro Region" & emphasis_area == "Birth") |> 
  select("state", birth="rate") |> 
  mutate(birth_comparison = case_when(
    birth < n ~ "Below National Average",
    birth >= n ~ "Above National Average"))

us_msa <- left_join(us_msa, m, by="state")

us_msa <- us_msa |>
  mutate(year = acs_yr) |>
  select("name", "population", "population_comparison", 
         "marriage", "marriage_comparison", 
         "youth", "youth_comparison",
         "birth", "birth_comparison",
         "year") |>
  rename(state = "name") |>
  arrange(state)

saveRDS(us_msa, "data/us_msa.rds")
rm(m, n)

