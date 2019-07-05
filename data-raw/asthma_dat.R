## code to prepare `asthma_dat` dataset goes here

library(tidyverse)
library(here)
library(DBI)
library(dbplyr)

# EDW connection
edwp <- dbConnect(odbc::odbc(), dsn = "edwp", dbname = "nhedwp", 
                  # uid = rstudioapi::showPrompt(title = "Username", 
                  # message = "Username", default = ""), 
                  uid = "michael.j.laviolette",
                  pwd = rstudioapi::askForPassword())

# geography
geo_tbl <- tbl(edwp, in_schema("WRQPRD", "GEOGRAPHY_TRANSFER")) %>% 
  filter(CENSUS_TOWN_CDE != "00000") %>% 
  select(GEO_CDE, COUNTY_CDE, COUNTY_NME) %>% 
  mutate(COUNTY_CDE = str_sub(COUNTY_CDE, 3)) 
county_names <- geo_tbl %>% 
  distinct(COUNTY_NME) %>% 
  arrange(COUNTY_NME) %>% 
  mutate(county = str_to_title(COUNTY_NME)) %>% 
  pull(county)

# Chronic Disease Indicators
# Indicator Group: Asthma
# Indicator 2.1: Emergency department visit rate for asthma

# population by county and age group
pop_dat <- 
  read_fwf(here("data-raw", "icen_2000_09_y09.zip"),
           fwf_cols(state = c(13, 14),
                    county = c(15, 17),
                    age = c(18, 19),
                    pop = c(22, 29))) %>% 
  filter(state == 33) %>% 
  mutate_at("county", function(x) factor(sprintf("%03d", x))) %>% 
  select(-state) %>% 
  group_by(county, age) %>% 
  summarize(pop = sum(pop)) %>% 
  mutate(agegroup = cut(age, c(0, 1, seq(5, 85, 10), Inf),
                        right = FALSE, include.lowest = TRUE)) %>% 
  group_by(county, agegroup) %>% 
  summarize(pop = sum(pop)) %>% 
  ungroup() %>% 
  mutate_at("county", factor, labels = county_names)

# ED discharges by county and age group for 2009--then join to population
asthma_dat <- tbl(edwp, 
                  in_schema("WRQPRD", "ED_STATISTICAL_ANALYSIS_MVW")) %>% 
  filter(DISCHARGE_YR == "2009", 
         str_sub(VR_GEOCODE, 1, 2) == "28",
         str_sub(PRINCIPAL_DIAGNOSIS_CDE, 1, 3) == "493") %>% 
  select(SYSTEM_ID, GEO_CDE = VR_GEOCODE, age = AGE_YRS) %>% 
  inner_join(geo_tbl, by = "GEO_CDE") %>% 
  collect() %>% 
  mutate(agegroup = cut(age, c(0, 1, seq(5, 85, 10), Inf),
                        right = FALSE, include.lowest = TRUE),
         county = factor(COUNTY_CDE, labels = county_names)) %>% 
  count(county, agegroup) %>% 
  complete(county, agegroup, fill = list(n = 0)) %>% 
  inner_join(pop_dat, by = c("county", "agegroup"))

save(asthma_dat, file = here("data-raw", "asthma.Rdata"))

usethis::use_data(asthma_dat)
