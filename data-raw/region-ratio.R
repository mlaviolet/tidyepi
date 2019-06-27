library(tidyverse)
library(here)
library(tidyepi)
library(DBI)
library(dbplyr)

# step1 <- 
#   read_tsv(
#     here("data-raw", "US cancer incidence, all sites, by year, division.txt"),
#     n_max = 2907) %>% 
#   select(year = Year, division = Division,
#          agegroup = `Age Groups Code`, n = Count, pop = Population) %>% 
#   mutate_at("division", fct_inorder) %>% 
#   mutate_at("agegroup", factor, labels = names(std_pop_list$seer_pop))
# div_levels <- levels(step1$division)

# XLConnect::writeWorksheetToFile(here("data-raw", "regions.xlsx"),
#                                 data.frame(step1), "Sheet1")

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

# population
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

# events, population, adjusted rate for parent region
asthma_dat_parent <- asthma_dat %>% 
  group_by(agegroup) %>% 
  summarize(n = sum(n), pop = sum(pop)) 

asthma_rate_parent <- asthma_dat_parent %>% 
  direct_adjust(agegroup, n, pop, std_pop_list$dist_01, base = 10000,
                decimals = 7) %>% 
  select(events, person_yrs, 
         adj_rate, adj_lci, adj_uci) %>% 
  mutate(county = "New Hampshire") %>% 
  select(county, everything())
         # crude_rate, crude_lci, crude_uci, 
         # rate_ratio, ratio_lci, ratio_uci)


# add counts, pop, within and outside subregion
asthma_dat <- asthma_dat %>% 
  inner_join(asthma_dat_parent, by = "agegroup") %>% 
  # group_by(agegroup) %>% 
  # summarize(n = sum(n), pop = sum(pop)) %>% 
  mutate(pop_c = pop.y - pop.x,
         n_c = n.y - n.x) %>% 
  select(county, agegroup, n = n.x, n_c, pop = pop.x, pop_c)

# proportion of population outside subregion
prop_outregions <- asthma_dat %>% 
  select(-matches("^n")) %>% 
  group_by(county) %>% 
  summarize(pop = sum(pop), pop_c = sum(pop_c)) %>% 
  mutate(prop_xc = pop_c / (pop_c + pop)) %>% 
  select(-matches("pop"))

# adjusted rate within subregions
adj_rate_subregions <- asthma_dat %>% 
  group_by(county) %>% 
  do(direct_adjust(., agegroup, n, pop, std_pop_list$dist_01,
                   decimals = 7, base = 10000)) %>% 
  mutate(adj_rate_var = adj_rate_stderr ^ 2) %>% 
  select(-adj_rate_stderr)

# adjusted rate outside subregions
adj_rate_outregions <- asthma_dat %>% 
  group_by(county) %>% 
  do(direct_adjust(., agegroup, n_c, pop_c, std_pop_list$dist_01,
                   decimals = 7, base = 10000)) %>% 
  select(county, adj_rate_c = adj_rate, adj_rate_stderr) %>% 
  mutate(adj_rate_var_c = adj_rate_stderr ^ 2) %>% 
  select(-adj_rate_stderr)

step6 <- list(adj_rate_subregions, adj_rate_outregions, prop_outregions) %>% 
  reduce(inner_join, by = "county") %>% 
  mutate(adj_rate_parent = pull(asthma_rate_parent, adj_rate)) %>% # enframe?
  mutate(rate_ratio = adj_rate / adj_rate_parent) %>% 
  mutate(moe = 
           1.96 * (adj_rate / adj_rate_parent ^ 2) * 
           sqrt(prop_xc * adj_rate_c ^ 2 * 
                  (adj_rate_var / adj_rate^2 + adj_rate_var_c / adj_rate_c ^2)),
         ratio_lci = max(rate_ratio - moe, 0), 
         ratio_uci = rate_ratio + moe) %>% 
  select(county, events, person_yrs, 
         adj_rate, adj_lci, adj_uci,
         # crude_rate, crude_lci, crude_uci, 
         rate_ratio, ratio_lci, ratio_uci) %>% 
  mutate(sig = case_when(ratio_lci > 1 ~ "Higher",
                         ratio_uci < 1 ~ "Lower",
                         TRUE ~ "Similar")) %>% 
  bind_rows(asthma_rate_parent)
  
# OK TO HERE --------------------------------------------------------------
# ADD SIGNIFICANCE FLAG

# # choose a year 
# step2 <- step1 %>% 
#   filter(year == "2015")
# 
# # count and population totals for parent region
# step3 <- step2 %>% 
#   group_by(agegroup) %>% 
#   summarize(n = sum(n), pop = sum(pop)) 
# 
# # count and population totals for each subregion
# step4 <- step2 %>% 
#   group_by(division, agegroup) %>% 
#   summarize(n = sum(n), pop = sum(pop)) 
# 
# # for each region find complementary count and population totals, also 
# #   proportion outside region
# step5 <- step3 %>% 
#   inner_join(step4, by = "agegroup") %>% 
#   mutate_at("division", factor, levels = div_levels) %>% 
#   arrange(division, agegroup) %>% 
#   mutate(n_c = n.x - n.y, pop_c = pop.x - pop.y)
# 
# # adjusted rates within subregions
# adj_rate_subregions <- step5 %>% 
#   group_by(division) %>% 
#   do(direct_adjust(., agegroup, n.y, pop.y, std_pop_list$seer_pop,
#                    decimals = 7)) %>% 
#   mutate(adj_rate_var = adj_rate_stderr ^ 2)
# # adjusted rates outside subregions
# adj_rate_outregions <- step5 %>% 
#   group_by(division) %>% 
#   do(direct_adjust(., agegroup, n_c, pop_c, std_pop_list$seer_pop,
#                    decimals = 7)) %>% 
#   mutate(adj_rate_var_c = adj_rate_stderr ^ 2) %>% 
#   select(division, adj_rate_c = adj_rate, adj_rate_var_c)
# 
# # proportion of population outside subregions
# prop_outregions <- step5 %>% 
#   group_by(division) %>% 
#   summarize(pop.x = sum(pop.x),
#             pop_c = sum(pop_c)) %>% 
#   mutate(p_xc = pop_c / pop.x) %>% 
#   select(division, p_xc)
# 
# # adjusted rate parent region 
# adj_rate_parent_region <- direct_adjust(step3, agegroup, n, pop, 
#                                         std_pop_list$seer_pop, decimals = 7) %>% 
#   pull(adj_rate)
# 
# step6 <- list(adj_rate_subregions, adj_rate_outregions, prop_outregions) %>% 
#   reduce(inner_join, by = "division") %>% 
#   mutate(adj_rate_parent = adj_rate_parent_region) %>% # enframe?
#   mutate(rate_ratio = adj_rate / adj_rate_parent) %>% 
#   mutate(moe = 
#            1.96 * (adj_rate / adj_rate_parent ^ 2) * 
#            sqrt(p_xc * adj_rate_c ^ 2 * 
#                   (adj_rate_var / adj_rate^2 + adj_rate_var_c / adj_rate_c ^2)),
#          ratio_lci = max(rate_ratio - moe, 0), 
#          ratio_uci = rate_ratio + moe)
# 
# names(adj_rate_outregions)
# names(adj_rate_subregions)
# 
# # adjusted rate each region
# 1 <- step5 %>% 
#   group_by(division) %>% 
#   do(direct_adjust(., agegroup, n.x, pop.x, std_pop_list$seer_pop,
#                    decimals = 7)) 
# # adjusted rate each region
# adj_rate_subregions <- step4 %>% 
#   group_by(division) %>% 
#   do(direct_adjust(., agegroup, n, pop, std_pop_list$seer_pop, decimals = 7)) 
# 
# step6 <- step5 %>% 
#   adj_rate_subregions %>% 
#   mutate(rate_ratio = adj_rate / adj_rate_parent_region )

  




  