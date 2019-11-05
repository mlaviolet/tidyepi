# Mortality with COPD as first-listed cause, age 55+, 2016
#  ICD-10 codes J40-J44, age groups 55-64, 65-74, 75-84, 85+
# indicator defined as age 45+, but using 55+ due to suppression

library(tidyverse)
library(here)
library(tidyepi)

copd <- 
  read_tsv(here("data-raw", "COPD mortality by state, 2016.txt"), 
          n_max = 204) %>% 
  # read_tsv(file.path(data_dir, "COPD mortality by state, 2016.txt"), 
  #          n_max = 204) %>% 
  select(-c(Notes, `State Code`, `Age Group`, `Crude Rate`),
         agegroup = `Age Group Code`) %>% 
  mutate_at(c("State", "agegroup"), factor)

save(copd, file = here("data-raw", "copd.rda"))

# usethis::use_data(copd)

state_dat <-
  correlated_rates(copd, State, agegroup, Deaths,
                   Population, c(23961, 18136, 12315, 4259),
                   parent = "United States")
state_sort <- state_dat %>% 
  arrange(desc(adj_rate))

# 
# test_dat <- 
#   correlated_rates(asthma_dat, county, agegroup, n, pop,
#                    std_pop_list$dist_01, parent = "New Hampshire", 
#                    base = 10000)






# data_dir <- ("R:/OCPH/EPI/BHSDM/Group/Michael Laviolette/_Projects/tidyepi/data-raw")
# library(asthma_dat, package = "tidyepi")
# 
# state_dat %>% 
#   group_by(State) %>% 
#   summarize(pop = sum(parent_pop), pop_c = sum(pop_c)) %>% 
#   mutate(prop_c = pop_c / pop) %>% 
#   select(-matches("^pop"))


