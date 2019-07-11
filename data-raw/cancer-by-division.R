# cancer incidence by US Census division, 1999-2015

library(tidyverse)
library(here)

cancer_by_division <- 
  read_tsv(here("data-raw",
                "US cancer incidence, all sites, by year, division.txt"),
           n_max = 2907) %>% 
  select(Year, agegroup = `Age Groups Code`, Division, n = Count,
         pop = Population) %>%
  mutate_at(c("Division", "agegroup"), fct_inorder) %>% 
  mutate_at("Year", as.character)
levels(cancer_by_division$agegroup)[1:3] <- c("00", "01-04", "05-09")

test_fn <- cancer_by_division %>% 
  filter(Year == "2015") %>% 
  correlated_rates(Division, agegroup, n, pop,
                  std_pop_list$seer_pop, parent = "United States", 
                  dec_ratio = 4)



