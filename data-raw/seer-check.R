library(here)
library(readr)
library(dplyr)
library(tidyepi)

std_pop <- dist_lst$seer_pop[13:17]

alameda <- read_tsv(here("data-raw", "alameda-crude.txt"), 
                    col_names = FALSE) %>% 
  filter(X1 %in% 13:17, X2 %in% 14:18) %>% 
  select(year = X1, agegroup = X2, n = X7, pop = X8) %>% 
  mutate(year = factor(year, labels = 2012:2016),
         agegroup = factor(agegroup, labels = 
                             c("65-69", "70-74", "75-79", "80-84", "85+")))

alameda %>% 
  group_by(year) %>% 
  do(direct_adjust(., agegroup, n, pop, std_pop))

  