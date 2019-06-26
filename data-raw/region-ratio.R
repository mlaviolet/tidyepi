library(tidyverse)
library(here)

step1 <- 
  read_tsv(
    here("data-raw", "US cancer incidence, all sites, by year, division.txt"),
    n_max = 2907) %>% 
  select(year = Year, division = Division,
         agegroup = `Age Groups Code`,n = Count, pop = Population) %>% 
  mutate_at("division", fct_inorder) %>% 
  mutate_at("agegroup", factor, labels = names(std_pop_list$seer_pop))
div_levels <- levels(step1$division)

# XLConnect::writeWorksheetToFile(here("data-raw", "regions.xlsx"),
#                                 data.frame(step1), "Sheet1")

# choose a year 
step2 <- step1 %>% 
  filter(year == "2015")

# count and population totals for parent region
step3 <- step2 %>% 
  group_by(agegroup) %>% 
  summarize(n = sum(n), pop = sum(pop)) 

# count and population totals for each subregion
step4 <- step2 %>% 
  group_by(division, agegroup) %>% 
  summarize(n = sum(n), pop = sum(pop)) 

# for each region find complementary count and population totals, also 
#   proportion outside region
step5 <- step3 %>% 
  inner_join(step4, by = "agegroup") %>% 
  mutate_at("division", factor, levels = div_levels) %>% 
  arrange(division, agegroup) %>% 
  mutate(n_c = n.x - n.y, pop_c = pop.x - pop.y,
         p_x = pop_c / pop.x) 

# adjusted rate each region
step6 <- step4 %>% 
  group_by(division) %>% 
  do(direct_adjust(., agegroup, n, pop, std_pop_list$seer_pop, decimals = 7)) 

# adjusted rate parent region 
adj_rate_parent_region <- direct_adjust(step3, agegroup, n, pop, 
                                        std_pop_list$seer_pop, decimals = 7) %>% 
  pull(adj_rate)

# LOOKS GOOD TO HERE


  