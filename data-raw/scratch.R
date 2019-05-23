library(tidyverse)
library(here)
library(tidyepi)

seer_pop <- 
  c(3795, 15192, 19920, 20057, 19820, 18257, 17722, 19511, 22180, 
   22479, 19806, 17224, 13307, 10654,  9410,  8726,  7415,  4900, 4259)

cancer_us <- 
  read_tsv(here("data-raw", 
                "US cancer incidence, all sites, by year and agegroup.txt"),
           n_max = 323) %>% 
  select(Year, age_group = `Age Groups Code`, Count, Population) %>% 
  mutate_at("age_group", fct_inorder) %>% 
  mutate_at("age_group", fct_recode, `0` = "1") %>% 
  group_by(Year) %>% 
  do(direct_adjust(., age_group, Count, Population, seer_pop))

#  `1-4` = "01-4",`5-9` = "05-09") 

# by Census division
cancer_us <- 
# year 2015 for subregion method
  read_tsv(here("data-raw", 
                "US cancer incidence, all sites, by year, division.txt"),
           n_max = 2907) %>% 
  select(Year, Division, age_group = `Age Groups Code`, Count, Population) %>% 
  mutate_at("Year", as.character) %>% 
  mutate_at("age_group", fct_inorder) %>% 
  mutate_at("age_group", fct_recode, `0` = "1") 
  # group_by(Year, Division) %>% 
cancer_by_division <- cancer_us %>% 
  filter(Year == "2015") %>% 
  group_by(Division, age_group) %>% 
  summarize(Count = sum(Count),
            Pop = sum(Population)) %>% 
  mutate(Count_c = sum(Count) - Count,
         Pop_c = sum(Pop) - Pop,
         prop = Pop_c / (Pop + Pop_c)) 

  # adjusted rate each division

step2 <- step1 %>% 
  group_by(age_group) %>% 
  summarize(n = sum(Count),
            pop = sum(Pop)) %>% 
  direct_adjust(age_group, n, pop, seer_pop)

cancer_us <- 
  read_tsv(here("data-raw", 
                "US cancer incidence, all sites, by year, division.txt"),
           n_max = 2907) %>% 
  select(Year, Division, age_group = `Age Groups Code`, Count, Population) %>% 
  filter(Year == "2015") %>% 
  mutate_at("age_group", fct_inorder) %>% 
  mutate_at("age_group", fct_recode, `0` = "1") %>% 



