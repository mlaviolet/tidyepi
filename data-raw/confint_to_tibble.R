# https://stackoverflow.com/questions/29614849/dplyrmutate-to-add-multiple-values
# latest solution by RyanFrost

library(tidyverse)
library(broom)
library(exactci)
library(here)

df <- data.frame(id = c("A", "B", "C"),  x = c(0, 10, 10), 
                 n = c(100, 400, 400))
# GROUPING BY RECORD ID WOULD OCCUR OUTSIDE WORKING FUNCTION
# nest() and unnest() would probably not be needed
# key is using tidy() to put test results in data frame
df %>%
  group_by(id, x, n) %>%
  nest() %>%
  mutate(ptest = map(data, ~ tidy(poisson.exact(x, n)))) %>%
  unnest(ptest) %>% 
  select(x, n, estimate, conf.low, conf.high)

# with real data
df2 <- read_tsv(here("data-raw", "Poisson rates.txt"), n_max = 6) %>% 
  select(State = States, Count, Population, Crude_Rate = `Crude Rate`) 

df2 %>%
  group_by(State, Count, Population) %>%
  nest() %>%
  mutate(ptest = map(data, ~ tidy(poisson.exact(Count, Population)))) %>%
  unnest(ptest) %>% 
  select(Count, Population, estimate, conf.low, conf.high)
   

f2 <- function(df, Count, Population) {
  df %>% 
    group_by(Count, Population) %>%
    nest() %>%
    mutate(ptest = map(data, ~ tidy(poisson.exact(Count, Population)))) %>%
    unnest(ptest) %>% 
    select(Count, Population, estimate, conf.low, conf.high)
  }

df2 %>% 
  filter(State == "New Hampshire") %>% 
  f2(Count, Population)

df2 %>% 
  group_by(State) %>% 
  group_modify(~ f2(.x, Count, Population))




