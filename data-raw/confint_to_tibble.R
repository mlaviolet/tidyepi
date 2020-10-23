# https://stackoverflow.com/questions/29614849/dplyrmutate-to-add-multiple-values
# latest solution by RyanFrost

library(tidyverse)
library(broom)
library(exactci)
library(here)

f2 <- function(df, x, n, multiplier = 100000) {
  df %>% 
    # rename data variables to match arguments
    rename_with(~ "x", {{ x }}) %>% 
    rename_with(~ "n", {{ n }}) %>% 
    group_by(x, n) %>%
    nest() %>%
    mutate(ptest = map(data, ~ tidy(poisson.exact(x, n)))) %>%
    unnest(ptest) %>% 
    select(Count = x, Population = n, rate = estimate, rate_low = conf.low, 
           rate_high = conf.high) %>% 
    mutate(across(starts_with("rate"), ~ multiplier * .x))
  }

df <- data.frame(id = c("A", "B", "C"),  x = c(0, 10, 10),
                 n = c(100, 400, 400))
df %>% 
  group_by(id) %>% 
  group_modify(~ f2(.x, x, n, 1000))

# with real data
df2 <- read_tsv(here("data-raw", "Poisson rates.txt"), n_max = 6) %>% 
  select(State = States, Count, Population, Crude_Rate = `Crude Rate`) 

df2 %>% 
  filter(State == "New Hampshire") %>% 
  f2(Count, Population)

df2 %>% 
  group_by(State) %>% 
  group_modify(~ f2(.x, Count, Population))




