# http://ocw.jhsph.edu/courses/FundEpi/PDFs/Lecture7.pdf

library(tidyepi)
library(dplyr)
  
std_pop <- 1000 * c(21151, 28473, 19298, 5864, 2530) %>% 
  set_names(c("15-24", "25-44", "45-64", "65-74", "75+"))

mortality <- tribble(~pop, ~deaths,
                     17724, 39745,
                      5390, 26372,
                      1210, 37125,
                       364, 33679,
                       199, 64386) %>% 
  mutate_at("pop", function(x) 1000 * x) %>% 
  mutate(agegroup = names(std_pop)) %>% 
  select(agegroup, everything())
  
# std_wgts <- std_pop %>%
#   enframe(name = "agegroup", value = "wgt") %>% 
#   mutate_at("wgt", function(x) x / sum(x))

test_adjust <- function(df, agegroup, events, person_yrs, std_pop,
                          base = 100000, level = 95, decimals = 1) {
  # store name of age group variable as a string--need to rename to "agegroup"
  #   for joining with standard weights
  J <- 1 / length(std_pop) # correction term for computing rates and variances
  alpha_lci <- (100 - level) / 200 # 2.5% lower limit for 95% interval
  alpha_uci <- (100 + level) / 200 
  # make data frame of age groups and standard population weights
  std_wgts <- std_pop %>%
    enframe(name = "agegroup", value = "wgt") %>% 
    mutate_at("wgt", function(x) x / sum(x))
  df <- df %>% 
    rename_at(vars({{ agegroup }}), ~ "agegroup") %>% 
    rename_at(vars({{  events }}), ~ "events") %>% 
    rename_at(vars({{  person_yrs }}), ~ "person_yrs")
  adjusted_rate_tbl <- df %>% 
    inner_join(std_wgts, by = "agegroup") %>% 
    summarize(
      adj_rate = sum(wgt * events / person_yrs),
      adj_rate_var = sum(wgt^2 * events / person_yrs^2),
      adj_rate_corr = sum(wgt * (events + J) / person_yrs),
      adj_rate_var_corr = sum(wgt^2 * (events + J) / person_yrs^2)) %>% 
    mutate(adj_rate_stderr = sqrt(adj_rate_var),
           adj_lci = qgamma(alpha_lci,
                            shape = adj_rate^2 / adj_rate_var,
                            scale = adj_rate_var / adj_rate),
           adj_uci = qgamma(alpha_uci,
                            shape = (adj_rate_corr^2) / adj_rate_var_corr,
                            scale = adj_rate_var_corr / adj_rate_corr))
  # TEST HERE
  crude_rate_tbl <- df %>% 
    summarize(events = sum(events),
              person_yrs = sum(person_yrs)) %>% 
    mutate(crude_rate = events / person_yrs,
           crude_lci = qgamma(alpha_lci, events) / person_yrs,
           crude_uci = qgamma(alpha_uci, events + 1) / person_yrs)  
  bind_cols(adjusted_rate_tbl, crude_rate_tbl) %>% 
    select(-matches("(corr|var)$")) %>%
    mutate_at(vars(matches("^(adj|crude)")),
              function(x) round(base * x, decimals)) %>%
    select(events, person_yrs, matches("^(adj|crude)"))
  }

mortality %>% 
  rename(age_group = agegroup) %>% 
  test_adjust(age_group, deaths, pop, std_pop, base = 1000, decimals = Inf) %>% 
  print()
  # mutate_all(function(x) 1000 * x) %>% 

cancer %>%
  group_by(Year, Sex) %>%
  group_modify(~ test_adjust(.x, agegroup, n, pop, std_pop_list$seer_pop))

cancer %>% 
  rename(events = n, person_yrs = pop) %>%
  group_by(Year, Sex) %>%
  group_modify(~ test_adjust(.x, agegroup, events, person_yrs, 
                             std_pop_list$seer_pop))


mortality %>% 
  summarize(events = sum(deaths),
            person_yrs = sum(pop))

