library(dplyr)
library(tidyr)
# library(purrr)
data(cancer)
data(seer_weight)

level <- 95
sir <- cancer %>%
  filter(Year == 2015) %>%
  mutate(std_group = if_else(Sex == "Female", 1, 0)) %>%
  gather(key, value, n, pop) %>%
  # Female is referent
  unite("std_group", std_group, key) %>%
  select(agegroup, std_group, value)  %>%
  spread(std_group, value) %>%
  # compute observed and expected for each age group, then sum
  summarize(expected = sum(`1_n` / `1_pop` * `0_pop`),
            observed = sum(`0_n`)) %>%
  # compute SIR and LCI, UCI
  mutate(sir = observed / expected,
         sir_lci = qgamma((100 - level) / 200, observed) / expected,
         sir_uci = qgamma((100 + level) / 200, observed + 1) / expected)

f3 <- function(df, std_group, agegroup, n, pop, level = 95) {
  n <- enquo(n)
  pop <- enquo(pop)
  std_group <- enquo(std_group)
  agegroup <- enquo(agegroup)
  df %>%
    gather(key, value, !!n, !!pop) %>%
  unite("std_group", !!std_group, key) %>%
  select(!!agegroup, !!std_group, value)  %>%
  spread(!!std_group, value) %>%
  # compute observed and expected for each age group, then sum
  summarize(expected = sum(`1_n` / `1_pop` * `0_pop`),
            observed = sum(`0_n`)) %>%
  # compute SIR and LCI, UCI
  mutate(sir = observed / expected,
         sir_lci = qgamma((100 - level) / 200, observed) / expected,
         sir_uci = qgamma((100 + level) / 200, observed + 1) / expected) %>%
  select(observed, expected, sir, sir_lci, sir_uci)
  }

df <- cancer %>%
  filter(Year == 2015) %>%
  mutate(std_group = if_else(Sex == "Female", 1, 0))
# this is OK

(f3(df, std_group, agegroup, n, pop))

sirs <- cancer %>%
  group_by(Year) %>%
  mutate(std_group = if_else(Sex == "Female", 1, 0)) %>%
  do(f3(., std_group, agegroup, n, pop))



# Good to here ------------------------------------------------------------



df <- cancer %>%
  filter(Year == 2015) %>%
  mutate(std_group = if_else(Sex == "Female", 1, 0))
f2 <- function(df) {
  df %>%
    gather(key, value, n, pop) %>%
    unite("std_group", std_group, key) %>%
    select(agegroup, std_group, value)  %>%
    spread(std_group, value) %>%
    # compute observed and expected for each age group, then sum
    summarize(expected = sum(`1_n` / `1_pop` * `0_pop`),
              observed = sum(`0_n`)) %>%
    # compute SIR and LCI, UCI
    mutate(sir = observed / expected,
           sir_lci = qgamma((100 - level) / 200, observed) / expected,
           sir_uci = qgamma((100 + level) / 200, observed + 1) / expected)
  }

(f2(df))
sir <- cancer %>%
  group_by(Year) %>%
  mutate(std_group = if_else(Sex == "Female", 1, 0)) %>%
  do(f2(.))

f3 <- function(df, std_group, agegroup, n, pop) {
  n <- enquo(n)
  pop <- enquo(pop)
  std_group <- enquo(std_group)
  agegroup <- enquo(agegroup)
  df %>%
    gather(key, value, !!n, !!pop) #%>%
    # unite("std_group", !!std_group, key) %>%
    # select(!!agegroup, !!std_group, value)  %>%
    # spread(!!std_group, value) %>%
    # # compute observed and expected for each age group, then sum
    # summarize(expected = sum(`1_n` / `1_pop` * `0_pop`),
    #           observed = sum(`0_n`)) %>%
    # # compute SIR and LCI, UCI
    # mutate(sir = observed / expected,
    #        sir_lci = qgamma((100 - level) / 200, observed) / expected,
    #        sir_uci = qgamma((100 + level) / 200, observed + 1) / expected)
  }

(f3(df, std_group, agegroup, n, pop))
sirs <- cancer %>%
  mutate(std_group = if_else(Sex == "Female", 1, 0)) %>%
  group_by(Year) %>%
  do(f3(., std_group, agegroup, n, pop))


f1 <- function(df) {
  df %>%
    group_by(agegroup) %>%
    summarize(n = sum(n),
              pop = sum(pop))
  }

sir <- cancer %>%
  # filter(Year == 2015) %>%
  mutate(std_group = if_else(Sex == "Female", 1, 0)) %>%
  group_by(Year, std_group) %>%
  nest() %>%
  mutate(agg_data = map(.$data, f1)) %>%
  # merge with standard weights
  mutate(agg_data = map(.$agg_data, inner_join, seer_weight,
                        by = "agegroup")) %>%
  # OK to here
  mutate(adj_rate = map(.$agg_data, directAdjust(.$data, n, pop, wgt)))
  # call to directAdjust is the problem

sir[1,"agg_data"][[1]]

chk1 <- do()
# split(.$std_group)
# try list columns

sir[2, 2]$data %>%
  # group_by(.data$agegroup) %>%
  summarize(n = sum(n),
            pop = sum(pop))

library(dplyr)
data(cancer)
cancer %>%
  group_by(Year) %>%
  do(reshapeForSIR(., agegroup, Sex, "Female", n, pop)) %>%
  do(indirectAdjust(., study_count, study_pop, ref_count, ref_pop))

# test using example from epitools::ageadjust.indirect
dth60 <- c(141, 926, 1253, 1080, 1869, 4891, 14956, 30888,
           41725, 26501, 5928)

pop60 <- c(1784033, 7065148, 15658730, 10482916, 9939972,
           10563872, 9114202, 6850263, 4702482, 1874619, 330915)

dth40 <- c(45, 201, 320, 670, 1126, 3160, 9723, 17935,
           22179, 13461, 2238)

pop40 <- c(906897, 3794573, 10003544, 10629526, 9465330,
           8249558, 7294330, 5022499, 2920220, 1019504, 142532)

dth_dat <- data.frame(dth60, pop60, dth40, pop40)
dth_dat %>% indirectAdjust(dth40, pop40, dth60, pop60)
epitools::ageadjust.indirect(dth40, pop40, dth60, pop60)$sir

dth_dat %>%
  # compute observed and expected for each age group, then sum
  summarize(expected = sum(dth60 / pop60 * pop40),
            observed = sum(dth40)) %>%
  # compute SIR and LCI, UCI
  mutate(sir = observed / expected,
         sir_lci = qgamma((100 - level) / 200, observed) / expected,
         sir_uci = qgamma((100 + level) / 200, observed + 1) / expected)


# looks good

