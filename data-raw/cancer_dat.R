library(tidyverse)
library(here)

directAdjust <-
  function(df, n, pop, stdpop, base = 100000, places = 1, level = 95) {
    # Compute age-adjusted rates with confidence intervals following
    #   Tiwari et al. (2006) "Efficient interval estimation for age-adjusted
    #   cancer rates," Statistical Methods in Medical Research 15:547-569
    # Using gamma approximation as described in section 2.1
    # Arguments:
    #   df = data frame or tibble; next three arguments are columns in df
    #   n = number of events
    #   pop = number of person-years at risk
    #   stdpop = standard population, expressed as proportion
    #   base = multiplier, defaults to 100,000
    #   places = number of places to round rates
    #   level = confidence level expressed as percent
    # Value: # Data tibble containing
    #   events = total events across all age groups
    #   person_yrs = total person-years across all age groups
    #   adj_rate = age-adjusted rate
    #   adj_rate_se, adj_lci, adj_uci = standard error, lower confidence
    #     limit, upper confidence limit for age-adusted rate
    #   crude_rate = crude (unadjusted) rate
    #   crude_lci, crude_uci = lower and upper confidence limits for crude rate
    # Confidence limits on crude rates computed following
    #   Garwood (1936) "Fiducial limits for the Poisson distribution,"
    #   Biometrika 28:437-442.
    alpha_lci <- (100 - level) / 200 # 0.025 for 95% confidence
    alpha_uci <- (100 + level) / 200 # 0.975 for 95% confidence
    n <- enquo(n)
    pop <- enquo(pop)
    stdpop <- enquo(stdpop)
    df %>%
      summarize(events = sum(!!n),
                person_yrs = sum(!!pop),
                adj_rate = sum(!!stdpop * !!n / !!pop),
                var_adj_rate = sum((!!stdpop)^2 * (!!n) / (!!pop)^2),
                rate_j = sum(!!stdpop * (!!n + 1/n()) / !!pop),
                var_j = sum((!!stdpop)^2 * ((!!n) + 1/n()) / (!!pop)^2)) %>%
      mutate(crude_rate = events / person_yrs,
             crude_lci = qgamma(alpha_lci, events) / person_yrs,
             crude_uci = qgamma(alpha_uci, events + 1) / person_yrs,
             adj_rate_se = sqrt(var_adj_rate),
             adj_lci = qgamma(alpha_lci,
                              shape = adj_rate^2 / var_adj_rate,
                              scale = var_adj_rate / adj_rate),
             adj_uci = qgamma(alpha_uci,
                              shape = (rate_j^2) / var_j,
                              scale = var_j / rate_j)) %>%
      select(events, person_yrs, adj_rate, adj_rate_se, adj_lci, adj_uci,
             crude_rate, crude_lci, crude_uci) %>%
      mutate_at(vars(adj_rate:crude_uci),
                function(x) round(x * base, places))
  }


cancer <- read_tsv(here("data-raw",
                            "US cancer incidence, all sites.txt"),
                            n_max = 646) %>%
  select(Year, agegroup = `Age Groups Code`, Sex, n = Count,
         pop = Population) %>%
  mutate(Sex = factor(Sex, levels = c("Male", "Female")),
         agegroup = sub("^1$", "0", agegroup),
         agegroup = fct_inorder(agegroup)) %>%
  arrange(Year, Sex, agegroup)

seer_weight <- data.frame(
  agegroup = levels(cancer_dat$agegroup),
  std_pop =  c(3795, 15192, 19920, 20057, 19820, 18257, 17722, 19511,
              22180, 22479, 19806, 17224, 13307, 10654,  9410,  8726,
               7415,  4900, 4259)) %>%
  mutate(agegroup = fct_inorder(agegroup),
           wgt = std_pop / sum(std_pop))

# cancer <- cancer_dat %>%
#   inner_join(seer_weight, by = "agegroup")
#
# cancer_rate <- cancer %>%
#   group_by(Year) %>%
#   do(directAdjust(., n, pop, wgt))




