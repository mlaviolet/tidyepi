#' Age-adjusted rates for correlated variables
#'
#' @param df A data frame with columns for age group, event counts, and person-years totals as described in the next three arguments
#' @param region Subregion for which to calculate adjusted rates.
#' @param agegroup Age group or other stratifying variable.
#' @param events Number of events.
#' @param person_yrs Number of person-years at risk.
#' @param std_pop Vector of standard population distribution. Can be totals, proportions, or percentages.
#' @param parent Name of parent region.
#' @param base Multiplier; e.g. per 100,000 population.
#' @param level Confidence level expressed as percentage.
#' @param dec_rate Decimal places to round adjusted rates.
#' @param dec_ratio Decimal places to round rate ratios.
#' 
#' @return A data table with the following fields:
#' \describe{
#'   \item{\code{events}}{Number of events.}
#'   \item{\code{person_yrs}}{Total person-years at risk.}
#'   \item{\code{adj_rate}}{Age-adjusted rate.}
#'   \item{\code{adj_rate_stderr}}{Standard error of age-adjusted rate.}
#'   \item{\code{adj_lci}}{Lower confidence limit of age-adjusted rate per Tiwari (2006)}
#'   \item{\code{adj_uci}}{Upper confidence limit for age-adusted rate per Tiwari (2006)}
#'   }
#'
#' @references Tiwari RC et al. (2006) Efficient interval estimation for age-adjusted 
#' cancer rates. Statistical Methods in Medical Research 15:547-569. 
#' https://www.ncbi.nlm.nih.gov/pubmed/17260923
#' 
#' @references Tiwari RC et al. (2010) Interval estimation for ratios of correlated age-adjusted rates.
#' Journal of Data Science 8:471-482. https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3279758/
#' 
#' @importFrom dplyr bind_rows
#' @importFrom dplyr case_when
#' @importFrom dplyr contains
#' @importFrom dplyr do 
#' @importFrom dplyr ends_with
#' @importFrom dplyr everything
#' @importFrom dplyr group_by
#' @importFrom dplyr group_by_at
#' @importFrom dplyr inner_join
#' @importFrom dplyr matches
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_at
#' @importFrom dplyr pull
#' @importFrom dplyr rename
#' @importFrom dplyr rename_at
#' @importFrom dplyr select
#' @importFrom dplyr starts_with
#' @importFrom dplyr summarize
#' @importFrom dplyr vars
#' @importFrom purrr reduce
#' @export
#' 
#' @examples
#' # Age-adjusted COPD mortality rates by state with ratio to US rate and confidence intervals, 2016
#' library(tidyepi)
#' copd_rates <- correlated_rates(copd, State, agegroup, Deaths, Population, 
#'               std_pop = c(23961, 18136, 12315, 4259), parent = "United States") 
#' 
# test function call
# test_dat <- 
#   correlated_rates(asthma_dat, county, agegroup, n, pop,
#                    std_pop_list$dist_01, parent = "New Hampshire", 
#                    base = 10000)#' 
correlated_rates <- function(df, region, agegroup, events, person_yrs, std_pop,
                             parent = "Parent Region", base = 100000, 
                             level = 95, dec_rate = 1, dec_ratio = 2) {
  # normal percentile for confidence intervals
  z <- qnorm((100 + level) / 200)
  
  # save names of region, events, person_yrs for later use
  region_name <- quo_name(enquo(region))
  events_name <- quo_name(enquo(events))
  pop_name <- quo_name(enquo(person_yrs))
  
  # events, population, adjusted rate for parent region
  parent_region_dat <- df %>% 
    group_by({{ agegroup }}) %>%
    summarize(parent_n = sum({{ events }}), 
              parent_pop = sum({{ person_yrs }} ))
  
  # adjusted rate for parent region
  parent_region_rate <- parent_region_dat %>%
    direct_adjust(agegroup, parent_n, parent_pop, std_pop, decimals = 7,
                  base = base) %>%
    select(events, person_yrs, adj_rate, adj_lci, adj_uci) %>%
    mutate(region = parent) %>%
    select(region, everything()) %>% 
    rename_at(vars(1), ~ region_name)
  
  # pull vector of parent region adjusted rate
  parent_adj_rate <- parent_region_rate %>% 
    pull(adj_rate)
  
  # add counts, pop, within and outside subregion
  full_dat <- df %>%
    inner_join(parent_region_dat %>%
                 select(agegroup, parent_n, parent_pop),
               by = "agegroup") %>% 
    mutate(pop_c = parent_pop - !!sym(pop_name),
           n_c = parent_n - !!sym(events_name)) 

  # proportion of population outside subregion
  prop_outregions <- full_dat %>%
    group_by_at(vars(region_name)) %>%
    summarize(pop = sum(parent_pop), pop_c = sum(pop_c)) %>%
    mutate(prop_c = pop_c / pop) %>% 
    select(-matches("^pop"))

  # adjusted rate within subregions
  adj_rate_subregions <- full_dat %>%
    group_by_at(vars(region_name)) %>%
    do(direct_adjust(., agegroup, !!sym(events_name), !!sym(pop_name), std_pop, 
                     base = base, level = level, decimals = Inf)) %>% 
    mutate(adj_rate_var = adj_rate_stderr ^ 2) %>% 
    select(-adj_rate_stderr, -starts_with("crude"))
  
  # adjusted rate outside subregions
  adj_rate_outregions <- full_dat %>%
    group_by_at(vars(region_name)) %>%
    do(direct_adjust(., agegroup, n_c, pop_c, std_pop, 
                     base = base, level = level, decimals = Inf)) %>% 
    mutate(adj_rate_var = adj_rate_stderr ^ 2) %>% 
    select(-adj_rate_stderr, -starts_with("crude"))
  
  # for each region, compute ratio of its adjusted rate to that of the parent
  #   region with confidence intervals
  # append event count, population total, and adjusted rates with confidence
  #   intervals for parent region
  final_tbl <-
    list(adj_rate_subregions, adj_rate_outregions, prop_outregions) %>%
    reduce(inner_join, by = region_name) %>%
    mutate(adj_rate_parent = parent_adj_rate) %>% 
    mutate(ratio_rate = adj_rate.x / adj_rate_parent) %>% 
    mutate(moe = 
             z * (adj_rate.x / adj_rate_parent ^ 2) * 
             sqrt(prop_c * adj_rate.y ^ 2 * 
                    (adj_rate_var.x / adj_rate.x^2 + 
                       adj_rate_var.y / adj_rate.y ^2)),
           ratio_lci = max(ratio_rate - moe, 0), 
           ratio_uci = ratio_rate + moe) %>% 
    rename(events = events.x, person_yrs = person_yrs.x,
           adj_rate = adj_rate.x, adj_lci = adj_lci.x, adj_uci = adj_uci.x) %>% 
    select(-contains("_var"), -matches("\\.y$"), -moe, -prop_c,
           -adj_rate_parent) %>% 
    mutate(sig = case_when(ratio_lci > 1 ~ "Higher",
                           ratio_uci < 1 ~ "Lower",
                           TRUE ~ "Similar")) %>% 
    bind_rows(parent_region_rate) %>% 
    mutate_at(vars(starts_with("adj")), function(x) round(x, dec_rate)) %>% 
    mutate_at(vars(starts_with("ratio")), function(x) round(x, dec_ratio))
  }



# state_dat <- 
#   correlated_rates(copd_by_state, State, agegroup, Deaths, 
#                    Population, c(23961, 18136, 12315, 4259),
#                    parent = "United States") 


