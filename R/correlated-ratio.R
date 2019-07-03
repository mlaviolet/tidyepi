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
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @export
#' 
correlated_rates <- function(df, region, agegroup, events, person_yrs, std_pop,
                             parent = "Parent Region", base = 100000, 
                             level = 95, dec_rate = 1, dec_ratio = 2) {
  # events, population, adjusted rate for parent region
  parent_region_dat <- df %>% 
    group_by({{ agegroup }}) %>%
    summarize(n = sum({{ events }}), pop = sum({{ person_yrs }} ))
  # adjusted rate for parent region
  parent_region_rate <- parent_region_dat %>% 
    direct_adjust(agegroup, n, pop, std_pop, decimals = 7, 
                  base = base) %>% 
    select(events, person_yrs, adj_rate, adj_lci, adj_uci) %>% 
    mutate(region = parent) %>% 
    select(region, everything())
  # add counts, pop, within and outside subregion
  full_dat <- df %>% 
    inner_join(parent_region_dat %>% 
                 select(agegroup, n, pop), 
               by = "agegroup") %>% 
    mutate(pop_c = pop.y - pop.x,
           n_c = n.y - n.x) %>% 
    select(-c(n.y, pop.y)) %>% 
    rename(n = n.x, pop = pop.x)
  }

