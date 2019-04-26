#' Find directly age-adjusted rates.
#'
#' @param df A data frame.
#' @param n Number of events.
#' @param pop Number of person-years at risk.
#' @param stdpop Standard population. Can be totals, proportions, or percentages.
#' @param base Multiplier; e.g. per 100,000 population.
#' @param places Number of decimal places to round rates.
#' @param level Confidence level expressed as percentage.
#' @return A data table with the following fields:
#' \describe{
#'   \item{\code{events}}{ Number of events.}
#'   \item{\code{person_yrs}}{Total person-years at risk.}
#'   \item{\code{adj_rate}}{Age-adjusted rate.}
#'   \item{\code{adj_rate_se}}{Standard error of age-adjusted rate.}
#'   \item{\code{adj_lci}}{Lower confidence limit of age-adjusted rate per Tiwari (2006)}
#'   \item{\code{adj_uci}}{Upper confidence limit for age-adusted rate per Tiwari (2006)}
#'   \item{\code{crude_rate}}{Crude (unadjusted) rate.}
#'   \item{\code{crude_lci}}{Lower confidence limit for crude rate.}
#'   \item{\code{crude_uci}}{Upper confidence limit for crude rate.}
#'  }
#' @references Tiwari RC et al. (2006) Efficient interval estimation for age-
#' adjusted cancer rates, Statistical Methods in Medical Research 15:547-569.
#' @references Garwood F (1936) Fiducial limits for the Poisson distribution,
#'  Biometrika 28:437-442.
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr mutate_at
#' @importFrom dplyr select
#' @export
#' @examples
#' # age-adjusted cancer rates by year
#' library(tidyepi)
#' library(dplyr)
#' by_year <- cancer %>%
#'   group_by(Year, agegroup) %>%
#'   summarize(n = sum(n), pop = sum(pop)) %>%
#'   inner_join(seer_weight, by = "agegroup") %>%
#'   do(directAdjust(., n, pop, std_pop))
#'
directAdjust <-
  function(df, n, pop, stdpop, base = 100000, places = 1, level = 95) {
    alpha_lci <- (100 - level) / 200 # 0.025 for 95% confidence
    alpha_uci <- (100 + level) / 200 # 0.975 for 95% confidence
    n <- enquo(n)
    pop <- enquo(pop)
    stdpop <- enquo(stdpop)
    df %>%
      # normalize standard population
      mutate(stdpop = !!stdpop / sum(!!stdpop)) %>%
      # compute adjusted rate with variances using method of Tiwari et al. 2006
      summarize(events = sum(!!n),
                person_yrs = sum(!!pop),
                adj_rate = sum(!!stdpop * !!n / !!pop),
                var_adj_rate = sum((!!stdpop)^2 * (!!n) / (!!pop)^2),
                rate_j = sum(!!stdpop * (!!n + 1/n()) / !!pop),
                var_j = sum((!!stdpop)^2 * ((!!n) + 1/n()) / (!!pop)^2)) %>%
      # compute crude rates with CI's, standard error and CI's for adjusted
      #   rates
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
      # select output columns and do rounding
      select(events, person_yrs, adj_rate, adj_rate_se, adj_lci, adj_uci,
             crude_rate, crude_lci, crude_uci) %>%
      mutate_at(vars(adj_rate:crude_uci),
                function(x) round(x * base, places))
  }
# need usethis::use_tidy_eval()

