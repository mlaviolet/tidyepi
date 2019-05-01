#' Crude or age-specific rates
#' 
#' @param df A data frame.
#' @param events Number of events.
#' @param person_yrs Number of person-years at risk.
#' @param base Multiplier; e.g. per 100,000 population.
#' @param level Confidence level expressed as percentage.
#' @param places Decimal places to round results.
#'
#' @return A data table with the following fields:
#' \describe{
#'   \item{\code{events}}{Number of events.}
#'   \item{\code{person_yrs}}{Total person-years at risk.}
#'   \item{\code{rate}}{Crude (unadjusted) rate.}
#'   \item{\code{rate_lci}}{Lower confidence limit for rate.}
#'   \item{\code{rate_uci}}{Upper confidence limit for rate.}
#'   }
#'   
#' @references Garwood F (1936) Fiducial limits for the Poisson distribution,
#'  Biometrika 28:437-442.
#'  
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_at
#' @export
#' 
#' @examples
#' # US cancer rates by year
#' library(tidyepi)
#' library(dplyr)
#' cancer %>% 
#'   group_by(Year) %>% 
#'   summarize_at(c("n", "pop"), sum) %>% 
#'   do(crude_rate(., n, pop))
#' 
crude_rate <- function(df, events, person_yrs, base = 100000, level = 95,
                       places = 1) {
  events <- enquo(events)
  person_yrs <- enquo(person_yrs)
  alpha_lci <- (100 - level) / 200
  alpha_uci <- (100 + level) / 200
  df %>% 
    mutate(rate = !!events / !!person_yrs, 
           rate_lci = qgamma(alpha_lci, !!events) / !!person_yrs,
           rate_uci = qgamma(alpha_uci, !!(events) + 1) / !!person_yrs) %>%
    mutate_at(vars(starts_with("rate")),
              function(x) round(base * x, places))
  }

# need usethis::use_tidy_eval()
