#' Crude or age-specific rates.
#' 
#' @param df A data frame.
#' @param events Number of events.
#' @param person_yrs Number of person-years at risk.
#' @param base Multiplier; e.g. per 100,000 population.
#' @param level Confidence level expressed as percentage.
#' @param decimals Decimal places to round results.
#'
#' @return A data table with the following fields:
#' \describe{
#'   \item{\code{events}}{Number of events.}
#'   \item{\code{person_yrs}}{Total person-years at risk.}
#'   \item{\code{rate}}{Crude (unadjusted) rate.}
#'   \item{\code{rate_stderr}}{Standard error of rate.}
#'   \item{\code{rate_lci}}{Lower confidence limit for rate.}
#'   \item{\code{rate_uci}}{Upper confidence limit for rate.}
#'   }
#'   
#' @references Garwood F (1936) Fiducial limits for the Poisson distribution,
#'  Biometrika 28:437-442.
#'  
#' @importFrom dplyr across
#' @importFrom dplyr mutate
#' @export
#' 
#' @note Confidence limits for crude rates are computed using the method of 
#' Garwood (1936).
#' 
#' @examples
#' # US cancer rates by year
#' library(dplyr)
#' cancer %>% 
#'   group_by(Year) %>% 
#'   summarize(across(c(n, pop), sum)) %>% 
#'   group_modify(~ crude_rate(.x, n, pop))
#' 
crude_rate <- function(df, events, person_yrs, base = 100000, level = 95,
                       decimals = 1) {
  alpha_lci <- (100 - level) / 200
  alpha_uci <- (100 + level) / 200
  df %>% 
    mutate(rate = {{ events }} / {{ person_yrs }}, 
           rate_stderr = sqrt({{ events }}) / {{ person_yrs }}, 
           rate_lci = qgamma(alpha_lci, {{ events }}) / {{ person_yrs }},
           rate_uci = 
             qgamma(alpha_uci, {{ events }} + 1) / {{ person_yrs }}) %>%
    mutate(across(starts_with("rate"), ~ round(base * .x, decimals)))
  }

# need usethis::use_tidy_eval()
