#' Directly age-adjusted rates.
#' @description Compute age-adjusted rates using direct standardization.
#' @param df A data frame with columns for age group, event counts, and person-years totals as described in the next three arguments
#' @param agegroup Age group or other stratifying variable.
#' @param events Number of events.
#' @param person_yrs Number of person-years at risk.
#' @param std_pop Vector of standard population distribution. Can be totals, proportions, or percentages.
#' @param base Multiplier; e.g. per 100,000 population.
#' @param level Confidence level expressed as percentage.
#' @param decimals Decimal places to round results.
#'
#' @return A data table with the following fields:
#' \describe{
#'   \item{\code{events}}{Number of events.}
#'   \item{\code{person_yrs}}{Total person-years at risk.}
#'   \item{\code{adj_rate}}{Age-adjusted rate.}
#'   \item{\code{adj_rate_stderr}}{Standard error of age-adjusted rate.}
#'   \item{\code{adj_lci}}{Lower confidence limit of age-adjusted rate per Tiwari (2006)}
#'   \item{\code{adj_uci}}{Upper confidence limit for age-adusted rate per Tiwari.}
#'   \item{\code{crude_rate}}{Crude (unadjusted) rate.}
#'   \item{\code{crude_lci}}{Lower confidence limit for crude rate, per Garwood (1936).}
#'   \item{\code{crude_uci}}{Upper confidence limit for crude rate, per Garwood.}
#'   }
#'
#' @references Anderson RN and Rosenberg HM (1998) Age standardization of 
#' death rates: Implementation of the year 2000 standard. National Vital 
#' Statistics Reports 47(3). Hyattsville, Maryland: National Center 
#' for Health Statistics. https://www.cdc.gov/nchs/data/nvsr/nvsr47/nvs47_03.pdf
#' @references Garwood F (1936) Fiducial limits for the Poisson distribution,
#'  Biometrika 28:437-442.
#' @references Tiwari RC et al. (2006) Efficient interval estimation for age-adjusted 
#' cancer rates. Statistical Methods in Medical Research 15:547-569. 
#' https://www.ncbi.nlm.nih.gov/pubmed/17260923
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr inner_join
#' @importFrom dplyr matches
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_at
#' @importFrom dplyr pull
#' @importFrom dplyr select
#' @importFrom dplyr summarize
#' @importFrom dplyr vars
#' @importFrom tibble enframe
#' @export
#'
#' @note Confidence limits for adjusted rates are computed using the method of 
#' Tiwari et al. (2006). The upper limit is adjusted with a continuity 
#' correction prompted by the use of a continuous distribution (gamma) to 
#' approximate a discrete random variable (Poisson). 
#' 
#' Confidence limits for crude rates are copmuted using the method of 
#' Garwood (1936).
#' 
#' @examples
#' # US age-adjusted cancer rates by year and sex
#' # using standard SEER age groups 0, 1-4, 5-9, 10-14, 15-19, ..., 80-84,
#' library(dplyr)
#' cancer_by_year_sex <- cancer %>%
#'   group_by(Year, Sex) %>%
#'   group_modify(~ direct_adjust(.x, agegroup, n, pop, std_pop_list$seer_pop))
#'
#' # same rates by year
#' cancer_by_year <- cancer %>%
#'   group_by(Year, agegroup) %>%
#'   summarize(n = sum(n), pop = sum(pop)) %>% 
#'   group_modify(~ direct_adjust(.x, agegroup, n, pop, std_pop_list$seer_pop))
#'   
direct_adjust <- function(df, agegroup, events, person_yrs, std_pop,
                          base = 100000, level = 95, decimals = 4) {
  # store name of age group variable as a string--need to rename to "agegroup"
  #   for joining with standard weights
  J <- 1 / length(std_pop) # correction term for computing rates and variances
  alpha_lci <- (100 - level) / 200 # 2.5% lower limit for 95% interval
  alpha_uci <- (100 + level) / 200 
  # make data frame of age groups and standard population weights
  std_wgts <- std_pop %>%
    enframe(name = "agegroup", value = "wgt") %>% 
    mutate(wgt = wgt / sum(wgt))
  # rename input variables to match argument names
  df <- df %>% 
    rename_with( ~ "agegroup", {{ agegroup }}) %>% 
    rename_with(~ "events", {{ events }} ) %>% 
    rename_with(~ "person_yrs", {{ person_yrs }})
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
    summarize(across(c(events, person_yrs), sum)) %>% 
    mutate(crude_rate = events / person_yrs,
           crude_lci = qgamma(alpha_lci, events) / person_yrs,
           crude_uci = qgamma(alpha_uci, events + 1) / person_yrs)  
  bind_cols(adjusted_rate_tbl, crude_rate_tbl) %>% 
    select(-matches("(corr|var)$")) %>%
    mutate(across(matches("^(adj|crude)"), 
                  ~ round(base * .x, decimals))) %>%
    select(events, person_yrs, matches("^(adj|crude)"))
  }

# need usethis::use_tidy_eval()
