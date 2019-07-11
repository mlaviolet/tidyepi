#' Data reshaping for indirect adjustment
#'
#' Reshape data to four columns for indirect adjustment.
#' @param df A data frame.
#' @param strata Variable that contains strata (such as age).
#' @param split_var Variable to split into study and referent groups.
#' @param study_group Value of split_var to use as study group.
#' @param n Number of events.
#' @param pop Number of person-years at risk.
#' @return A data table with the following fields:
#' \describe{
#'   \item{\code{strata}}{Grouping variable}
#'   \item{\code{study_count}}{Events in study group}
#'   \item{\code{study_pop}}{Person-years at risk in study group}
#'   \item{\code{ref_count}}{Events in referent group}
#'   \item{\code{ref_pop}}{Person-years at risk in referent group}}
#' @importFrom dplyr mutate
#' @importFrom tidyr gather
#' @importFrom tidyr unite
#' @importFrom dplyr select
#' @importFrom tidyr spread
#' @export
#'
reshape_for_SIR <- function(df, strata, split_var, study_group, n, pop) {
  strata <- enquo(strata)
  split_var <- enquo(split_var)
  n <- enquo(n)
  pop <- enquo(pop)
  df %>%
    # add indicator for referent group (1) and study group (0)
    mutate(group_id = if_else({{ split_var }} == study_group, 0, 1)) %>%
    # reshape into columns strata variable, study_count, study_pop,
    #   ref_count, ref_pop
    gather(key, value, {{ n }}, {{ pop }}) %>%
    unite("group_id", group_id, key) %>%
    select({{ strata }}, group_id, value)  %>%
    spread(group_id, value) %>%
    setNames(c(names(.)[1],
               "study_count", "study_pop", "ref_count", "ref_pop"))
  }

#' Indirect adjustment.
#'
#' Find standardized incidence ratios and standardized mortality ratios with
#' indirect adjustment.
#' @param df A data frame.
#' @param study_count Number of events in study group.
#' @param study_pop Number of person-years at risk in study group.
#' @param ref_count Number of events in referent group.
#' @param ref_pop Number of person-years at risk in referent group.
#' @param decimals Number of decimal places in expected counts, SIR's and confidence intervals.
#' @param level Confidence level as percentage.
#' @return A data table with the following fields:
#' \describe{
#'   \item{\code{observed}}{Number of events in study group}
#'   \item{\code{expected}}{Number of events expected in study group based on
#'   rate in referent group}
#'   \item{\code{sir}}{SIR: Standardized incidence (or mortality) ratio}
#'   \item{\code{sir_lci}}{Lower confidence limit of SIR using method of
#'   Garwood (1936)}
#'   \item{\code{sir_uci}}{Upper confidence limit of SIR by Garwood}}
#' @references Garwood F (1936) Fiducial limits for the Poisson distribution,
#'  Biometrika 28:437-442.
#' @importFrom dplyr everything
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_at
#' @importFrom dplyr select
#' @importFrom dplyr starts_with
#' @importFrom dplyr summarize
#' @importFrom dplyr vars
#' @export
#'
#' @examples
#' # standardized incidence ratio of US cancer incidence for males
#' #   compared to females
#' library(tidyepi)
#' library(dplyr)
#' sir_by_year <- cancer %>%
#'   group_by(Year) %>%
#'   do(reshape_for_SIR(., agegroup, Sex, "Male", n, pop)) %>%
#'   do(indirect_adjust(., study_count, study_pop, ref_count, ref_pop, decimals = 4))
indirect_adjust <- function(df, study_count, study_pop, ref_count, ref_pop,
                           level = 95, decimals = 2) {
  df %>%
    # compute observed and expected for each age group, then sum
    summarize(expected = 
                sum({{ study_pop }} * {{ ref_count }} / {{ ref_pop }}),
              observed = sum({{ study_count }})) %>%
    # compute SIR and LCI, UCI using method of Garwood (1936) -- this method
    #   is very conservative
    mutate(sir = observed / expected,
           sir_lci = qgamma((100 - level) / 200, observed) / expected,
           sir_uci = qgamma((100 + level) / 200, observed + 1) / expected) %>%
    mutate_at(vars(expected, starts_with("sir")), 
              function(x) round(x, decimals)) %>%
    select(observed, everything())
  }

# need usethis::use_tidy_eval()
