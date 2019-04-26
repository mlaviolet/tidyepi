#' Cancer indidence data.
#'
#' Dataset of cancer incidence by year, sex and age group, United States 1999-2015
#'
#' @docType data
#'
#' @usage data(cancer)
#'
#' @format A data table with 646 rows and 5 variables.
#' \describe{
#'   \item{\code{Year}}{Calendar year}
#'   \item{\code{agegroup}}{SEER age group 0, 1-4, 5-9, 10-14, ... 75-84, 85+}
#'   \item{\code{Sex}}{Male or Female}
#'   \item{\code{n}}{Number of newly diagnosed cases}
#'   \item{\code{pop}}{Census estimated population}
#'  }
#'
#' @references United States Cancer Statistics (1999-2015) Incidence, WONDER
#'  Online Database. United States Department of Health and Human Services,
#'  Centers for Disease Control and Prevention and National Cancer Institute.
#'  http://wonder.cdc.gov/cancer-v2015.html
#'
#' @keywords datasets
#'
#' @source {CDC WONDER online database} \url{https://wonder.cdc.gov/}
"cancer"
