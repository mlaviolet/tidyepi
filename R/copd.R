#' COPD mortality data.
#'
#' Dataset of mortality from chronic obstructive pulmonary disease (ICD-10 codes J40-J44) by year for ages 55 and older, state, and age group, United States 2016
#'
#' @docType data
#'
#' @usage data(copd)
#'
#' @format A data table with 204 rows and 4 variables.
#' \describe{
#'   \item{\code{State}}{The 50 states plus the District of Columbia}
#'   \item{\code{agegroup}}{Age group 0, 1-4, 5-9, 10-14, ... 75-84, 85+}
#'   \item{\code{Deaths}}{Number of deaths}
#'   \item{\code{Population}}{Estimated state population}
#'  }
#'
#' @references Centers for Disease Control and Prevention, National Center for Health Statistics. Compressed Mortality File 
#' http://wonder.cdc.gov/cmf-icd10.html
#'
#' @keywords datasets
#'
#' @source {CDC WONDER online database} \url{https://wonder.cdc.gov/}
"copd"
