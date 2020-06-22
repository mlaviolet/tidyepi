#' Standard population distributions.
#'
#' Standard 2000 populations for direct age adjustment, from National Center for Health Statistics
#' 
#' @docType data
#'
#' @usage data(std_pop_list)
#' 
#' @format A named list with 26 named vectors: master age grouping, SEER age grouping, five-year age groups, ten-year age groups, and distributions #1 through #22 as published by NCHS
#' 
#' @note Populations are in thousands.
#' 
#' @references Klein RJ, Schoenborn CA. Age adjustment using the 2000
#' projected U.S. population. Healthy People Statistical Notes, no. 20.
#' Hyattsville, Maryland: National Center for Health Statistics. January 2001.
#' https://www.cdc.gov/nchs/data/statnt/statnt20.pdf
"std_pop_list" 

#' Standard population distributions by single year of age.
#'
#' Standard 2000 populations for direct age adjustment, from National Center for Health Statistics
#' 
#' @docType data
#'
#' @usage data(std_pop_single_age)
#' 
#' @format A data frame with two columns:
#'  \describe{
#'   \item{\code{age}}{Year of age from 0 to 100+}
#'   \item{\code{population}}{Standard population in thousands.}
#'   }
#' 
#' @references US Bureau of the Census. Population projections of the United States by age, sex, race, and Hispanic origin: 1995 to 2050 (P25-1130). February 1996, pp. 52-53.
#' 
#' @references National Cancer Institute, Surveillance, Epidemiology, and End Results Progam. Standard populations (millions) for age-adjustment.
#' https://seer.cancer.gov/stdpopulations
#' 
#' @keywords datasets
#' 
#' @source {US Bureau of the Census} \url{https://www.census.gov/prod/1/pop/p25-1130/p251130.pdf}
"std_pop_single_age"
