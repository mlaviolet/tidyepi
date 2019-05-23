#' SEER standard population weights.
#'
#' Dataset of standard population weights
#'
#' @docType data
#'
#' @usage data(seer_weight)
#'
#' @format A data frame with 19 rows and 3 variables:
#' \itemize{
#'   \item agegroup SEER age group 0, 1-4, 5-9, 10-14, ..., 75-84, 85+
#'   \item std_pop  2000 US standard population (thousands)
#'   \item wgt      2000 US standard population (proportions)
#'  }
#'
#' @keywords datasets
#'
#' @references Klein RJ, Schoenborn CA (2001) Age Adjustment Using
#' the 2000 Projected U.S. Population. Statistical Notes No. 20,
#' Centers for Disease Control and Prevention / National Center for Health
#' Statistics
#' \url{https://www.cdc.gov/nchs/data/statnt/statnt20.pdf}
"seer_weight"
