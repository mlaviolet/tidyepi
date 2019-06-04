#' Convert a number to a percentage.
#'
#' @param x A number.
#' @param decimals Number of decimal places in final result (default = 1).
#' @return The number expressed as a percentage.
#' @export
percent <- function(x, places = 1) round(100 * x, decimals)

#' Compute complete years between events.
#' @param begin_date Beginning date (usually birth).
#' @param end_date Ending date (usually death).
#' @return Number of anniversaries of begin_date (usually person's age).
#' @importFrom lubridate %--%
#' @importFrom lubridate years
#' @export
#' @examples
#' # Dave Brubeck
#' age_in_years("1920-12-06", "2012-12-05")
#' # John F. Kennedy
#' age_in_years("1917-05-29", "1963-11-22")
#' # Elvis Presley
#' age_in_years("1935-01-08", "1977-08-16")
#' # Groucho Marx
#' age_in_years("1890-10-02", "1977-08-19")

age_in_years <- function(begin_date, end_date) {
  (begin_date %--% end_date) %/% years(1)
  }