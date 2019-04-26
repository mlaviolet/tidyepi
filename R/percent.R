#' Convert a number to a percentage.
#'
#' @param x A number.
#' @param places Number of decimal places in final result (default = 1).
#' @return The number expressed as a percentage.
#' @export
percent <- function(x, places = 1) round(100 * x, places)



