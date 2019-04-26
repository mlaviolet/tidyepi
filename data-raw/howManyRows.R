#' Report the number of rows in a data table.
#'
#' @param df A data frame or tibble.
#' @return The number of rows in the data frame or tibble.
#' @export
howManyRows <- function(df) {
  number_of_rows <- df %>%
    nrow()
  }
# need usethis::use_pipe()
