#' Find means by groups.
#'
#' @param df A data frame.
#' @param group_var Grouping variable, as bare name.
#' @param data_var Analysis variable, as bare name.
#' @return Table of group means.
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @export
# this version uses bare name for data_var
tidy_mean <- function(df, group_var, data_var) {
  # make data_var into string for use in name of summary column
  mean_name <- paste0("mean_", quo_name(enquo(data_var)))
  df %>%
    group_by({{ group_var }}) %>%
    summarize(!!mean_name := mean({{ data_var }}))
  }

# tidy_mean(mtcars, cyl, mpg)



