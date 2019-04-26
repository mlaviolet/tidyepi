#' Find means by groups.
#'
#' @param df A data frame.
#' @param group_var Grouping variable, as bare name.
#' @param data_var Analysis variable, as a string.
#' @return Table of group means.
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @export
tidy_mean <- function(df, group_var, data_var) {
  group_var <- enquo(group_var)
  data_col <- sym(data_var)
  mean_name <- paste0("mean_", data_var)
  df %>%
    group_by(!!group_var) %>%
    summarize(!!mean_name := mean(!!data_col))
  }
