#' Select Subset of Rows
#'
#' Function \code{\link{select_sample}} selects subset of rows from data set.
#' This is useful if data is large and we need just a sample to calculate profiles.
#'
#' Note that \code{select_subsample()} function is S3 generic.
#' If you want to work on non standard data sources (like H2O ddf, external databases)
#' you should overload it.
#'
#' @param data set of observations. Profile will be calculated for every observation (every row)
#' @param n number of observations to select.
#' @param seed seed for random number generator.
#'
#' @return a data frame with selected rows
#'
#' @examples
#' library("ingredients")
#'
#' small_apartments <- select_sample(DALEX::apartments_test)
#' head(small_apartments)
#'
#' @export
select_sample <- function(data, n = 100, seed = 1313) {
  UseMethod("select_sample")
}

#' @export
select_sample.default <- function(data, n = 100, seed = 1313) {
  set.seed(seed)
  ids <- sample.int(nrow(data), n, replace = TRUE)
  data[ids,,drop = FALSE]
}
