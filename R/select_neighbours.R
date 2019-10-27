#' Select Subset of Rows Closest to a Specified Observation
#'
#' Function \code{\link{select_neighbours}} selects subset of rows from data set.
#' This is useful if data is large and we need just a sample to calculate profiles.
#'
#' Note that \code{select_neighbours()} function is S3 generic.
#' If you want to work on non standard data sources (like H2O ddf, external databases)
#' you should overload it.
#'
#' @param observation single observation
#' @param data set of observations
#' @param variables names of variables that shall be used for calculation of distance.
#' By default these are all variables present in \code{data} and \code{observation}
#' @param distance the distance function, by default the \code{gower_dist()} function.
#' @param n number of neighbours to select
#' @param frac if \code{n} is not specified (NULL), then will be calculated as \code{frac} * number of rows in \code{data}.
#' Either \code{n} or \code{frac} need to be specified.
#'
#' @return a data frame with selected rows
#'
#' @examples
#' library("DALEX")
#'
#' new_apartment <- apartments[1,]
#' small_apartments <- select_neighbours(new_apartment, apartments_test, n = 10)
#'
#' new_apartment
#' small_apartments
#'
#' @export
select_neighbours <- function(observation, data, variables = NULL, distance = gower::gower_dist, n = 20, frac = NULL) {
  UseMethod("select_neighbours")
}

#' @export
select_neighbours.default <- function(observation, data, variables = NULL, distance = gower::gower_dist, n = 20, frac = NULL) {
 if (is.null(variables)) {
   variables <- intersect(colnames(observation),
                          colnames(data))
 }
 if (is.null(n)) {
   n <- ceiling(nrow(data)*frac)
 }

  distances <- distance(observation[,variables, drop = FALSE],
                          data[,variables, drop = FALSE])
  selected_points <- head(order(distances), n)
  data[selected_points, ]
}

