#' Prints Aggregated Profiles
#'
#' @param x an individual variable profile explainer produced with the `aggregate_profiles()` function
#' @param ... other arguments that will be passed to `head()`
#'
#' @export
#'
#' @examples
#' library("DALEX2")
#'  \dontrun{
#' }
print.aggregated_ceteris_paribus_explainer <- function(x, ...) {
  cat("Top profiles    : \n")
  class(x) <- "data.frame"
  print(head(x, ...))
  return(invisible(NULL))
}

