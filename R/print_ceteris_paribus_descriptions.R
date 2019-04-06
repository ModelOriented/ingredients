#' Prints Ceteris Paribus Descriptions
#'
#' @param x an individual variable profile explainer produced with the `describe()` function
#' @param ... other arguments
#'
#' @export
#'
#' @examples
#' library("DALEX")
#'  \donttest{
#' }
print.ceteris_paribus_descriptions <- function(x, ...) {
  for (element in x) {
    cat(element, "\n")
  }

  return(invisible(NULL))
}
