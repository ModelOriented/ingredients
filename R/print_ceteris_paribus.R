#' Prints Individual Variable Explainer Summary
#'
#' @param x an individual variable profile explainer produced with the \code{ceteris_paribus()} function
#' @param ... other arguments that will be passed to \code{head()}
#'
#' @examples
#' library("DALEX")
#' library("randomForest")
#'
#' apartments_rf_model <- randomForest(m2.price ~., data = apartments)
#'
#' explainer_rf <- explain(apartments_rf_model,
#'                         data = apartments_test[,-1],
#'                         y = apartments_test[,1])
#'
#' apartments_small <- select_sample(apartments_test, 10)
#'
#' cp_rf <- ceteris_paribus(explainer_rf, apartments_small)
#' cp_rf
#'
#' @export
print.ceteris_paribus_explainer <- function(x, ...) {
  cat("Top profiles    : \n")
  class(x) <- "data.frame"
  print(head(x, ...))

  cat("\n\nTop observations:\n")
  print(head(as.data.frame(attr(x, "observations")), ...))
  return(invisible(NULL))
}
