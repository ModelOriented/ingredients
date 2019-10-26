#' Prints Aggregated Profiles
#'
#' @param x an individual variable profile explainer produced with the \code{aggregate_profiles()} function
#' @param ... other arguments that will be passed to \code{head()}
#'
#' @examples
#' library("DALEX")
#'
#' model_titanic_glm <- glm(survived ~ gender + age + fare,
#'                          data = titanic_imputed, family = "binomial")
#'
#' explain_titanic_glm <- explain(model_titanic_glm,
#'                                data = titanic_imputed[,-8],
#'                                y = titanic_imputed[,8])
#'
#' selected_passangers <- select_sample(titanic_imputed, n = 100)
#' cp_rf <- ceteris_paribus(explain_titanic_glm, selected_passangers)
#'
#' head(cp_rf)
#'
#' pdp_rf <- aggregate_profiles(cp_rf, variables = "age")
#' head(pdp_rf)
#'
#' \donttest{
#' library("randomForest")
#'
#' model_titanic_rf <- randomForest(survived ~.,  data = titanic_imputed)
#'
#' explain_titanic_rf <- explain(model_titanic_rf,
#'                               data = titanic_imputed[,-8],
#'                               y = titanic_imputed[,8],
#'                               verbose = FALSE, precalculate = FALSE)
#'
#' cp_rf <- ceteris_paribus(explain_titanic_rf, selected_passangers)
#' cp_rf
#'
#' pdp_rf <- aggregate_profiles(cp_rf, variables = "age")
#' head(pdp_rf)
#' }
#'
#' @export
print.aggregated_profiles_explainer <- function(x, ...) {

  cat("Top profiles    : \n")
  class(x) <- "data.frame"
  print(head(x, ...))
  return(invisible(NULL))
}

