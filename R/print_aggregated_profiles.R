#' Prints Aggregated Profiles
#'
#' @param x an individual variable profile explainer produced with the \code{aggregate_profiles()} function
#' @param ... other arguments that will be passed to \code{head()}
#'
#' @examples
#' library("DALEX")

#' titanic <- na.omit(titanic)
#'
#' model_titanic_glm <- glm(survived == "yes" ~ gender + age + fare,
#'                          data = titanic, family = "binomial")
#'
#' explain_titanic_glm <- explain(model_titanic_glm,
#'                                data = titanic[,-9],
#'                                y = titanic$survived == "yes")
#' selected_passangers <- select_sample(titanic, n = 100)
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
#' model_titanic_rf <- randomForest(survived ~ gender + age + class + embarked +
#'                                     fare + sibsp + parch,  data = titanic)
#'
#' explain_titanic_rf <- explain(model_titanic_rf,
#'                               data = titanic[,-9],
#'                               y = titanic$survived,
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

