#' Prints Ceteris Paribus Descriptions
#'
#' @param x an individual variable profile explainer produced with the `describe()` function
#' @param ... other arguments
#'
#' @export
#'
#' @examples
#' library("DALEX")
#' # Toy examples, because CRAN angels ask for them
#' titanic <- na.omit(titanic)
#' model_titanic_glm <- glm(survived == "yes" ~ gender + age + fare,
#'                        data = titanic, family = "binomial")
#'
#' explain_titanic_glm <- explain(model_titanic_glm,
#'                            data = titanic[,-9],
#'                            y = titanic$survived == "yes")
#' cp_rf <- ceteris_paribus(explain_titanic_glm, titanic[1,])
#' cp_rf
#' describe(cp_rf)
#' plot(cp_rf)
#'
#'  \donttest{
#'  library("randomForest")
#'  model_titanic_rf <- randomForest(survived == "yes" ~ gender + age + class + embarked +
#'                                     fare + sibsp + parch,  data = titanic)
#'  model_titanic_rf
#'
#'  explain_titanic_rf <- explain(model_titanic_rf,
#'                            data = titanic[,-9],
#'                            y = titanic$survived == "yes",
#'                            label = "Random Forest v7")
#'
#' selected_passangers <- select_sample(titanic, n = 1)
#' cp_rf <- ceteris_paribus(explain_titanic_rf, selected_passangers)
#' cp_rf
#' describe(cp_rf)
#' plot(cp_rf) +
#'   show_observations(cp_rf)
#'  }
#'
print.ceteris_paribus_descriptions <- function(x, ...) {
  for (element in x) {
    cat(element, "\n")
  }

  return(invisible(NULL))
}
