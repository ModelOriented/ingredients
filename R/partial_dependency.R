#' Partial Dependency Profiles
#'
#' Partial Dependency Profiles are averages from Ceteris Paribus Profiles.
#' Function 'partial_dependency' calls 'ceteris_paribus' and then 'aggregate_profiles'.
#'
#' Find more detailes in the \href{https://pbiecek.github.io/PM_VEE/partialDependenceProfiles.html}{Partial Dependence Profiles Chapter}.
#'
#' @param x a model to be explained, or an explainer created with function `DALEX::explain()` or  object of the class `ceteris_paribus_explainer`.
#' @param data validation dataset, will be extracted from `x` if it's an explainer
#' @param predict_function predict function, will be extracted from `x` if it's an explainer
#' @param variables names of variables for which profiles shall be calculated. Will be passed to `calculate_variable_splits()`. If NULL then all variables from the validation data will be used.
#' @param N number of observations used for calculation of partial dependency profiles. By default 500.
#' @param ... other parameters
#' @param variable_splits named list of splits for variables, in most cases created with `calculate_variable_splits()`. If NULL then it will be calculated based on validation data avaliable in the `explainer`.
#' @param grid_points number of points for profile. Will be passed to `calculate_variable_splits()`.
#' @param label name of the model. By default it's extracted from the 'class' attribute of the model
#'
#' @references Predictive Models: Visual Exploration, Explanation and Debugging \url{https://pbiecek.github.io/PM_VEE}
#'
#' @return an 'aggregated_profiles_explainer' layer
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
#'
#' pdp_rf <- partial_dependency(explain_titanic_glm, N = 50)
#' plot(pdp_rf)
#'
#' \donttest{
#'  library("randomForest")
#'  model_titanic_rf <- randomForest(survived ~ gender + age + class + embarked +
#'                                     fare + sibsp + parch,  data = titanic)
#'  model_titanic_rf
#'
#'  explain_titanic_rf <- explain(model_titanic_rf,
#'                            data = titanic[,-9],
#'                            y = titanic$survived,
#'                            label = "Random Forest v7")
#'
#' pdp_rf <- partial_dependency(explain_titanic_rf, variables = "age")
#' plot(pdp_rf)
#'
#' pdp_rf <- partial_dependency(explain_titanic_rf)
#' plot(pdp_rf)
#' }
#' @export
#' @rdname partial_dependency
partial_dependency <- function(x, ...)
  UseMethod("partial_dependency")

#' @export
#' @rdname partial_dependency
partial_dependency.explainer <- function(x, variables = NULL, N = 500,
                                      variable_splits = NULL, grid_points = 101,
                                      ...) {
  # extracts model, data and predict function from the explainer
  model <- x$model
  data <- x$data
  predict_function <- x$predict_function
  label <- x$label

  partial_dependency.default(model, data, predict_function,
                          label = label,
                          variables = variables,
                          grid_points = grid_points,
                          variable_splits = variable_splits,
                          N = N,
                          ...)
}


#' @export
#' @rdname partial_dependency
partial_dependency.default <- function(x, data, predict_function = predict,
                           label = class(x)[1],
                           variables = NULL,
                           grid_points = grid_points,
                           variable_splits = variable_splits,
                           N = 500,
                           ...) {
  if (N < nrow(data)) {
    # sample N points
    ndata <- data[sample(1:nrow(data), N),]
  } else {
    ndata <- data
  }

  cp <- ceteris_paribus.default(x, data, predict_function = predict_function,
                            ndata, variables = variables,
                            grid_points = grid_points,
                            variable_splits = variable_splits,
                            label = label, ...)

  aggregate_profiles(cp, variables = variables, type = "partial")
}



#' @export
#' @rdname partial_dependency
partial_dependency.ceteris_paribus_explainer <- function(x, ...,
                           variables = NULL) {
  aggregate_profiles(x, ..., type = "partial", variables = variables)
}

