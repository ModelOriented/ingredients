#' Conditional Dependency Profiles
#'
#' Conditional Dependency Profiles (aka Local Profiles) average localy Ceteris Paribus Profiles.
#' Function 'conditional_dependency' calls 'ceteris_paribus' and then 'aggregate_profiles'.
#'
#' Find more detailes in \href{https://pbiecek.github.io/PM_VEE/conditionalProfiles.html}{Local Dependency Profiles Chapter}.
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
#' @return object of class \code{aggregated_profile_explainer}
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
#'
#' pdp_rf <- conditional_dependency(explain_titanic_glm, N = 50)
#' plot(pdp_rf)
#'
#'  \donttest{
#' library("randomForest")
#'
#' titanic_small <- titanic[,c("survived", "class", "gender", "age",
#'                          "sibsp", "parch", "fare", "embarked")]
#' titanic_small <- na.omit(titanic_small)
#' rf_model <- randomForest(survived == "yes" ~ class + gender + age + sibsp + parch + fare + embarked,
#'                          data = titanic_small)
#' explainer_rf <- explain(rf_model, data = titanic_small,
#'                         y = titanic_small$survived == "1yes", label = "RF")
#'
#' pdp_rf <- conditional_dependency(explainer_rf)
#' plot(pdp_rf)
#' }
#'
#' @export
#' @rdname conditional_dependency
conditional_dependency <- function(x, ...)
  UseMethod("conditional_dependency")

#' @export
#' @rdname conditional_dependency
conditional_dependency.explainer <- function(x,
                                             variables = NULL,
                                             N = 500,
                                             variable_splits = NULL,
                                             grid_points = 101,
                                             ...) {
  # extracts model, data and predict function from the explainer
  model <- x$model
  data <- x$data
  predict_function <- x$predict_function
  label <- x$label

  conditional_dependency.default(x = model,
                                 data = data,
                                 predict_function = predict_function,
                                 label = label,
                                 variables = variables,
                                 N = N,
                                 variable_splits = variable_splits,
                                 grid_points = grid_points,
                                 ...)
}


#' @export
#' @rdname conditional_dependency
conditional_dependency.default <- function(x,
                                           data,
                                           predict_function = predict,
                                           label = class(x)[1],
                                           variables = NULL,
                                           N = 500,
                                           variable_splits = NULL,
                                           grid_points = 101,
                                           ...) {

  if (N < nrow(data)) {
    # sample N points
    ndata <- data[sample(1:nrow(data), N),]
  } else {
    ndata <- data
  }

  cp <- ceteris_paribus.default(x,
                                data,
                                predict_function = predict_function,
                                new_observation = ndata,
                                variables = variables,
                                grid_points = grid_points,
                                variable_splits = variable_splits,
                                label = label, ...)

  conditional_dependency.ceteris_paribus_explainer(cp, variables = variables)
}


#' @export
#' @rdname conditional_dependency
conditional_dependency.ceteris_paribus_explainer <- function(x, ...,
                                                         variables = NULL) {

  aggregate_profiles(x, ..., type = "conditional", variables = variables)
}

#' @export
#' @rdname conditional_dependency
local_dependency <- conditional_dependency
