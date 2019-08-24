#' Accumulated Local Effects Profiles aka ALEPlots
#'
#' Accumulated Local Effects Profiles accumulate local changes in Ceteris Paribus Profiles.
#' Function \code{accumulated_dependency} calls \code{ceteris_paribus} and then \code{aggregate_profiles}.
#'
#' Find more detailes in the \href{https://pbiecek.github.io/PM_VEE/accumulatedLocalProfiles.html}{Accumulated Local Dependency Chapter}.
#'
#' @param x a model to be explained, or an explainer created with function
#' \code{DALEX::explain()} or object of the class \code{ceteris_paribus_explainer}.
#' @param data validation dataset Will be extracted from \code{x} if it's an explainer
#' @param predict_function predict function Will be extracted from \code{x} if it's an explainer
#' @param variables names of variables for which profiles shall be calculated. Will be passed to \code{calculate_variable_splits()}.
#' If NULL then all variables from the validation data will be used.
#' @param N number of observations used for calculation of partial dependency profiles. By default, 500 observations will be chosen randomly.
#' @param ... other parameters
#' @param variable_splits named list of splits for variables, in most cases created with \code{calculate_variable_splits()}.
#' If NULL then it will be calculated based on validation data avaliable in the \code{explainer}.
#' @param grid_points number of points for profile. Will be passed to \code{calculate_variable_splits()}.
#' @param label name of the model. By default it's extracted from the 'class' attribute of the model
#' @param variable_type a character. If "numerical" then only numerical variables will be calculated.
#' If "categorical" then only categorical variables will be calculated.
#'
#' @references ALEPlot: Accumulated Local Effects (ALE) Plots and Partial Dependence (PD) Plots \url{https://cran.r-project.org/package=ALEPlot},
#' Predictive Models: Visual Exploration, Explanation and Debugging \url{https://pbiecek.github.io/PM_VEE}
#'
#' @return an \code{aggregated_profiles_explainer} object
#'
#' @examples
#' library("DALEX")
#'
#' titanic_imputed$country <- NULL
#'
#' model_titanic_glm <- glm(survived == "yes" ~ gender + age + fare,
#'                          data = titanic_imputed, family = "binomial")
#'
#' explain_titanic_glm <- explain(model_titanic_glm,
#'                                data = titanic_imputed[,-8],
#'                                y = titanic_imputed$survived == "yes",
#'                                verbose = FALSE)
#'
#' pdp_glm <- accumulated_dependency(explain_titanic_glm,
#'                                   N = 50, variables = c("age", "fare"))
#' head(pdp_glm)
#' plot(pdp_glm)
#'
#' \donttest{
#' library("randomForest")
#'
#' model_titanic_rf <- randomForest(survived == "yes" ~.,  data = titanic_imputed)
#'
#' explain_titanic_rf <- explain(model_titanic_rf,
#'                               data = titanic_imputed[,-8],
#'                               y = titanic_imputed$survived == "yes")
#'
#' pdp_rf <- accumulated_dependency(explain_titanic_rf)
#' plot(pdp_rf)
#'
#' pdp_rf <- accumulated_dependency(explain_titanic_rf, variable_type = "categorical")
#' plot(pdp_rf)
#' }
#'
#' @export
#' @rdname accumulated_dependency
accumulated_dependency <- function(x, ...)
  UseMethod("accumulated_dependency")

#' @export
#' @rdname accumulated_dependency
accumulated_dependency.explainer <- function(x,
                                             variables = NULL,
                                             N = 500,
                                             variable_splits = NULL,
                                             grid_points = 101,
                                             ...,
                                             variable_type = "numerical") {
  # extracts model, data and predict function from the explainer
  model <- x$model
  data <- x$data
  predict_function <- x$predict_function
  label <- x$label

  accumulated_dependency.default(x = model,
                                 data = data,
                                 predict_function = predict_function,
                                 label = label,
                                 variables = variables,
                                 grid_points = grid_points,
                                 variable_splits = variable_splits,
                                 N = N,
                                 ..., variable_type = variable_type)
}


#' @export
#' @rdname accumulated_dependency
accumulated_dependency.default <- function(x,
                                           data,
                                           predict_function = predict,
                                           label = class(x)[1],
                                           variables = NULL,
                                           N = 500,
                                           variable_splits = NULL,
                                           grid_points = 101,
                                           ...,
                                           variable_type = "numerical") {
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

  aggregate_profiles(cp, variables = variables, type = "accumulated", variable_type = variable_type)
}



#' @export
#' @rdname accumulated_dependency
accumulated_dependency.ceteris_paribus_explainer <- function(x, ...,
                                                             variables = NULL) {

  aggregate_profiles(x, ..., type = "accumulated", variables = variables)
}

