#' Accumulated Local Effects Profiles aka ALEPlots
#'
#' Accumulated Local Effects Profiles accumulate local changes in Ceteris Paribus Profiles.
#' Function \code{\link{accumulated_dependence}} calls \code{\link{ceteris_paribus}} and then \code{\link{aggregate_profiles}}.
#'
#' Find more detailes in the \href{https://pbiecek.github.io/ema/accumulatedLocalProfiles.html}{Accumulated Local Dependence Chapter}.
#'
#' @param x an explainer created with function \code{DALEX::explain()}, an object of the class \code{ceteris_paribus_explainer}
#' or a model to be explained.
#' @param data validation dataset Will be extracted from \code{x} if it's an explainer
#' NOTE: It is best when target variable is not present in the \code{data}
#' @param predict_function predict function Will be extracted from \code{x} if it's an explainer
#' @param variables names of variables for which profiles shall be calculated.
#' Will be passed to \code{\link{calculate_variable_split}}.
#' If \code{NULL} then all variables from the validation data will be used.
#' @param N number of observations used for calculation of partial dependence profiles.
#' By default, 500 observations will be chosen randomly.
#' @param ... other parameters
#' @param variable_splits named list of splits for variables, in most cases created with \code{\link{calculate_variable_split}}.
#' If \code{NULL} then it will be calculated based on validation data avaliable in the \code{explainer}.
#' @param grid_points number of points for profile. Will be passed to\code{\link{calculate_variable_split}}.
#' @param label name of the model. By default it's extracted from the \code{class} attribute of the model
#' @param variable_type a character. If "numerical" then only numerical variables will be calculated.
#' If "categorical" then only categorical variables will be calculated.
#'
#' @references ALEPlot: Accumulated Local Effects (ALE) Plots and Partial Dependence (PD) Plots \url{https://cran.r-project.org/package=ALEPlot},
#' Explanatory Model Analysis. Explore, Explain, and Examine Predictive Models. \url{https://pbiecek.github.io/ema/}
#'
#' @return an object of the class \code{aggregated_profiles_explainer}
#'
#' @examples
#' library("DALEX")
#' library("ingredients")
#'
#' model_titanic_glm <- glm(survived ~ gender + age + fare,
#'                          data = titanic_imputed, family = "binomial")
#'
#' explain_titanic_glm <- explain(model_titanic_glm,
#'                                data = titanic_imputed[,-8],
#'                                y = titanic_imputed[,8],
#'                                verbose = FALSE)
#'
#' adp_glm <- accumulated_dependence(explain_titanic_glm,
#'                                   N = 25, variables = c("age", "fare"))
#' head(adp_glm)
#' plot(adp_glm)
#'
#' \donttest{
#' library("ranger")
#'
#' model_titanic_rf <- ranger(survived ~., data = titanic_imputed, probability = TRUE)
#'
#' explain_titanic_rf <- explain(model_titanic_rf,
#'                               data = titanic_imputed[,-8],
#'                               y = titanic_imputed[,8],
#'                               verbose = FALSE)
#'
#' adp_rf <- accumulated_dependence(explain_titanic_rf, N = 200, variable_type = "numerical")
#' plot(adp_rf)
#'
#' adp_rf <- accumulated_dependence(explain_titanic_rf, N = 200, variable_type = "categorical")
#' plotD3(adp_rf, label_margin = 80, scale_plot = TRUE)
#' }
#'
#' @export
#' @rdname accumulated_dependence
accumulated_dependence <- function(x, ...)
  UseMethod("accumulated_dependence")

#' @export
#' @rdname accumulated_dependence
accumulated_dependence.explainer <- function(x,
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

  accumulated_dependence.default(x = model,
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
#' @rdname accumulated_dependence
accumulated_dependence.default <- function(x,
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

  aggregate_profiles(cp, variables = variables, type = "accumulated", variable_type = variable_type, ...)
}



#' @export
#' @rdname accumulated_dependence
accumulated_dependence.ceteris_paribus_explainer <- function(x, ...,
                                                             variables = NULL) {

  aggregate_profiles(x, ..., type = "accumulated", variables = variables)
}

#' @export
#' @rdname accumulated_dependence
accumulated_dependency <- accumulated_dependence
