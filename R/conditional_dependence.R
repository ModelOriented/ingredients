#' Conditional Dependence Profiles
#'
#' Conditional Dependence Profiles (aka Local Profiles) average localy Ceteris Paribus Profiles.
#' Function 'conditional_dependence' calls 'ceteris_paribus' and then 'aggregate_profiles'.
#'
#' Find more detailes in the \href{https://pbiecek.github.io/ema/accumulatedLocalProfiles.html}{Accumulated Local Dependence Chapter}.
#'
#' @param x an explainer created with function \code{DALEX::explain()}, an object of the class \code{ceteris_paribus_explainer}
#' or a model to be explained.
#' @param data validation dataset, will be extracted from \code{x} if it's an explainer
#' NOTE: It is best when target variable is not present in the \code{data}
#' @param predict_function predict function, will be extracted from \code{x} if it's an explainer
#' @param variables names of variables for which profiles shall be calculated.
#' Will be passed to \code{\link{calculate_variable_split}}. If \code{NULL} then all variables from the validation data will be used.
#' @param N number of observations used for calculation of partial dependence profiles. By default \code{500}.
#' @param ... other parameters
#' @param variable_splits named list of splits for variables, in most cases created with \code{\link{calculate_variable_split}}.
#' If \code{NULL} then it will be calculated based on validation data avaliable in the \code{explainer}.
#' @param grid_points number of points for profile. Will be passed to \code{\link{calculate_variable_split}}.
#' @param label name of the model. By default it's extracted from the \code{class} attribute of the model
#' @param variable_type a character. If \code{"numerical"} then only numerical variables will be calculated.
#' If \code{"categorical"} then only categorical variables will be calculated.
#'
#' @references Explanatory Model Analysis. Explore, Explain, and Examine Predictive Models. \url{https://pbiecek.github.io/ema/}
#'
#' @return an object of the class \code{aggregated_profile_explainer}
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
#'                                y = titanic_imputed[,8])
#'
#' cdp_glm <- conditional_dependence(explain_titanic_glm,
#'                                   N = 150, variables = c("age", "fare"))
#' head(cdp_glm)
#' plot(cdp_glm)
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
#' cdp_rf <- conditional_dependence(explain_titanic_rf, N = 200, variable_type = "numerical")
#' plot(cdp_rf)
#'
#' cdp_rf <- conditional_dependence(explain_titanic_rf, N = 200, variable_type = "categorical")
#' plotD3(cdp_rf, label_margin = 100, scale_plot = TRUE)
#' }
#'
#' @export
#' @rdname conditional_dependence
conditional_dependence <- function(x, ...)
  UseMethod("conditional_dependence")

#' @export
#' @rdname conditional_dependence
conditional_dependence.explainer <- function(x,
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

  conditional_dependence.default(x = model,
                                 data = data,
                                 predict_function = predict_function,
                                 label = label,
                                 variables = variables,
                                 N = N,
                                 variable_splits = variable_splits,
                                 grid_points = grid_points,
                                 ..., variable_type = variable_type)
}


#' @export
#' @rdname conditional_dependence
conditional_dependence.default <- function(x,
                                           data,
                                           predict_function = predict,
                                           label = class(x)[1],
                                           variables = NULL,
                                           N = 500,
                                           variable_splits = NULL,
                                           grid_points = 101,
                                           ...,
                                           variable_type = "numerical") {

  if (!is.null(N) && N < nrow(data)) {
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

  conditional_dependence.ceteris_paribus_explainer(cp, variables = variables, variable_type = variable_type, ...)
}


#' @export
#' @rdname conditional_dependence
conditional_dependence.ceteris_paribus_explainer <- function(x, ...,
                                                         variables = NULL) {

  aggregate_profiles(x, ..., type = "conditional", variables = variables)
}

#' @export
#' @rdname conditional_dependence
local_dependency <- conditional_dependence

#' @export
#' @rdname conditional_dependence
conditional_dependency <- conditional_dependence
