#' Ceteris Paribus Profiles aka Individual Variable Profiles
#'
#' This explainer works for individual observations.
#' For each observation it calculates Ceteris Paribus Profiles for selected variables.
#' Such profiles can be used to hypothesize about model results if selected variable is changed.
#' For this reason it is also called 'What-If Profiles'.
#'
#' Find more details in \href{https://ema.drwhy.ai/ceterisParibus.html}{Ceteris Paribus Chapter}.
#'
#' @param x an explainer created with the \code{DALEX::explain()} function, or a model to be explained.
#' @param data validation dataset. It will be extracted from \code{x} if it's an explainer
#' NOTE: It is best when target variable is not present in the \code{data}
#' @param predict_function predict function. It will be extracted from \code{x} if it's an explainer
#' @param new_observation a new observation with columns that corresponds to variables used in the model
#' @param y true labels for \code{new_observation}. If specified then will be added to ceteris paribus plots.
#' NOTE: It is best when target variable is not present in the \code{new_observation}
#' @param variables names of variables for which profiles shall be calculated.
#' Will be passed to \code{\link{calculate_variable_split}}.
#' If NULL then all variables from the validation data will be used.
#' @param ... other parameters
#' @param variable_splits named list of splits for variables, in most cases created with \code{\link{calculate_variable_split}}.
#' If NULL then it will be calculated based on validation data available in the \code{explainer}.
#' @param grid_points maximum number of points for profile calculations. Note that the finaln number of points may be lower than \code{grid_points}, eg. if there is not enough unique values for a given variable. Will be passed to \code{\link{calculate_variable_split}}.
#' @param label name of the model. By default it's extracted from the \code{class} attribute of the model
#' @param variable_splits_type how variable grids shall be calculated? Use "quantiles" (default) for percentiles or "uniform" to get uniform grid of points
#' @param variable_splits_with_obs if \code{TRUE} then all values in \code{new_observation} will be included in \code{variable_splits}
#'
#' @references Explanatory Model Analysis. Explore, Explain, and Examine Predictive Models. \url{https://ema.drwhy.ai/}
#'
#' @return an object of the class \code{ceteris_paribus_explainer}.
#'
#' @examples
#' library("DALEX")
#' library("ingredients")
#' titanic_small <- select_sample(titanic_imputed, n = 500, seed = 1313)
#'
#' # build a model
#' model_titanic_glm <- glm(survived ~ gender + age + fare,
#'                          data = titanic_small,
#'                          family = "binomial")
#'
#' explain_titanic_glm <- explain(model_titanic_glm,
#'                                data = titanic_small[,-8],
#'                                y = titanic_small[,8])
#'
#' cp_rf <- ceteris_paribus(explain_titanic_glm, titanic_small[1,])
#' cp_rf
#'
#' plot(cp_rf, variables = "age")
#'
#' \donttest{
#' library("ranger")
#' model_titanic_rf <- ranger(survived ~., data = titanic_imputed, probability = TRUE)
#'
#'
#' explain_titanic_rf <- explain(model_titanic_rf,
#'                               data = titanic_imputed[,-8],
#'                               y = titanic_imputed[,8],
#'                               label = "ranger forest",
#'                               verbose = FALSE)
#'
#' # select few passangers
#' selected_passangers <- select_sample(titanic_imputed, n = 20)
#' cp_rf <- ceteris_paribus(explain_titanic_rf, selected_passangers)
#' cp_rf
#'
#' plot(cp_rf, variables = "age") +
#'   show_observations(cp_rf, variables = "age") +
#'   show_rugs(cp_rf, variables = "age", color = "red")
#'
#' }
#'
#' @export
#' @rdname ceteris_paribus
ceteris_paribus <- function(x, ...)
  UseMethod("ceteris_paribus")

#' @export
#' @rdname ceteris_paribus
ceteris_paribus.explainer <- function(x,
                                      new_observation,
                                      y = NULL,
                                      variables = NULL,
                                      variable_splits = NULL,
                                      grid_points = 101,
                                      variable_splits_type = "quantiles",
                                      ...) {
  # extracts model, data and predict function from the explainer
  model <- x$model
  data <- x$data
  predict_function <- x$predict_function
  label <- x$label

  ceteris_paribus.default(x = model,
                          data = data,
                          predict_function = predict_function,
                          new_observation = new_observation,
                          y = y,
                          variables = variables,
                          variable_splits = variable_splits,
                          grid_points = grid_points,
                          label = label,
                          variable_splits_type = variable_splits_type,
                          ...)
}


#' @export
#' @rdname ceteris_paribus
ceteris_paribus.default <- function(x,
                                    data,
                                    predict_function = predict,
                                    new_observation,
                                    y = NULL,
                                    variables = NULL,
                                    variable_splits = NULL,
                                    grid_points = 101,
                                    variable_splits_type = "quantiles",
                                    variable_splits_with_obs = FALSE,
                                    label = class(x)[1],
                                    ...) {
  # here one can add model and data and new observation
  # just in case only some variables are specified
  # this will work only for data.frames
  if (is.data.frame(data)) {
    common_variables <- intersect(colnames(new_observation), colnames(data))
    new_observation <- new_observation[, common_variables, drop = FALSE]
    data <- data[,common_variables, drop = FALSE]
  }

  # calculate splits
  # if splits are not provided, then will be calculated
  if (is.null(variable_splits)) {
    # need validation data from the explainer
    if (is.null(data))
      stop("The ceteris_paribus() function requires explainers created with specified 'data'.")
    # need variables, if not provided, will be extracted from data
    if (is.null(variables))
      variables <- colnames(data)

    variable_splits <- calculate_variable_split(data, variables = variables, grid_points = grid_points,
                                    variable_splits_type = variable_splits_type,
                                    new_observation = if(variable_splits_with_obs) new_observation else NA)
  }

  # calculate profiles
  profiles <- calculate_variable_profile(new_observation,
                                         variable_splits, x, predict_function)

  # if there is more then one collumn with `_yhat_`
  # then we need to convert it to a single collumn
  col_yhat <- grep(colnames(profiles), pattern = "^_yhat_")
  if (length(col_yhat) == 1) {
    profiles$`_label_` <- label

    # add points of interests
    predictions <- predict_function(x, new_observation)
    # if new_observation is a matrix then turn into data.frame. see #26
    if (!is.data.frame(new_observation)) {
      new_observation <- as.data.frame(new_observation)
    }

    new_observation$`_yhat_` <- predictions
    new_observation$`_label_` <- label
    new_observation$`_ids_` <- 1:nrow(new_observation)
    if (!is.null(y)) new_observation$`_y_` <- y
  } else {
    # we need to recreate _yhat_ and create proper labels
    new_profiles <- profiles[rep(1:nrow(profiles), times = length(col_yhat)), -col_yhat]
    new_profiles$`_yhat_` <- unlist(c(profiles[,col_yhat]))
    stripped_names <- gsub(colnames(profiles)[col_yhat], pattern = "_yhat_", replacement = "")
    new_profiles$`_label_` <- paste0(label, rep(stripped_names, each = nrow(profiles)))
    profiles <- new_profiles

    # add points of interests
    new_observation_ext <- new_observation[rep(1:nrow(new_observation), times = length(col_yhat)),]
    predict_obs <- predict_function(x, new_observation)
    # if new_observation is a matrix then turn into data.frame. see #26
    if (!is.data.frame(new_observation_ext)) {
      new_observation_ext <- as.data.frame(new_observation_ext)
    }

    new_observation_ext$`_yhat_` <- unlist(c(predict_obs))
    new_observation_ext$`_label_` <- paste0(label, rep(stripped_names, each = nrow(new_observation)))
    new_observation_ext$`_ids_` <- rep(1:nrow(new_observation), each = length(col_yhat))

    # add y
    if (!is.null(y)) new_observation_ext$`_y_` <- rep(y, times = length(col_yhat))

    new_observation <- new_observation_ext
  }

  # prepare final object
  attr(profiles, "observations") <- new_observation
  class(profiles) <- c("ceteris_paribus_explainer", "data.frame")
  profiles
}

