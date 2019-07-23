#' Ceteris Paribus Profiles aka Individual Variable Profiles
#'
#' This explainer works for individual observations.
#' For each observation it calculates Ceteris Paribus Profiles for selected variables.
#' Such profiles can be used to hypothesize about model results if selected variable is changed.
#' For this reason it is also called 'What-If Profiles'.
#'
#' Find more details in \href{https://pbiecek.github.io/PM_VEE/ceterisParibus.html}{Ceteris Paribus Chapter}.
#'
#' @param x a model to be explained, or an explainer created with the `DALEX::explain()` function.
#' @param data validation dataset. It will be extracted from `x` if it's an explainer
#' @param predict_function predict function. It will be extracted from `x` if it's an explainer
#' @param new_observation a new observation with columns that corresponds to variables used in the model
#' @param y true labels for `new_observation`. If specified then will be added to ceteris paribus plots.
#' @param variables names of variables for which profiles shall be calculated. Will be passed to `calculate_variable_splits()`. If NULL then all variables from the validation data will be used.
#' @param ... other parameters
#' @param variable_splits named list of splits for variables, in most cases created with `calculate_variable_splits()`. If NULL then it will be calculated based on validation data available in the `explainer`.
#' @param grid_points number of points for profile. Will be passed to `calculate_variable_splits()`.
#' @param label name of the model. By default it's extracted from the 'class' attribute of the model
#'
#' @references Predictive Models: Visual Exploration, Explanation and Debugging \url{https://pbiecek.github.io/PM_VEE}
#'
#' @return An object of the class 'ceteris_paribus_explainer'.
#' It's a data frame with calculated average responses.
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
#' plot(cp_rf, variables = "age")
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
#' # select few passangers
#' selected_passangers <- select_sample(titanic, n = 20)
#' cp_rf <- ceteris_paribus(explain_titanic_rf, selected_passangers)
#' cp_rf
#'
#' plot(cp_rf, variables = "age") +
#'   show_observations(cp_rf, variables = "age") +
#'   show_rugs(cp_rf, variables = "age", color = "red")
#'
#' }
#' @export
#' @rdname ceteris_paribus
ceteris_paribus <- function(x, ...)
  UseMethod("ceteris_paribus")

#' @export
#' @rdname ceteris_paribus
ceteris_paribus.explainer <- function(x, new_observation, y = NULL, variables = NULL,
                                      variable_splits = NULL, grid_points = 101,
                                      ...) {
  # extracts model, data and predict function from the explainer
  model <- x$model
  data <- x$data
  predict_function <- x$predict_function
  label <- x$label

  ceteris_paribus.default(model, data, predict_function,
                          new_observation = new_observation,
                          label = label,
                          variables = variables,
                          grid_points = grid_points,
                          variable_splits = variable_splits,
                          y = y,
                          ...)
}


#' @export
#' @rdname ceteris_paribus
ceteris_paribus.default <- function(x, data, predict_function = predict,
                                    new_observation, y = NULL, variables = NULL,
                                    variable_splits = NULL,
                                    grid_points = 101,
                                    label = class(x)[1], ...) {
  # here one can add model and data and new observation
  # just in case only some variables are specified
  # this will work only for data.frames
  if ("data.frame" %in% class(data)) {
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

    variable_splits <- calculate_variable_split(data, variables = variables, grid_points = grid_points)
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
    if (class(new_observation) != "data.frame") {
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
    if (class(new_observation_ext) != "data.frame") {
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
  class(profiles) = c("ceteris_paribus_explainer", "data.frame")
  profiles
}

