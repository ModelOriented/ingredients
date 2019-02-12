#' Calculates Individual Variable Profiles aka Ceteris Paribus Profiles
#'
#' This explainer works for individual observations.
#' For each observation it calculates Individual Variable Profiles for selected variables.
#' For this reason it is also called 'Local Profile Plot'.
#'
#' @param x a model to be explained, or an explainer created with function `DALEX::explain()`.
#' @param data validation dataset, will be extracted from `x` if it's an explainer
#' @param predict_function predict function, will be extracted from `x` if it's an explainer
#' @param new_observation a new observation with columns that corresponds to variables used in the model
#' @param y true labels for `new_observation`. If specified then will be added to ceteris paribus plots.
#' @param variables names of variables for which profiles shall be calculated. Will be passed to `calculate_variable_splits()`. If NULL then all variables from the validation data will be used.
#' @param ... other parameters
#' @param variable_splits named list of splits for variables, in most cases created with `calculate_variable_splits()`. If NULL then it will be calculated based on validation data avaliable in the `explainer`.
#' @param grid_points number of points for profile. Will be passed to `calculate_variable_splits()`.
#' @param label name of the model. By default it's extracted from the 'class' attribute of the model
#'
#'
#' @return An object of the class 'ceteris_paribus_explainer'.
#' It's a data frame with calculated average responses.
#' @export
#'
#' @examples
#' library("DALEX")
#'  \dontrun{
#' library("titanic")
#' library("randomForest")
#'
#' titanic_small <- titanic_train[,c("Survived", "Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked")]
#' titanic_small$Survived <- factor(titanic_small$Survived)
#' titanic_small$Sex <- factor(titanic_small$Sex)
#' titanic_small$Embarked <- factor(titanic_small$Embarked)
#' titanic_small <- na.omit(titanic_small)
#' rf_model <- randomForest(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
#'                          data = titanic_small)
#' predict_fuction <- function(m,x) predict(m, x, type="prob")[,2]
#' explainer_rf <- explain(rf_model, data = titanic_small,
#'                         y = titanic_small$Survived == "1", label = "RF",
#'                         predict_function = predict_rf_fuction)
#'
#' selected_passangers <- select_sample(titanic_small, n = 100)
#' cp_rf <- ceteris_paribus(explainer_rf, selected_passangers)
#' cp_rf
#'
#' plot(cp_rf, selected_variables = "Age", color = "grey") +
#' show_observations(cp_rf, selected_variables = "Age", color = "grey") +
#'   show_rugs(cp_rf, selected_variables = "Age", color = "red")
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
  p <- ncol(data)

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
    new_observation$`_yhat_` <- predict_function(x, new_observation)
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
    new_observation_ext$`_yhat_` <- unlist(c(predict_obs))
    new_observation_ext$`_label_` <- paste0(label, rep(stripped_names, each = nrow(new_observation)))
    new_observation_ext$`_ids_` <- rep(1:nrow(new_observation), each = nrow(new_observation))

    # add y
    if (!is.null(y)) new_observation_ext$`_y_` <- rep(y, times = length(col_yhat))

    new_observation <- new_observation_ext
  }

  # prepare final object
  attr(profiles, "observations") <- new_observation
  class(profiles) = c("ceteris_paribus_explainer", "data.frame")
  profiles
}

