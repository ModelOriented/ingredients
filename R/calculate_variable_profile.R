#' Internal Function for Individual Variable Profiles
#'
#' This function calculates individual variable profiles (ceteris paribus profiles), i.e. series of predictions from a model calculated for observations with altered single coordinate.
#'
#' Note that \code{\link{calculate_variable_profile}} function is S3 generic.
#' If you want to work on non standard data sources (like H2O ddf, external databases)
#' you should overload it.
#'
#' @param data set of observations. Profile will be calculated for every observation (every row)
#' @param variable_splits named list of vectors. Elements of the list are vectors with points in which profiles should be calculated. See an example for more details.
#' @param predict_function function that takes data and model and returns numeric predictions. Note that the ... arguments will be passed to this function.
#' @param model a model that will be passed to the \code{predict_function}
#' @param ... other parameters that will be passed to the \code{predict_function}
#'
#' @references Explanatory Model Analysis. Explore, Explain, and Examine Predictive Models. \url{https://pbiecek.github.io/ema/}
#'
#' @return a data frame with profiles for selected variables and selected observations
#'
#' @rdname calculate_variable_profile
calculate_variable_profile <- function(data, variable_splits, model, predict_function = predict, ...) {
  UseMethod("calculate_variable_profile")
}

#' @rdname calculate_variable_profile
calculate_variable_profile.default <- function(data, variable_splits, model, predict_function = predict, ...) {

  variables <- names(variable_splits)
  profiles <- lapply(variables, function(variable) {
    split_points <- variable_splits[[variable]]

    # remember ids of selected points
    if (is.null(rownames(data))) {
      ids <- rep(1:nrow(data), each = length(split_points)) # it never goes here, because null rownames are automatically setted to 1:n
    } else {
      ids <- rep(rownames(data), each = length(split_points))
    }

    new_data <- data[rep(1:nrow(data), each = length(split_points)), , drop = FALSE]
    new_data[, variable] <- rep(split_points, nrow(data))

    yhat <- predict_function(model, new_data, ...)
    new_data <- data.frame(new_data,
                      `_yhat_` = yhat,
                      `_vname_` = variable,
                      `_ids_` = ids,
                      check.names = FALSE)
    new_data
  })

  profile <- do.call(rbind, profiles)
  class(profile) <- c("individual_variable_profile", class(profile))
  profile
}


#' Internal Function for Split Points for Selected Variables
#'
#' This function calculate candidate splits for each selected variable.
#' For numerical variables splits are calculated as percentiles
#' (in general uniform quantiles of the length grid_points).
#' For all other variables splits are calculated as unique values.
#'
#' Note that \code{\link{calculate_variable_split}} function is S3 generic.
#' If you want to work on non standard data sources (like H2O ddf, external databases)
#' you should overload it.
#'
#' @param data validation dataset. Is used to determine distribution of observations.
#' @param variables names of variables for which splits shall be calculated
#' @param grid_points number of points used for response path
#' @param variable_splits_type how variable grids shall be calculated? Use "quantiles" (default) for percentiles or "uniform" to get uniform grid of points
#' @param new_observation if specified (not \code{NA}) then all values in \code{new_observation} will be included in \code{variable_splits}
#'
#' @return A named list with splits for selected variables
#' @importFrom stats predict
#'
#' @rdname calculate_variable_split
calculate_variable_split <- function(data, variables = colnames(data), grid_points = 101, variable_splits_type = "quantiles", new_observation = NA) {
  UseMethod("calculate_variable_split")
}
#' @rdname calculate_variable_split
calculate_variable_split.default <- function(data, variables = colnames(data), grid_points = 101, variable_splits_type = "quantiles", new_observation = NA) {
  variable_splits <- lapply(variables, function(var) {
    selected_column <- na.omit(data[,var])
    # numeric?
    if (is.numeric(selected_column)) {
      probs <- seq(0, 1, length.out = grid_points)
      if (variable_splits_type == "quantiles") {
        # variable quantiles
        selected_splits <- unique(quantile(selected_column, probs = probs))
      } else {
        selected_splits <- seq(min(selected_column, na.rm = TRUE), max(selected_column, na.rm = TRUE), length.out = grid_points)
      }
      # fixing https://github.com/ModelOriented/ingredients/issues/124
      if (!any(is.na(new_observation)))
        selected_splits <- sort(unique(c(selected_splits, na.omit(new_observation[,var]))))
    } else {
      # sort will change order of factors in a good way
      if (any(is.na(new_observation))) {
        selected_splits <- sort(unique(selected_column))
      } else {
        # fixing https://github.com/ModelOriented/ingredients/issues/124
        selected_splits <- sort(unique(rbind(data[, var, drop = FALSE],
                                  new_observation[, var, drop = FALSE])[,1]))
      }
    }
    selected_splits
  })
  names(variable_splits) <- variables
  variable_splits
}

