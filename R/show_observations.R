#' Adds a Layer with Observations to a Profile Plot
#'
#' Function 'show_observations' adds a layer to a plot created with 'plot.ceteris_paribus_explainer' for selected observations.
#' Various parameters help to decide what should be plotted, profiles, aggregated profiles, points or rugs.
#'
#' @param x a ceteris paribus explainer produced with function `ceteris_paribus()`
#' @param ... other explainers that shall be plotted together
#' @param color a character. Either name of a color or name of a variable that should be used for coloring
#' @param size a numeric. Size of lines to be plotted
#' @param alpha a numeric between 0 and 1. Opacity of lines
#' @param variables if not NULL then only `variables` will be presented
#' @param variable_type a character. If "numerical" then only numerical variables will be plotted.
#' If "categorical" then only categorical variables will be plotted.
#'
#' @return a ggplot2 layer
#' @examples
#' library("DALEX")
#'  \dontrun{
#' library("titanic")
#' library("randomForest")
#'
#' titanic_small <- titanic_train[,c("Survived", "Pclass", "Sex", "Age",
#'                                   "SibSp", "Parch", "Fare", "Embarked")]
#' titanic_small$Survived <- factor(titanic_small$Survived)
#' titanic_small$Sex <- factor(titanic_small$Sex)
#' titanic_small$Embarked <- factor(titanic_small$Embarked)
#' titanic_small <- na.omit(titanic_small)
#' rf_model <- randomForest(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
#'                          data = titanic_small)
#' explainer_rf <- explain(rf_model, data = titanic_small,
#'                         y = titanic_small$Survived == "1", label = "RF")
#'
#' selected_passangers <- select_sample(titanic_small, n = 100)
#' cp_rf <- ceteris_paribus(explainer_rf, selected_passangers)
#' cp_rf
#'
#' plot(cp_rf, variables = "Age", color = "grey") +
#' show_observations(cp_rf, variables = "Age", color = "grey") +
#'   show_rugs(cp_rf, variables = "Age", color = "red")
#'
#' }
#' @export
show_observations <- function(x, ...,
                              size = 2,
                              alpha = 1,
                              color = "#371ea3",
                              variable_type = "numerical",
                              variables = NULL) {

  check_variable_type(variable_type)

  # if there is more explainers, they should be merged into a single data frame
  dfl <- c(list(x), list(...))

  all_observations <- lapply(dfl, function(tmp) {
    attr(tmp, "observations")
  })
  all_observations <- do.call(rbind, all_observations)
  all_observations$`_ids_` <- factor(rownames(all_observations))

  # variables to use
  all_variables <- grep(colnames(all_observations), pattern = "^[^_]", value = TRUE)
  if (!is.null(variables)) {
    all_variables <- intersect(all_variables, variables)
    if (length(all_variables) == 0) stop(paste0("variables do not overlap with ", paste(all_variables, collapse = ", ")))
  }
  # only numerical or only factors?
  is_numeric <- sapply(all_observations[, all_variables, drop = FALSE], is.numeric)
  if (variable_type == "numerical") {
    vnames <- all_variables[which(is_numeric)]
    if (length(vnames) == 0) stop("There are no numerical variables")
  } else {
    vnames <- all_variables[which(!is_numeric)]
    if (length(vnames) == 0) stop("There are no non-numerical variables")
  }

  # prepare data for plotting points
  is_color_points_a_variable    <- color %in% c(all_variables, "_label_", "_vname_", "_ids_")

  tmp <- lapply(vnames, function(var) {
    data.frame(`_x_` = all_observations[,var],
               `_vname_` = var,
               `_yhat_`  = all_observations$`_yhat_`,
               `_y_`     = if (is.null(all_observations$`_y_`)) NA else all_observations$`_y_`,
               `_color_` = if (!is_color_points_a_variable) NA else {
                 if (color == "_vname_") var else all_observations[,color]
               },
               `_ids_`   = all_observations$`_ids_`,
               `_label_`  = all_observations$`_label_`)
  })
  all_observations_long <- do.call(rbind, tmp)
  colnames(all_observations_long) <- c("_x_", "_vname_", "_yhat_", "_y_", "_color_", "_ids_", "_label_")
  if ((is_color_points_a_variable ) & !(color %in% colnames(all_observations_long)))
    colnames(all_observations_long)[5] = color

  # show observations
  if (is_color_points_a_variable) {
    res <- geom_point(data = all_observations_long, aes_string(color = paste0("`",color,"`")), size = size, alpha = alpha)
  } else {
    res <- geom_point(data = all_observations_long, size = size, alpha = alpha, color = color)
  }

  res
}
