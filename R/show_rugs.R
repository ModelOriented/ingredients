#' Adds a Layer with Rugs to a Profile Plot
#'
#' Function 'show_rugs' adds a layer to a plot created with 'plot.ceteris_paribus_explainer' for selected observations.
#' Various parameters help to decide what should be plotted, profiles, aggregated profiles, points or rugs.
#'
#' @param x a ceteris paribus explainer produced with function `ceteris_paribus()`
#' @param ... other explainers that shall be plotted together
#' @param color a character. Either name of a color or name of a variable that should be used for coloring
#' @param size a numeric. Size of lines to be plotted
#' @param sides a string containing any of "trbl", for top, right, bottom, and left. Passed to geom rug.
#' @param alpha a numeric between 0 and 1. Opacity of lines
#' @param variables if not NULL then only `variables` will be presented
#' @param variable_type a character. If "numerical" then only numerical variables will be plotted.
#' If "categorical" then only categorical variables will be plotted.
#'
#' @return a ggplot2 layer
#' @examples
#' library("DALEX")
#' # Toy examples, because CRAN angels ask for them
#' titanic <- na.omit(titanic)
#' selected_passangers <- select_sample(titanic, n = 100)
#'
#' model_titanic_glm <- glm(survived == "yes" ~ gender + age + fare,
#'                        data = titanic, family = "binomial")
#'
#' explain_titanic_glm <- explain(model_titanic_glm,
#'                            data = titanic[,-9],
#'                            y = titanic$survived == "yes")
#'
#' cp_rf <- ceteris_paribus(explain_titanic_glm, selected_passangers)
#' pdp_rf <- aggregate_profiles(cp_rf, variables = "age")
#' plot(cp_rf, variables = "age") +
#'   show_observations(cp_rf, variables = "age") +
#'   show_aggregated_profiles(pdp_rf, size = 3)
#'
#'  \donttest{
#' library("randomForest")
#'  model_titanic_rf <- randomForest(survived ~ gender + age + class + embarked +
#'                                     fare + sibsp + parch,  data = titanic)
#'  model_titanic_rf
#'
#'  explain_titanic_rf <- explain(model_titanic_rf,
#'                            data = titanic[,-9],
#'                            y = titanic$survived)
#'
#' cp_rf <- ceteris_paribus(explain_titanic_rf, selected_passangers)
#' cp_rf
#'
#' pdp_rf <- aggregate_profiles(cp_rf, variables = "age")
#' head(pdp_rf)
#'
#' plot(cp_rf, variables = "age") +
#'   show_observations(cp_rf, variables = "age") +
#'   show_rugs(cp_rf, variables = "age", color = "red") +
#'   show_aggregated_profiles(pdp_rf, size = 3)
#'
#' plot(pdp_rf, variables = "age", color = "grey")
#'
#' }
#' @export
show_rugs <- function(x, ...,
                      size = 0.5,
                      alpha = 1,
                      color = "#371ea3",
                      variable_type = "numerical",
                      sides = "b",
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

  # show rugs
  if (is_color_points_a_variable) {
    res <- geom_rug(data = all_observations_long, aes_string(color = paste0("`",color,"`")), size = size, alpha = alpha, sides = sides)
  } else {
    res <- geom_rug(data = all_observations_long, size = size, alpha = alpha, color = color, sides = sides)
  }

  res
}

