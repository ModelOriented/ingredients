#' Adds a Layer with Residuals to a Profile Plot
#'
#' Function \code{\link{show_residuals}} adds a layer to a plot created with
#' \code{\link{plot.ceteris_paribus_explainer}} for selected observations.
#' Note that the \code{y} argument has to be specified in the \code{\link{ceteris_paribus}} function.
#'
#' @param x a ceteris paribus explainer produced with function \code{ceteris_paribus()}.
#' Note that \code{y} parameter shall be supplied in this function.
#' @param ... other explainers that shall be plotted together
#' @param color a character. Either name of a color or name of a variable that should be used for coloring
#' @param size a numeric. Size of lines to be plotted
#' @param alpha a numeric between \code{0} and \code{1}. Opacity of lines
#' @param variables if not \code{NULL} then only \code{variables} will be presented
#'
#' @return a \code{ggplot2} layer
#'
#' @references Explanatory Model Analysis. Explore, Explain, and Examine Predictive Models. \url{https://ema.drwhy.ai/}
#'
#' @examples
#' library("DALEX")
#' library("ingredients")
#' library("ranger")
#'
#' johny_d <- data.frame(
#'   class = factor("1st", levels = c("1st", "2nd", "3rd", "deck crew", "engineering crew",
#'                                    "restaurant staff", "victualling crew")),
#'   gender = factor("male", levels = c("female", "male")),
#'   age = 8,
#'   sibsp = 0,
#'   parch = 0,
#'   fare = 72,
#'   embarked = factor("Southampton", levels = c("Belfast", "Cherbourg", "Queenstown", "Southampton"))
#' )
#'
#' \donttest{
#' model_titanic_rf <- ranger(survived ~., data = titanic_imputed, probability = TRUE)
#'
#' explain_titanic_rf <- explain(model_titanic_rf,
#'                               data = titanic_imputed[,-8],
#'                               y = titanic_imputed[,8],
#'                               label = "ranger forest",
#'                               verbose = FALSE)
#'
#' johny_neighbours <- select_neighbours(data = titanic_imputed,
#'                                       observation = johny_d,
#'                                       variables = c("age", "gender", "class",
#'                                                   "fare", "sibsp", "parch"),
#'                                       n = 10)
#'
#' cp_neighbours <- ceteris_paribus(explain_titanic_rf,
#'                                  johny_neighbours,
#'                                  y = johny_neighbours$survived == "yes",
#'                                  variable_splits = list(age = seq(0,70, length.out = 1000)))
#'
#' plot(cp_neighbours, variables = "age") +
#'   show_observations(cp_neighbours, variables = "age")
#'
#'
#' cp_johny <- ceteris_paribus(explain_titanic_rf, johny_d,
#'                             variable_splits = list(age = seq(0,70, length.out = 1000)))
#'
#' plot(cp_johny, variables = "age", size = 1.5, color = "#8bdcbe") +
#'   show_profiles(cp_neighbours, variables = "age", color = "#ceced9") +
#'   show_observations(cp_johny, variables = "age", size = 5, color = "#371ea3") +
#'   show_residuals(cp_neighbours, variables = "age")
#'
#' }
#' @export
show_residuals <- function(x, ...,
                              size = 0.75,
                              alpha = 1,
                              color = c('TRUE' = '#8bdcbe', 'FALSE' = '#f05a71'),
                              variables = NULL) {

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
  if (all(!is_numeric)) {
    # only factors
    vnames <- all_variables
  } else {
    vnames <- all_variables[which(is_numeric)]
  }

  # prepare data for plotting points
  is_color_points_a_variable    <- head(color,1) %in% c(all_variables, "_label_", "_vname_", "_ids_")

  tmp <- lapply(vnames, function(var) {
    data.frame(`_x_` = all_observations[,var],
               `_vname_` = var,
               `_yhat_`  = all_observations$`_yhat_`,
               `_y_`     = if (is.null(all_observations$`_y_`)) NA else as.numeric(all_observations$`_y_`),
               `_color_` = if (!is_color_points_a_variable) NA else {
                 if (color == "_vname_") var else all_observations[,head(color,1)]
               },
               `_ids_`   = all_observations$`_ids_`,
               `_label_`  = all_observations$`_label_`)
  })
  all_observations_long <- do.call(rbind, tmp)
  colnames(all_observations_long) <- c("_x_", "_vname_", "_yhat_", "_y_", "_color_", "_ids_", "_label_")
  if ((is_color_points_a_variable ) & !(head(color,1) %in% colnames(all_observations_long)))
    colnames(all_observations_long)[5] = head(color,1)
  all_observations_long$`_sign_` <- factor(all_observations_long$`_y_` > all_observations_long$`_yhat_`)

  `_y_` <- `_yhat_` <- `_sign_` <- NULL

  # show observations
  if (is_color_points_a_variable) {
    pl <- list(geom_point(data = all_observations_long, aes_string(y = "`_y_`", color = paste0("`",color,"`")), size = size, alpha = alpha),
           geom_linerange(data = all_observations_long, aes_string(ymin = "`_y_`", ymax = "`_yhat_`", color = paste0("`",color,"`")), size = size, alpha = alpha))
  } else {
    if (length(color) > 1) {
      # pair of colors
      pl <- list(geom_point(data = all_observations_long, aes(y = `_y_`, color = `_sign_`), size = size, alpha = alpha),
             geom_linerange(data = all_observations_long, aes(ymin = `_y_`, ymax = `_yhat_`, color = `_sign_`), size = size, alpha = alpha),
             scale_color_manual(name = "", values = color),
             theme(legend.position = "none"))
    } else {
      pl <- list(geom_point(data = all_observations_long, aes(y = `_y_`), size = size, alpha = alpha, color = color),
             geom_linerange(data = all_observations_long, aes(ymin = `_y_`, ymax = `_yhat_`), size = size, alpha = alpha, color = color))
    }
  }
  pl
}
