#' Adds a Layer with Aggregated Profiles
#'
#' Function \code{\link{show_aggregated_profiles}} adds a layer to a plot created
#' with \code{\link{plot.ceteris_paribus_explainer}}.
#'
#' @param x a ceteris paribus explainer produced with function \code{ceteris_paribus()}
#' @param ... other explainers that shall be plotted together
#' @param color a character. Either name of a color or name of a variable that should be used for coloring
#' @param size a numeric. Size of lines to be plotted
#' @param alpha a numeric between \code{0} and \code{1}. Opacity of lines
#' @param variables if not \code{NULL} then only \code{variables} will be presented
#'
#' @return a \code{ggplot2} layer
#'
#' @references Explanatory Model Analysis. Explore, Explain, and Examine Predictive Models. \url{https://pbiecek.github.io/ema/}
#'
#' @examples
#' library("DALEX")
#' library("ingredients")
#'
#' selected_passangers <- select_sample(titanic_imputed, n = 100)
#'
#' model_titanic_glm <- glm(survived ~ gender + age + fare,
#'                          data = titanic_imputed, family = "binomial")
#'
#' explain_titanic_glm <- explain(model_titanic_glm,
#'                                data = titanic_imputed[,-8],
#'                                y = titanic_imputed[,8])
#'
#' cp_rf <- ceteris_paribus(explain_titanic_glm, selected_passangers)
#'
#' pdp_rf <- aggregate_profiles(cp_rf, type = "partial", variables = "age")
#'
#' plot(cp_rf, variables = "age") +
#'   show_observations(cp_rf, variables = "age") +
#'   show_aggregated_profiles(pdp_rf, size = 3)
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
#' cp_rf <- ceteris_paribus(explain_titanic_rf, selected_passangers)
#' cp_rf
#'
#' pdp_rf <- aggregate_profiles(cp_rf, type = "partial", variables = "age")
#' head(pdp_rf)
#'
#' plot(cp_rf, variables = "age") +
#'   show_observations(cp_rf, variables = "age") +
#'   show_rugs(cp_rf, variables = "age", color = "red") +
#'   show_aggregated_profiles(pdp_rf, size = 3)
#' }
#'
#' @export
show_aggregated_profiles <- function(x, ...,
                                     size = 0.5,
                                     alpha = 1,
                                     color = "#371ea3",
                                     variables = NULL) {

  # if there is more explainers, they should be merged into a single data frame
  dfl <- c(list(x), list(...))
  aggregated_profiles <- do.call(rbind, dfl)
  class(aggregated_profiles) <- "data.frame"

  all_variables <- unique(aggregated_profiles$`_vname_`)
  if (!is.null(variables)) {
    all_variables <- intersect(all_variables, variables)
    if (length(all_variables) == 0) stop(paste0("variables do not overlap with ", paste(all_variables, collapse = ", ")))
  }
  aggregated_profiles <- aggregated_profiles[aggregated_profiles$`_vname_` %in% all_variables, ]
  is_color_a_variable <- color %in% c(all_variables, "_label_", "_vname_", "_ids_")

  `_yhat_` <- NULL
  if (is_color_a_variable) {
    res <- geom_line(data = aggregated_profiles, aes_string(y = "`_yhat_`", color = paste0("`",color,"`")), size = size, alpha = alpha)
  } else {
    res <- geom_line(data = aggregated_profiles, aes(y = `_yhat_`), size = size, alpha = alpha, color = color)
  }
  res
}
