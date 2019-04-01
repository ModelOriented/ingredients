#' Adds a Layer with Aggregated Profiles
#'
#' Function 'show_aggreagated_profiles' adds a layer to a plot created with 'plot.ceteris_paribus_explainer'.
#'
#' @param x a ceteris paribus explainer produced with function `ceteris_paribus()`
#' @param ... other explainers that shall be plotted together
#' @param color a character. Either name of a color or name of a variable that should be used for coloring
#' @param size a numeric. Size of lines to be plotted
#' @param alpha a numeric between 0 and 1. Opacity of lines
#' @param variables if not NULL then only `variables` will be presented
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
#'   show_aggreagated_profiles(pdp_rf, size = 3)
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
#'   show_aggreagated_profiles(pdp_rf, size = 3)
#'
#' plot(pdp_rf, variables = "age", color = "grey")
#'
#' }
#' @export
show_aggreagated_profiles <- function(x, ...,
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
  is_color_a_variable <- color %in% c(all_variables, "_label_", "_vname_", "_ids_")

  `_yhat_` <- NULL
  if (is_color_a_variable) {
    res <- geom_line(data = aggregated_profiles, aes_string(y = "`_yhat_`", color = paste0("`",color,"`")), size = size, alpha = alpha)
  } else {
    res <- geom_line(data = aggregated_profiles, aes(y = `_yhat_`), size = size, alpha = alpha, color = color)
  }
  res
}
