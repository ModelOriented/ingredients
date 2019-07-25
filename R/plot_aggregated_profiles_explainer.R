#' Plots Aggregated Profiles
#'
#' Function 'plot.aggregated_profiles_explainer' creates a 'ggplot2' plot with partial dependency plot or accumulated effect plot.
#' It works in a similar way to 'plot.ceteris_paribus', but instead of individual profiles show average profiles for each variable
#' listed in the 'variables' vector.
#'
#' @param x a ceteris paribus explainer produced with function `aggregate_profiles()`
#' @param ... other explainers that shall be plotted together
#' @param color a character. Either name of a color or name of a variable that should be used for coloring
#' @param size a numeric. Size of lines to be plotted
#' @param alpha a numeric between 0 and 1. Opacity of lines
#' @param facet_ncol number of columns for the `facet_wrap()`
#' @param variables if not NULL then only `variables` will be presented
#'
#' @references Predictive Models: Visual Exploration, Explanation and Debugging \url{https://pbiecek.github.io/PM_VEE}
#'
#' @return a ggplot2 object
#' @examples
#' library("DALEX")
#' # Toy example
#' titanic <- na.omit(titanic)
#' model_titanic_glm <- glm(survived == "yes" ~ gender + age + fare,
#'                        data = titanic, family = "binomial")
#'
#' explain_titanic_glm <- explain(model_titanic_glm,
#'                            data = titanic[,-9],
#'                            y = titanic$survived == "yes")
#'
#' pdp_rf_p <- partial_dependency(explain_titanic_glm, N = 50)
#' pdp_rf_p$`_label_` <- "RF_partial"
#' pdp_rf_l <- conditional_dependency(explain_titanic_glm, N = 50)
#' pdp_rf_l$`_label_` <- "RF_local"
#' pdp_rf_a<- accumulated_dependency(explain_titanic_glm, N = 50)
#' pdp_rf_a$`_label_` <- "RF_accumulated"
#' head(pdp_rf_p)
#' plot(pdp_rf_p, pdp_rf_l, pdp_rf_a, color = "_label_")
#'
#'  \donttest{
#'  library("randomForest")
#'  titanic <- na.omit(titanic)
#'  model_titanic_rf <- randomForest(survived == "yes" ~ gender + age + class + embarked +
#'                                     fare + sibsp + parch,  data = titanic)
#'  model_titanic_rf
#'
#'  explain_titanic_rf <- explain(model_titanic_rf,
#'                            data = titanic[,-9],
#'                            y = titanic$survived == "yes",
#'                            label = "Random Forest v7")
#'
#' selected_passangers <- select_sample(titanic, n = 100)
#' cp_rf <- ceteris_paribus(explain_titanic_rf, selected_passangers)
#' cp_rf
#'
#' pdp_rf_p <- aggregate_profiles(cp_rf, variables = "age", type = "partial")
#' pdp_rf_p$`_label_` <- "RF_partial"
#' pdp_rf_c <- aggregate_profiles(cp_rf, variables = "age", type = "conditional")
#' pdp_rf_c$`_label_` <- "RF_conditional"
#' pdp_rf_a <- aggregate_profiles(cp_rf, variables = "age", type = "accumulated")
#' pdp_rf_a$`_label_` <- "RF_accumulated"
#' head(pdp_rf_p)
#' plot(pdp_rf_p)
#' plot(pdp_rf_p, pdp_rf_c, pdp_rf_a)
#'
#' plot(cp_rf, variables = "age") +
#'   show_observations(cp_rf, variables = "age") +
#'   show_rugs(cp_rf, variables = "age", color = "red") +
#'   show_aggregated_profiles(pdp_rf_p, size = 2)
#'
#' }
#' @export
plot.aggregated_profiles_explainer <- function(x, ...,
                                          size = 1,
                                          alpha = 1,
                                          color = "_label_",
                                          facet_ncol = NULL,
                                          variables = NULL) {

  # if there are more explainers, they should be merged into a single data frame
  dfl <- c(list(x), list(...))
  aggregated_profiles <- do.call(rbind, dfl)
  class(aggregated_profiles) <- "data.frame"

  all_variables <- unique(aggregated_profiles$`_vname_`)
  if (!is.null(variables)) {
    all_variables <- intersect(all_variables, variables)
    if (length(all_variables) == 0) stop(paste0("variables do not overlap with ", paste(all_variables, collapse = ", ")))
  }
  is_color_a_variable <- color %in% c(all_variables, "_label_", "_vname_", "_ids_")
  is_x_numeric <- is.numeric(aggregated_profiles$`_x_`)

  # initiate plot
  `_x_` <- `_yhat_` <- `_ids_` <- `_label_` <- NULL
  res <- ggplot(data = aggregated_profiles, aes(`_x_`, group = paste(`_ids_`, `_label_`)))

  # what kind of plot shall be plotted?
  # numerical
  if (is_color_a_variable & is_x_numeric) {
    nlabels <- length(unique(aggregated_profiles$`_label_`))
    res <- res +
      geom_line(aes_string(y = "`_yhat_`", color = paste0("`",color,"`")), size = size, alpha = alpha) +
      scale_color_manual(name = "", values = theme_drwhy_colors(nlabels))
  }
  if (!is_color_a_variable & is_x_numeric) {
    res <- res +
      geom_line(aes(y = `_yhat_`), size = size, alpha = alpha, color = color)
  }
  # what kind of plot shall be plotted?
  # categorical
  if (is_color_a_variable & !is_x_numeric) {
    nlabels <- length(unique(aggregated_profiles$`_label_`))
    res <- res +
      geom_col(aes_string(y = "`_yhat_`", fill = paste0("`",color,"`")), size = size, alpha = alpha, position = "dodge") +
      scale_fill_manual(name = "", values = theme_drwhy_colors(nlabels))
  }
  if (!is_color_a_variable & !is_x_numeric) {
    res <- res +
      geom_col(aes(y = `_yhat_`), size = size, alpha = alpha, fill = color, position = "dodge")
  }

  res + theme_drwhy() + ylab("average prediction") + xlab("") +
    facet_wrap(~ `_vname_`, scales = "free_x", ncol = facet_ncol)
}

