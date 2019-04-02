#' Adds a Layer with Aggregated Profiles
#'
#' Function 'show_aggreagated_profiles' adds a layer to a plot created with 'plot.ceteris_paribus_explainer'.
#'
#' @param x a ceteris paribus explainer produced with function `ceteris_paribus()`
#' @param ... other explainers that shall be plotted together
#' @param color a character. Either name of a color or name of a variable that should be used for coloring
#' @param size a numeric. Size of lines to be plotted
#' @param alpha a numeric between 0 and 1. Opacity of lines
#' @param facet_ncol number of columns for the `facet_wrap()`
#' @param variables if not NULL then only `variables` will be presented
#'
#' @references Predictive Models: Visual Exploration, Explanation and Debugging \url{https://pbiecek.github.io/PM_VEE}
#'
#' @return a ggplot2 layer
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
#' plot(pdp_rf_p, pdp_rf_c, pdp_rf_a, color = "_label_")
#'
#' plot(cp_rf, variables = "age") +
#' show_observations(cp_rf, variables = "age") +
#'   show_rugs(cp_rf, variables = "age", color = "red") +
#'   show_aggreagated_profiles(pdp_rf, size = 2)
#'
#' plot(pdp_rf, variables = "age")
#'
#' }
#' @export
plot.aggregated_profiles_explainer <- function(x, ...,
                                                      size = 1,
                                                      alpha = 1,
                                                      color = "#371ea3",
                                                      facet_ncol = NULL,
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

  `_x_` <- `_yhat_` <- `_ids_` <- `_label_` <- NULL
  res <- ggplot(data = aggregated_profiles, aes(`_x_`, group = paste(`_ids_`, `_label_`)))
  if (is_color_a_variable) {
    res <- res +
      geom_line(aes_string(y = "`_yhat_`", color = paste0("`",color,"`")), size = size, alpha = alpha)
  } else {
    res <- res +
      geom_line(aes(y = `_yhat_`), size = size, alpha = alpha, color = color)
  }
  res + theme_drwhy() + ylab("average prediction") + xlab("") +
    facet_wrap(~ `_vname_`, scales = "free_x", ncol = facet_ncol)
}

