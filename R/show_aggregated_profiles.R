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
#'  \dontrun{
#' library("titanic")
#' library("randomForest")
#'
#' titanic_small <- titanic_train[,c("Survived", "Pclass", "Sex", "Age",
#'                                    "SibSp", "Parch", "Fare", "Embarked")]
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
#' pdp_rf <- aggregate_profiles(cp_rf, variables = "Age")
#' pdp_rf
#'
#' plot(cp_rf, variables = "Age", color = "grey") +
#' show_observations(cp_rf, variables = "Age", color = "grey") +
#'   show_rugs(cp_rf, variables = "Age", color = "red") +
#'   show_aggreagated_profiles(pdp_rf, size = 3)
#'
#' plot(pdp_rf, variables = "Age", color = "grey")
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
