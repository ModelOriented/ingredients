#' Adds a Layer with Profiles
#'
#' Function 'show_profiles' adds a layer to a plot created with 'plot.ceteris_paribus_explainer'.
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
#' selected_john <- titanic[1,]
#'
#' model_titanic_glm <- glm(survived == "yes" ~ gender + age + fare,
#'                        data = titanic, family = "binomial")
#'
#' explain_titanic_glm <- explain(model_titanic_glm,
#'                            data = titanic[,-9],
#'                            y = titanic$survived == "yes")
#'
#' cp_rf <- ceteris_paribus(explain_titanic_glm, selected_passangers)
#' cp_rf_john <- ceteris_paribus(explain_titanic_glm, selected_john)
#' plot(cp_rf, variables = "age") +
#'   show_profiles(cp_rf_john, variables = "age", size = 2)
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
#' cp_rf_john <- ceteris_paribus(explain_titanic_rf, selected_john)
#' cp_rf
#'
#' pdp_rf <- aggregate_profiles(cp_rf, variables = "age")
#' head(pdp_rf)
#'
#' plot(cp_rf, variables = "age") +
#'   show_observations(cp_rf, variables = "age") +
#'   show_rugs(cp_rf, variables = "age", color = "red") +
#'   show_profiles(cp_rf_john, variables = "age", size = 2)
#'
#' }
#' @export
show_profiles <- function(x, ...,
                      size = 0.5,
                      alpha = 1,
                      color = "#371ea3",
                      variables = NULL) {
  # if there is more explainers, they should be merged into a single data frame
  dfl <- c(list(x), list(...))
  all_profiles <- do.call(rbind, dfl)
  class(all_profiles) <- "data.frame"

  all_profiles$`_ids_` <- factor(all_profiles$`_ids_`)

  # variables to use
  all_variables <- na.omit(as.character(unique(all_profiles$`_vname_`)))
  if (!is.null(variables)) {
    all_variables <- intersect(all_variables, variables)
    if (length(all_variables) == 0) stop(paste0("variables do not overlap with ", paste(all_variables, collapse = ", ")))
  }
  # is color a variable or literal?
  is_color_a_variable <- color %in% c(all_variables, "_label_", "_vname_", "_ids_")
  # this function works only for numerical variables
  is_numeric <- sapply(all_profiles[, all_variables, drop = FALSE], is.numeric)
  vnames <- names(which(is_numeric))
  all_profiles$`_x_` <- 0

  if (length(vnames) == 0) {
      stop("There are no numerical variables")
  }

  # how to plot profiles
  # select only suitable variables  either in vnames or in variables
  all_profiles <- all_profiles[all_profiles$`_vname_` %in% vnames, ]
  tmp <- as.character(all_profiles$`_vname_`)
  for (i in seq_along(tmp)) {
    all_profiles$`_x_`[i] <- all_profiles[i, tmp[i]]
  }

  # show profiles without aggregation
  if (is_color_a_variable) {
    pl <- geom_line(data = all_profiles, aes_string(color = paste0("`",color,"`")), size = size, alpha = alpha)
  } else {
    pl <- geom_line(data = all_profiles, size = size, alpha = alpha, color = color)
  }

  pl
}
