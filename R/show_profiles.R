#' Adds a Layer with Profiles
#'
#' Function \code{\link{show_profiles}} adds a layer to a plot created with
#' \code{\link{plot.ceteris_paribus_explainer}}.
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
#' @references Explanatory Model Analysis. Explore, Explain, and Examine Predictive Models. \url{https://ema.drwhy.ai/}
#'
#' @examples
#' library("DALEX")
#' library("ingredients")
#'
#' selected_passangers <- select_sample(titanic_imputed, n = 100)
#' selected_john <- titanic_imputed[1,]
#'
#' model_titanic_glm <- glm(survived ~ gender + age + fare,
#'                          data = titanic_imputed, family = "binomial")
#'
#' explain_titanic_glm <- explain(model_titanic_glm,
#'                                data = titanic_imputed[,-8],
#'                                y = titanic_imputed[,8],
#'                                label = "glm", verbose = FALSE)
#'
#' cp_rf <- ceteris_paribus(explain_titanic_glm, selected_passangers)
#' cp_rf_john <- ceteris_paribus(explain_titanic_glm, selected_john)
#' plot(cp_rf, variables = "age") +
#'   show_profiles(cp_rf_john, variables = "age", size = 2)
#'
#' \donttest{
#' library("ranger")
#'
#' model_titanic_rf <- ranger(survived ~.,  data = titanic_imputed, probability = TRUE)
#'
#' explain_titanic_rf <- explain(model_titanic_rf,
#'                               data = titanic_imputed[,-8],
#'                               y = titanic_imputed[,8],
#'                               label = "ranger forest",
#'                               verbose = FALSE)
#'
#' cp_rf <- ceteris_paribus(explain_titanic_rf, selected_passangers)
#' cp_rf_john <- ceteris_paribus(explain_titanic_rf, selected_john)
#'
#' cp_rf
#'
#' pdp_rf <- aggregate_profiles(cp_rf, variables = "age")
#' head(pdp_rf)
#'
#' plot(cp_rf, variables = "age") +
#'   show_observations(cp_rf, variables = "age") +
#'   show_rugs(cp_rf, variables = "age", color = "red") +
#'   show_profiles(cp_rf_john, variables = "age", color = "red", size = 2)
#' }
#'
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

  # only numerical or only factors?
  is_numeric <- sapply(all_profiles[, all_variables, drop = FALSE], is.numeric)
  if (all(!is_numeric)) {
    # only factors
    vnames <- all_variables
    all_profiles$`_x_` <- ""

    lapply(variables, function(sv) {
      tmp <- all_profiles[all_profiles$`_vname_` == sv,
                          c(sv, "_vname_", "_yhat_", "_label_", "_ids_")]
      colnames(tmp)[1]    <- "_x_"
      tmp$`_x_` <- as.character(tmp$`_x_`)
      tmp
    }) -> lsc
    # transformed data frame
    selected_cp_flat <- do.call(rbind, lsc)

    # prepare plot
    pl <- geom_line(data = selected_cp_flat,
                      aes_string("`_x_`", "`_yhat_`", group = "`_ids_`"),
                    size = size, alpha = alpha, color = color)
  } else {
    vnames <- all_variables[which(is_numeric)]
    all_profiles$`_x_` <- 0

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
  }

  pl
}
