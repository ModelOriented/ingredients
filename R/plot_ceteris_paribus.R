#' Plots Individual Variable Profile Explanations
#'
#' Function 'plot.ceteris_paribus_explainer' plots Individual Variable Profiles for selected observations.
#' Various parameters help to decide what should be plotted, profiles, aggregated profiles, points or rugs.
#'
#' @param x a ceteris paribus explainer produced with function `ceteris_paribus()`
#' @param ... other explainers that shall be plotted together
#' @param color a character. Either name of a color or name of a variable that should be used for coloring
#' @param size a numeric. Size of lines to be plotted
#' @param alpha a numeric between 0 and 1. Opacity of lines
#' @param facet_ncol number of columns for the `facet_wrap()`
#' @param selected_variables if not NULL then only `selected_variables` will be presented
#' @param only_numerical a logical. If TRUE then only numerical variables will be plotted. If FALSE then only categorical variables will be plotted.
#'
#' @return a ggplot2 object
#' @export
#' @import ggplot2
#' @importFrom stats aggregate
#'
#' @examples
#' library("DALEX")
#'  \dontrun{
#' library("titanic")
#' library("randomForest")
#'
#' titanic_small <- titanic_train[,c("Survived", "Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked")]
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
#' plot(cp_rf, selected_variables = "Age", color = "grey") +
#' show_observations(cp_rf, selected_variables = "Age", color = "grey") +
#'   show_rugs(cp_rf, selected_variables = "Age", color = "red")
#'
#' }
#' @export
plot.ceteris_paribus_explainer <- function(x, ...,
   size = 0.5,
   alpha = 0.8,
   color = "black",
   only_numerical = TRUE,
   facet_ncol = NULL, selected_variables = NULL) {

  # if there is more explainers, they should be merged into a single data frame
  dfl <- c(list(x), list(...))
  all_profiles <- do.call(rbind, dfl)
  class(all_profiles) <- "data.frame"

  all_profiles$`_ids_` <- factor(all_profiles$`_ids_`)

  # variables to use
  all_variables <- na.omit(as.character(unique(all_profiles$`_vname_`)))
  if (!is.null(selected_variables)) {
    all_variables <- intersect(all_variables, selected_variables)
    if (length(all_variables) == 0) stop(paste0("selected_variables do not overlap with ", paste(all_variables, collapse = ", ")))
  }
  # is color a variable or literal?
  is_color_a_variable <- color %in% c(all_variables, "_label_", "_vname_", "_ids_")
  # only numerical or only factors?
  is_numeric <- sapply(all_profiles[, all_variables, drop = FALSE], is.numeric)
  if (only_numerical) {
    vnames <- names(which(is_numeric))
    if (length(vnames) == 0) stop("There are no numerical variables")
    all_profiles$`_x_` <- 0
  } else {
    vnames <- names(which(!is_numeric))
    if (length(vnames) == 0) stop("There are no non-numerical variables")
    all_profiles$`_x_` <- ""
  }
  # select only suitable variables
  all_profiles <- all_profiles[all_profiles$`_vname_` %in% vnames, ]
  # create _x_
  tmp <- as.character(all_profiles$`_vname_`)
  for (i in seq_along(tmp)) {
    all_profiles$`_x_`[i] <- all_profiles[i, tmp[i]]
  }

  # prepare plot
  `_x_` <- `_y_` <- `_yhat_` <- `_ids_` <- `_label_` <- NULL
  pl <- ggplot(all_profiles, aes(`_x_`, `_yhat_`, group = paste(`_ids_`, `_label_`))) +
      facet_wrap(~ `_vname_`, scales = "free_x", ncol = facet_ncol)

  # show profiles without aggregation
  if (is_color_a_variable) {
    pl <- pl + geom_line(data = all_profiles, aes_string(color = paste0("`",color,"`")), size = size, alpha = alpha)
  } else {
    pl <- pl + geom_line(data = all_profiles, size = size, alpha = alpha, color = color)
  }

  pl <- pl + theme_classic() +
    theme(panel.border = element_blank(),
          axis.line.y = element_line(color = "white"),
          axis.ticks.y = element_line(color = "white"),
          axis.text = element_text(size = 10)) + xlab("") + ylab("")

  pl
}


