#' Plot Ceteris Paribus Oscillations
#'
#' This function plots local variable importance plots
#' calculated as oscillations in the Ceteris Paribus Profiles.
#'
#' @param x a ceteris paribus oscillation explainer produced with function \code{calculate_oscillations()}
#' @param ... other explainers that shall be plotted together
#' @param bar_width width of bars. By default 10
#'
#' @references Predictive Models: Visual Exploration, Explanation and Debugging \url{https://pbiecek.github.io/PM_VEE}
#'
#' @return a \code{ggplot2} object
#'
#' @importFrom stats reorder
#'
#' @examples
#' library("DALEX")
#' \donttest{
#' library("randomForest")
#'
#' apartments_rf_model <- randomForest(m2.price ~ construction.year + surface + floor +
#'                                     no.rooms + district, data = apartments)
#'
#' explainer_rf <- explain(apartments_rf_model,
#'                         data = apartments_test[,2:6],
#'                         y = apartments_test$m2.price)
#'
#' apartment <- apartments_test[1:2,]
#'
#' cp_rf <- ceteris_paribus(explainer_rf, apartment)
#' plot(cp_rf, color = "_ids_")
#'
#' vips <- calculate_oscillations(cp_rf)
#' vips
#'
#' plot(vips)
#' }
#'
#' @export
plot.ceteris_paribus_oscillations <- function(x, ..., bar_width = 10) {

  x <- as.data.frame(x)
  x$`_vname_` <- reorder(x$`_vname_`, x$oscillations, mean, na.rm = TRUE)
  x$`_ids_` <- paste0("ID: ",x$`_ids_`)

  # plot it
  nlabels <- length(unique(x$`_ids_`))
  `_ids_` <- `_vname_` <- oscillations <- NULL
  ggplot(x, aes(`_vname_`, ymin = 0, ymax = oscillations, color = `_ids_`)) +
    geom_linerange(size = bar_width) + coord_flip() +
    facet_wrap(~`_ids_`, scales = "free_y") +
    ylab("Oscillations") + xlab("") + theme_drwhy_vertical() +
    theme(legend.position = "none") +
    scale_color_manual(values = theme_drwhy_colors(nlabels))

}
