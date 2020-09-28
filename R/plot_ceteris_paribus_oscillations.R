#' Plot Ceteris Paribus Oscillations
#'
#' This function plots local variable importance plots
#' calculated as oscillations in the Ceteris Paribus Profiles.
#'
#' @param x a ceteris paribus oscillation explainer produced with function \code{calculate_oscillations()}
#' @param ... other explainers that shall be plotted together
#' @param bar_width width of bars. By default \code{10}.
#'
#' @references Explanatory Model Analysis. Explore, Explain, and Examine Predictive Models. \url{https://pbiecek.github.io/ema/}
#'
#' @return a \code{ggplot2} object
#'
#' @importFrom stats reorder
#'
#' @examples
#' \donttest{
#' library("DALEX")
#' library("ranger")
#'
#' apartments_rf_model <- ranger(m2.price ~., data = apartments)
#'
#' explainer_rf <- explain(apartments_rf_model,
#'                         data = apartments_test[,-1],
#'                         y = apartments_test[,1])
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
    ylab("Oscillations") + xlab("") + DALEX::theme_drwhy_vertical() +
    theme(legend.position = "none") +
    scale_color_manual(values = DALEX::colors_discrete_drwhy(nlabels))

}
