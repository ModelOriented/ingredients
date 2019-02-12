#' Adds a Layer with Aggregated Profiles
#'
#' Function 'show_aggreagated_profiles' adds a layer to a plot created with 'plot.ceteris_paribus_explainer'.
#'
#' @param x a ceteris paribus explainer produced with function `ceteris_paribus()`
#' @param ... other explainers that shall be plotted together
#' @param color a character. Either name of a color or name of a variable that should be used for coloring
#' @param size a numeric. Size of lines to be plotted
#' @param sides a string containing any of "trbl", for top, right, bottom, and left. Passed to geom rug.
#' @param alpha a numeric between 0 and 1. Opacity of lines
#' @param selected_variables if not NULL then only `selected_variables` will be presented
#'
#' @return a ggplot2 layer
#' @export
show_aggreagated_profiles <- function(x, ...,
                      size = 0.5,
                      alpha = 1,
                      color = "black",

                      selected_variables = NULL) {

  # if there is more explainers, they should be merged into a single data frame
  dfl <- c(list(x), list(...))
  aggregated_profiles <- do.call(rbind, dfl)
  class(aggregated_profiles) <- "data.frame"

  all_variables <- unique(aggregated_profiles$`_vname_`)
  if (!is.null(selected_variables)) {
    all_variables <- intersect(all_variables, selected_variables)
    if (length(all_variables) == 0) stop(paste0("selected_variables do not overlap with ", paste(all_variables, collapse = ", ")))
  }
  is_color_a_variable <- color %in% c(all_variables, "_label_", "_vname_", "_ids_")

  if (is_color_a_variable) {
    res <- geom_line(data = aggregated_profiles, aes_string(y = "`_yhat_`", color = paste0("`",color,"`")), size = size, alpha = alpha)
  } else {
    res <- geom_line(data = aggregated_profiles, aes(y = `_yhat_`), size = size, alpha = alpha, color = color)
  }

}

