#' DrWhy Theme for ggplot objects
#'
#' @return theme for ggplot2 objects
#' @export
theme_drwhy <- function() {
    theme_bw(base_line_size = 0) %+replace%
    theme(axis.ticks = element_blank(), legend.background = element_blank(),
          legend.key = element_blank(), panel.background = element_blank(),
          panel.border = element_blank(), strip.background = element_blank(),
          plot.background = element_blank(), complete = TRUE,
          legend.direction = "horizontal", legend.position = "top",
          axis.line.y = element_line(color = "white"),
          axis.ticks.y = element_line(color = "white"),
          #axis.line = element_line(color = "#371ea3", size = 0.5, linetype = 1),
          axis.title = element_text(color = "#371ea3"),
          axis.text = element_text(color = "#371ea3", size = 10),
          strip.text = element_text(color = "#371ea3", size = 12, hjust = 0),
          panel.grid.major.y = element_line(color = "grey90", size = 0.5, linetype = 1),
          panel.grid.minor.y = element_line(color = "grey90", size = 0.5,  linetype = 1))

}
