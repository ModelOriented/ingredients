#' Plot Ceteris Paribus 2D Explanations
#'
#' Function 'ceteris_paribus_2d_explainer' plots What-If Plots for a single prediction / observation.
#'
#' @param x a ceteris paribus explainer produced with the 'ceteris_paribus_2d' function
#' @param ... currently will be ignored
#' @param facet_ncol number of columns for the `facet_wrap()`
#' @param add_raster if TRUE then `geom_raster` will be added to present levels with diverging colors
#' @param add_contour if TRUE then `geom_contour` will be added to present contours
#' @param bins number of contours to be added
#' @param add_observation if TRUE then `geom_point` will be added to present observation that is explained
#' @param pch character, symbol used to plot observations
#' @param size numeric, size of individual datapoints
#'
#' @references Predictive Models: Visual Exploration, Explanation and Debugging \url{https://pbiecek.github.io/PM_VEE}
#'
#' @return a ggplot2 object
#' @export
#'
#' @examples
#' library("DALEX")
#'  \donttest{
#' library("randomForest")
#' set.seed(59)
#'
#' apartments_rf_model <- randomForest(m2.price ~ construction.year + surface + floor +
#'       no.rooms + district, data = apartments)
#'
#' explainer_rf <- explain(apartments_rf_model,
#'       data = apartmentsTest[,2:6], y = apartmentsTest$m2.price)
#'
#' new_apartment <- apartmentsTest[1, ]
#' new_apartment
#'
#' wi_rf_2d <- ceteris_paribus_2d(explainer_rf, observation = new_apartment)
#' head(wi_rf_2d)
#'
#' plot(wi_rf_2d)
#' plot(wi_rf_2d, add_contour = FALSE)
#' plot(wi_rf_2d, add_observation = FALSE)
#' plot(wi_rf_2d, add_raster = FALSE)
#'
#' # HR data
#' model <- randomForest(status ~ gender + age + hours + evaluation + salary, data = HR)
#' pred1 <- function(m, x)   predict(m, x, type = "prob")[,1]
#' explainer_rf_fired <- explain(model, data = HR[,1:5],
#'    y = HR$status == "fired",
#'    predict_function = pred1, label = "fired")
#'
#' new_emp <- HR[1, ]
#' new_emp
#'
#' wi_rf_2d <- ceteris_paribus_2d(explainer_rf_fired, observation = new_emp)
#' head(wi_rf_2d)
#'
#' plot(wi_rf_2d)
#' }
plot.ceteris_paribus_2d_explainer <- function(x, ..., facet_ncol = NULL, add_raster = TRUE,
                                              add_contour = TRUE, bins = 3, add_observation = TRUE,
                                              pch = "+", size = 6) {
  all_responses <- x
  class(all_responses) <- "data.frame"

  midpoint <- mean(all_responses$y_hat, na.rm = TRUE)
  new_x1 <- y_hat <- new_x2 <- NULL

  pred <- attr(x, "prediction")$observation
  all_pairs <- unique(all_responses[,c("vname1","vname2")])
  observations <- lapply(1:nrow(all_pairs), function(i) {
    pair <- all_pairs[i,]
    data.frame(vname1 = pair$vname1,
               vname2 = pair$vname2,
               new_x1 = pred[,as.character(pair$vname1)],
               new_x2 = pred[,as.character(pair$vname2)],
               y_hat = midpoint)
  })
  observation <- do.call(rbind, observations)

  pl <- ggplot(all_responses, aes(new_x1, new_x2, fill = y_hat, z = y_hat)) +
    facet_wrap(vname1 ~ vname2, scales = "free", ncol = facet_ncol) +
    xlab("") + ylab("") +
    scale_fill_gradient2(name = 'Prediction', midpoint = midpoint,
                         low = "#2cd9dd", high = "#ff4940", mid = "#f0f0f4")
  # or use scale_fill_gradient with midpoint

  if (add_raster) {
    pl <- pl + geom_raster(data = all_responses)
  }

  if (add_contour) {
    pl <- pl + geom_contour(data = all_responses, color = "white", alpha = 0.5, bins = bins)
  }

  if (add_observation) {
    pl <- pl + geom_point(data = observation, fill = "black", pch = pch, size = size)
  }
  pl + theme_drwhy_blank()
}

theme_drwhy_blank <- function() {
  theme_bw(base_line_size = 0) %+replace%
    theme(axis.ticks = element_blank(), legend.background = element_blank(),
          legend.key = element_blank(), panel.background = element_blank(),
          panel.border = element_blank(), strip.background = element_blank(),
          plot.background = element_blank(), complete = TRUE,
          legend.direction = "horizontal", legend.position = "top",
          plot.title = element_text(color = "#371ea3", size = 16, hjust = 0),
          plot.subtitle = element_text(color = "#371ea3", size = 14, hjust = 0),
          axis.line.x = element_line(color = "white"),
          axis.ticks.x = element_line(color = "white"),
          axis.title = element_text(color = "#371ea3"),
          axis.text = element_text(color = "#371ea3", size = 10),
          strip.text = element_text(color = "#371ea3", size = 12, hjust = 0, margin = margin(0, 0, 1, 0)))
}
