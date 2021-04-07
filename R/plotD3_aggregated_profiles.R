#' @title Plots Aggregated Ceteris Paribus Profiles in D3 with r2d3 Package.
#'
#' @description
#' Function \code{\link{plotD3.aggregated_profiles_explainer}} plots an aggregate of ceteris paribus profiles.
#' It works in a similar way to \code{\link{plotD3.ceteris_paribus_explainer}} but, instead of individual profiles,
#' show average profiles for each variable listed in the \code{variables} vector.
#'
#' Find more details in \href{https://ema.drwhy.ai/ceterisParibus.html}{Ceteris Paribus Chapter}.
#'
#' @param x a aggregated profiles explainer produced with function \code{aggregate_profiles()}
#' @param ... other explainers that shall be plotted together
#' @param color a character.  Set line/bar color
#' @param size a numeric. Set width of lines
#' @param alpha a numeric between \code{0} and \code{1}. Opacity of lines
#' @param facet_ncol number of columns for the \code{\link[ggplot2]{facet_wrap}}
#' @param scale_plot a logical. If \code{TRUE}, the height of plot scales with window size. By default it's \code{FALSE}
#' @param variables if not \code{NULL} then only \code{variables} will be presented
#' @param chart_title a character. Set custom title
#' @param label_margin a numeric. Set width of label margins in \code{categorical} type
#'
#' @references Explanatory Model Analysis. Explore, Explain, and Examine Predictive Models. \url{https://ema.drwhy.ai/}
#'
#' @return a \code{r2d3} object.
#'
#' @examples
#' library("DALEX")
#' library("ingredients")
#' library("ranger")
#'
#' # smaller data, quicker example
#' titanic_small <- select_sample(titanic_imputed, n = 500, seed = 1313)
#'
#' # build a model
#' model_titanic_rf <- ranger(survived ~., data = titanic_small, probability = TRUE)
#'
#' explain_titanic_rf <- explain(model_titanic_rf,
#'                               data = titanic_small[,-8],
#'                               y = titanic_small[,8],
#'                               label = "ranger forest",
#'                               verbose = FALSE)
#'
#' selected_passangers <- select_sample(titanic_small, n = 100)
#' cp_rf <- ceteris_paribus(explain_titanic_rf, selected_passangers)
#'
#' pdp_rf_p <- aggregate_profiles(cp_rf, type = "partial", variable_type = "numerical")
#' pdp_rf_p$`_label_` <- "RF_partial"
#' pdp_rf_c <- aggregate_profiles(cp_rf, type = "conditional", variable_type = "numerical")
#' pdp_rf_c$`_label_` <- "RF_conditional"
#' pdp_rf_a <- aggregate_profiles(cp_rf, type = "accumulated", variable_type = "numerical")
#' pdp_rf_a$`_label_` <- "RF_accumulated"
#'
#' plotD3(pdp_rf_p, pdp_rf_c, pdp_rf_a, scale_plot = TRUE)
#'
#' pdp <- aggregate_profiles(cp_rf, type = "partial", variable_type = "categorical")
#' pdp$`_label_` <- "RF_partial"
#'
#' plotD3(pdp, variables = c("gender","class"), label_margin = 70)
#'
#' @export
#' @rdname plotD3_aggregated_profiles
plotD3.aggregated_profiles_explainer <- function(x, ..., size = 2, alpha = 1,
                                                 color = "#46bac2",
                                                 facet_ncol = 2, scale_plot = FALSE,
                                                 variables = NULL,
                                                 chart_title = "Aggregated Profiles",
                                                 label_margin = 60) {

  # if there is more explainers, they should be merged into a single data frame
  dfl <- c(list(x), list(...))
  aggregated_profiles <- do.call(rbind, dfl)
  class(aggregated_profiles) <- "data.frame"

  all_variables <- na.omit(as.character(unique(aggregated_profiles$`_vname_`)))
  if (!is.null(variables)) {
    all_variables <- intersect(all_variables, variables)
    if (length(all_variables) == 0) stop(paste0("variables do not overlap with ", paste(all_variables, collapse = ", ")))

    aggregated_profiles <- aggregated_profiles[aggregated_profiles$`_vname_` %in% all_variables, ]
  }

  is_x_numeric <- is.numeric(aggregated_profiles$`_x_`)

  # prepare profiles data
  rownames(aggregated_profiles) <- NULL

  aggregated_profiles_list <- split(aggregated_profiles, aggregated_profiles$`_vname_`, drop = TRUE)

  min_max_list <- ymean <- label_names <- NULL

  # line plot or bar plot?
  if (is_x_numeric) {
    aggregated_profiles_list <- lapply(aggregated_profiles_list, function(x){
      ret <- x[, c('_x_', "_yhat_", "_vname_", "_label_")]
      colnames(ret) <- c("xhat", "yhat", "vname", "label")
      ret$xhat <- as.numeric(ret$xhat)
      ret$yhat <- as.numeric(ret$yhat)
      ret[order(ret$xhat),]
    })

    min_max_list <- lapply(aggregated_profiles_list, function(df){
      list(max(df$xhat), min(df$xhat))
    })

    aggregated_profiles_list <- lapply(aggregated_profiles_list, function(x){
      split(x, f = x$label)
    })

    label_names <- names(aggregated_profiles_list[[1]])

  } else {
    if (length(dfl) > 1) stop("Please pick one aggregated profile.")

    aggregated_profiles_list <- lapply(aggregated_profiles_list, function(x){
      ret <- x[, c("_x_", "_yhat_", "_vname_", "_label_")]
      colnames(ret) <- c("xhat", "yhat", "vname", "label")
      ret$yhat <- as.numeric(ret$yhat)
      ret
    })


    ymean <- ifelse("partial_dependence_explainer" %in% class(x), round(attr(x, "mean_prediction"), 3), 0)
  }

  ymax <- max(aggregated_profiles$`_yhat_`, ymean)
  ymin <- min(aggregated_profiles$`_yhat_`, ymean)
  ymargin <- abs(ymax - ymin)*0.1

  options <- list(variableNames = as.list(all_variables),
                  n = length(all_variables), c = length(list(...)) + 1,
                  yMax = ymax + ymargin, yMin = ymin - ymargin,
                  yMean = ymean, labelNames = label_names,
                  size = size, alpha = alpha, color = color,
                  onlyNumerical = is_x_numeric,
                  facetNcol = facet_ncol, scalePlot = scale_plot,
                  chartTitle = chart_title, labelMargin = label_margin)

  temp <- jsonlite::toJSON(list(aggregated_profiles_list, min_max_list))

  r2d3::r2d3(temp, system.file("d3js/aggregatedProfiles.js", package = "ingredients"),
             dependencies = list(
               system.file("d3js/colorsDrWhy.js", package = "ingredients"),
               system.file("d3js/d3-tip.js", package = "ingredients"),
               system.file("d3js/hackHead.js", package = "ingredients")
             ),
             css = system.file("d3js/themeDrWhy.css", package = "ingredients"),
             d3_version = 4,
             options = options)

}
