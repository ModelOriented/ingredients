#' @title Plots Aggregated Ceteris Paribus Profiles in D3 with r2d3 Package.
#'
#' @description
#' Function 'plotD3.aggregated_profiles_explainer' plots an aggregate of ceteris paribus profiles.
#' It works in a similar way to 'plotD3.ceteris_paribus' but, instead of individual profiles,
#' show average profiles for each variable listed in the 'variables' vector.
#'
#' Find more detailes in \href{https://pbiecek.github.io/PM_VEE/ceterisParibus.html}{Ceteris Paribus Chapter}.
#'
#' @param x a aggregated profiles explainer produced with function `aggregate_profiles()`
#' @param ... other explainers that shall be plotted together
#' @param color a character.  Set line/bar color
#' @param size a numeric. Set width of lines
#' @param alpha a numeric between 0 and 1. Opacity of lines
#' @param only_numerical a logical. If TRUE then only numerical variables will be plotted. If FALSE then only selected variables will be plotted as bars.
#' @param facet_ncol number of columns for the `facet_wrap`
#' @param scale_plot a logical. Should size of the plot scale with window size? By default it's FALSE
#' @param variables if not NULL then only `variables` will be presented
#' @param chartTitle a character. Set custom title
#' @param label_margin a numeric. Set width of label margins, when only_numerical is FALSE
#'
#' @references Predictive Models: Visual Exploration, Explanation and Debugging \url{https://pbiecek.github.io/PM_VEE}
#'
#' @return a `r2d3` object.
#'
#' @examples
#'
#' library("DALEX")
#' library("ingredients")
#' library("randomForest")
#' titanic_small <- na.omit(titanic[1:500,-5])
#' model_titanic_rf <- randomForest(survived == "yes" ~ gender + age + embarked + class +
#'                                    fare + sibsp + parch,  data = titanic_small)
#'
#' explain_titanic_rf <- explain(model_titanic_rf,
#'                               data = titanic_small[,-8],
#'                               y = titanic$survived == "yes",
#'                               label = "Random Forest v7")
#'
#' selected_passangers <- select_sample(titanic_small, n = 100)
#' cp_rf <- ceteris_paribus(explain_titanic_rf, selected_passangers)
#'
#' pdp_rf_p <- aggregate_profiles(cp_rf, type = "partial", only_numerical = TRUE)
#' pdp_rf_p$`_label_` <- "RF_partial"
#' pdp_rf_c <- aggregate_profiles(cp_rf, type = "conditional", only_numerical = TRUE)
#' pdp_rf_c$`_label_` <- "RF_conditional"
#' pdp_rf_a <- aggregate_profiles(cp_rf, type = "accumulated", only_numerical = TRUE)
#' pdp_rf_a$`_label_` <- "RF_accumulated"
#'
#' plotD3(pdp_rf_p, pdp_rf_c, pdp_rf_a, only_numerical = TRUE)
#' plot(pdp_rf_p, pdp_rf_c, pdp_rf_a)
#'
#' pdp <- aggregate_profiles(cp_rf, type = "partial", only_numerical = FALSE)
#' pdp$`_label_` <- "RF_partial"
#'
#' plotD3(pdp, only_numerical = FALSE)
#'
#' @export
#' @rdname plotD3_aggregated_profiles
plotD3.aggregated_profiles_explainer <- function(x, ..., size = 2, alpha = 1,
                                                 color = "#46bac2",
                                                 only_numerical = TRUE,
                                                 facet_ncol = 2, scale_plot = FALSE,
                                                 variables = NULL, chartTitle = NULL,
                                                 label_margin = 60){

  # if there is more explainers, they should be merged into a single data frame
  dfl <- c(list(x), list(...))
  aggregated_profiles <- do.call(rbind, dfl)
  class(aggregated_profiles) <- "data.frame"

  all_variables <- na.omit(as.character(unique(aggregated_profiles$`_vname_`)))
  if (!is.null(variables)) {
    all_variables <- intersect(all_variables, variables)
    if (length(all_variables) == 0) stop(paste0("variables do not overlap with ", paste(all_variables, collapse = ", ")))
  }

  hl <- split(aggregated_profiles, f = as.character(aggregated_profiles$`_vname_`), drop = FALSE)

  # only numerical or only factor?
  is_numeric <- unlist(lapply(hl, function(x){
    is.numeric(x$`_x_`)
  }))

  if (only_numerical) {
    vnames <- names(which(is_numeric))

    if (length(vnames) == 0) {
      # but `variables` are selected, then change to factor
      if (length(variables) > 0) {
        only_numerical <- FALSE
        vnames <- variables
      } else {
        stop("There are no numerical variables")
      }
    }
  } else {
    vnames <- names(which(!is_numeric))
    # there are no numerical features
    if (length(vnames) == 0) stop("There are no non-numerical variables")
  }

  # prepare profiles data
  aggregated_profiles <- aggregated_profiles[aggregated_profiles$`_vname_` %in% vnames, ]
  aggregated_profiles$`_vname_` <- droplevels(aggregated_profiles$`_vname_`)
  rownames(aggregated_profiles) <- NULL

  yMax <- max(aggregated_profiles$`_yhat_`)
  yMin <- min(aggregated_profiles$`_yhat_`)
  yMargin <- abs(yMax - yMin)*0.1;

  aggregated_profiles_list <- split(aggregated_profiles, aggregated_profiles$`_vname_`)

  min_max_list <- y_mean <- list()

  # line plot or bar plot?
  if (only_numerical) {
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

  } else {
    if (length(dfl) > 1) stop("Please pick one aggregated profile.")

    aggregated_profiles_list <- lapply(aggregated_profiles_list, function(x){
      ret <- x[, c("_x_", "_yhat_", "_vname_", "_label_")]
      colnames(ret) <- c("xhat", "yhat", "vname", "label")
      ret$yhat <- as.numeric(ret$yhat)
      ret
    })

    y_mean <- round(attr(x, "mean_prediction"),3)
  }

  if (is.null(chartTitle)) chartTitle = paste("Ceteris Paribus Profiles")

  options <- list(variableNames = as.list(vnames),
                  n = length(vnames), c = length(list(...)) + 1,
                  yMax = yMax + yMargin, yMin = yMin - yMargin,
                  yMean = y_mean,
                  size = size, alpha = alpha, color = color,
                  onlyNumerical = only_numerical,
                  facetNcol = facet_ncol, scalePlot = scale_plot,
                  chartTitle = chartTitle, labelsMargin = label_margin)

  temp <- jsonlite::toJSON(list(aggregated_profiles_list, min_max_list))

  r2d3::r2d3(temp, system.file("d3js/aggregatedProfiles.js", package = "ingredients"),
             dependencies = list(
               system.file("d3js/colorsDrWhy.js", package = "ingredients"),
               system.file("d3js/tooltipD3.js", package = "ingredients")
             ),
             css = system.file("d3js/themeDrWhy.css", package = "ingredients"),
             d3_version = 4,
             options = options)

}
