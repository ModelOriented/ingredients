#' @title Plots Ceteris Paribus Profiles in D3 with r2d3 Package.
#'
#' @description
#' Function 'plotD3.ceteris_paribus_explainer' plots Individual Variable Profiles for selected observations.
#' Various parameters help to decide what should be plotted, profiles, aggregated profiles, points or rugs.
#'
#' Find more detailes in \href{https://pbiecek.github.io/PM_VEE/ceterisParibus.html}{Ceteris Paribus Chapter}.
#'
#' @param x a ceteris paribus explainer produced with function `ceteris_paribus()`
#' @param ... other explainers that shall be plotted together
#' @param color a character.  Set line color
#' @param size a numeric. Set width of lines
#' @param alpha a numeric between 0 and 1. Opacity of lines
#' @param only_numerical a logical. If TRUE then only numerical variables will be plotted. If FALSE then only selected variables will be plotted as bars.
#' @param facet_ncol number of columns for the `facet_wrap`
#' @param scale_plot a logical. Should size of the plot scale with window size? By default it's FALSE
#' @param variables if not NULL then only `variables` will be presented
#' @param chartTitle a character. Set custom title
#' @param label_margin a numeric. Set width of label margins, when only_numerical is FALSE
#' @param show_observations a logcal. Adds observations layer to a plot. By default it's TRUE
#' @param show_rugs a logcal. Adds rugs layer to a plot. By default it's TRUE
#'
#' @return an `r2d3` object.
#'
#' @examples
#' \dontrun{
#' library("DALEX")
#' library("ingredients")
#' library("randomForest")
#'
#' titanic <- na.omit(titanic)
#'
#' model_titanic_rf <- randomForest(survived == "yes" ~ gender + age + class + embarked +
#'                                  fare + sibsp + parch,  data = titanic)
#' explain_titanic_rf <- explain(model_titanic_rf,
#'                              data = titanic[,-9],
#'                              y = titanic$survived == "yes",
#'                              label = "rf")
#'
#' selected_passangers <- select_sample(titanic, n = 10)
#' cp_rf <- ceteris_paribus(explain_titanic_rf, selected_passangers)
#'
#' plotD3(cp_rf, variables = c("age","parch","fare","sibsp"),
#'      facet_ncol = 2, scale_plot = TRUE)
#'
#' selected_passanger <- select_sample(titanic, n = 1)
#' cp_rf <- ceteris_paribus(explain_titanic_rf, selected_passanger)
#'
#' plotD3(cp_rf, variables = c("class", "embarked", "gender", "sibsp"),
#'      facet_ncol = 2, only_numerical = FALSE, label_margin = 100, scale_plot = TRUE)
#' }
#'
#' @export
#' @rdname plotD3_ceteris_paribus
plotD3 <- function(x, ...)
  UseMethod("plotD3")

#' @export
#' @rdname plotD3_ceteris_paribus
plotD3.ceteris_paribus_explainer <- function(x, ..., size = 2, alpha = 1,
                                 color = "#46bac2", only_numerical = TRUE,
                                 facet_ncol = 2, scale_plot = FALSE,
                                 variables = NULL, chartTitle = NULL, label_margin = 60,
                                 show_observations = TRUE, show_rugs = TRUE) {

  # if there is more explainers, they should be merged into a single data frame
  dfl <- c(list(x), list(...))
  all_profiles <- do.call(rbind, dfl)
  class(all_profiles) <- "data.frame"

  # variables to use
  all_variables <- na.omit(as.character(unique(all_profiles$`_vname_`)))
  if (!is.null(variables)) {
    all_variables <- intersect(all_variables, variables)
    if (length(all_variables) == 0) stop(paste0("variables do not overlap with ", paste(all_variables, collapse = ", ")))
  }

  # is color a variable or literal?
  # is_color_a_variable <- color %in% c(all_variables, "_label_")

  # only numerical or only factor?
  is_numeric <- sapply(all_profiles[, all_variables, drop = FALSE], is.numeric)

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

  # prepare clean observations data for tooltips
  all_observations <- list()

  all_observations <- lapply(dfl, function(tmp) {
      attr(tmp, "observations")
  })
  all_observations <- do.call(rbind, all_observations)
  m <- dim(all_observations)[2]
  colnames(all_observations) <- c(colnames(all_observations)[1:(m-3)], "yhat","model","observation.id")
  all_observations <- all_observations[,c(m,m-1,m-2,1:(m-3))]
  all_observations$observation.id <- rownames(all_observations)


  # prepare profiles data
  all_profiles <- all_profiles[all_profiles$`_vname_` %in% vnames, ]
  all_profiles$`_vname_` <- droplevels(all_profiles$`_vname_`)
  rownames(all_profiles) <- NULL

  yMax <- max(all_profiles$`_yhat_`)
  yMin <- min(all_profiles$`_yhat_`)
  yMargin <- abs(yMax - yMin)*0.1;

  all_profiles_list <- split(all_profiles, all_profiles$`_vname_`)

  min_max_list <- list()

  # line plot or bar plot?
  if (only_numerical) {
    all_profiles_list <- lapply(all_profiles_list, function(x){
      name <- as.character(head(x$`_vname_`,1))
      ret <- x[, c(name, "_yhat_", "_ids_", "_vname_")]
      colnames(ret) <- c("xhat", "yhat", "id", "vname")
      ret$xhat <- as.numeric(ret$xhat)
      ret$yhat <- as.numeric(ret$yhat)
      ret[order(ret$xhat),]
    })

    min_max_list <- lapply(all_profiles_list, function(df){
      list(max(df$xhat), min(df$xhat))
    })

    all_profiles_list <- lapply(all_profiles_list, function(x){
      split(x, f = x$id)
    })

  } else {
    if (dim(attr(x, "observations"))[1] > 1) stop("Please pick one observation.")

    all_profiles_list <- lapply(all_profiles_list, function(x){
      name <- as.character(head(x$`_vname_`,1))
      ret <- x[, c(name, "_yhat_", "_vname_")]
      colnames(ret) <- c("xhat", "yhat", "vname")
      ret$yhat <- as.numeric(ret$yhat)
      ret
    })
  }

  if (is.null(chartTitle)) chartTitle = paste("Ceteris Paribus Profiles")

  options <- list(variableNames = as.list(vnames), n = length(vnames),
                  yMax = yMax + yMargin, yMin = yMin - yMargin,
                  size = size, alpha = alpha, color = color,
                  onlyNumerical = only_numerical,
                  facetNcol = facet_ncol, scalePlot = scale_plot,
                  chartTitle = chartTitle, labelsMargin = label_margin,
                  showObservations = show_observations, showRugs = show_rugs)

  temp <- jsonlite::toJSON(list(all_profiles_list, min_max_list, all_observations))

  r2d3::r2d3(temp, system.file("d3js/ceterisParibus.js", package = "ingredients"),
             dependencies = list(
               system.file("d3js/colorsDrWhy.js", package = "ingredients"),
               system.file("d3js/tooltipD3.js", package = "ingredients")
               ),
             css = system.file("d3js/themeDrWhy.css", package = "ingredients"),
             d3_version = 4,
             options = options)
}
