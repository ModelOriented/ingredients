#' @title Plots Ceteris Paribus Profiles in D3 with r2d3 Package.
#'
#' @description
#' Function \code{\link{plotD3.ceteris_paribus_explainer}} plots Individual Variable Profiles for selected observations.
#' It uses output from \code{\link{ceteris_paribus}} function.
#' Various parameters help to decide what should be plotted, profiles, aggregated profiles, points or rugs.
#'
#' Find more detailes in \href{https://pbiecek.github.io/PM_VEE/ceterisParibus.html}{Ceteris Paribus Chapter}.
#'
#' @param x a ceteris paribus explainer produced with function \code{ceteris_paribus()}
#' @param ... other explainers that shall be plotted together
#' @param color a character.  Set line color
#' @param size a numeric. Set width of lines
#' @param alpha a numeric between \code{0} and \code{1}. Opacity of lines
#' @param variable_type a character. If "numerical" then only numerical variables will be plotted.
#' If "categorical" then only categorical variables will be plotted.
#' @param facet_ncol number of columns for the \code{\link[ggplot2]{facet_wrap}}
#' @param scale_plot a logical. If \code{TRUE}, the height of plot scales with window size. By default it's \code{FALSE}
#' @param variables if not \code{NULL} then only \code{variables} will be presented
#' @param chart_title a character. Set custom title
#' @param label_margin a numeric. Set width of label margins in \code{categorical} type
#' @param show_observations a logical. Adds observations layer to a plot. By default it's \code{TRUE}
#' @param show_rugs a logical. Adds rugs layer to a plot. By default it's \code{TRUE}
#'
#' @return a \code{r2d3} object.
#'
#' @examples
#' library("DALEX")
#' library("randomForest")
#'
#' model_titanic_rf <- randomForest(survived ~.,  data = titanic_imputed)
#'
#' explain_titanic_rf <- explain(model_titanic_rf,
#'                               data = titanic_imputed[,-8],
#'                               y = titanic_imputed[,8],
#'                               label = "rf")
#'
#' selected_passangers <- select_sample(titanic_imputed, n = 10)
#' cp_rf <- ceteris_paribus(explain_titanic_rf, selected_passangers)
#'
#' plotD3(cp_rf, variables = c("age","parch","fare","sibsp"),
#'      facet_ncol = 2, scale_plot = TRUE)
#'
#' selected_passanger <- select_sample(titanic_imputed, n = 1)
#' cp_rf <- ceteris_paribus(explain_titanic_rf, selected_passanger)
#'
#' plotD3(cp_rf, variables = c("class", "embarked", "gender", "sibsp"),
#'      facet_ncol = 2, variable_type = "categorical", label_margin = 100, scale_plot = TRUE)
#'
#'
#' @export
#' @rdname plotD3_ceteris_paribus
plotD3 <- function(x, ...)
  UseMethod("plotD3")

#' @export
#' @rdname plotD3_ceteris_paribus
plotD3.ceteris_paribus_explainer <- function(x, ..., size = 2, alpha = 1,
                                             color = "#46bac2",
                                             variable_type = "numerical",
                                             facet_ncol = 2,
                                             scale_plot = FALSE,
                                             variables = NULL,
                                             chart_title = "Ceteris Paribus Profiles",
                                             label_margin = 60,
                                             show_observations = TRUE,
                                             show_rugs = TRUE) {

  check_variable_type(variable_type)

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

  # only numerical or only factor?
  is_numeric <- sapply(all_profiles[, all_variables, drop = FALSE], is.numeric)

  if (variable_type == "numerical") {
    vnames <- names(which(is_numeric))
    all_profiles$`_x_` <- 0

    # there are no numerical variables
    if (length(vnames) == 0) {
      # change to categorical
      variable_type <- "categorical"
      all_profiles$`_x_` <- ""
      # send message
      message("'variable_type' changed to 'categorical' due to lack of numerical variables.")
      # take all
      vnames <- all_variables
    } else if (length(vnames) != length(variables)) {
      message("Non-numerical variables (from the 'variables' argument) are rejected.")
    }
  } else {
    vnames <- names(which(!is_numeric))
    all_profiles$`_x_` <- ""

    # there are variables selected
    if (!is.null(variables)) {
      # take all
      vnames <- all_variables
    } else if (length(vnames) == 0) {
      # there were no variables selected and there are no categorical variables
      stop("There are no non-numerical variables.")
    }
  }

  # prepare clean observations data for tooltips
  all_observations <- list()

  all_observations <- lapply(dfl, function(tmp) {
      attr(tmp, "observations")
  })
  all_observations <- do.call(rbind, all_observations)
  m <- dim(all_observations)[2]
  colnames(all_observations) <- c(colnames(all_observations)[1:(m-3)],
                                  "yhat", "model", "observation.id")
  all_observations <- all_observations[,c(m,m-1,m-2,1:(m-3))]
  all_observations$observation.id <- rownames(all_observations)


  # prepare profiles data
  all_profiles <- all_profiles[all_profiles$`_vname_` %in% vnames, ]
  all_profiles$`_vname_` <- droplevels(all_profiles$`_vname_`)
  rownames(all_profiles) <- NULL

  ymax <- max(all_profiles$`_yhat_`)
  ymin <- min(all_profiles$`_yhat_`)
  ymargin <- abs(ymax-ymin)*0.1;

  all_profiles_list <- split(all_profiles, all_profiles$`_vname_`)

  min_max_list <- list()

  # line plot or bar plot?
  if (variable_type == "numerical") {
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

  options <- list(variableNames = as.list(vnames), n = length(vnames),
                  yMax = ymax + ymargin, yMin = ymin - ymargin,
                  size = size, alpha = alpha, color = color,
                  onlyNumerical = variable_type == "numerical",
                  facetNcol = facet_ncol, scalePlot = scale_plot,
                  chartTitle = chart_title, labelMargin = label_margin,
                  showObservations = show_observations, showRugs = show_rugs)

  temp <- jsonlite::toJSON(list(all_profiles_list, min_max_list, all_observations))

  r2d3::r2d3(temp, system.file("d3js/ceterisParibus.js", package = "ingredients"),
             dependencies = list(
               system.file("d3js/colorsDrWhy.js", package = "ingredients"),
               system.file("d3js/d3-tip.js", package = "ingredients"),
               system.file("d3js/hackHead.js", package = "ingredients")
               ),
             css = system.file("d3js/themeDrWhy.css", package = "ingredients"),
             d3_version = 4,
             options = options)
}
