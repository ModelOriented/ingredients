#' Describe Ceteris Paribus Explainers.
#'
#' Function \code{describe.ceteris_paribus_explainer} generates text descriptions for Ceteris Paribus Profiles.
#' Note that currently only first observation is being described.
#'
#' @param x a ceteris paribus explainer produced with function `ceteris_paribus()`
#' @param ... other explainers that shall be plotted together
#'
#' @return an `ceteris_paribus_descriptions` object.
#'
#' @examples
#' \dontrun{
#' }
#' @export
#' @rdname describe

describe <- function(x, ...)
  UseMethod("describe")

#' @export
#' @rdname describe
describe.ceteris_paribus_explainer <- function(x, ...) {
  # split ceteris_paribus_explainer into groups
  split_x <- split(x, x$`_vname_`)

  original_x <- attr(sp_rf, "observations")
  original_yhat <- original_x$`_yhat_`

  descriptions <- lapply(split_x, function(x_part) {
    vname <- as.character(head(x_part$`_vname_`, 1))

    if (class(x_part[,vname]) == "numeric") {
      cutpoint <- find_optimal_cutpoint( x_part$`_yhat_`)
      sufix <- describe_numeric_variable(original_x, x_part, cutpoint, vname)
    } else {
      sufix <- ""
    }

    # prefix
    prefix = paste0("For ",vname," = ", original_x[1, vname], " the model response is ", round(original_x[1, "_yhat_"], 2), "\n")

    paste(prefix, sufix)
  })

  descriptions
}


find_optimal_cutpoint <- function(x) {
  diffs <- sapply(1:(length(x)-1), function(cutpoint) {
    abs(mean(x[1:cutpoint]) - mean(x[-(1:cutpoint)]))
  })
  w <- pmin(seq_along(diffs) / length(diffs), 1 - seq_along(diffs) / length(diffs))
  which.max(diffs * w^0.25)
}

describe_factor_variable <- function() {

}

describe_numeric_variable <- function(original_x, x_part, cutpoint, vname) {
  # selected point is on the left from cutpoint
  if (original_x[1, vname] <= x_part[cutpoint, vname]) {
    # point is higher than the average
    if (mean(x_part[1:cutpoint, vname]) > mean(x_part[, vname])) {
      sufix = paste0("For higher values of ", vname, " model responses are lower\n")
    } else {
      sufix = paste0("For higher values of ", vname, " model responses are higher\n")
    }
  } else {
    # point is higher than the average
    if (mean(x_part[1:cutpoint, vname]) > mean(x_part[, vname])) {
      sufix = paste0("For lower values of ", vname, " model responses are higher\n")
    } else {
      sufix = paste0("For lower values of ", vname, " model responses are lower\n")
    }
  }
  sufix
}

