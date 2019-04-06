#' Describe Ceteris Paribus Explainers.
#'
#' Function \code{describe.ceteris_paribus_explainer} generates text descriptions for Ceteris Paribus Profiles.
#' Note that currently only first observation is being described.
#'
#' @param x a ceteris paribus explainer produced with function `ceteris_paribus()`
#' @param ... other explainers that shall be plotted together
#'
#' @return an `ceteris_paribus_descriptions` object.
#' @importFrom stats median
#' @examples
#' library("DALEX")
#' # Toy examples, because CRAN angels ask for them
#' titanic <- na.omit(titanic)
#' model_titanic_glm <- glm(survived == "yes" ~ gender + age + fare,
#'                        data = titanic, family = "binomial")
#'
#' explain_titanic_glm <- explain(model_titanic_glm,
#'                            data = titanic[,-9],
#'                            y = titanic$survived == "yes")
#' cp_rf <- ceteris_paribus(explain_titanic_glm, titanic[1,])
#' cp_rf
#' describe(cp_rf)
#' plot(cp_rf)
#'
#'  \donttest{
#'  library("randomForest")
#'  model_titanic_rf <- randomForest(survived == "yes" ~ gender + age + class + embarked +
#'                                     fare + sibsp + parch,  data = titanic)
#'  model_titanic_rf
#'
#'  explain_titanic_rf <- explain(model_titanic_rf,
#'                            data = titanic[,-9],
#'                            y = titanic$survived == "yes",
#'                            label = "Random Forest v7")
#'
#' selected_passangers <- select_sample(titanic, n = 1)
#' cp_rf <- ceteris_paribus(explain_titanic_rf, selected_passangers)
#' cp_rf
#' describe(cp_rf)
#'
#' plot(cp_rf) +
#'   show_observations(cp_rf)
#'  }
#' @export
#' @rdname describe

describe <- function(x, ...)
  UseMethod("describe")

#' @export
#' @rdname describe
describe.ceteris_paribus_explainer <- function(x, ...) {
  # split ceteris_paribus_explainer into groups
  split_x <- split(x, x$`_vname_`)

  original_x <- attr(x, "observations")

  descriptions <- lapply(split_x, function(x_part) {
    vname <- as.character(head(x_part$`_vname_`, 1))

    if (class(x_part[,vname]) == "numeric") {
      cutpoint <- find_optimal_cutpoint(x_part$`_yhat_`)
      sufix <- describe_numeric_variable(original_x, x_part, cutpoint, vname)
    } else {
      sufix <- describe_factor_variable(original_x, x_part, vname)
    }

    # prefix
    prefix = paste0("For ",vname," = ", original_x[1, vname], " the model response is ", round(original_x[1, "_yhat_"], 2), "\n")

    paste(prefix, sufix)
  })

  class(descriptions) = "ceteris_paribus_descriptions"
  descriptions
}


find_optimal_cutpoint <- function(x) {
  diffs <- sapply(1:(length(x) - 1), function(cutpoint) {
    abs(mean(x[1:cutpoint]) - mean(x[-(1:cutpoint)]))
  })
  w <- pmin(seq_along(diffs) / length(diffs), 1 - seq_along(diffs) / length(diffs))
  which.max(diffs * w^0.25)
}

describe_factor_variable <- function(original_x, x_part, vname) {
  sorted_x_part <- x_part[order(x_part$`_yhat_`),]
  sorted_id <- head(which(sorted_x_part[,vname] == as.character(original_x[,vname])), 1)
  if (median(x_part$`_yhat_`) < head(original_x$`_yhat_`,1)) {
    # x is higher than average, show average
    levs <- rev(as.character(sorted_x_part[1:(sorted_id - 1), vname]))
    sufix <- paste0("Model results higher values for: ", paste(levs, collapse = ", "), " \n")
  } else {
    levs <- as.character(sorted_x_part[(sorted_id+1):nrow(sorted_x_part), vname])
    sufix <- paste0("Model results higher values for: ", paste(levs, collapse = ", "), " \n")
  }
  sufix
}

describe_numeric_variable <- function(original_x, x_part, cutpoint, vname) {
  # selected point is on the left from cutpoint
  if (original_x[1, vname] <= x_part[cutpoint, vname]) {
    # point is higher than the average
    if (mean(x_part[1:cutpoint, vname]) > mean(x_part[, vname])) {
      sufix = paste0("Model responses are *lower* for *higher* values of ", vname, " \n")
    } else {
      sufix = paste0("Model responses are *higher* for *higher* values of ", vname, " \n")
    }
  } else {
    # point is higher than the average
    if (mean(x_part[1:cutpoint, vname]) > mean(x_part[, vname])) {
      sufix = paste0("Model responses are *higher* for *lower* values of ", vname, " \n")
    } else {
      sufix = paste0("Model responses are *lower* for *lower* values of ", vname, " \n")
    }
  }
  sufix
}

