#' Aggregates Ceteris Paribus Profiles
#'
#' The function \code{aggregate_profiles()} calculates an aggregate of ceteris paribus profiles.
#' It can be: Partial Dependency Profile (average across Ceteris Paribus Profiles),
#' Conditional Dependency Profile (local weighted average across Ceteris Paribus Profiles) or
#' Accumulated Local Dependency Profile (cummulated average local changes in Ceteris Paribus Profiles).
#'
#' @param x a ceteris paribus explainer produced with function \code{ceteris_paribus()}
#' @param ... other explainers that shall be calculated together
#' @param variables if not \code{NULL} then aggregate only for selected \code{variables} will be calculated
#' @param type either \code{partial/conditional/accumulated} for parital dependence, conditional profiles of accumulated local effects
#' @param groups a variable name that will be used for grouping.
#' By default \code{NULL} which means that no groups shall be calculated
#' @param variable_type a character. If \code{numerical} then only numerical variables will be calculated.
#' If \code{categorical} then only categorical variables will be calculated.
#'
#' @references Predictive Models: Visual Exploration, Explanation and Debugging \url{https://pbiecek.github.io/PM_VEE}
#'
#' @importFrom stats na.omit quantile weighted.mean
#'
#' @return an object of the class \code{aggregated_profiles_explainer}
#'
#' @examples
#' library("DALEX")
#' library("randomForest")
#'
#' model_titanic_rf <- randomForest(survived ~ .,  data = titanic_imputed)
#'
#' explain_titanic_rf <- explain(model_titanic_rf,
#'                               data = titanic_imputed[,-8],
#'                               y = titanic_imputed[,8])
#'
#' selected_passangers <- select_sample(titanic_imputed, n = 100)
#' cp_rf <- ceteris_paribus(explain_titanic_rf, selected_passangers)
#' head(cp_rf)
#'
#' # continouse variable
#' pdp_rf_p <- aggregate_profiles(cp_rf, variables = "age", type = "partial")
#' pdp_rf_p$`_label_` <- "RF_partial"
#' pdp_rf_c <- aggregate_profiles(cp_rf, variables = "age", type = "conditional")
#' pdp_rf_c$`_label_` <- "RF_conditional"
#' pdp_rf_a <- aggregate_profiles(cp_rf, variables = "age", type = "accumulated")
#' pdp_rf_a$`_label_` <- "RF_accumulated"
#'
#' plot(pdp_rf_p, pdp_rf_c, pdp_rf_a, color = "_label_")
#'
#' \donttest{
#' pdp_rf <- aggregate_profiles(cp_rf, variables = "age",
#'                              groups = "gender")
#'
#' head(pdp_rf)
#' plot(cp_rf, variables = "age") +
#'   show_observations(cp_rf, variables = "age") +
#'   show_rugs(cp_rf, variables = "age", color = "red") +
#'   show_aggregated_profiles(pdp_rf, size = 3, color = "_label_")
#'
#' # categorical variable
#' pdp_rf_p <- aggregate_profiles(cp_rf, variables = "class",
#'                                variable_type = "categorical",  type = "partial")
#' pdp_rf_p$`_label_` <- "RF_partial"
#' pdp_rf_c <- aggregate_profiles(cp_rf, variables = "class",
#'                                variable_type = "categorical", type = "conditional")
#' pdp_rf_c$`_label_` <- "RF_conditional"
#' pdp_rf_a <- aggregate_profiles(cp_rf, variables = "class",
#'                                variable_type = "categorical", type = "accumulated")
#' pdp_rf_a$`_label_` <- "RF_accumulated"
#' plot(pdp_rf_p, pdp_rf_c, pdp_rf_a, color = "_label_")
#'
#' # or maybe flipped?
#' library(ggplot2)
#' plot(pdp_rf_p, pdp_rf_c, pdp_rf_a, color = "_label_") + coord_flip()
#'
#' pdp_rf <- aggregate_profiles(cp_rf, variables = "class", variable_type = "categorical",
#'                              groups = "gender")
#' head(pdp_rf)
#' plot(pdp_rf, variables = "class")
#' # or maybe flipped?
#' plot(pdp_rf, variables = "class") + coord_flip()
#'
#' }
#'
#' @export
#' @rdname aggregate_profiles
aggregate_profiles <- function(x, ...,
                               variable_type = "numerical",
                               groups = NULL,
                               type = "partial",
                               variables = NULL) {

  check_variable_type(variable_type)
  check_type(type)

  # if there is more ceteris paribuses, they should be merged into a single data frame
  dfl <- c(list(x), list(...))
  all_profiles <- do.call(rbind, dfl)
  class(all_profiles) <- "data.frame"

  all_profiles$`_ids_` <- factor(all_profiles$`_ids_`)

  # variables to use
  all_variables <- na.omit(as.character(unique(all_profiles$`_vname_`)))
  if (!is.null(variables)) {
    all_variables_intersect <- intersect(all_variables, variables)
    if (length(all_variables_intersect) == 0) stop(paste0("parameter variables do not overlap with ", paste(all_variables, collapse = ", ")))
    all_variables <- all_variables_intersect
  }
  # only numerical or only factors?
  is_numeric <- sapply(all_profiles[, all_variables, drop = FALSE], is.numeric)
  if (variable_type == "numerical") {
    vnames <- names(which(is_numeric))
    if (length(vnames) == 0) stop("There are no numerical variables")
    all_profiles$`_x_` <- 0
  } else {
    vnames <- names(which(!is_numeric))
    if (length(vnames) == 0) stop("There are no non-numerical variables")
    all_profiles$`_x_` <- ""
  }

  # select only suitable variables
  all_profiles <- all_profiles[all_profiles$`_vname_` %in% vnames, ]
  # create _x_
  tmp <- as.character(all_profiles$`_vname_`)
  for (viname in unique(tmp)) {
    all_profiles$`_x_`[tmp == viname] <- all_profiles[tmp == viname, viname]
  }

  if (class(all_profiles) != "data.frame") {
    all_profiles <- as.data.frame(all_profiles)
  }

  # change x column to proper character values
  if (variable_type == "categorical") {
    all_profiles$`_x_` <- as.character(apply(all_profiles, 1, function(x) x[x["_vname_"]]))
  }
  
  if (!is.null(groups) & ! groups %in% colnames(all_profiles)) {
    stop("groups parameter is not a name of any column")
  }

  # standard partial profiles
  # just average
  if (type == "partial") {
    aggregated_profiles <- aggregated_profiles_partial(all_profiles, groups)
    class(aggregated_profiles) <- c("aggregated_profiles_explainer",
                                    "partial_dependency_explainer", "data.frame")
  }
  if (type == "conditional") {
    aggregated_profiles <- aggregated_profiles_conditional(all_profiles, groups)
    class(aggregated_profiles) <- c("aggregated_profiles_explainer",
                                    "conditional_dependency_explainer", "data.frame")
  }
  if (type == "accumulated") {
    aggregated_profiles <- aggregated_profiles_accumulated(all_profiles, groups)
    class(aggregated_profiles) <- c("aggregated_profiles_explainer",
                                    "accumulated_dependency_explainer", "data.frame")
  }

  # calculate mean(all observation's _yhat_), mean of prediction
  attr(aggregated_profiles, "mean_prediction") <-
    mean(do.call(rbind, lapply(dfl, function(x){ attr(x, "observation")}))$`_yhat_`)

  aggregated_profiles
}


aggregated_profiles_accumulated <- function(all_profiles, groups = NULL) {
  observations <- attr(all_profiles, "observations")
  # just initialisation
  if (is.numeric(all_profiles$`_x_`)) {
    all_profiles$`_orginal_` <- 0
    for (i in 1:nrow(all_profiles)) {
      all_profiles$`_orginal_`[i] <- observations[as.character(all_profiles$`_ids_`[i]) ,
                                                  as.character(all_profiles$`_vname_`[i])]
    }
  } else {
    all_profiles$`_orginal_` <- ""
    for (i in 1:nrow(all_profiles)) {
      all_profiles$`_orginal_`[i] <- as.character(observations[as.character(all_profiles$`_ids_`[i]) ,
                                                               as.character(all_profiles$`_vname_`[i])])
    }
  }

  # split all_profiles into groups
  if (is.null(groups)) {
    tmp <- all_profiles[,c("_vname_", "_label_", "_x_", "_yhat_", "_ids_","_orginal_")]
  } else {
    tmp <- all_profiles[,c("_vname_", "_label_", "_x_", "_yhat_",groups)]
  }

    split_profiles <- split(tmp, tmp[,c("_vname_", "_label_")])

    # calculate for each group
    chunks <- lapply(split_profiles, function(split_profile) {
      if (nrow(split_profile) == 0) return(NULL)

      if (is.numeric(split_profile$`_x_`)) {
        # for continuous variables we will calculate weighted average
        # where weights depends on square distance between points
        diffs <- (split_profile$`_orginal_` - split_profile$`_x_`)^2
        diffsd <- sqrt(mean(diffs^2))
        split_profile$`_w_` <- diffs/ifelse(diffsd > 0, diffsd, 1)
      } else {
        # for categorical variables we will calculate weighted average
        # but weights are 0-1, 1 if it's the same level and 0 otherwise
        split_profile$`_w_` <- split_profile$`_orginal_` == split_profile$`_x_`
      }

      #  diffs
      per_points <- split(split_profile, split_profile$`_ids_`)
      chunks <- lapply(per_points, function(per_point) {
        per_point$`_yhat_` <- c(0, diff(per_point$`_yhat_`))
        per_point
      })
      split_profile <- do.call(rbind, chunks)

      # weighed means
      per_points <- split(split_profile, split_profile[, c("_x_", groups)])
      chunks <- lapply(per_points, function(per_point) {
        avg <- weighted.mean(per_point$`_yhat_`, w = per_point$`_w_`)
        res <- per_point[1, c("_vname_", "_label_", "_x_", "_yhat_", groups)]
        # NaN occurs when all weights are 0 , #43
        res$`_yhat_` <- ifelse(is.nan(avg), 0, avg)
        res
      })
      par_profile <- do.call(rbind, chunks)

      # postprocessing
      if (is.null(groups)) {
        par_profile$`_yhat_` <- cumsum(par_profile$`_yhat_`)
      } else {
        # cumsum per group
        per_points <- split(split_profile, split_profile[, groups])
        chunks <- lapply(per_points, function(per_point) {
          per_point$`_yhat_` <- cumsum(per_point$`_yhat_`)
          per_point
        })

        par_profile <- do.call(rbind, per_points)
      }

      par_profile
    })
    aggregated_profiles <- do.call(rbind, chunks)

    # postprocessing
    if (!is.null(groups)) {
      colnames(aggregated_profiles)[5] = "_groups_"
    }

  aggregated_profiles$`_ids_` <- 0
  aggregated_profiles
}


aggregated_profiles_partial <- function(all_profiles, groups = NULL) {

  if (is.null(groups)) {
    tmp <- all_profiles[,c("_vname_", "_label_", "_x_", "_yhat_")]
    aggregated_profiles <- aggregate(tmp$`_yhat_`, by = list(tmp$`_vname_`, tmp$`_label_`, tmp$`_x_`), FUN = mean)
    colnames(aggregated_profiles) <- c("_vname_", "_label_", "_x_", "_yhat_")
  } else {
    tmp <- all_profiles[,c("_vname_", "_label_", "_x_", "_yhat_",groups)]
    aggregated_profiles <- aggregate(tmp$`_yhat_`, by = list(tmp$`_vname_`, tmp$`_label_`, tmp$`_x_`, tmp[,groups]), FUN = mean)
    colnames(aggregated_profiles) <- c("_vname_", "_label_", "_x_", "_groups_", "_yhat_")
    aggregated_profiles$`_label_` <- paste(aggregated_profiles$`_label_`, aggregated_profiles$`_groups_`, sep = "_")
  }
  aggregated_profiles$`_ids_` <- 0

  aggregated_profiles
}

aggregated_profiles_conditional <- function(all_profiles, groups = NULL) {

  observations <- attr(all_profiles, "observations")
  # just initialisation
  if (is.numeric(all_profiles$`_x_`)) {
    all_profiles$`_orginal_` <- 0
    for (i in 1:nrow(all_profiles)) {
      all_profiles$`_orginal_`[i] <- observations[as.character(all_profiles$`_ids_`[i]) ,
                                                  as.character(all_profiles$`_vname_`[i])]
    }
  } else {
    all_profiles$`_orginal_` <- ""
    for (i in 1:nrow(all_profiles)) {
      all_profiles$`_orginal_`[i] <- as.character(observations[as.character(all_profiles$`_ids_`[i]) ,
                                                               as.character(all_profiles$`_vname_`[i])])
    }
  }

  # split all_profiles into groups
  if (is.null(groups)) {
    tmp <- all_profiles[,c("_vname_", "_label_", "_x_", "_yhat_", "_ids_","_orginal_")]
  } else {
    tmp <- all_profiles[,c("_vname_", "_label_", "_x_", "_yhat_",groups)]
  }

  split_profiles <- split(tmp, tmp[,c("_vname_", "_label_")])

  # calculate for each group
  chunks <- lapply(split_profiles, function(split_profile) {
      if (nrow(split_profile) == 0) return(NULL)

      if (is.numeric(split_profile$`_x_`)) {
        # for continuous variables we will calculate weighted average
        # where weights depends on square distance between points
        diffs <- (split_profile$`_orginal_` - split_profile$`_x_`)^2
        diffsd <- sqrt(mean(diffs^2))
        split_profile$`_w_` <- diffs/ifelse(diffsd > 0, diffsd, 1)
      } else {
        # for categorical variables we will calculate weighted average
        # but weights are 0-1, 1 if it's the same level and 0 otherwise
        split_profile$`_w_` <- split_profile$`_orginal_` == split_profile$`_x_`
      }

      per_points <- split(split_profile, split_profile[, c("_x_", groups)])

      chunks <- lapply(per_points, function(per_point) {
        avg <- weighted.mean(per_point$`_yhat_`, w = per_point$`_w_`)
        res <- per_point[1, c("_vname_", "_label_", "_x_", "_yhat_", groups)]
        # NaN occurs when all weights are 0 , #43
        res$`_yhat_` <- ifelse(is.nan(avg), 0, avg)
        res
      })
      do.call(rbind, chunks)
    })
    aggregated_profiles <- do.call(rbind, chunks)

    if (!is.null(groups)) {
      colnames(aggregated_profiles)[5] = "_groups_"
    }

  aggregated_profiles$`_ids_` <- 0

  aggregated_profiles
}


#'@noRd
#'@title Check if variable_type is "numerical" or "categorical"
#'
#'@param variable_type a character
#'
check_variable_type <- function(variable_type) {
  if (!(variable_type %in% c("numerical", "categorical")))
    stop("variable_type needs to be 'numerical' or 'categorical'")
}

#'@noRd
#'@title Check if type is partial/conditional/accumulated
#'
#'@param type a character
#'
check_type <- function(type) {
  if (!(type %in% c("partial", "conditional", "accumulated")))
    stop("type needs to be 'partial', 'conditional' or 'accumulated'")
}
