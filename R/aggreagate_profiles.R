#' Aggregate Ceteris Paribus Profiles
#'
#' The function 'aggregate_profiles' calculates an aggregate of ceteris paribus profiles.
#' It can be: Partial Dependency Profile (average across Ceteris Paribus Profiles),
#' Conditional Dependency Profile (local weighted average across Ceteris Paribus Profiles) or
#' Accumulated Local Dependency Profile (cummulated average local changes in Ceteris Paribus Profiles).
#'
#' @param x a ceteris paribus explainer produced with function `ceteris_paribus()`
#' @param ... other explainers that shall be plotted together
#' @param variables if not NULL then only `variables` will be presented
#' @param type either 'partial'/'conditional'/'accumulated' for parital dependence, conditional profiles of accumulated local effects
#' @param groups a variable name that will be usef for grouping. By default 'NULL' which means that no groups shall be calculated
#' @param only_numerical a logical. If TRUE then only numerical variables will be plotted. If FALSE then only categorical variables will be plotted.
#'
#' @references Predictive Models: Visual Exploration, Explanation and Debugging \url{https://pbiecek.github.io/PM_VEE}
#'
#' @importFrom stats na.omit quantile weighted.mean
#' @return an 'aggregated_profiles_explainer' layer
#' @examples
#' library("DALEX")
#'  \donttest{
#' library("randomForest")
#'  model_titanic_rf <- randomForest(survived ~ gender + age + class + embarked +
#'                                     fare + sibsp + parch,  data = titanic)
#'  model_titanic_rf
#'
#'  explain_titanic_rf <- explain(model_titanic_rf,
#'                            data = titanic[,-9],
#'                            y = titanic$survived)
#'
#' selected_passangers <- select_sample(titanic, n = 100)
#' cp_rf <- ceteris_paribus(explain_titanic_rf, selected_passangers)
#' head(cp_rf)
#'
#' pdp_rf_p <- aggregate_profiles(cp_rf, variables = "age", type = "partial")
#' pdp_rf_p$`_label_` <- "RF_partial"
#' pdp_rf_c <- aggregate_profiles(cp_rf, variables = "age", type = "conditional")
#' pdp_rf_c$`_label_` <- "RF_conditional"
#' pdp_rf_a <- aggregate_profiles(cp_rf, variables = "age", type = "accumulated")
#' pdp_rf_a$`_label_` <- "RF_accumulated"
#' plot(pdp_rf_p, pdp_rf_c, pdp_rf_a, color = "_label_")
#'
#' pdp_rf <- aggregate_profiles(cp_rf, variables = "age",
#'                              groups = "gender")
#' head(pdp_rf)
#' plot(cp_rf, variables = "age") +
#'   show_observations(cp_rf, variables = "age") +
#'   show_rugs(cp_rf, variables = "age", color = "red") +
#'   show_aggregated_profiles(pdp_rf, size = 3, color = "_label_")
#' }
#' @export
aggregate_profiles <- function(x, ...,
                      only_numerical = TRUE,
                      groups = NULL,
                      type = 'partial',
                      variables = NULL) {
  # if there is more explainers, they should be merged into a single data frame
  dfl <- c(list(x), list(...))
  all_profiles <- do.call(rbind, dfl)
  class(all_profiles) <- "data.frame"

  all_profiles$`_ids_` <- factor(all_profiles$`_ids_`)

  # variables to use
  all_variables <- na.omit(as.character(unique(all_profiles$`_vname_`)))
  if (!is.null(variables)) {
    all_variables <- intersect(all_variables, variables)
    if (length(all_variables) == 0) stop(paste0("variables do not overlap with ", paste(all_variables, collapse = ", ")))
  }
  # only numerical or only factors?
  is_numeric <- sapply(all_profiles[, all_variables, drop = FALSE], is.numeric)
  if (only_numerical) {
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
  for (i in seq_along(tmp)) {
    all_profiles$`_x_`[i] <- all_profiles[i, tmp[i]]
  }

  #
  # stardard partial profiles
  # just average
  if (type == 'partial')
    aggregated_profiles <- aggregated_profiles_partial(all_profiles, groups)
  if (type == 'conditional')
    aggregated_profiles <- aggregated_profiles_conditional(all_profiles, groups)
  if (type == 'accumulated')
    aggregated_profiles <- aggregated_profiles_accumulated(all_profiles, groups)

  class(aggregated_profiles) = c("aggregated_profiles_explainer", "data.frame")
  aggregated_profiles
}

aggregated_profiles_accumulated <- function(all_profiles, groups = NULL) {

  all_profiles$`_orginal_` <- 0
  observations <- attr(all_profiles, "observations")
  for (i in 1:nrow(all_profiles)) {
    all_profiles$`_orginal_`[i] <- observations[as.character(all_profiles$`_ids_`[i]) ,
                                                as.character(all_profiles$`_vname_`[i])]
  }

  # split all_profiles into groups
  if (is.null(groups)) {
    tmp <- all_profiles[,c("_vname_", "_label_", "_x_", "_yhat_", "_ids_","_orginal_")]
    split_profiles <- split(tmp, tmp[,c("_vname_", "_label_")])

    # calculate for each group
    chunks <- lapply(split_profiles, function(split_profile) {
      if (nrow(split_profile) == 0) return(NULL)

      diffs <- (split_profile$`_orginal_` - split_profile$`_x_`)^2
      diffsd <- sqrt(mean(diffs^2))

      split_profile$`_w_` <- diffs/ifelse(diffsd > 0, diffsd, 1)

      #  diffs
      per_points <- split(split_profile, split_profile$`_ids_`)
      chunks <- lapply(per_points, function(per_point) {
        per_point$`_yhat_` <- c(0, diff(per_point$`_yhat_`))
        per_point
      })
      split_profile <- do.call(rbind, chunks)

      # weighed means
      per_points <- split(split_profile, split_profile$`_x_`)
      chunks <- lapply(per_points, function(per_point) {
        avg <- weighted.mean(per_point$`_yhat_`, w = per_point$`_w_`)
        res <- per_point[1, c("_vname_", "_label_", "_x_", "_yhat_")]
        res$`_yhat_` <- avg
        res
      })
      par_profile <- do.call(rbind, chunks)
      par_profile$`_yhat_` <- cumsum(par_profile$`_yhat_`)
      par_profile
    })
    aggregated_profiles <- do.call(rbind, chunks)

  } else {
    tmp <- all_profiles[,c("_vname_", "_label_", "_x_", "_yhat_",groups)]
    split_profiles <- split(tmp, tmp[,c("_vname_", "_label_")])

    # calculate for each group
    chunks <- lapply(split_profiles, function(split_profile) {
      if (nrow(split_profile) == 0) return(NULL)

      diffs <- (split_profile$`_orginal_` - split_profile$`_x_`)^2
      diffsd <- sqrt(mean(diffs^2))

      split_profile$`_w_` <- diffs/ifelse(diffsd > 0, diffsd, 1)

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
        res$`_yhat_` <- avg
        res
      })

      par_profile <- do.call(rbind, chunks)
      # cumsum per group
      per_points <- split(split_profile, split_profile[, groups])
      chunks <- lapply(per_points, function(per_point) {
        per_point$`_yhat_` <- cumsum(per_point$`_yhat_`)
        per_point
      })

      par_profile <- do.call(rbind, per_points)
      par_profile
    })
    aggregated_profiles <- do.call(rbind, chunks)
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
  all_profiles$`_orginal_` <- 0
  observations <- attr(all_profiles, "observations")
  for (i in 1:nrow(all_profiles)) {
    all_profiles$`_orginal_`[i] <- observations[as.character(all_profiles$`_ids_`[i]) ,
                                                as.character(all_profiles$`_vname_`[i])]
  }

  # split all_profiles into groups
  if (is.null(groups)) {
    tmp <- all_profiles[,c("_vname_", "_label_", "_x_", "_yhat_", "_ids_","_orginal_")]
    split_profiles <- split(tmp, tmp[,c("_vname_", "_label_")])

    # calculate for each group
    chunks <- lapply(split_profiles, function(split_profile) {
      if (nrow(split_profile) == 0) return(NULL)

      diffs <- (split_profile$`_orginal_` - split_profile$`_x_`)^2
      diffsd <- sqrt(mean(diffs^2))

      split_profile$`_w_` <- diffs/ifelse(diffsd > 0, diffsd, 1)

      per_points <- split(split_profile, split_profile$`_x_`)
      chunks <- lapply(per_points, function(per_point) {
        avg <- weighted.mean(per_point$`_yhat_`, w = per_point$`_w_`)
        res <- per_point[1, c("_vname_", "_label_", "_x_", "_yhat_")]
        res$`_yhat_` <- avg
        res
      })
      do.call(rbind, chunks)
    })
    aggregated_profiles <- do.call(rbind, chunks)

  } else {
    tmp <- all_profiles[,c("_vname_", "_label_", "_x_", "_yhat_",groups)]
    split_profiles <- split(tmp, tmp[,c("_vname_", "_label_")])

    # calculate for each group
    chunks <- lapply(split_profiles, function(split_profile) {
      if (nrow(split_profile) == 0) return(NULL)

      diffs <- (split_profile$`_orginal_` - split_profile$`_x_`)^2
      diffsd <- sqrt(mean(diffs^2))

      split_profile$`_w_` <- diffs/ifelse(diffsd > 0, diffsd, 1)

      per_points <- split(split_profile, split_profile[, c("_x_", groups)])
      chunks <- lapply(per_points, function(per_point) {
        avg <- weighted.mean(per_point$`_yhat_`, w = per_point$`_w_`)
        res <- per_point[1, c("_vname_", "_label_", "_x_", "_yhat_", groups)]
        res$`_yhat_` <- avg
        res
      })
      do.call(rbind, chunks)
    })
    aggregated_profiles <- do.call(rbind, chunks)
    colnames(aggregated_profiles)[5] = "_groups_"
  }
  aggregated_profiles$`_ids_` <- 0
  aggregated_profiles
}

