#' Aggregate Ceteris Paribus into Partial Dependency Plots
#'
#' Function 'aggregate_profiles' calculates aggregate of ceteris paribus profiles
#'
#' @param x a ceteris paribus explainer produced with function `ceteris_paribus()`
#' @param ... other explainers that shall be plotted together
#' @param selected_variables if not NULL then only `selected_variables` will be presented
#' @param aggregate_function a function for profile aggregation. By default it's 'mean'
#' @param groups a variable name that will be usef for grouping. By default 'NULL' which means that no groups shall be calculated
#' @param only_numerical a logical. If TRUE then only numerical variables will be plotted. If FALSE then only categorical variables will be plotted.
#'
#' @importFrom stats na.omit quantile
#' @return an 'aggregated_ceteris_paribus_explainer' layer
#' @examples
#' library("DALEX")
#'  \dontrun{
#' library("titanic")
#' library("randomForest")
#'
#' titanic_small <- titanic_train[,c("Survived", "Pclass", "Sex", "Age",
#'                                    "SibSp", "Parch", "Fare", "Embarked")]
#' titanic_small$Survived <- factor(titanic_small$Survived)
#' titanic_small$Sex <- factor(titanic_small$Sex)
#' titanic_small$Embarked <- factor(titanic_small$Embarked)
#' titanic_small <- na.omit(titanic_small)
#' rf_model <- randomForest(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
#'                          data = titanic_small)
#' predict_fuction <- function(m,x) predict(m, x, type="prob")[,2]
#' explainer_rf <- explain(rf_model, data = titanic_small,
#'                         y = titanic_small$Survived == "1", label = "RF",
#'                         predict_function = predict_rf_fuction)
#'
#' selected_passangers <- select_sample(titanic_small, n = 100)
#' cp_rf <- ceteris_paribus(explainer_rf, selected_passangers)
#' cp_rf
#'
#' pdp_rf <- aggregate_profiles(cp_rf, selected_variables = "Age")
#' pdp_rf
#'
#' pdp_rf <- aggregate_profiles(cp_rf, selected_variables = "Age",
#'                              groups = "Sex")
#' pdp_rf
#' plot(cp_rf, selected_variables = "Age", color = "grey") +
#'   show_observations(cp_rf, selected_variables = "Age", color = "grey") +
#'   show_rugs(cp_rf, selected_variables = "Age", color = "red") +
#'   show_aggreagated_profiles(pdp_rf, size = 3, color = "_label_")
#' }
#' @export
aggregate_profiles <- function(x, ...,
                      aggregate_function = mean,
                      only_numerical = TRUE,
                      groups = NULL,
                      selected_variables = NULL) {
  # if there is more explainers, they should be merged into a single data frame
  dfl <- c(list(x), list(...))
  all_profiles <- do.call(rbind, dfl)
  class(all_profiles) <- "data.frame"

  all_profiles$`_ids_` <- factor(all_profiles$`_ids_`)

  # variables to use
  all_variables <- na.omit(as.character(unique(all_profiles$`_vname_`)))
  if (!is.null(selected_variables)) {
    all_variables <- intersect(all_variables, selected_variables)
    if (length(all_variables) == 0) stop(paste0("selected_variables do not overlap with ", paste(all_variables, collapse = ", ")))
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

  if (is.null(groups)) {
    tmp <- all_profiles[,c("_vname_", "_label_", "_x_", "_yhat_")]
    aggregated_profiles <- aggregate(tmp$`_yhat_`, by = list(tmp$`_vname_`, tmp$`_label_`, tmp$`_x_`), FUN = aggregate_function)
    colnames(aggregated_profiles) <- c("_vname_", "_label_", "_x_", "_yhat_")
  } else {
    tmp <- all_profiles[,c("_vname_", "_label_", "_x_", "_yhat_",groups)]
    aggregated_profiles <- aggregate(tmp$`_yhat_`, by = list(tmp$`_vname_`, tmp$`_label_`, tmp$`_x_`, tmp[,groups]), FUN = aggregate_function)
    colnames(aggregated_profiles) <- c("_vname_", "_label_", "_x_", "_groups_", "_yhat_")
    aggregated_profiles$`_label_` <- paste(aggregated_profiles$`_label_`, aggregated_profiles$`_groups_`, sep = "_")
  }
  aggregated_profiles$`_ids_` <- 0

  class(aggregated_profiles) = c("aggregated_ceteris_paribus_explainer", "data.frame")
  aggregated_profiles
}

