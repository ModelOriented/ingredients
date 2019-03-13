#' Cluster Ceteris Paribus
#'
#' Function 'cluster_profiles' calculates aggregate of ceteris paribus profiles
#'
#' @param x a ceteris paribus explainer produced with function `ceteris_paribus()`
#' @param ... other explainers that shall be plotted together
#' @param variables if not NULL then only `variables` will be presented
#' @param k number of clusters for the hclust function
#' @param center shall profiles be centered before clustering
#' @param aggregate_function a function for profile aggregation. By default it's 'mean'
#' @param only_numerical a logical. If TRUE then only numerical variables will be plotted. If FALSE then only categorical variables will be plotted.
#'
#' @importFrom stats as.dist cutree hclust
#' @return a 'aggregated_ceteris_paribus_explainer' layer
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
#' pdp_rf <- aggregate_profiles(cp_rf, variables = "Age")
#' pdp_rf
#' clust_rf <- cluster_profiles(cp_rf, k = 3, variables = "Age")
#' clust_rf
#'
#' plot(clust_rf, color = "_label_") +
#'   show_aggreagated_profiles(pdp_rf, color = "black", size = 3)
#'
#' plot(cp_rf, color = "grey", variables = "Age") +
#'   show_aggreagated_profiles(clust_rf, color = "_label_", size = 2)
#'
#' clust_rf <- cluster_profiles(cp_rf, k = 3, center = TRUE, variables = "Age")
#' clust_rf
#' }
#' @export
cluster_profiles <- function(x, ...,
                               aggregate_function = mean,
                               only_numerical = TRUE,
                               center = FALSE,
                               k = 3,
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

  ## clustering
  tmp <- all_profiles[,c("_ids_", "_vname_", "_label_", "_x_", "_yhat_")]
  tmp <- tmp[order(tmp$`_vname_`, tmp$`_x_`, tmp$`_label_`),]
  tmp_list <- split(tmp, tmp$`_ids_`)
  ids <- unique(tmp$`_ids_`)

  if (length(ids) <= k) stop("k larger than the number of different profiles")

  dist_mat <- matrix(0, length(ids), length(ids))
  if (length(ids) > 1) {
    for (i in 1:(length(ids) - 1)) {
      for (j in (i + 1):length(ids)) {
        yhi <- tmp_list[[i]]$`_yhat_`
        yhj <- tmp_list[[j]]$`_yhat_`
        if (center) {
          yhi <- yhi - mean(yhi)
          yhj <- yhj - mean(yhj)
        }
        dist_mat[i,j] <- sqrt(mean((yhi - yhj)^2))
      }
    }
  }
  dist_mat <- dist_mat + t(dist_mat)
  clus <- cutree(hclust(as.dist(dist_mat), method = "ward.D2"), k = k)
  names(clus) <- names(tmp_list)
  tmp$clust <- clus[as.character(tmp$`_ids_`)]
  ##

  aggregated_profiles <- aggregate(tmp$`_yhat_`, by = list(tmp$`_vname_`, tmp$`_label_`, tmp$`_x_`, tmp$clust), FUN = aggregate_function)
  colnames(aggregated_profiles) <- c("_vname_", "_label_", "_x_", "_cluster_", "_yhat_")
  aggregated_profiles$`_label_` <- paste(aggregated_profiles$`_label_`, aggregated_profiles$`_cluster_`, sep = "_")
  aggregated_profiles$`_ids_` <- 0

  class(aggregated_profiles) = c("aggregated_ceteris_paribus_explainer", "data.frame")
  aggregated_profiles
}
