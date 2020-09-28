#' Cluster Ceteris Paribus Profiles
#'
#' This function calculates aggregates of ceteris paribus profiles based on
#' hierarchical clustering.
#'
#' Find more detailes in the \href{https://pbiecek.github.io/ema/partialDependenceProfiles.html}{Clustering Profiles Chapter}.
#'
#' @param x a ceteris paribus explainer produced with function \code{ceteris_paribus()}
#' @param ... other explainers that shall be plotted together
#' @param variables if not \code{NULL} then only \code{variables} will be presented
#' @param k number of clusters for the hclust function
#' @param center shall profiles be centered before clustering
#' @param aggregate_function a function for profile aggregation. By default it's \code{mean}
#' @param variable_type a character. If \code{numerical} then only numerical variables will be computed.
#' If \code{categorical} then only categorical variables will be computed.
#'
#' @references Explanatory Model Analysis. Explore, Explain, and Examine Predictive Models. \url{https://pbiecek.github.io/ema/}
#'
#' @importFrom stats as.dist cutree hclust
#'
#' @return an object of the class \code{aggregated_profiles_explainer}
#'
#' @examples
#' library("DALEX")
#' library("ingredients")
#'
#' selected_passangers <- select_sample(titanic_imputed, n = 100)
#' model_titanic_glm <- glm(survived ~ gender + age + fare,
#'                          data = titanic_imputed, family = "binomial")
#'
#' explain_titanic_glm <- explain(model_titanic_glm,
#'                                data = titanic_imputed[,-8],
#'                                y = titanic_imputed[,8])
#'
#' cp_rf <- ceteris_paribus(explain_titanic_glm, selected_passangers)
#' clust_rf <- cluster_profiles(cp_rf, k = 3, variables = "age")
#' plot(clust_rf)
#'
#' \donttest{
#' library("ranger")
#' model_titanic_rf <- ranger(survived ~., data = titanic_imputed, probability = TRUE)
#'
#' explain_titanic_rf <- explain(model_titanic_rf,
#'                               data = titanic_imputed[,-8],
#'                               y = titanic_imputed[,8],
#'                               verbose = FALSE)
#'
#' cp_rf <- ceteris_paribus(explain_titanic_rf, selected_passangers)
#' cp_rf
#'
#' pdp_rf <- aggregate_profiles(cp_rf, variables = "age")
#' head(pdp_rf)
#' clust_rf <- cluster_profiles(cp_rf, k = 3, variables = "age")
#' head(clust_rf)
#'
#' plot(clust_rf, color = "_label_") +
#'   show_aggregated_profiles(pdp_rf, color = "black", size = 3)
#'
#' plot(cp_rf, color = "grey", variables = "age") +
#'   show_aggregated_profiles(clust_rf, color = "_label_", size = 2)
#'
#' clust_rf <- cluster_profiles(cp_rf, k = 3, center = TRUE, variables = "age")
#' head(clust_rf)
#' }
#' @export
cluster_profiles <- function(x,
                             ...,
                             aggregate_function = mean,
                             variable_type = "numerical",
                             center = FALSE,
                             k = 3,
                             variables = NULL) {

  check_variable_type(variable_type)

  # if there is more explainers, they should be merged into a single data frame
  elist <- list(...)
  if (length(elist) > 1) {
    # only ceteris_paribus_explainer objects
    elist <-  elist[sapply(elist, function(x) "ceteris_paribus_explainer" %in% class(x))]
  } else {
    elist <- NULL
  }
  dfl <- c(list(x), elist)

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

  class(aggregated_profiles) <- c("aggregated_profiles_explainer", "data.frame")
  aggregated_profiles
}
