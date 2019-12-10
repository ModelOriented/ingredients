#' Plots Feature Importance
#'
#' This function plots variable importance calculated as changes in the loss function after variable drops.
#' It uses output from \code{feature_importance} function that corresponds to
#' permutation based measure of variable importance.
#' Variables are sorted in the same order in all panels.
#' The order depends on the average drop out loss.
#' In different panels variable contributions may not look like sorted if variable
#' importance is different in different in different models.
#'
#' Find more details in the \href{https://pbiecek.github.io/PM_VEE/featureImportance.html}{Feature Importance Chapter}.
#'
#' @param x a feature importance explainer produced with the \code{feature_importance()} function
#' @param ... other explainers that shall be plotted together
#' @param max_vars maximum number of variables that shall be presented for for each model.
#' By default \code{NULL} what means all variables
#' @param show_boxplots logical if \code{TRUE} (default) boxplot will be plotted to show permutation data.
#' @param bar_width width of bars. By default \code{10}
#'
#' @importFrom stats model.frame reorder
#' @importFrom utils head tail
#' @importFrom DALEX loss_root_mean_square
#' @importFrom DALEX theme_drwhy theme_drwhy_vertical colors_discrete_drwhy
#'
#' @return a \code{ggplot2} object
#'
#' @references Predictive Models: Visual Exploration, Explanation and Debugging \url{https://pbiecek.github.io/PM_VEE}
#'
#' @examples
#' library("DALEX")
#'
#' model_titanic_glm <- glm(survived ~ gender + age + fare,
#'                          data = titanic_imputed, family = "binomial")
#'
#' explain_titanic_glm <- explain(model_titanic_glm,
#'                                data = titanic_imputed[,-8],
#'                                y = titanic_imputed[,8])
#'
#' fi_rf <- feature_importance(explain_titanic_glm)
#' plot(fi_rf)
#'
#' \donttest{
#' library("randomForest")
#'
#' model_titanic_rf <- randomForest(survived ~.,  data = titanic_imputed)
#'
#' explain_titanic_rf <- explain(model_titanic_rf,
#'                               data = titanic_imputed[,-8],
#'                               y = titanic_imputed[,8])
#'
#' fi_rf <- feature_importance(explain_titanic_rf)
#' plot(fi_rf)
#'
#' HR_rf_model <- randomForest(status~., data = HR, ntree = 100)
#'
#' explainer_rf  <- explain(HR_rf_model, data = HR, y = HR$status,
#'                          verbose = FALSE, precalculate = FALSE)
#'
#' fi_rf <- feature_importance(explainer_rf, type = "raw",
#'                             loss_function = loss_cross_entropy)
#' head(fi_rf)
#' plot(fi_rf)
#'
#' HR_glm_model <- glm(status == "fired"~., data = HR, family = "binomial")
#' explainer_glm <- explain(HR_glm_model, data = HR, y = HR$status == "fired")
#'
#' fi_glm <- feature_importance(explainer_glm, type = "raw",
#'                              loss_function = loss_root_mean_square)
#' head(fi_glm)
#' plot(fi_glm)
#'
#' library("xgboost")
#'
#' model_martix_train <- model.matrix(status == "fired" ~ . -1, HR)
#' data_train <- xgb.DMatrix(model_martix_train, label = HR$status == "fired")
#'
#' param <- list(max_depth = 2, eta = 1, silent = 1, nthread = 2,
#'               objective = "binary:logistic", eval_metric = "auc")
#'
#' HR_xgb_model <- xgb.train(param, data_train, nrounds = 50)
#'
#' explainer_xgb <- explain(HR_xgb_model, data = model_martix_train,
#'                          y = HR$status == "fired", label = "xgboost")
#'
#' fi_xgb <- feature_importance(explainer_xgb, type = "raw")
#'
#' head(fi_xgb)
#' plot(fi_glm, fi_xgb, bar_width = 5)
#' }
#'
#' @export
plot.feature_importance_explainer <- function(x, ..., max_vars = NULL, show_boxplots = TRUE, bar_width = 10) {

  dfl <- c(list(x), list(...))

  # combine all explainers in a single frame
  fi_df <- do.call(rbind, dfl)

  # add this so it works as before boxplots
  expl_df <- fi_df[fi_df$permutation == 0,]

  # add an additional column that serve as a baseline
  bestFits <- expl_df[expl_df$variable == "_full_model_", ]
  ext_expl_df <- merge(expl_df, bestFits[,c("label", "dropout_loss")], by = "label")

  # set the order of variables depending on their contribution
  ext_expl_df$variable <- reorder(ext_expl_df$variable,
                                  ext_expl_df$dropout_loss.x - ext_expl_df$dropout_loss.y,
                                  mean)

  # remove rows that starts with _
  ext_expl_df <- ext_expl_df[!(substr(ext_expl_df$variable,1,1) == "_"),]

  # for each model leave only max_vars
  if (!is.null(max_vars)) {
    trimmed_parts <- lapply(unique(ext_expl_df$label), function(label) {
      tmp <- ext_expl_df[ext_expl_df$label == label, ]
      tmp[tail(order(tmp$dropout_loss.x), max_vars), ]
    })
    ext_expl_df <- do.call(rbind, trimmed_parts)
  }

  variable <- dropout_loss.x <- dropout_loss.y <- label <- dropout_loss <- NULL
  nlabels <- length(unique(bestFits$label))

  # plot it
  pl <- ggplot(ext_expl_df, aes(variable, ymin = dropout_loss.y, ymax = dropout_loss.x, color = label)) +
          geom_hline(data = bestFits, aes(yintercept = dropout_loss, color = label), lty= 3) +
          geom_linerange(size = bar_width)

  if (show_boxplots) {
    pl <- pl +
      geom_boxplot(data = fi_df[!(substr(fi_df$variable, 1, 1) == "_"),],
                   aes(x = variable, y = dropout_loss), coef = 100,
                   fill = "#371ea3", color = "#371ea3", width = 0.25)
  }

  # facets have fixed space, can be resolved with ggforce https://github.com/tidyverse/ggplot2/issues/2933

  pl + coord_flip() +
      scale_color_manual(values = colors_discrete_drwhy(nlabels)) +
      facet_wrap(~label, ncol = 1, scales = "free_y") + theme_drwhy_vertical() +
      theme(legend.position = "none") +
      ylab("Drop-out loss") + xlab("")

}

