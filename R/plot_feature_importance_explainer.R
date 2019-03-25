#' Plots Global Model Explanations (Variable Importance)
#'
#' Function \code{plot.variable_dropout_explainer} plots dropouts for variables used in the model.
#' It uses output from \code{variable_dropout} function that corresponds to permutation based measure of variable importance.
#' Variables are sorted in the same order in all panels. The order depends on the average drop out loss. In different panels variable contributions may not look like sorted if variable importance is different in different in different mdoels.
#'
#' @param x a variable dropout exlainer produced with the 'variable_dropout' function
#' @param ... other explainers that shall be plotted together
#' @param max_vars maximum number of variables that shall be presented for for each model. By default NULL what means all variables
#' @param bar_width width of bars. By default 10
#'
#' @importFrom stats model.frame reorder
#' @importFrom utils head tail
#' @importFrom DALEX loss_root_mean_square
#' @return a ggplot2 object
#' @export
#'
#' @examples
#'
#'  \dontrun{
#' library("DALEX")
#' library("breakDown")
#' library("randomForest")
#' HR_rf_model <- randomForest(status~., data = HR, ntree = 100)
#' explainer_rf  <- explain(HR_rf_model, data = HR, y = HR$status)
#' vd_rf <- feature_importance(explainer_rf, type = "raw",
#'                             loss_function = loss_cross_entropy)
#' head(vd_rf)
#' plot(vd_rf)
#'
#' HR_glm_model <- glm(status == "fired"~., data = HR, family = "binomial")
#' explainer_glm <- explain(HR_glm_model, data = HR, y = HR$status == "fired")
#' vd_glm <- feature_importance(explainer_glm, type = "raw",
#'                         loss_function = loss_root_mean_square)
#' head(vd_glm)
#' plot(vd_glm)
#'
#' library("xgboost")
#' model_martix_train <- model.matrix(status == "fired" ~ . -1, HR)
#' data_train <- xgb.DMatrix(model_martix_train, label = HR$status == "fired")
#' param <- list(max_depth = 2, eta = 1, silent = 1, nthread = 2,
#'               objective = "binary:logistic", eval_metric = "auc")
#' HR_xgb_model <- xgb.train(param, data_train, nrounds = 50)
#' explainer_xgb <- explain(HR_xgb_model, data = model_martix_train,
#'                      y = HR$status == "fired", label = "xgboost")
#' vd_xgb <- feature_importance(explainer_xgb, type = "raw")
#' head(vd_xgb)
#'
#' plot(vd_glm, vd_xgb, bar_width = 5)
#'  }
#'
plot.feature_importance_explainer <- function(x, ..., max_vars = NULL, bar_width = 10) {
  dfl <- c(list(x), list(...))

  # combine all explainers in a single frame
  expl_df <- do.call(rbind, dfl)

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
  ggplot(ext_expl_df, aes(variable, ymin = dropout_loss.y, ymax = dropout_loss.x, color = label)) +
    geom_hline(data = bestFits, aes(yintercept = dropout_loss, color = label), lty= 3) +
    geom_linerange(size = bar_width) + coord_flip() +
    scale_color_manual(values = theme_drwhy_colors(nlabels)) +
    facet_wrap(~label, ncol = 1, scales = "free_y") + theme_drwhy_vertical() +
    theme(legend.position = "none") +
    ylab("Loss-drop after perturbations") + xlab("")

}

