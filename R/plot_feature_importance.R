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
#' Find more details in the \href{https://pbiecek.github.io/ema/featureImportance.html}{Feature Importance Chapter}.
#'
#' @param x a feature importance explainer produced with the \code{feature_importance()} function
#' @param ... other explainers that shall be plotted together
#' @param max_vars maximum number of variables that shall be presented for for each model.
#' By default \code{NULL} what means all variables
#' @param show_boxplots logical if \code{TRUE} (default) boxplot will be plotted to show permutation data.
#' @param bar_width width of bars. By default \code{10}
#' @param desc_sorting logical. Should the bars be sorted descending? By default TRUE
#' @param title the plot's title, by default \code{'Feature Importance'}
#' @param subtitle the plot's subtitle. By default - NA, which means
#' the subtitle will be 'created for the XXX model', where XXX is the label of explainer(s)
#'
#' @importFrom stats model.frame reorder
#' @importFrom utils head tail
#' @importFrom DALEX loss_root_mean_square
#' @importFrom DALEX theme_drwhy theme_drwhy_vertical colors_discrete_drwhy
#'
#' @return a \code{ggplot2} object
#'
#' @references Explanatory Model Analysis. Explore, Explain and Examine Predictive Models. \url{https://pbiecek.github.io/ema}
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
#' fi_rf <- feature_importance(explainer_rf, type = "raw", max_vars = 3,
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
plot.feature_importance_explainer <- function(x, ..., max_vars = NULL, show_boxplots = TRUE, bar_width = 10,
                                              desc_sorting = TRUE, title = "Feature Importance", subtitle = NA) {

  if (!is.logical(desc_sorting)) {
    stop("desc_sorting is not logical")
  }

  dfl <- c(list(x), list(...))

  # add boxplot data
  if (show_boxplots) {
    dfl <- lapply(dfl, function(x) {
      result <- data.frame(
        min = tapply(x$dropout_loss, x$variable, min, na.rm = TRUE),
        q1 = tapply(x$dropout_loss, x$variable, quantile, 0.25, na.rm = TRUE),
        median = tapply(x$dropout_loss, x$variable, median, na.rm = TRUE),
        q3 = tapply(x$dropout_loss, x$variable, quantile, 0.75, na.rm = TRUE),
        max = tapply(x$dropout_loss, x$variable, max, na.rm = TRUE)
      )

      result$min <- as.numeric(result$min)
      result$q1 <- as.numeric(result$q1)
      result$median <- as.numeric(result$median)
      result$q3 <- as.numeric(result$q3)
      result$max <- as.numeric(result$max)

      merge(x[x$permutation == 0,], cbind(rownames(result),result), by.x = "variable", by.y = "rownames(result)")
    })
  } else {
    dfl <- lapply(dfl, function(x) {
      x[x$permutation == 0,]
    })
  }

  # combine all explainers in a single frame
  expl_df <- do.call(rbind, dfl)

  # add an additional column that serve as a baseline
  bestFits <- expl_df[expl_df$variable == "_full_model_", ]
  ext_expl_df <- merge(expl_df, bestFits[,c("label", "dropout_loss")], by = "label")

  # set the order of variables depending on their contribution
  ext_expl_df$variable <- reorder(ext_expl_df$variable,
                                  (ext_expl_df$dropout_loss.x - ext_expl_df$dropout_loss.y) * ifelse(desc_sorting, 1, -1),
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

  variable <- q1 <- q3 <- dropout_loss.x <- dropout_loss.y <- label <- dropout_loss <- NULL
  nlabels <- length(unique(bestFits$label))

  # extract labels for plot's subtitle
  if (is.na(subtitle)) {
    glm_labels <- paste0(unique(ext_expl_df$label), collapse = ", ")
    subtitle <- paste0("created for the ", glm_labels, " model")
  }

  # plot it
  pl <- ggplot(ext_expl_df, aes(variable, ymin = dropout_loss.y, ymax = dropout_loss.x, color = label)) +
          geom_hline(data = bestFits, aes(yintercept = dropout_loss, color = label), lty= 3) +
          geom_linerange(size = bar_width)

  if (show_boxplots) {
    pl <- pl +
      geom_boxplot(aes(ymin = min, lower = q1, middle = median, upper = q3, ymax = max),
                   stat = "identity", fill = "#371ea3", color = "#371ea3", width = 0.25)
  }

  # facets have fixed space, can be resolved with ggforce https://github.com/tidyverse/ggplot2/issues/2933

  pl + coord_flip() +
      scale_color_manual(values = colors_discrete_drwhy(nlabels)) +
      facet_wrap(~label, ncol = 1, scales = "free_y") + theme_drwhy_vertical() +
      ylab("Drop-out loss") + xlab("") +
      labs(title = title, subtitle = subtitle) +
      theme(legend.position = "none",
            plot.subtitle = element_text(hjust = 0.5, vjust = 0.15))

}

