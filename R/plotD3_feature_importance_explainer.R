#' Plot Feature Importance Objects in D3 with r2d3 package.
#'
#' Function \code{plotD3.feature_importance_explainer} plots dropouts for variables used in the model.
#' It uses output from \code{feature_importance} function that corresponds to permutation based measure of feature importance.
#' Variables are sorted in the same order in all panels. The order depends on the average drop out loss. In different panels variable contributions may not look like sorted if variable importance is different in different models.
#'
#' @param x a feature importance explainer produced with the 'feature_importance' function
#' @param ... other explainers that shall be plotted together
#' @param split either \code{"model"} or \code{"feature"} determines the plot layout
#' @param scale_height should the height of plot scale with window size? By default it's FALSE
#'
#' @return an `r2d3` object.
#'
#' @examples
#' \dontrun{
#' library("DALEX")
#' library("ingredients")
#' library("caret")
#'
#' rf_model <- train(m2.price~., data = apartments, method="rf", ntree = 100)
#' explainer_rf <- explain(rf_model, data = apartments_test[,2:6],
#'                         y = apartments_test$m2.price, label="rf")
#' fi_rf <- feature_importance(explainer_rf, loss_function = loss_root_mean_square)
#'
#' head(fi_rf)
#' plotD3(fi_rf)
#'
#' svm_model <- train(m2.price~., data = apartments, method="svmLinear")
#' explainer_svm <- explain(svm_model, data = apartments_test[,2:6],
#'                         y = apartments_test$m2.price, label="svm")
#' fi_svm <- feature_importance(explainer_svm, loss_function = loss_root_mean_square)
#'
#' head(fi_svm)
#' plotD3(fi_rf, fi_svm)
#'
#' plotD3(fi_rf, fi_svm, split = "feature")
#' }
#' @export
#' @rdname plotD3

plotD3 <- function(x, ...)
  UseMethod("plotD3")

#' @export
#' @rdname plotD3
plotD3.feature_importance_explainer <-  function(x, ..., split = "model", scale_height = FALSE){
    if (!(split %in% c("model", "feature"))){
      stop("The plotD3.feature_importance_explainer() function requires split to be model or feature.")
    }

    n <- length(list(...)) + 1
    m <- dim(x)[1]

    dfl <- c(list(x), list(...))
    df <- do.call(rbind, dfl)

    xmax <- max(df$dropout_loss)
    xmin <- min(df$dropout_loss)

    ticksMargin <- round(abs(xmin-xmax)*0.1);

    bestFits <- df[df$variable == "_full_model_", ]
    df <- merge(df, bestFits[,c("label", "dropout_loss")], by = "label")

    perm <- aggregate(df$dropout_loss.x, by = list(Category=df$variable), FUN = mean)

    options <- list(xmin = xmin, xmax = xmax, ticksMargin = ticksMargin, scaleHeight = scale_height)

    if (split == "model"){
      # one plot for each model

      # sorting bars in groups
      perm <- perm$Category[order(perm$x)]
      df$variable <- factor(as.character(df$variable), levels = as.character(perm))
      df <- df[order(df$label, df$variable),]

      colnames(df) <- c("label","variable","dropout_loss")
      temp <- split(df[,2:3], f = df$label)
      temp <- jsonlite::toJSON(temp)

      # n - number of models, m - number of features
      options["n"] <- n
      options["m"] <- m

      r2d3::r2d3(data = temp, script = system.file("featureImportance.js", package = "ingredients"),
                 dependencies = system.file("colorsDrWhy.js", package = "ingredients"),
                 css = system.file("themeDrWhy.css", package = "ingredients"),
           d3_version = 4,
           options = options)

    } else if (split == "feature"){
      # one plot for each feature

      colnames(df) <- c("label","variable","dropout_loss")
      temp <- split(df[,c(1,3)], f = df$variable)
      temp <- temp[perm$Category[order(-perm$x)]]
      temp <- jsonlite::toJSON(temp)

      # n - number of features, m - number of models
      options["n"] <- m
      options["m"] <- n

      r2d3::r2d3(data = temp, script = system.file("featureImportanceSplit.js", package = "ingredients"),
                 dependencies = system.file("colorsDrWhy.js", package = "ingredients"),
                 css = system.file("themeDrWhy.css", package = "ingredients"),
           d3_version = 4,
           options = options)
    }
}

