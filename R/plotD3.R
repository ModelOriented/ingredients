#' Plot Feature Importance Objects in D3 with r2d3 package.
#'
#' @param x the model model of `feature_importance_explainer` class.
#' @param ... other parameters.
#' @param label default TRUE
#' @param scaleHeight default FALSE
#'
#' @return an `r2d3` object.
#'
#' @examples
#' \dontrun{
#' ## Not run:
#' set.seed(59)
#' library("DALEX")
#' library("caret")
#'
#' regr_rf <- train(m2.price~., data = apartments, method="rf", ntree = 100)
#' regr_svm <- train(m2.price~., data = apartments, method="svmLinear")
#' explainer_rf <- explain(regr_rf, data = apartmentsTest[,2:6],
#' y = apartmentsTest$m2.price, label="rf")
#' explainer_svm <- explain(regr_svm, data = apartmentsTest[,2:6],
#' y = apartmentsTest$m2.price, label="svm")
#' fi_rf <- feature_importance(explainer_rf, loss_function = loss_root_mean_square)
#' fi_svm <- feature_importance(explainer_svm, loss_function = loss_root_mean_square)
#'
#' plotD3(fi_rf, fi_svm)
#' }
#' @export
#'
#' @importFrom jsonlite toJSON
#' @importFrom r2d3 r2d3
#' @rdname plotD3

plotD3 <- function(x, ...)
  UseMethod("plotD3")

#' @export
#' @rdname plotD3
plotD3.feature_importance_explainer <-  function(x, ..., label = TRUE, scaleHeight = FALSE){

    n <- length(list(...)) + 1
    m <- dim(x)[1]

    dfl <- c(list(x), list(...))
    df <- do.call(rbind, dfl)

    xmax <- max(df$dropout_loss)
    xmin <- min(df$dropout_loss)

    ticksMargin <- round(abs(xmin-xmax)*0.1);

    bestFits <- df[df$variable == "_full_model_", ]
    df <- merge(df, bestFits[,c("label", "dropout_loss")], by = "label")

    perm <- aggregate(df$dropout_loss.x, by=list(Category=df$variable), FUN=mean)

    options <- list(xmin = xmin, xmax = xmax, ticksMargin=ticksMargin, scaleHeight=scaleHeight)

    if (label == TRUE){
      # one plot for each model

      # sorting bars in groups
      adding <- c()
      for (i in 1:n){
        adding <- c(adding, rep((i-1)*m,m))
      }
      perm <- match(df$variable, perm$Category[order(perm$x)]) + adding

      colnames(df) <- c("label","variable","dropout_loss")
      temp <- split(df[perm,2:3], f = df$label)
      temp <- toJSON(temp)

      # n - number of models, m - number of features
      options["n"] <- n
      options["m"] <- m

      r2d3(data = temp, script = system.file("featureImportance.js", package = "ingredients"),
           d3_version = 4,
           options = options)

    } else {
      # one plot for each feature

      colnames(df) <- c("label","variable","dropout_loss")
      temp <- split(df[,c(1,3)], f = df$variable)
      temp <- temp[perm$Category[order(-perm$x)]]
      temp <- toJSON(temp)

      # n - number of features, m - number of models
      options["n"] <- m
      options["m"] <- n

      r2d3(data = temp, script = system.file("featureImportance2.js", package = "ingredients"),
           d3_version = 4,
           options = options)
    }
}

