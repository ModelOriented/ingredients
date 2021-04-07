#' Print Generic for Feature Importance Object
#'
#' @param x an explanation created with \code{\link{feature_importance}}
#' @param ... other parameters.
#'
#' @references Explanatory Model Analysis. Explore, Explain, and Examine Predictive Models. \url{https://ema.drwhy.ai/}
#'
#' @return a data frame.
#' @importFrom stats quantile median
#'
#' @examples
#' library("DALEX")
#' library("ingredients")
#'
#' model_titanic_glm <- glm(survived ~ gender + age + fare,
#'                          data = titanic_imputed, family = "binomial")
#'
#' explain_titanic_glm <- explain(model_titanic_glm,
#'                                data = titanic_imputed[,-8],
#'                                y = titanic_imputed[,8],
#'                                verbose = FALSE)
#'
#' fi_glm <- feature_importance(explain_titanic_glm)
#'
#' fi_glm
#'
#'
#' @export
print.feature_importance_explainer <- function(x, ...) {

  colnames(x) <- c("variable", "B", "mean_dropout_loss", "label")
  result <- x[x$B == "0", c("variable", "mean_dropout_loss", "label")]

  print.data.frame(result)
}
