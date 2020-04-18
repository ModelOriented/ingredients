#' @details Function \code{describe.feature_importance_explainer()} generates a natural language
#' description of feature importance explanation. It prints the number of important variables, that
#' have significant dropout difference from the full model, depending on \code{nonsignificance_treshold}.
#' The description prints the three most important variables for the model's prediction.
#' The current design of DALEX explainer does not allow for displaying variables values.
#'
#' @importFrom graphics plot
#' @importFrom stats quantile
#'
#' @examples
#' library("DALEX")
#'
#' lm_model <- lm(m2.price~., data = apartments)
#' explainer_lm <- explain(lm_model, data = apartments[,-1], y = apartments[,1])
#'
#' fi_lm <- feature_importance(explainer_lm, loss_function = DALEX::loss_root_mean_square)
#'
#' plot(fi_lm)
#' describe(fi_lm)
#'
#' @export
#' @rdname describe
describe.feature_importance_explainer <- function(x,
                                                  nonsignificance_treshold = 0.15,
                                                  ...) {

  #Error handling
  if (!(class(nonsignificance_treshold) == 'numeric')) {
    stop("Arguments are not valid")
  }
  # fix for https://github.com/ModelOriented/ingredients/issues/95
  x <- x[x$permutation == 0,]

  model_name <- x[1, 'label']

  dropout_full_model <- x[1, 'dropout_loss']
  dropout_baseline <- x[nrow(x), 'dropout_loss']
  treshold <- dropout_full_model + abs(dropout_full_model - dropout_baseline)*nonsignificance_treshold

  df <- x[-c(1,nrow(x)), c('variable', 'dropout_loss')]
  n_of_all <- nrow(df)
  df <- df[order(df$dropout_loss, decreasing = TRUE), ]
  df <- df[which(df$dropout_loss > treshold), ] #treshold

  n_of_important <- nrow(df)
  n_of_displayed <- min(n_of_important, 3)

  important_variables <- paste(df[1:n_of_displayed, 'variable'], collapse = ", ")

  description <- paste0("The number of important variables for ",
                        model_name, "'s prediction is ",
                        n_of_important, " out of ",
                        n_of_all,". \n Variables ",
                        important_variables,
                        " have the highest importantance.")

  class(description) <- c("feature_importance_description", "description", "character")

  description
}
