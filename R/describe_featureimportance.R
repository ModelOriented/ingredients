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
#'library("ingredients")
#'
#'lm_model <- lm(m2.price~., data = apartments)
#'explainer_lm <- explain(lm_model, data = apartments[,2:6],
#'                        y = apartments$m2.price, label="lm")
#'fi_lm <- feature_importance(explainer_lm, loss_function = loss_root_mean_square)
#'
#'plot(fi_lm)
#'describe(fi_lm)
#'
#' @export
#' @rdname describe

describe.feature_importance_explainer <- function(explainer,
                                                  nonsignificance_treshold = 0.15,
                                                  ...) {

  #Error handling
  if (!(class(nonsignificance_treshold) == 'numeric')) {
    stop("Arguments are not valid")
  }

  model_name <- explainer[1, 'label']

  dropout_full_model <- explainer[1, 'dropout_loss']
  dropout_baseline <- explainer[nrow(explainer), 'dropout_loss']
  treshold <- dropout_full_model + abs(dropout_full_model - dropout_baseline)*nonsignificance_treshold

  df <- explainer[-c(1,nrow(explainer)), c('variable', 'dropout_loss')]
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
  class(description) = "ceteris_paribus_description"
  description

}
