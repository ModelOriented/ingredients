#' Aspect Importance
#'
#' This explainer calculates the feature groups importance (called aspects importance) for a given model.
#' Aspect Importance function takes a sample from a given dataset and modifies it.
#' Modification is made by replacing part of its aspects by values from one observation.
#' Then function is calculating the difference between the prediction made on modified sample and the original sample.
#' Finally, it measures the impact of aspects on the change of prediction by using the linear model.
#'
#' @param x an explainer created with the `DALEX::explain()` function
#' @param model model created on data, it will be extracted from `x` if it's an explainer
#' @param data dataset, it will be extracted from `x` if it's an explainer
#' @param predict_function predict function, it will be extracted from `x` if it's an explainer
#' @param new_observation a new observation with columns that corresponds to variables used in the model
#' @param aspects_list list containg grouping of variables for importance shall be calculated
#' @param B number of data rows to be sampled from data
#'
#' @return An object of the class 'aspect_importance'.
#' Contains linear model describing aspects' importance.
#' @export
#'
#' @examples
#' library("DALEX")
#' titanic <- na.omit(titanic)
#' model_titanic_glm <- glm(survived == "yes" ~
#'                            class+gender+age+sibsp+parch+fare+embarked,
#'                          data = titanic, family = "binomial")
#'
#' explain_titanic_glm <- explain(model_titanic_glm,
#'                                data = titanic[,-9],
#'                                y = titanic$survived == "yes")
#'
#' aspects <- list(wealth = c("class", "fare"),
#'                 family = c("gender", "sibsp", "parch"),
#'                 age = "age",
#'                 embarked = "embarked")
#'
#' aspect_importance(explain_titanic_glm, new_observation = titanic[1,],
#'                   aspects_list = aspects)
#'
#' \donttest{
#'
#' library("randomForest")
#' model_titanic_rf <- randomForest(survived ~ class + gender + age + sibsp +
#'                                    parch + fare + embarked, data = titanic)
#'
#' explain_titanic_rf <- explain(model_titanic_rf,
#'                               data = titanic[,-9],
#'                               y = titanic$survived == "yes")
#'
#' aspect_importance(explain_titanic_rf, new_observation = titanic[1,],
#'                   aspects_list = aspects)
#'
#' }
#'
#' @export
#' @rdname aspect_importance

aspect_importance <- function(x, ...)
  UseMethod("aspect_importance")

#' @export
#' @rdname aspect_importance

aspect_importance.explainer <- function(x, new_observation,
                                        aspects_list, B = 100) {

  # extracts model, data and predict function from the explainer
  data <- x$data
  model <- x$model
  predict_function <- x$predict_function

  # calls target function
  aspect_importance.default(model, data, predict_function,
                            new_observation, aspects_list, B)
}

#' @export
#' @rdname aspect_importance

aspect_importance.default <- function(model,data, predict_function = predict,
                                      new_observation,
                                      aspects_list, B = 100) {
  # look only for common variables in data and new observation
  if ("data.frame" %in% class(data)) {
    common_variables <- intersect(colnames(new_observation), colnames(data))
    new_observation <- new_observation[, common_variables, drop = FALSE]
    data <- data[,common_variables, drop = FALSE]
  }

  # print error if no common variables are found
  if (length(common_variables) == 0) {
    stop("No common variables in dataset and new observation")
  } else if (length(setdiff(unlist(aspects_list),colnames(new_observation))) != 0) {
    stop("Different columns in new_observation and aspects list")
  }

  # new_obs_vars <- colnames(new_observation)
  # model_vars <- all.vars(model$call$formula)[-1]
  #
  # # print error if model has variables not present in new observation
  # if (length(setdiff(model_vars,new_obs_vars)) > 0) {
  #   stop("Model uses variable that do not exist in new observation")
  # }

  # create empty matrix and data frames
  n_sample <- select_sample(data, n = B)
  n_sample_changed <- n_sample

  # sample which aspects will be replaced
  new_X <- get_sample(B, length(aspects_list), method = "default")

  # replace aspects
  for (i in 1:nrow(n_sample)) {
    vars <- unlist(aspects_list[new_X[i, ] == 1])
    n_sample_changed[i,vars] <- new_observation[vars]
  }

  # calculate change in predictions
  y_changed <- predict_function(model, n_sample_changed) -
    predict_function(model, n_sample)

  # fit linear model to estimate aspects importance
  colnames(new_X) <- names(aspects_list)
  new_df <- data.frame(new_X, y_changed)
  new_model <- lm(y_changed~., data = new_df)
  class(new_model) <- c("aspect_importance", "lm")

  return(new_model)
}

#' Function for getting binary matrix
#'
#' This function creates binary matrix
#' Either by using binomial distribution or by using sample() function
#'
#' @param n number of rows
#' @param m number of columns
#' @param method sampling method
#'
#' @return a binary matrix
#'
#' @examples
#'  \dontrun{
#'  get_sample(100,4,"binom")
#' }
#' @export

get_sample <- function(n, m, method = c("default","binom")) {
  method = match.arg(method)
  x <- matrix(0, n, m)
  if (method == "binom") {
    x <- apply(x, 2, rbinom, 1, 0.5)
  } else {
    for (i in 1:n) {
      x[i, unique(sample(1:m, 2, replace = TRUE)) ] <- 1
    }
  }
  return(x)
}
