#' Aspect Importance
#' This explainer calculates the feature groups importance (called aspects importance) for a selected observation.
#'
#' Aspect Importance function takes a sample from a given dataset and modifies it.
#' Modification is made by replacing part of its aspects by values from the observation.
#' Then function is calculating the difference between the prediction made on modified sample and the original sample.
#' Finally, it measures the impact of aspects on the change of prediction by using the linear model.
#'
#' @param x an explainer created with the `DALEX::explain()` function
#' @param model model created on data, it will be extracted from `x` if it's an explainer
#' @param data dataset, it will be extracted from `x` if it's an explainer
#' @param predict_function predict function, it will be extracted from `x` if it's an explainer
#' @param new_observation selected observation with columns that corresponds to variables used in the model
#' @param aspects_list list containg grouping of features into aspects
#' @param B number of rows to be sampled from data
#' @param method sampling method in get_sample()
#'
#' @return An object of the class 'aspect_importance'.
#' Contains dataframe that describes aspects' importance.
#'
#' @importFrom stats lm
#'
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

aspect_importance.explainer <- function(x, new_observation, aspects_list,
                                        B = 100, method = "default") {

  # extracts model, data and predict function from the explainer
  data <- x$data
  model <- x$model
  predict_function <- x$predict_function

  # calls target function
  aspect_importance.default(model, data, predict_function,
                            new_observation, aspects_list, B, method)
}

#' @export
#' @rdname aspect_importance

aspect_importance.default <- function(model,data, predict_function = predict,
                                      new_observation,
                                      aspects_list, B = 100, method = "default") {
  # look only for common variables in data and new observation
  if ("data.frame" %in% class(data)) {
    common_variables <- intersect(colnames(new_observation), colnames(data))
    new_observation <- new_observation[, common_variables, drop = FALSE]
    data <- data[,common_variables, drop = FALSE]
  }

  # stop if no common variables are found
  stopifnot(length(common_variables) > 0,
            length(setdiff(unlist(aspects_list),colnames(new_observation))) == 0)

  # create empty matrix and data frames
  n_sample <- select_sample(data, n = B)
  n_sample_changed <- n_sample

  # sample which aspects will be replaced
  new_X <- get_sample(B, length(aspects_list), method)

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

  # building dataframe with results
  res <- as.data.frame(names(new_model$coefficients))
  colnames(res)[1] <- "aspects"
  res$importance <- unname(new_model$coefficients)
  res <- res[!res$aspects == "(Intercept)",]
  res <- res[order(-abs(res$importance)),]
  class(res) <- c("aspect_importance", "data.frame")

  return(res)
}

#' Function for getting binary matrix
#'
#' Function creates binary matrix.
#' It starts with a zero matrix. Then it replaces some zeros with ones.
#' It either randomly replaces one or two zeros per row.
#' Or replace random number of zeros per row - average number of replaced zeros can be controled by parameter f.
#' Function doesn't allow the returned matrix to have rows with only zeros.
#'
#' @param n number of rows
#' @param p number of columns
#' @param method sampling method
#' @param f probability for binomial sampling
#'
#' @return a binary matrix
#'
#' @importFrom stats rbinom
#'
#' @examples
#'  \dontrun{
#'  get_sample(100,6,"binom",3)
#' }
#' @export

get_sample <- function(n, p, method = c("default","binom"), f = 2) {
  method = match.arg(method)
  stopifnot(n > 0, p > 0, f > 0)
  x <- matrix(0, n, p)
  if (method == "binom") {
     for (i in 1:n) {
       n_of_changes <- pmax(rbinom(1, p, f/p),1)
       x[i, unique(sample(1:p, n_of_changes, replace = TRUE)) ] <- 1
      }
  } else {
    for (i in 1:n) {
      x[i, unique(sample(1:p, 2, replace = TRUE)) ] <- 1
    }
  }
  return(x)
}

#' Function for plotting aspect_importance results
#'
#' This function plots the results of aspect_importance
#'
#' @param x object of aspect_importance class
#' @param bar_width bar width
#'
#' @return a ggplot2 object
#' @export
#'
#' @import ggplot2
#'
#' @export



plot.aspect_importance <- function(x, bar_width = 10) {

  x$a_sign <- ifelse(x$importance > 0,"positive","negative")
  x$aspects <- reorder(x$aspects, abs(x$importance), na.rm = TRUE)

  # plot it
  ggplot(x, aes(aspects, ymin = 0, ymax = importance, color = a_sign)) +
    geom_linerange(size = bar_width) + coord_flip() +
    ylab("Aspects importance") + xlab("") + theme_drwhy_vertical() +
    theme(legend.position = "none")
}

#' @export
#' @rdname aspect_importance
a_lime <- function(x, ...) {
  aspect_importance(x, ...)
}
