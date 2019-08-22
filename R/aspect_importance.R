#' Calculates the feature groups importance (called aspects importance) for a
#' selected observation
#'
#' Aspect Importance function takes a sample from a given dataset and modifies
#' it. Modification is made by replacing part of its aspects by values from the
#' observation. Then function is calculating the difference between the
#' prediction made on modified sample and the original sample. Finally, it
#' measures the impact of aspects on the change of prediction by using the
#' linear model or lasso.
#'
#'
#' @param x a model to be explained or an explainer created with the
#'   `DALEX::explain()` function
#' @param data dataset, it will be extracted from `x`` if it's an explainer
#' @param predict_function predict function, it will be extracted from `x`` if
#'   it's an explainer
#' @param new_observation selected observation with columns that corresponds to
#'   variables used in the model
#' @param aspects_list list containting grouping of features into aspects
#' @param B number of rows to be sampled from data
#' @param sample_method sampling method in \code{\link{get_sample}}
#' @param n_var how many non-zero coefficients should be after lasso fitting,
#'   if zero than linear regression is used
#' @param f frequency in in \code{\link{get_sample}}
#' @param show_cor show if all features in aspect are pairwise positivly
#'   correlated, works only if dataset contains solely numeric values
#' @param ... other parameters
#'
#' @return An object of the class 'aspect_importance'. Contains dataframe that
#'   describes aspects' importance.
#'
#' @importFrom stats lm
#' @importFrom stats coef
#' @importFrom stats model.matrix
#' @importFrom glmnet glmnet
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
#'                 family = c("sibsp", "parch"),
#'                 personal = c("gender", "age"),
#'                 embarked = "embarked")
#'
#' aspect_importance(explain_titanic_glm, new_observation = titanic[1,],
#'                   aspects_list = aspects)
#'
#' \donttest{
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

aspect_importance <- function(x, ...)
  UseMethod("aspect_importance")

#' @export
#' @rdname aspect_importance

aspect_importance.explainer <- function(x, new_observation, aspects_list,
                                        B = 100, sample_method = "default",
                                        n_var = 0, f = 2, show_cor = FALSE, ...) {

  # extracts model, data and predict function from the explainer
  data <- x$data
  model <- x$model
  predict_function <- x$predict_function

  # calls target function
  aspect_importance.default(model, data, predict_function,
                            new_observation, aspects_list, B, sample_method,
                            n_var, f)
}

#' @export
#' @rdname aspect_importance

aspect_importance.default <- function(x, data, predict_function = predict,
                                      new_observation,
                                      aspects_list, B = 100,
                                      sample_method = "default", n_var = 0,
                                      f = 2, show_cor = FALSE, ...) {

  # look only for common variables in data and new observation
  if ("data.frame" %in% class(data)) {
    common_variables <- intersect(colnames(new_observation), colnames(data))
    new_observation <- new_observation[, common_variables, drop = FALSE]
    data <- data[,common_variables, drop = FALSE]
  }

  # stop if no common variables are found
  stopifnot(length(common_variables) > 0,
            length(setdiff(unlist(aspects_list),
                           colnames(new_observation))) == 0)

  #number of expected coefficients cannot be negative
  stopifnot(n_var >= 0)

  # create empty matrix and data frames
  n_sample <- select_sample(data, n = B)
  n_sample_changed <- n_sample

  # sample which aspects will be replaced
  new_X <- get_sample(B, length(aspects_list), sample_method, f)

  # replace aspects
  for (i in 1:nrow(n_sample)) {
    vars <- unlist(aspects_list[new_X[i, ] == 1])
    n_sample_changed[i,vars] <- new_observation[vars]
  }

  # calculate change in predictions
  y_changed <- predict_function(x, n_sample_changed) -
    predict_function(x, n_sample)

  # fit linear model/lasso to estimate aspects importance
  colnames(new_X) <- names(aspects_list)
  new_df <- data.frame(y_changed, new_X)

  if (n_var == 0) {
    lm_model <- lm(y_changed~., data = new_df)
    model_coef <- lm_model$coefficients
  } else {
    x_new_df <- model.matrix(y_changed ~ ., data = new_df)[,-1]
    y_new_df <- y_changed
    glmnet_model <- glmnet(x_new_df, y_new_df, alpha = 1)
    indx <- min(which(glmnet_model$df >= n_var))
    model_coef <- coef(glmnet_model)[,indx]
  }

  #prepare dataframe with results
  res <- data.frame(names(model_coef),unname(model_coef))
  colnames(res) <- c("aspects","importance")
  res <- res[!res$aspects == "(Intercept)",]
  res <- res[order(-abs(res$importance)),]

  for (i in 1:length(aspects_list)) {
    res$features[i] <- aspects_list[as.character(res[i,1])]
    vars <- unlist(res$features[i])
    if (all(sapply(data[,vars], is.numeric)) & length(vars)>1 & show_cor == T) {
      cor_matrix <- cor(data[,vars], method = "spearman")
      res$min_cor[i] <- min(abs(cor_matrix))
      res$sign[i] <- ifelse(max(cor_matrix) > 0 & min(cor_matrix) < 0, "neg","pos")
    } else if (show_cor == T) {
      res$min_cor[i] <- NA
      res$sign[i] <- ''
    }
  }

  res$importance <- as.numeric(format(res$importance, digits = 4))
  class(res) <- c("aspect_importance", "data.frame")

  return(res)
}

#' Function for plotting aspect_importance results
#'
#' This function plots the results of aspect_importance.
#'
#' @param x object of aspect_importance class
#' @param bar_width bar width
#' @param ... other parameters
#'
#' @return a ggplot2 object
#'
#' @import ggplot2
#'
#' @export


plot.aspect_importance <- function(x, bar_width = 10, ...) {

  stopifnot("aspect_importance" %in% class(x))

  importance <- a_sign <- aspects <- NULL
  x$a_sign <- ifelse(x$importance > 0,"positive","negative")
  x$aspects <- reorder(x$aspects, abs(x[,2]), na.rm = TRUE)

  # plot it
  ggplot(x, aes(aspects, ymin = 0, ymax = importance, color = a_sign)) +
    geom_linerange(size = bar_width) + coord_flip() +
    ylab("Aspects importance") + xlab("") + theme_drwhy_vertical() +
    theme(legend.position = "none")
}


#' @export
#' @rdname aspect_importance
lime <- function(x, ...) {
  aspect_importance(x, ...)
}

#' Aspects importance for single aspects
#'
#' Calculates aspect_importance for single aspects (every aspect contains only
#' one feature).
#'
#' @param x a model to be explained
#' @param data dataset
#' @param predict_function predict function
#' @param new_observation selected observation with columns that corresponds to
#'   variables used in the model
#' @param B number of rows to be sampled from data
#' @param sample_method sampling method in \code{\link{get_sample}}
#' @param n_var how many non-zero coefficients for lasso fitting, if zero than
#'   linear regression is used
#' @param f frequency in in \code{\link{get_sample}}
#' @param response_variable name of response variable, should be provided if it is
#'   included in data
#'
#' @return An object of the class 'aspect_importance'. Contains dataframe that
#'   describes aspects' importance.
#'
#' @examples
#' library("DALEX")
#' titanic <- na.omit(titanic)
#' model_titanic_glm <- glm(survived == "yes" ~
#'                            class+gender+age+sibsp+parch+fare+embarked,
#'                          data = titanic, family = "binomial")
#'
#' aspect_importance_single(model_titanic_glm, titanic, new_observation = titanic[1,],
#'                   response_variable = "survived")
#'
#' @export

aspect_importance_single <- function(x, data, predict_function = predict,
                                     new_observation, B = 100,
                                     sample_method = "default", n_var = 0,
                                     f = 2, response_variable = "") {

  #remove response variable
  new_observation_changed <-
    try(new_observation <-
          new_observation[,!colnames(new_observation) == response_variable])

  if("try-error" %in% class(t)) {
    new_observation_changed <- new_observation
  }

  #create aspect list
  single_aspect_list <- vector("list", length(new_observation_changed))
  names(single_aspect_list) <- names(new_observation_changed)

  for (i in c(1:length(single_aspect_list))) {
    single_aspect_list[i] <- names(new_observation_changed)[i]
  }

  #call aspect importance function
  res_ai <- aspect_importance(x, data, predict_function,
                              new_observation_changed, single_aspect_list, B,
                              sample_method, n_var, f)

  #create data frame with results
  res_ai[,3] <- as.character(res_ai[,1])
  for (i in c(1:length(new_observation_changed))) {
    tmp_val <- new_observation_changed[as.character(res_ai[i,1])]
    if (is.numeric(tmp_val)) {
      tmp_val <- round(tmp_val, digits = 2)
    } else {
      tmp_val <- as.character(tmp_val[1,1])
    }
    res_ai[i,3] <- paste0(res_ai[i,1]," = ",tmp_val)
  }
  colnames(res_ai)[3] <- "new observation"

  return(res_ai)
}

#' Function for getting binary matrix
#'
#' Function creates binary matrix, to be used in aspect_importance method. It
#' starts with a zero matrix. Then it replaces some zeros with ones. It either
#' randomly replaces one or two zeros per row. Or replace random number of zeros
#' per row - average number of replaced zeros can be controled by parameter f.
#' Function doesn't allow the returned matrix to have rows with only zeros.
#'
#' @param n number of rows
#' @param p number of columns
#' @param sample_method sampling method
#' @param f frequency for binomial sampling
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
#' @rdname get_sample

get_sample <- function(n, p, sample_method = c("default","binom"), f = 2) {
  sample_method = match.arg(sample_method)
  stopifnot(n > 0, p > 0, f > 0)
  x <- matrix(0, n, p)
  if (sample_method == "binom") {
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


#' Groups numeric features into aspects
#'
#' Divides correlated features into groups, called aspects. Division is based on
#' correlation cutoff level.
#'
#' @param x dataframe with only numeric columns
#' @param p correlation value for cut-off level
#' @param clust_method the agglomeration method to be used,
#' see \code{\link[stats]{hclust}} methods
#' @param draw_tree if TRUE, function plots tree that illustrates grouping
#'
#' @return list of aspects
#' @export
#'
#' @importFrom stats hclust
#' @importFrom stats cor
#' @importFrom graphics plot
#' @importFrom graphics abline
#'
#' @examples
#' library("DALEX")
#' dragons_data <- dragons[,c(2,3,4,7,8)]
#' group_variables(dragons_data, p = 0.7, clust_method = "complete")
#' @export
#' @rdname group_variables

group_variables <- function(x, p = 0.5, clust_method = "complete",
                            draw_tree = FALSE) {
  stopifnot(all(sapply(x, is.numeric) ))
  stopifnot(p >= 0, p <= 1)
  val <- NULL

  # build and cut a tree
  x_hc <- hclust(as.dist(1 - abs(cor(x, method = "spearman"))),
                 method = clust_method)
  clust_list <- cutree(x_hc, h = (1 - p))

  #prepare a list with aspects grouping
  df <- data.frame(names(clust_list), unname(clust_list))
  colnames(df) <- c("name","val")
  res <- vector("list", max(clust_list))
  names(res) <- paste0("aspect.group",seq_along(res))

  for (i in c(1:length(res))) {
    res[i] <- list(as.character(subset(df,val == i)$name))
  }

  if (draw_tree == TRUE) {
    plot(x_hc, hang = -1)
    abline(h = 1 - p, lty = 2)
  }

  return(res)
}

