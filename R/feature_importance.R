#' Feature Importance Plots
#'
#' This function calculates variable importance based on the drop in the Loss function after single-variable-perturbations.
#' For this reason it is also called the Variable Dropout Plot.
#'
#' Find more detailes in the \href{https://pbiecek.github.io/PM_VEE/variableImportance.html}{Feature Importance Chapter}.
#'
#' @param x a model to be explained, or an explainer created with function `DALEX::explain()`.
#' @param data validation dataset, will be extracted from `x` if it's an explainer
#' @param predict_function predict function, will be extracted from `x` if it's an explainer
#' @param y true labels for `data`, will be extracted from `x` if it's an explainer
#' @param label name of the model. By default it's extracted from the 'class' attribute of the model
#' @param loss_function a function thet will be used to assess variable importance
#' @param ... other parameters
#' @param type character, type of transformation that should be applied for dropout loss. 'raw' results raw drop lossess, 'ratio' returns \code{drop_loss/drop_loss_full_model} while 'difference' returns \code{drop_loss - drop_loss_full_model}
#' @param n_sample number of observations that should be sampled for calculation of variable importance. If NULL then variable importance will be calculated on whole dataset (no sampling).
#' @param variables vector of variables. If NULL then variable importance will be tested for each variable from the `data` separately. By default NULL
#' @param variable_groups list of variables names vectors. This is for testing joint variable importance. If NULL then variable importance will be tested separately for `variables`. By default NULL. If specified then it will override `variables`
#'
#' @references Predictive Models: Visual Exploration, Explanation and Debugging \url{https://pbiecek.github.io/PM_VEE}
#'
#' @return An object of the class 'feature_importance'.
#' It's a data frame with calculated average response.
#'
#' @export
#' @examples
#' library("DALEX")
#' titanic <- na.omit(titanic)
#' model_titanic_glm <- glm(survived == "yes" ~ gender + age + fare,
#'                        data = titanic, family = "binomial")
#'
#' explain_titanic_glm <- explain(model_titanic_glm,
#'                            data = titanic[,-9],
#'                            y = titanic$survived == "yes")
#'
#' vd_rf <- feature_importance(explain_titanic_glm)
#' plot(vd_rf)
#'
#' vd_rf_joint1 <- feature_importance(explain_titanic_glm,
#'                    variable_groups = list("demographics" = c("gender", "age"),
#'                    "ticket_type" = c("fare"))
#' )
#'
#' plot(vd_rf_joint1)
#'
#' vd_rf_joint2 <- feature_importance(explain_titanic_glm,
#'                    variable_groups = list("demographics" = c("gender", "age"),
#'                    "wealth" = c("fare", "class"),
#'                    "family" = c("sibsp", "parch"),
#'                    "embarked" = "embarked")
#' )
#'
#' plot(vd_rf_joint2, vd_rf_joint1)
#'
#' explain_titanic_glm
#'
#'
#'  \donttest{
#' library("randomForest")
#'
#'  titanic <- na.omit(titanic)
#'  model_titanic_rf <- randomForest(survived == "yes" ~ gender + age + class + embarked +
#'                                     fare + sibsp + parch,  data = titanic)
#'  explain_titanic_rf <- explain(model_titanic_rf,
#'                            data = titanic[,-9],
#'                            y = titanic$survived == "yes")
#'
#' vd_rf <- feature_importance(explain_titanic_rf)
#' plot(vd_rf)
#'
#' vd_rf_group <- feature_importance(explain_titanic_rf,
#'                    variable_groups = list("demographics" = c("gender", "age"),
#'                    "wealth" = c("fare", "class"),
#'                    "family" = c("sibsp", "parch"),
#'                    "embarked" = "embarked")
#' )
#' plot(vd_rf_group, vd_rf)
#'
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
#' plot(vd_xgb, vd_glm)
#'  }
#' @export
#' @rdname feature_importance
feature_importance <- function(x, ...)
  UseMethod("feature_importance")

#' @export
#' @rdname feature_importance
feature_importance.explainer <- function(x,
                                         loss_function = loss_root_mean_square,
                                         ...,
                                         type = "raw",
                                         n_sample = NULL,
                                         variables = NULL,
                                         variable_groups = NULL) {
  if (is.null(x$data)) stop("The feature_importance() function requires explainers created with specified 'data' parameter.")
  if (is.null(x$y)) stop("The feature_importance() function requires explainers created with specified 'y' parameter.")

  # extracts model, data and predict function from the explainer
  model <- x$model
  data <- x$data
  predict_function <- x$predict_function
  label <- x$label
  y <- x$y

  feature_importance.default(model,
                             data,
                             y,
                             predict_function,
                             loss_function = loss_function,
                             label = label,
                             type = type,
                             n_sample = n_sample,
                             variables = variables,
                             variable_groups = variable_groups,
                             ...
  )
}

#' @export
#' @rdname feature_importance
feature_importance.default <- function(x,
                                       data,
                                       y,
                                       predict_function,
                                       loss_function = loss_root_mean_square,
                                       ...,
                                       label = class(x)[1],
                                       type = "raw",
                                       n_sample = NULL,
                                       variables = NULL,
                                       variable_groups = NULL) {
  if (!is.null(variable_groups)) {
    if (!inherits(variable_groups, "list")) stop("variable_groups should be of class list")

    wrong_names <- !all(sapply(variable_groups, function(variable_set) {
        all(variable_set %in% names(data))
      }))

    if (wrong_names) stop("You have passed wrong variables names in variable_groups argument")
    if (!all(sapply(variable_groups, class) == "character")) stop("Elements of variable_groups argument should be of class character")
    if (is.null(names(variable_groups))) warning("You have passed an unnamed list. The names of variable groupings will be created from variables names.")

  }

  if (!(type %in% c("difference", "ratio", "raw")))
    stop("Type shall be one of 'difference', 'ratio', 'raw'")



  # Adding variable set name when not specified
  if (!is.null(variable_groups) && is.null(names(variable_groups))) {
    names(variable_groups) <- sapply(variable_groups, function(variable_set) {
      paste0(variable_set, collapse = "; ")
    })
  }

  # if `variable_groups` are not specified, then extract from `variables`
  if (is.null(variable_groups)) {
    # if `variables` are not specified, then extract from data
    if (is.null(variables)) {
      variables <- colnames(data)
      names(variables) <- colnames(data)
    }
  } else {
    variables <- variable_groups
  }

  #variables <- colnames(data)
  if (!is.null(n_sample)) {
    sampled_rows <- sample.int(nrow(data), n_sample, replace = TRUE)
  } else {
    sampled_rows <- 1:nrow(data)
  }
  sampled_data <- data[sampled_rows, ]
  observed <- y[sampled_rows]

  loss_0 <- loss_function(observed,
                          predict_function(x, sampled_data))
  loss_full <- loss_function(sample(observed),
                             predict_function(x, sampled_data))

  res <- sapply(variables, function(variables_set) {
    ndf <- sampled_data
    # sample variables in variables_set
    ndf[, variables_set] <- ndf[sample(1:nrow(ndf)), variables_set]

    predicted <- predict_function(x, ndf)
    loss_function(observed, predicted)
  })

  res <- sort(res)
  res <-
    data.frame(
      variable = c("_full_model_", names(res), "_baseline_"),
      dropout_loss = c(loss_0, res, loss_full)
    )
  if (type == "ratio") {
    res$dropout_loss = res$dropout_loss / loss_0
  }
  if (type == "difference") {
    res$dropout_loss = res$dropout_loss - loss_0
  }

  class(res) <- c("feature_importance_explainer", "data.frame")
  res$label <- label
  res
}
