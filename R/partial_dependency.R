#' Partial Dependency Plots
#'
#' Function 'partial_dependency' calles 'ceteris_paribus' and then 'aggregate_profiles'
#'
#' @param x a model to be explained, or an explainer created with function `DALEX::explain()`.
#' @param data validation dataset, will be extracted from `x` if it's an explainer
#' @param predict_function predict function, will be extracted from `x` if it's an explainer
#' @param variables names of variables for which profiles shall be calculated. Will be passed to `calculate_variable_splits()`. If NULL then all variables from the validation data will be used.
#' @param N number of observations used for calculation of partial dependency profiles. By default 500.
#' @param ... other parameters
#' @param variable_splits named list of splits for variables, in most cases created with `calculate_variable_splits()`. If NULL then it will be calculated based on validation data avaliable in the `explainer`.
#' @param grid_points number of points for profile. Will be passed to `calculate_variable_splits()`.
#' @param label name of the model. By default it's extracted from the 'class' attribute of the model
#'
#' @return an 'aggregated_ceteris_paribus_explainer' layer
#' @examples
#' library("DALEX")
#'  \dontrun{
#' library("titanic")
#' library("randomForest")
#'
#' titanic_small <- titanic_train[,c("Survived", "Pclass", "Sex", "Age",
#'                                    "SibSp", "Parch", "Fare", "Embarked")]
#' titanic_small$Survived <- factor(titanic_small$Survived)
#' titanic_small$Sex <- factor(titanic_small$Sex)
#' titanic_small$Embarked <- factor(titanic_small$Embarked)
#' titanic_small <- na.omit(titanic_small)
#' rf_model <- randomForest(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
#'                          data = titanic_small)
#' explainer_rf <- explain(rf_model, data = titanic_small,
#'                         y = titanic_small$Survived == "1", label = "RF")
#'
#' pdp_rf <- partial_dependency(explainer_rf, selected_variables = "Age")
#' pdp_rf
#'
#' plot(pdp_rf)
#'
#' pdp_rf <- partial_dependency(explainer_rf)
#' pdp_rf
#'
#' plot(pdp_rf)
#' }
#' @export
#' @rdname partial_dependency
partial_dependency <- function(x, ...)
  UseMethod("partial_dependency")

#' @export
#' @rdname partial_dependency
partial_dependency.explainer <- function(x, variables = NULL, N = 500,
                                      variable_splits = NULL, grid_points = 101,
                                      ...) {
  # extracts model, data and predict function from the explainer
  model <- x$model
  data <- x$data
  predict_function <- x$predict_function
  label <- x$label

  partial_dependency.default(model, data, predict_function,
                          label = label,
                          variables = variables,
                          grid_points = grid_points,
                          variable_splits = variable_splits,
                          N = N,
                          ...)
}


#' @export
#' @rdname partial_dependency
partial_dependency.default <- function(x, data, predict_function = predict,
                           label = class(x)[1],
                           variables = NULL,
                           grid_points = grid_points,
                           variable_splits = variable_splits,
                           N = 500,
                           ...) {
  if (N < nrow(data)) {
    # sample N points
    ndata <- data[sample(1:nrow(data), N),]
  } else {
    ndata <- data
  }

  cp <- ceteris_paribus.default(x, data, predict_function = predict_function,
                            ndata, variables = variables,
                            grid_points = grid_points,
                            variable_splits = variable_splits,
                            label = label, ...)

  aggregate_profiles(cp, selected_variables = variables)
}

