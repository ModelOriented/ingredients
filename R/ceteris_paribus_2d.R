#' Ceteris Paribus 2D Plot
#'
#' This function calculates ceteris paribus profiles for grid of values spanned by two variables.
#' It may be useful to identify or present interactions between two variables.
#'
#' Find more details in \href{https://pbiecek.github.io/PM_VEE/ceterisParibus2d}{Ceteris Paribus 2D}.
#'
#' @param explainer a model to be explained, preprocessed by the \code{DALEX::explain()} function
#' @param observation a new observation for which predictions need to be explained
#' @param grid_points number of points used for response path. Will be used for both variables
#' @param variables if specified, then only these variables will be explained
#'
#' @return an object of the class \code{ceteris_paribus_2d_explainer}.
#'
#' @examples
#' library("DALEX")
#'
#' model_titanic_glm <- glm(survived ~ gender + age + fare,
#'                        data = titanic_imputed, family = "binomial")
#'
#' explain_titanic_glm <- explain(model_titanic_glm,
#'                                data = titanic_imputed[,-8],
#'                                y = titanic_imputed[,8])
#'
#' cp_rf <- ceteris_paribus_2d(explain_titanic_glm, titanic_imputed[1,])
#' head(cp_rf)
#' plot(cp_rf)
#'
#' \donttest{
#' library("randomForest")
#' set.seed(59)
#'
#' apartments_rf_model <- randomForest(m2.price ~., data = apartments)
#'
#' explainer_rf <- explain(apartments_rf_model,
#'                         data = apartments_test[,-1],
#'                          y = apartments_test[,1])
#'
#' new_apartment <- apartments_test[1,]
#' new_apartment
#'
#' wi_rf_2d <- ceteris_paribus_2d(explainer_rf, observation = new_apartment,
#'                                variables = c("surface", "floor", "no.rooms"))
#' head(wi_rf_2d)
#' plot(wi_rf_2d)
#' }
#'
#' @importFrom stats quantile
#' @importFrom utils head
#'
#' @export
#' @rdname ceteris_paribus_2d
ceteris_paribus_2d <- function(explainer, observation, grid_points = 101, variables = NULL) {

  #:# could use some comments

  if (!("explainer" %in% class(explainer)))
    stop("The what_if() function requires an object created with explain() function.")
  if (is.null(explainer$data))
    stop("The what_if() function requires explainers created with specified 'data' parameter.")

  data <- base::as.data.frame(explainer$data)
  model <- explainer$model
  predict_function <- explainer$predict_function
  var_to_present <- which(sapply(data, is.numeric))
  names_to_present <- colnames(data)[var_to_present]

  if (!is.null(variables)) {
    names_to_present <- intersect(names_to_present, variables)
  }

  # generate all pairs of names_to_present
  v1 <- rep(names_to_present, times = rev(seq_along(names_to_present)) - 1)
  v2 <- unlist(sapply(seq_along(names_to_present), function(x) names_to_present[-(1:x)]))

  responses <- lapply(seq_along(v1), function(i) {
    vname1 <- v1[i]
    vname2 <- v2[i]
    probs <- seq(0, 1, length.out = grid_points)
    new_x1 <- min(data[,vname1]) + probs*diff(range(data[,vname1]))
    new_x2 <- min(data[,vname2]) + probs*diff(range(data[,vname2]))
    new_data <- observation[rep(1, length(new_x1) * length(new_x2)),]
    new_data[,vname1] <- rep(new_x1, each = length(new_x2))
    new_data[,vname2] <- rep(new_x2, times = length(new_x1))
    data.frame(y_hat = predict_function(model, new_data), new_x1 = new_data[,vname1], new_x2 = new_data[,vname2],
               vname1 = vname1, vname2 = vname2, label = explainer$label)
  })

  all_responses <- do.call(rbind, responses)
  new_y_hat <- predict_function(model, observation)

  attr(all_responses, "prediction") <- list(observation = observation, new_y_hat = new_y_hat)
  class(all_responses) <- c("ceteris_paribus_2d_explainer", "data.frame")

  all_responses
}


