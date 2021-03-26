#' Calculate Oscillations for Ceteris Paribus Explainer
#'
#' Oscillations are proxies for local feature importance at the instance level.
#' Find more details in \href{http://ema.drwhy.ai/ceterisParibusOscillations.html}{Ceteris Paribus Oscillations Chapter}.
#'
#' @param x a ceteris paribus explainer produced with the \code{ceteris_paribus()} function
#' @param sort a logical value. If \code{TRUE} then rows are sorted along the oscillations
#' @param ... other arguments
#'
#' @references Explanatory Model Analysis. Explore, Explain, and Examine Predictive Models. \url{http://ema.drwhy.ai/}
#'
#' @return an object of the class \code{ceteris_paribus_oscillations}
#'
#' @examples
#' library("DALEX")
#' library("ingredients")
#'
#' titanic_small <- select_sample(titanic_imputed, n = 500, seed = 1313)
#'
#' # build a model
#' model_titanic_glm <- glm(survived ~ gender + age + fare,
#'                          data = titanic_small, family = "binomial")
#'
#' explain_titanic_glm <- explain(model_titanic_glm,
#'                                data = titanic_small[,-8],
#'                                y = titanic_small[,8])
#'
#' cp_rf <- ceteris_paribus(explain_titanic_glm, titanic_small[1,])
#'
#' calculate_oscillations(cp_rf)
#'
#' \donttest{
#' library("ranger")
#'
#' apartments_rf_model <- ranger(m2.price ~ construction.year + surface + floor +
#'                                     no.rooms + district, data = apartments)
#'
#' explainer_rf <- explain(apartments_rf_model,
#'                         data = apartments_test[,-1],
#'                         y = apartments_test$m2.price,
#'                         label = "ranger forest",
#'                         verbose = FALSE)
#'
#' apartment <- apartments_test[1,]
#'
#' cp_rf <- ceteris_paribus(explainer_rf, apartment)
#'
#' calculate_oscillations(cp_rf)
#' }
#'
#' @export
#' @rdname calculate_oscillations
calculate_oscillations <- function(x, sort = TRUE, ...) {

  stopifnot("ceteris_paribus_explainer" %in% class(x))

  observations <- attr(x, "observations")
  variables <- unique(as.character(x$`_vname_`))
  ids <- unique(as.character(x$`_ids_`))

  tmp <- lapply(variables, function(variable) {

    tmp <- lapply(ids, function(id) {
      diffs <- x[x$`_vname_` == variable & x$`_ids_` == id,"_yhat_"] - observations[id,"_yhat_"]
      data.frame(`_vname_` = variable, `_ids_` = id, oscillations = mean(abs(diffs)))
    })

    do.call(rbind, tmp)
  })

  res <- do.call(rbind, tmp)
  colnames(res) <- c("_vname_", "_ids_", "oscillations")

  if (sort) {
    res <- res[order(res$oscillations, decreasing = TRUE),]
  }

  class(res) <- c("ceteris_paribus_oscillations", "data.frame")
  res
}

