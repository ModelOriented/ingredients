#' Plots Aggregated Profiles
#'
#' Function \code{plot.aggregated_profiles_explainer} plots partial dependence plot or accumulated effect plot.
#' It works in a similar way to \code{plot.ceteris_paribus}, but instead of individual profiles
#' show average profiles for each variable listed in the \code{variables} vector.
#'
#' @param x a ceteris paribus explainer produced with function \code{aggregate_profiles()}
#' @param ... other explainers that shall be plotted together
#' @param color a character. Either name of a color, or hex code for a color, or \code{_label_} if models shall be colored, or \code{_ids_} if instances shall be colored
#' @param size a numeric. Size of lines to be plotted
#' @param alpha a numeric between \code{0} and \code{1}. Opacity of lines
#' @param facet_ncol number of columns for the \code{\link[ggplot2]{facet_wrap}}
#' @param facet_scales a character value for the \code{\link[ggplot2]{facet_wrap}}. Default is \code{"free_x"}.
#' @param variables if not \code{NULL} then only \code{variables} will be presented
#' @param title a character. Partial and accumulated dependence explainers have deafult value.
#' @param subtitle a character. If \code{NULL} value will be dependent on model usage.
#'
#' @references Explanatory Model Analysis. Explore, Explain, and Examine Predictive Models. \url{https://ema.drwhy.ai/}
#'
#' @return a \code{ggplot2} object
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
#' pdp_rf_p <- partial_dependence(explain_titanic_glm, N = 50)
#' pdp_rf_p$`_label_` <- "RF_partial"
#' pdp_rf_l <- conditional_dependence(explain_titanic_glm, N = 50)
#' pdp_rf_l$`_label_` <- "RF_local"
#' pdp_rf_a<- accumulated_dependence(explain_titanic_glm, N = 50)
#' pdp_rf_a$`_label_` <- "RF_accumulated"
#' head(pdp_rf_p)
#' plot(pdp_rf_p, pdp_rf_l, pdp_rf_a, color = "_label_")
#'
#' \donttest{
#' library("ranger")
#'
#' model_titanic_rf <- ranger(survived ~., data = titanic_imputed, probability = TRUE)
#'
#' explain_titanic_rf <- explain(model_titanic_rf,
#'                               data = titanic_imputed[,-8],
#'                               y = titanic_imputed[,8],
#'                               label = "ranger forest",
#'                               verbose = FALSE)
#'
#' selected_passangers <- select_sample(titanic_imputed, n = 100)
#' cp_rf <- ceteris_paribus(explain_titanic_rf, selected_passangers)
#' cp_rf
#'
#' pdp_rf_p <- aggregate_profiles(cp_rf, variables = "age", type = "partial")
#' pdp_rf_p$`_label_` <- "RF_partial"
#' pdp_rf_c <- aggregate_profiles(cp_rf, variables = "age", type = "conditional")
#' pdp_rf_c$`_label_` <- "RF_conditional"
#' pdp_rf_a <- aggregate_profiles(cp_rf, variables = "age", type = "accumulated")
#' pdp_rf_a$`_label_` <- "RF_accumulated"
#'
#' head(pdp_rf_p)
#' plot(pdp_rf_p)
#' plot(pdp_rf_p, pdp_rf_c, pdp_rf_a)
#'
#' plot(cp_rf, variables = "age") +
#'   show_observations(cp_rf, variables = "age") +
#'   show_rugs(cp_rf, variables = "age", color = "red") +
#'   show_aggregated_profiles(pdp_rf_p, size = 2)
#'
#' }
#'
#' @export
plot.aggregated_profiles_explainer <- function(x, ...,
                                               size = 1,
                                               alpha = 1,
                                               color = "_label_",
                                               facet_ncol = NULL,
                                               facet_scales = "free_x",
                                               variables = NULL,
                                               title = NULL,
                                               subtitle = NULL) {

  # if there are more explainers, they should be merged into a single data frame
  dfl <- c(list(x), list(...))
  aggregated_profiles <- do.call(rbind, dfl)
  class(aggregated_profiles) <- "data.frame"

  all_variables <- unique(aggregated_profiles$`_vname_`)
  if (!is.null(variables)) {
    all_variables <- intersect(all_variables, variables)
    if (length(all_variables) == 0) stop(paste0("variables do not overlap with ", paste(all_variables, collapse = ", ")))

    aggregated_profiles <- aggregated_profiles[aggregated_profiles$`_vname_` %in% all_variables, ]
  }

  is_color_a_variable <- color %in% c(all_variables, "_label_", "_vname_", "_ids_")
  is_x_numeric <- is.numeric(aggregated_profiles$`_x_`)

  # initiate plot
  `_x_` <- `_yhat_` <- `_ids_` <- `_label_` <- NULL
  res <- ggplot(data = aggregated_profiles, aes(`_x_`, group = paste(`_ids_`, `_label_`)))

  # what kind of plot shall be plotted?
  # numerical
  if (is_color_a_variable & is_x_numeric) {
    nlabels <- length(unique(aggregated_profiles[,color]))
    res <- res +
      geom_line(aes_string(y = "`_yhat_`", color = paste0("`",color,"`")), size = size, alpha = alpha) +
      scale_color_manual(name = "", values = DALEX::colors_discrete_drwhy(nlabels))
  }
  if (!is_color_a_variable & is_x_numeric) {
    res <- res +
      geom_line(aes(y = `_yhat_`), size = size, alpha = alpha, color = color)
  }
  # what kind of plot shall be plotted?
  # categorical
  if (is_color_a_variable & !is_x_numeric) {
    nlabels <- length(unique(aggregated_profiles[,color]))
    res <- res +
      geom_col(aes_string(y = "`_yhat_`", fill = paste0("`",color,"`")), size = size, alpha = alpha, position = "dodge") +
      scale_fill_manual(name = "", values = DALEX::colors_discrete_drwhy(nlabels))
  }
  if (!is_color_a_variable & !is_x_numeric) {
    res <- res +
      geom_col(aes(y = `_yhat_`), size = size, alpha = alpha, fill = color, position = "dodge")
  }

  # If created with partial dependance or accumulated dependance add title
  if (is.null(title) & ("partial_dependence_explainer" %in% class(x) | "accumulated_dependence_explainer" %in% class(x))){
    # Get title form the class
    ifelse("partial_dependence_explainer" %in% class(x),
           title <- "Partial Dependence profile",
           title <- "Accumulated Dependence profile" )
  }

  # If null add subtitle
  if (is.null(subtitle)){
    # get model names and classes
    model_name <- unique(unlist(sapply(dfl, function(tmp) tmp$`_label`)))
    subtitle_models <- paste(model_name, collapse = ", ", sep = ",")
    subtitle <- paste("Created for the", subtitle_models, "model")
  }

  # adding to plot
  res <-  res +  ggtitle(title,
                         subtitle = subtitle)


  res + DALEX::theme_default_dalex() + ylab("average prediction") + xlab("") +
        theme(plot.title = element_text(hjust =0),
              plot.subtitle = element_text(vjust = -2, hjust = 0)) +
    facet_wrap(~ `_vname_`, scales = facet_scales, ncol = facet_ncol)
}

