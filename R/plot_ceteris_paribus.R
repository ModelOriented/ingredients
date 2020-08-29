#' @title Plots Ceteris Paribus Profiles
#'
#' @description
#' Function \code{plot.ceteris_paribus_explainer} plots Individual Variable Profiles for selected observations.
#' Various parameters help to decide what should be plotted, profiles, aggregated profiles, points or rugs.
#'
#' Find more detailes in \href{https://pbiecek.github.io/ema/ceterisParibus.html}{Ceteris Paribus Chapter}.
#'
#' @param x a ceteris paribus explainer produced with function \code{ceteris_paribus()}
#' @param ... other explainers that shall be plotted together
#' @param color a character. Either name of a color or name of a variable that should be used for coloring
#' @param size a numeric. Size of lines to be plotted
#' @param alpha a numeric between \code{0} and \code{1}. Opacity of lines
#' @param facet_ncol number of columns for the \code{\link[ggplot2]{facet_wrap}}
#' @param variables if not \code{NULL} then only \code{variables} will be presented
#' @param variable_type a character. If \code{numerical} then only numerical variables will be plotted.
#' If \code{categorical} then only categorical variables will be plotted.
#' @param title a character. Plot title. By default "Ceteris Paribus profile".
#' @param subtitle a character. Plot subtitle. By default \code{NULL} - then subtitle is set to "created for the XXX, YYY model",
#' where XXX, YYY are labels of given explainers.
#' @param categorical_type a character. How categorical variables shall be plotted? Either \code{"profiles"} (default) or \code{"bars"} or \code{"lines"}.
#'
#' @return a \code{ggplot2} object
#'
#' @import ggplot2
#' @importFrom stats aggregate
#' @importFrom scales trans_new
#'
#' @references Explanatory Model Analysis. Explore, Explain and Examine Predictive Models. \url{https://pbiecek.github.io/ema}
#'
#' @examples
#' library("DALEX")
#'
#' model_titanic_glm <- glm(survived ~ gender + age + fare,
#'                          data = titanic_imputed, family = "binomial")
#'
#' explain_titanic_glm <- explain(model_titanic_glm,
#'                                data = titanic_imputed[,-8],
#'                                y = titanic_imputed[,8],
#'                                verbose = FALSE)
#'
#' cp_glm <- ceteris_paribus(explain_titanic_glm, titanic_imputed[1,])
#' cp_glm
#'
#' plot(cp_glm, variables = "age")
#'
#' \donttest{
#' library("randomForest")
#' model_titanic_rf <- randomForest(survived ~.,  data = titanic_imputed)
#'
#' explain_titanic_rf <- explain(model_titanic_rf,
#'                               data = titanic_imputed[,-8],
#'                               y = titanic_imputed[,8],
#'                               label = "Random Forest v7",
#'                               verbose = FALSE)
#'
#' selected_passangers <- select_sample(titanic_imputed, n = 100)
#'
#' cp_rf <- ceteris_paribus(explain_titanic_rf, selected_passangers)
#' cp_rf
#'
#' plot(cp_rf, variables = "age") +
#'   show_observations(cp_rf, variables = "age") +
#'   show_rugs(cp_rf, variables = "age", color = "red")
#'
#' selected_passangers <- select_sample(titanic_imputed, n = 1)
#' selected_passangers
#'
#' cp_rf <- ceteris_paribus(explain_titanic_rf, selected_passangers)
#'
#' plot(cp_rf) +
#'   show_observations(cp_rf)
#'
#' plot(cp_rf, variables = "age") +
#'   show_observations(cp_rf, variables = "age")
#'
#' plot(cp_rf, variables = "class")
#' plot(cp_rf, variables = c("class", "embarked"), facet_ncol = 1)
#' plot(cp_rf, variables = c("class", "embarked"), facet_ncol = 1, categorical_type = "bars")
#' plotD3(cp_rf, variables = c("class", "embarked", "gender"),
#'               variable_type = "categorical", scale_plot = TRUE,
#'               label_margin = 70)
#'
#' }
#' @export
plot.ceteris_paribus_explainer <- function(x, ...,
   size = 1,
   alpha = 1,
   color = "#46bac2",
   variable_type = "numerical",
   facet_ncol = NULL,
   variables = NULL,
   title = "Ceteris Paribus profile",
   subtitle = NULL,
   categorical_type = "profiles") {

  check_variable_type(variable_type)

  # if there is more explainers, they should be merged into a single data frame
  dfl <- c(list(x), list(...))
  all_profiles <- do.call(rbind, dfl)
  class(all_profiles) <- "data.frame"

  all_profiles$`_ids_` <- factor(all_profiles$`_ids_`)

  # fix for https://github.com/ModelOriented/ingredients/issues/123 set colors to _label_ with multiple models
  if (color == "#46bac2" && length(unique(all_profiles$`_label_`)) > 1)
    color <- "_label_"

  # extract labels to use in the default subtitle
  if (is.null(subtitle)) {
    labels <- paste0(unique(all_profiles$`_label_`), collapse = ", ")
    subtitle <- paste0("created for the ", labels, " model")
  }

  # variables to use
  all_variables <- na.omit(as.character(unique(all_profiles$`_vname_`)))
  if (!is.null(variables)) {
    all_variables <- intersect(all_variables, variables)
    if (length(all_variables) == 0) stop(paste0("variables do not overlap with ", paste(all_variables, collapse = ", ")))
  }

  # is color a variable or literal?
  if (color %in% c(all_variables, "_label_", "_vname_", "_ids_")) {
    color_values <- DALEX::colors_discrete_drwhy(length(unique(all_profiles[,color])))
  } else {
    color_values <- NULL # if NULL then don't color by variable
  }

  # only numerical or only factors?
  is_numeric <- sapply(all_profiles[, all_variables, drop = FALSE], is.numeric)

  if (variable_type == "numerical") {
    vnames <- names(which(is_numeric))

    # there are no numerical variables
    if (length(vnames) == 0) {
      # change to categorical
      variable_type <- "categorical"
      # send message
      message("'variable_type' changed to 'categorical' due to lack of numerical variables.")
      # take all
      vnames <- all_variables
    } else if (!is.null(variables) && length(vnames) != length(variables)) {
      message("Non-numerical variables (from the 'variables' argument) are rejected.")
    }
  } else {
    vnames <- names(which(!is_numeric))

    # there are variables selected
    if (!is.null(variables)) {
      # take all
      vnames <- all_variables
    } else if (length(vnames) == 0) {
      # there were no variables selected and there are no categorical variables
      stop("There are no non-numerical variables.")
    }
  }

  # how to plot profiles
  if (variable_type == "numerical") {
    # select only suitable variables  either in vnames or in variables
    all_profiles <- all_profiles[all_profiles$`_vname_` %in% vnames, ]
    pl <- plot_numerical_ceteris_paribus(all_profiles = all_profiles,
                                         color_values = color_values,
                                         facet_ncol = facet_ncol,
                                         color = color,
                                         size = size,
                                         alpha = alpha)
  } else {
    # select only suitable variables  either in vnames or in variables
    all_profiles <- all_profiles[all_profiles$`_vname_` %in% c(vnames, variables), ]
    if (is.null(variables)) {
      variables <- vnames
    }
    pl <- switch(categorical_type,
           profiles = plot_categorical_ceteris_paribus_profiles(
                   all_profiles = all_profiles,
                   selected_observation = attr(x, "observation"),
                   variables = variables,
                   color_values = color_values,
                   facet_ncol = facet_ncol,
                   color = color,
                   size = size,
                   alpha = alpha),
           lines = plot_categorical_ceteris_paribus(
                      all_profiles = all_profiles,
                      selected_observation = attr(x, "observation"),
                      variables = variables,
                      color_values = color_values,
                      facet_ncol = facet_ncol,
                      color = color,
                      size = size,
                      alpha = alpha),
            bars = plot_categorical_ceteris_paribus_bars(
                      all_profiles = all_profiles,
                      selected_observation = attr(x, "observation"),
                      variables = variables,
                      color_values = color_values,
                      facet_ncol = facet_ncol,
                      color = color,
                      size = size,
                      alpha = alpha),
            stop("`categorical_type` shall be either `lines`, `profiles` or `bars`")
    )
  }
  pl +
    labs(title = title, subtitle = subtitle) +
    xlab("") + ylab("prediction")
}


plot_numerical_ceteris_paribus <- function(all_profiles,
                                           color_values,
                                           facet_ncol = NULL,
                                           color = "#46bac2",
                                           size = 1,
                                           alpha = 1) {
  # create _x_
  tmp <- as.character(all_profiles$`_vname_`)
  for (i in seq_along(tmp)) {
    all_profiles$`_x_`[i] <- all_profiles[i, tmp[i]]
  }

  # prepare plot
  `_x_` <- `_yhat_` <- `_ids_` <- `_label_` <- NULL
  pl <- ggplot(all_profiles, aes(`_x_`, `_yhat_`, group = paste(`_ids_`, `_label_`)))

  # show profiles without aggregation
  if (!is.null(color_values)) {
    pl <- pl +
      geom_line(data = all_profiles, aes_string(color = paste0("`", color, "`")), size = size, alpha = alpha) +
      scale_color_manual(name = "", values = color_values)
  } else {
    pl <- pl + geom_line(data = all_profiles, size = size, alpha = alpha, color = color)
  }

  pl +
    facet_wrap(~`_vname_`, scales = "free_x", ncol = facet_ncol) +
    DALEX::theme_drwhy()
}


plot_categorical_ceteris_paribus <- function(all_profiles,
                                             selected_observation,
                                             variables,
                                             color_values,
                                             facet_ncol = NULL,
                                             color = "#46bac2",
                                             size = 2,
                                             alpha = 1) {

  lsc <- lapply(variables, function(sv) {
    tmp <- all_profiles[all_profiles$`_vname_` == sv,
                        c(sv, "_vname_", "_yhat_", "_label_", "_ids_")]
    # instances to be merged
    key <- selected_observation[, sv, drop = FALSE]
    # add right values to profiles
    tmp$`_real_point_` <- tmp[, sv] == key[as.character(tmp$`_ids_`), sv]
    tmp$`_vname_value_` <- paste(tmp$`_vname_`, "=", key[as.character(tmp$`_ids_`), sv])
    colnames(tmp)[1] <- "_x_"
    tmp$`_x_` <- as.character(tmp$`_x_`)
    tmp
  })
  # transformed data frame
  selected_cp_flat <- do.call(rbind, lsc)
  `_yhat_` <- NULL

  if (!is.null(color_values)) {
    selected_cp_flat$`_ids_color_` <- paste(selected_cp_flat[,"_ids_"], selected_cp_flat[,color], sep = "_")
    pl <- ggplot(selected_cp_flat, aes_string("`_x_`", "`_yhat_`", group = "`_ids_color_`", color = paste0("`",color,"`"))) +
      scale_color_manual(name = "", values = color_values) +
      geom_errorbar(aes(ymin = `_yhat_`, ymax = `_yhat_`), size = size/2, alpha = alpha) +
#      geom_line(size = size/2, alpha = alpha) +
      geom_point(data = selected_cp_flat[selected_cp_flat$`_real_point_`, ],
                 size = size, alpha = alpha)
  } else {
    pl <- ggplot(selected_cp_flat, aes_string("`_x_`", "`_yhat_`", group = "`_ids_`")) +
      geom_errorbar(aes(ymin = `_yhat_`, ymax = `_yhat_`), size = size/2, alpha = alpha, color = color) +
      #      geom_line(size = size/2, alpha = alpha) +
      geom_point(data = selected_cp_flat[selected_cp_flat$`_real_point_`, ],
                 color = color, size = size, alpha = alpha)
  }

  # prepare plot
  pl +
    DALEX::theme_drwhy()  +
    facet_wrap(~`_vname_`, scales = "free_x", ncol = facet_ncol) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}


plot_categorical_ceteris_paribus_profiles <- function(all_profiles,
                                                      selected_observation,
                                                      variables,
                                                      color_values,
                                                      facet_ncol = NULL,
                                                      color = "#46bac2",
                                                      size = 2,
                                                      alpha = 1) {

  lsc <- lapply(variables, function(sv) {
    tmp <- all_profiles[all_profiles$`_vname_` == sv,
                        c(sv, "_vname_", "_yhat_", "_label_", "_ids_")]
    # instances to be merged
    key <- selected_observation[, sv, drop = FALSE]
    # add right values to profiles
    tmp$`_real_point_` <- tmp[, sv] == key[as.character(tmp$`_ids_`), sv]
    tmp$`_vname_value_` <- paste(tmp$`_vname_`, "=", key[as.character(tmp$`_ids_`), sv])
    colnames(tmp)[1] <- "_x_"
    tmp$`_x_` <- as.character(tmp$`_x_`)
    tmp
  })
  # transformed data frame
  selected_cp_flat <- do.call(rbind, lsc)
  `_yhat_` <- NULL

  if (!is.null(color_values)) {
    selected_cp_flat$`_ids_color_` <- paste(selected_cp_flat[,"_ids_"], selected_cp_flat[,color], sep = "_")
    pl <- ggplot(selected_cp_flat, aes_string("`_x_`", "`_yhat_`", group = "`_ids_color_`", color = paste0("`",color,"`"))) +
      scale_color_manual(name = "", values = color_values) +
      geom_line(size = size/2, alpha = alpha) +
      geom_line(data = selected_cp_flat[selected_cp_flat$`_real_point_`, ],
                 size = size, alpha = alpha)
  } else {
    pl <- ggplot(selected_cp_flat, aes_string("`_x_`", "`_yhat_`", group = "`_ids_`")) +
      geom_line(size = size/2, alpha = alpha, color = color) +
      geom_point(data = selected_cp_flat[selected_cp_flat$`_real_point_`, ],
                 color = color, size = size, alpha = alpha)
  }

  # prepare plot
  pl +
    DALEX::theme_drwhy()  +
    facet_wrap(~`_vname_`, scales = "free_x", ncol = facet_ncol) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}


plot_categorical_ceteris_paribus_bars <- function(all_profiles,
                                                  selected_observation,
                                                  variables,
                                                  color_values,
                                                  facet_ncol = NULL,
                                                  color = "#46bac2",
                                                  size = 2,
                                                  alpha = 1) {

  lsc <- lapply(variables, function(sv) {
    tmp <- all_profiles[all_profiles$`_vname_` == sv,
                        c(sv, "_vname_", "_yhat_", "_label_", "_ids_")]
    # instances to be merged
    key <- selected_observation[, sv, drop = FALSE]
    # add right values to profiles
    tmp$`_real_point_` <- tmp[, sv] == key[as.character(tmp$`_ids_`), sv]
    tmp$`_vname_value_` <- paste(tmp$`_vname_`, "=", key[as.character(tmp$`_ids_`), sv])
    colnames(tmp)[1] <- "_x_"
    tmp$`_x_` <- as.character(tmp$`_x_`)
    tmp
  })
  # transformed data frame
  selected_cp_flat <- do.call(rbind, lsc)

  selected_y <- selected_observation$`_yhat_`
  t_shift <- trans_new("shift",
                       transform = function(x) {x - selected_y},
                       inverse = function(x) {x + selected_y})

  # prepare plot
  `_vname_` <- NULL

  if (!is.null(color_values)) {
    pl <- ggplot(selected_cp_flat, aes_string("`_x_`", "`_yhat_`", fill = paste0("`",color,"`"))) +
      geom_col(position = "dodge") +
      scale_fill_manual(name = "", values = color_values)
  } else {
    pl <- ggplot(selected_cp_flat, aes_string("`_x_`", "`_yhat_`")) +
      geom_col(fill = color)
  }

  pl +
    facet_wrap(~`_vname_`, scales = "free_y", ncol = facet_ncol)+
    scale_y_continuous(trans = t_shift) +
    coord_flip() +
    DALEX::theme_drwhy_vertical() +
    geom_hline(yintercept=selected_y, size = 0.5, linetype = 2, color = "#371ea3") +
    xlab("") + ylab("prediction")
}
