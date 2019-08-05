#' Natural language description of a DALEX explainer
#'
#' @description  Generic function \code{describe} generates a natural language
#' description of \code{ceteris_paribus()}, \code{aggregated_profiles()} and
#' \code{feature_importance()} explanations what enchaces their interpretability.
#'
#' @details Function \code{describe.ceteris_paribus()} generates a natural language description of
#' ceteris paribus profile. The description summarizes variable values, that would change
#' model's prediction at most. If a ceteris paribus profile for multiple variables is passed,
#' \code{variables} must specify a single variable to be described. Works only for a ceteris paribus profile
#' for one observation. For `display_numbers = TRUE`
#' three most important variable values are displayed, while `display_numbers = FALSE` displays
#' all the important variables, however without further details.
#'
#' @param explainer a DALEX explainer
#' @param nonsignificance_treshold a parameter specifying a treshold for variable importance
#' @param ... other arguments
#' @param display_values allows for displaying variable values
#' @param display_numbers allows for displaying numerical values
#' @param variables a character of a single variable name to be described
#' @param label label for model's prediction
#'
#' @importFrom graphics plot
#' @importFrom stats setNames smooth
#'
#' @examples
#'library("DALEX")
#'library("ingredients")
#'library("randomForest")
#'
#'titanic <- na.omit(titanic)
#'
#'model_titanic_rf <- randomForest(survived == "yes" ~ gender + age + class + embarked +
#'                                   fare + sibsp + parch,  data = titanic)
#'explain_titanic_rf <- explain(model_titanic_rf,
#'                              data = titanic[,-9],
#'                              y = titanic$survived == "yes",
#'                              label = "rf")
#'
#'selected_passanger <- select_sample(titanic, n = 1, seed = 123)
#'cp_rf <- ceteris_paribus(explain_titanic_rf, selected_passanger)
#'plot(cp_rf, only_numerical = FALSE)
#'describe(cp_rf, variables = "class", label = "the predicted probability")
#'
#' @export
#' @rdname describe
#'


describe <- function(explainer, nonsignificance_treshold = 0.15, ...)
  UseMethod("describe")

#' @export
#' @rdname describe

describe.ceteris_paribus_explainer <- function(explainer,
                                               nonsignificance_treshold = 0.15,
                                               ...,
                                               display_values = FALSE,
                                               display_numbers = FALSE,
                                               variables = NULL,
                                               label = "prediction"){
  # ERROR HANDLING
  if (length(unique(explainer[ ,'_vname_'])) == 1) variables <- as.character(explainer[1,'_vname_'])
  if (is.null(variables)) stop("Choose a single variable to be described.")
  if (!class(variables) == "character") stop("Enter the single variables name as character.")

  # Assigning model's name
  model_name <- as.character(explainer[1,'_label_'])
  model_name <- paste(toupper(substr(model_name, 1, 1)), substr(model_name, 2, nchar(model_name)), sep="")

  variables_value <- ifelse(class(attr(explainer, "observations")[ ,variables][[1]]) == 'numeric',
                            round(attr(explainer, "observations")[ ,variables][[1]],3),
                            attr(explainer, "observations")[ ,variables][[1]]
                      )
  value <- ifelse(display_values,
                  paste0(" (", variables, " = ",
                         variables_value, ")"), "")

  # Specifying a df for variable description
  baseline_prediction <- attr(explainer, "observations")[ ,'_yhat_'][[1]]
  df <- explainer[which(explainer[ ,'_vname_'] == variables), ]
  if (nrow(df) == 0) stop("There is no such variable.")
  if (length(unique(df[ ,variables])) != length(df[ ,variables])) {
    stop("Use aggregated_profile() function for describing a ceteris paribus explanation for more than one instance.")
  }

  df <- df[ ,c(variables,"_yhat_")]

  if (class(df[ ,variables]) == "factor" | class(df[ ,variables]) == "character") {
    df['importance'] <- sapply(df[ ,'_yhat_'], function(x) abs(x-baseline_prediction))
    df['importance'] <- round(df['importance'],3)
    df <- df[order(df[ ,'importance'], decreasing = TRUE), ]
    most_important_prediction <- max(df[ ,'importance'])
    treshold <- most_important_prediction * nonsignificance_treshold

    # Modifying names for better description display
    df['variable_name'] <- sapply(df[ ,variables], function(x) paste0('"',x,'"'))

    df <- df[-which(df[ ,variables] == attr(explainer, "observations")[,variables][[1]]), ]
    df['important'] <- sapply(df[ ,'importance'], function(x) if (x < treshold) FALSE else TRUE)

    # Choosing the mode of the explanation
    if (display_numbers) {
      sign1 <- if (df[1,'_yhat_'] > baseline_prediction) "increases" else "decreases"
      argument1 <- paste0("The", df$order[1], " most important change in ", model_name,
                          "'s prediction would occur for ", variables, " = ",
                          df[1,'variable_name'], ". It ",sign1,
                          " the prediction by ", df$importance[1], ".")
      if (nrow(df) == 1) argumentation <- argument1
      if (nrow(df) >= 2) {
      sign2 <- if (df[2,'_yhat_'] > baseline_prediction) "increases" else "decreases"
      argument2 <- paste0("The second most important change in ", "the",
                          " prediction would occur for ", variables, " = ",
                          df[2,'variable_name'], ". It ",sign2,
                          " the prediction by ", df$importance[2], ".")
      }
      if (nrow(df) == 2) {
        argumentation <- paste(argument1, argument2, sep = "\n")
      }
      if (nrow(df) >= 3) {
      sign3 <- if (df[3,'_yhat_'] > baseline_prediction) "increases" else "decreases"
      argument3 <- paste0("The third most important change in ", "the",
                          " prediction would occur for ", variables, " = ",
                          df[3,'variable_name'], ". It ",sign3,
                          " the prediction by ", df$importance[3], ".")
      argumentation <- paste(argument1, argument2, argument3, sep = " \n")
      }

      introduction <- paste0(model_name," predicts that for the selected instance", value, ", ",
                             label, " is equal to ", round(baseline_prediction, 3))

      summary <- ifelse((nrow(df) > 3),
                        paste0("Other variable values are with less importance.",
                               " They do not change the ", label, " by more than ",
                               df$importance[4],"."),
                               "All variables are being displayed.")

      description <- paste0(introduction," \n",
                            argumentation, "\n",
                            summary)
    } else {
      df_important <- df[which(df[ ,'important'] == TRUE ), ]
      # Choosing three variable values
      #if (nrow(df_important) > 3) df_important <- df_important[1:3, ]
      df_positive <- df[which(df_important[ ,'_yhat_'] >= baseline_prediction), ]
      df_negative <- df[which(df_important[ ,'_yhat_'] < baseline_prediction), ]

      include_everything <- TRUE
      if (nrow(df_positive) == 0) {
        arguments_increasing <-  ""
        include_everything <- FALSE
      } else {
        increasing <- paste(df_positive[ ,'variable_name'], collapse = ", ")
        arguments_increasing <- paste0("increase substantially if the value of ",
                                       variables, " variable would change to ", increasing)
      }
      if (nrow(df_negative) == 0) {
        arguments_decreasing <-  ""
        include_everything <- FALSE
      } else {
        decreasing <- paste(df_negative[ ,'variable_name'], collapse = ", ")
        arguments_decreasing <- paste0("decrease substantially if the value of ",
                                       variables, " variable would change to ", decreasing)
      }
      introduction <- paste0("For the selected instance",value,", ",
                              label," for ", model_name, " is equal to ",
                              round(baseline_prediction,3), ".")

      described_all <- (nrow(df_important) == nrow(df))
      summary <- ifelse(described_all,
                        "All the variables were displayed.",
                        paste0("Other variables are with less importance and they do not change ",
                               label, " by more than ", round(treshold,2), "%."))

      description <- ifelse(include_everything,
                            paste0(introduction, "\n",
                                   "Model's prediction would ", arguments_increasing, ". On the other hand, for ",
                                   model_name, label, " would ", arguments_decreasing,
                                   ". The largest change would be marked if ",
                                   variables, " variable would change to ",
                                   df_important[1,"variable_name"], ". \n",
                                   summary),
                            paste0(introduction, "\n", "Model's prediction would ", arguments_increasing,
                                   arguments_decreasing, ".\n",
                                   "The largest change would be marked if ",
                                   variables, " variable would change to ",
                                   df_important[1,"variable_name"], ". \n",
                                   summary))

    }
  }

  if (class(df[ ,variables]) == "numeric") {
    df <- explainer[which(explainer[ ,'_vname_'] == variables), ]
    if (nrow(df) == 0) stop("There is no such variable.")
    if (length(unique(df[ ,variables])) != length(df[ ,variables])) {
      stop("Use aggregated_profile() function for describing a ceteris paribus explanation for more than one instance.")
    }
    df <- df[ ,c(variables,"_yhat_")]

    #introduction
    baseline_prediction <- attr(explainer, "observations")[ ,'_yhat_'][[1]]
    introduction <- paste0(model_name," predicts that for the selected instance", value, ", ",
                           label, " is equal to ", round(baseline_prediction, 3), ".")

    # prefix
    max_name <- df[which.max(df$`_yhat_`), variables]
    min_name <- df[which.min(df$`_yhat_`), variables]
    cutpoint <- find_break(smooth(df$`_yhat_`))
    cut_name <- round(df[cutpoint, variables], 3)
    prefix = paste0("The highest prediction occurs for (", variables, " = ", max_name, "),",
                    " while the lowest for (", variables, " = ", min_name, ").\n",
                    "Breakpoint is identified at (", variables, " = ", cut_name, ").")

    #sufix
    sufix <- describe_numeric_variable(original_x = attr(explainer, "observations"),
                                       df = df,
                                       cutpoint = cutpoint,
                                       variables = variables)


    description <- paste(introduction, prefix, sufix, sep = "\n")

  }
  class(description) = "ceteris_paribus_description"
  description
}

# Describes a numeric variable's ceteris_paribus profile given a cutpoint

describe_numeric_variable <- function(original_x, df, cutpoint, variables) {
  # Different default value for ceteris_paribus and pdp
  default <- if (!(is.null(original_x))) original_x[1, variables] else mean(df[,'_yhat_'])
  # selected point is on the left from cutpoint
  if (default <= df[cutpoint, variables]) {
    # point is higher than the average
    if (mean(df[1:cutpoint, "_yhat_"]) > mean(df[, "_yhat_"])) {
      sufix = paste0("Average model responses are *lower* for variable values *higher* than breakpoint.")
    } else {
      sufix = paste0("Average model responses are *higher* for variable values *higher* than breakpoint.")
    }
  } else {
    # point is higher than the average
    if (mean(df[1:cutpoint, "_yhat_"]) > mean(df[, "_yhat_"])) {
      sufix = paste0("Average model responses are *higher* for variable values *lower* than breakpoint.")
    } else {
      sufix = paste0("Average model responses are *lower* for variable values *lower* than breakpoint.")
    }
  }
  sufix
}

# Finds breakpoints for ceteris_paribus profile description

find_break <- function(vector_predictions) {
  vector_observations <- 1:length(vector_predictions)
  breakpoints_all <-sapply(vector_observations, function(x) {
    dum_x <- ifelse(vector_observations <= x, 1, 0)
    fit = lm(vector_predictions ~ vector_observations*dum_x)
    p <- summary(fit)$coefficients[,4][c('dum_x', 'vector_observations:dum_x')]
    important <- ifelse(any(is.na(p)), NA,
                        if (all(unname(p) < 0.5)) abs(summary(fit)$coefficients[2,1] - summary(fit)$coefficients[3,1])  else 0)
    setNames(important, x)
  })
  breakpoints_all <- breakpoints_all[order(abs(breakpoints_all), decreasing = TRUE, na.last = TRUE)]
  as.numeric(names(breakpoints_all[1]))
}

