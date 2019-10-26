#' Natural language description of Ceteris Paribus explainer
#'
#' @description  Generic function \code{describe} generates a natural language
#' description of \code{ceteris_paribus()}, \code{aggregated_profiles()} and
#' \code{feature_importance()} explanations what enchaces their interpretability.
#'
#' @details Function \code{describe.ceteris_paribus()} generates a natural language description of
#' ceteris paribus profile. The description summarizes variable values, that would change
#' model's prediction at most. If a ceteris paribus profile for multiple variables is passed,
#' \code{variables} must specify a single variable to be described. Works only for a ceteris paribus profile
#' for one observation. For \code{display_numbers = TRUE}
#' three most important variable values are displayed, while \code{display_numbers = FALSE} displays
#' all the important variables, however without further details.
#'
#' @param x a ceteris paribus explanation produced with function \code{ceteris_paribus()}
#' @param nonsignificance_treshold a parameter specifying a treshold for variable importance
#' @param ... other arguments
#' @param display_values allows for displaying variable values
#' @param display_numbers allows for displaying numerical values
#' @param variables a character of a single variable name to be described
#' @param label label for model's prediction
#'
#' @importFrom graphics plot
#' @importFrom stats setNames smooth lm
#'
#' @examples
#' library("DALEX")
#' library("ingredients")
#' library("randomForest")
#' titanic <- na.omit(titanic)
#' \donttest{
#' model_titanic_rf <- randomForest(survived == "yes" ~ gender + age + class + embarked +
#'                                  fare + sibsp + parch,  data = titanic)
#' explain_titanic_rf <- explain(model_titanic_rf,
#'                               data = titanic[,-9],
#'                               y = titanic$survived == "yes",
#'                               label = "rf")
#'
#' selected_passanger <- select_sample(titanic, n = 1, seed = 123)
#' cp_rf <- ceteris_paribus(explain_titanic_rf, selected_passanger)
#'
#' plot(cp_rf, variable_type = "categorical")
#' describe(cp_rf, variables = "class", label = "the predicted probability")
#' }
#' @export
#' @rdname describe
describe <- function(x, ...)
  UseMethod("describe")

#' @export
#' @rdname describe
describe.ceteris_paribus_explainer <- function(x,
                                               nonsignificance_treshold = 0.15,
                                               ...,
                                               display_values = FALSE,
                                               display_numbers = FALSE,
                                               variables = NULL,
                                               label = "prediction") {
  # ERROR HANDLING
  if (length(unique(x[ ,'_vname_'])) == 1) variables <- as.character(x[1,'_vname_'])
  if (is.null(variables)) stop("Choose a single variable to be described.")
  if (!class(variables) == "character") stop("Enter the single variables name as character.")

  # Assigning model's name
  model_name <- as.character(x[1,'_label_'])
  model_name <- paste(toupper(substr(model_name, 1, 1)), substr(model_name, 2, nchar(model_name)), sep="")

  # Assigning a value to the choosen variable
  variables_value <- ifelse(class(attr(x, "observations")[,variables]) == 'numeric',
                            round(attr(x, "observations")[,variables],3),
                            as.character(attr(x, "observations")[,variables]))

  value <- ifelse(display_values,
                  paste0(" (", variables, " = ",
                         variables_value, ")"),
                  "")

  # Generating description
  if (class(x[ ,variables]) == "numeric") {
    description <- describe_ceteris_paribus_continuous(x = x,
                                                      nonsignificance_treshold = nonsignificance_treshold,
                                                      display_values = display_values,
                                                      display_numbers = display_numbers,
                                                      variables = variables,
                                                      label = label,
                                                      model_name = model_name,
                                                      value = value)
  } else {
    x[ ,variables] <- as.character(x[ ,variables])

    description <- describe_ceteris_paribus_factor(x = x,
                                                  nonsignificance_treshold = nonsignificance_treshold,
                                                  display_values = display_values,
                                                  display_numbers = display_numbers,
                                                  variables = variables,
                                                  label = label,
                                                  model_name = model_name,
                                                  value = value)
  }

  class(description) <- c("ceteris_paribus_description", "description", "character")
  description
}


specify_df <- function(x, variables, nonsignificance_treshold) {

  #Creates a df for easier description generation
  df <- x[which(x[ ,'_vname_'] == variables), ]

  #ERROR HANDLING
  if (nrow(df) == 0) stop("There is no such variable.")
  if (length(unique(df[ ,variables])) != length(df[ ,variables])) {
    stop("Use aggregated_profile() function for describing a ceteris paribus explanation for more than one instance.")
  }

  df <- df[ ,c(variables,"_yhat_")]
  treshold <- NULL
  if (class(df[ ,variables]) == "factor" | class(df[ ,variables]) == "character") {
    #choosing the prediction value for the observation being explained
    baseline_prediction <- attr(x, "observations")[1,'_yhat_']
    df['importance'] <- sapply(df[ ,'_yhat_'], function(x) abs(x-baseline_prediction))
    df['importance'] <- round(df['importance'],3)
    df <- df[order(df[ ,'importance'], decreasing = TRUE), ]
    df['variable_name'] <- paste0('"', df[ ,variables],'"')
    df <- df[-which(df[ ,variables] == attr(x, "observations")[1,variables]), ]

    most_important_prediction <- max(df[ ,'importance'])
    treshold <- most_important_prediction * nonsignificance_treshold
    df['important'] <- sapply(df[ ,'importance'], function(x) if (x < treshold) FALSE else TRUE)
  }

  list("df" = df, "treshold" = treshold)
}


describe_ceteris_paribus_factor <- function(x,
                                           nonsignificance_treshold,
                                           display_values,
                                           display_numbers,
                                           variables,
                                           label,
                                           model_name,
                                           value) {

  # Specifying a df for easier variable description

  df_list <- specify_df(x = x,
                   variables = variables,
                   nonsignificance_treshold = nonsignificance_treshold)
  df <- df_list$df
  treshold <- df_list$treshold
  #Selecting model's prediction for the observation being explained
  baseline_prediction <- attr(x, "observations")[1,'_yhat_']

  # Choosing the mode of the explanation
  if (display_numbers) {
    argument1 <- NULL
    argument2 <- NULL
    argument3 <- NULL

    if (nrow(df) > 0) {
      sign1 <- if (df[1,'_yhat_'] > baseline_prediction) "increases" else "decreases"
      argument1 <- paste0("The most important change in ", model_name,
                          "'s prediction would occur for ", variables, " = ",
                          df[1,'variable_name'], ". It ",sign1,
                          " the prediction by ", df$importance[1], ".")
    }
    if (nrow(df) > 1) {
      sign2 <- if (df[2,'_yhat_'] > baseline_prediction) "increases" else "decreases"
      argument2 <- paste0("The second most important change in ", "the",
                          " prediction would occur for ", variables, " = ",
                          df[2,'variable_name'], ". It ",sign2,
                          " the prediction by ", df$importance[2], ".")
    }
    if (nrow(df) > 2) {
      sign3 <- if (df[3,'_yhat_'] > baseline_prediction) "increases" else "decreases"
      argument3 <- paste0("The third most important change in ", "the",
                          " prediction would occur for ", variables, " = ",
                          df[3,'variable_name'], ". It ",sign3,
                          " the prediction by ", df$importance[3], ".")
    }

    introduction <- paste0(model_name," predicts that for the selected instance", value, ", ",
                           label, " is equal to ", round(baseline_prediction, 3))

    argumentation <- paste(argument1, argument2, argument3, sep = " \n")

    summary <- ifelse((nrow(df) > 3),
                      paste0("Other variable values are with less importance.",
                             " They do not change the ", label, " by more than ",
                             df$importance[4],"."),
                      "All variables are being displayed.")

    description <- paste0(introduction, "\n\n",
                          argumentation, "\n\n",
                          summary)
  } else {
    df_important <- df[which(df[ ,'important'] == TRUE ), ]
    df_positive <- df[which(df_important[ ,'_yhat_'] >= baseline_prediction), ]
    df_negative <- df[which(df_important[ ,'_yhat_'] < baseline_prediction), ]

    if (nrow(df_positive) == 0) {
      arguments_increasing <-  NULL
    } else {
      increasing <- paste(df_positive[ ,'variable_name'], collapse = ", ")
      arguments_increasing <- paste0("increase substantially if the value of ",
                                     variables, " variable would change to ", increasing)
    }

    if (nrow(df_negative) == 0) {
      arguments_decreasing <- NULL
    } else {
      decreasing <- paste(df_negative[ ,'variable_name'], collapse = ", ")
      arguments_decreasing <- paste0("decrease substantially if the value of ",
                                     variables, " variable would change to ", decreasing)
    }

    introduction <- paste0("For the selected instance", value,", ",
                           label," estimated by ", model_name, " is equal to ",
                           round(baseline_prediction,3), ".")


    argumentation <- ifelse((is.null(arguments_increasing) | is.null(arguments_decreasing)),
                            paste0("Model's prediction would ", arguments_increasing,
                                   arguments_decreasing, ".\n",
                                   "The largest change would be marked if ",
                                   variables, " variable would change to ",
                                   df_important[1,"variable_name"], "."),
                            paste0("Model's prediction would ", arguments_increasing,
                                   ". On the other hand, ",
                                   model_name,"'s ", label, " would ", arguments_decreasing,
                                   ". The largest change would be marked if ",
                                   variables, " variable would change to ",
                                   df_important[1,"variable_name"], "."))

    described_all <- (nrow(df_important) == nrow(df))
    summary <- ifelse(described_all,
                      "All the variables were displayed.",
                      paste0("Other variables are with less importance and they do not change ",
                             label, " by more than ", round(treshold,2), "%."))

    description <- paste0(introduction, "\n\n",
                          argumentation, "\n\n",
                          summary)
  }

  description
}


describe_ceteris_paribus_continuous <- function(x,
                                               nonsignificance_treshold,
                                               display_values,
                                               display_numbers,
                                               variables,
                                               label,
                                               model_name,
                                               value) {

  # Specifying a df for variable description
  df <- specify_df(x = x, variables = variables)$df

  baseline_prediction <- attr(x, "observations")[1,'_yhat_']

  introduction <- paste0(model_name," predicts that for the selected instance", value, ", ",
                         label, " is equal to ", round(baseline_prediction, 3))

  # prefix
  max_name <- df[which.max(df$`_yhat_`), variables]
  min_name <- df[which.min(df$`_yhat_`), variables]
  cutpoint <- find_optimal_cutpoint_average(smooth(df$`_yhat_`))
  cut_name <- round(df[cutpoint, variables], 3)

  # Test if the break point is between max_name and min_name
  multiple_breakpoints <- ifelse((cut_name < min(min_name, max_name) | cut_name > max(min_name, max_name)),
                                 TRUE,
                                 FALSE)
  if (multiple_breakpoints) {
    df_additional <- df[which(df[ ,variables] == min(min_name, max_name)):which(df[ ,variables] == max(min_name, max_name)), ]
    cutpoint_additional <- find_optimal_cutpoint_average(smooth(na.omit(df_additional$`_yhat_`)))
  }

  breakpoint_description <- ifelse(multiple_breakpoints,
                                   paste0("Breakpoints are identified at (",
                                          variables, " = ", cut_name, " and ",
                                          variables, " = ",
                                          round(df[cutpoint_additional, variables], 3), ")."),
                                   paste0("Breakpoint is identified at (",
                                          variables, " = ", cut_name, ")."))

  prefix <- paste0("The highest prediction occurs for (", variables, " = ", max_name, "),",
                   " while the lowest for (", variables, " = ", min_name, ").\n",
                   breakpoint_description)


  cutpoint <- ifelse(multiple_breakpoints,
                     cutpoint_additional,
                     cutpoint)



  sufix <- describe_numeric_variable(original_x = attr(x, "observations"),
                                     df = df,
                                     cutpoint = cutpoint,
                                     variables = variables)


  description <- paste(introduction, prefix, sufix, sep = "\n\n")

  description
}


#:# could use some comments
find_optimal_cutpoint <- function(vector_predictions) {
  # Finds breakpoints for ceteris_paribus profile description
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


#:# could use some comments
find_optimal_cutpoint_average <- function(x) {

  diffs <- sapply(1:(length(x) - 1), function(cutpoint) {
    abs(mean(x[1:cutpoint]) - mean(x[-(1:cutpoint)]))
  })

  w <- pmin(seq_along(diffs) / length(diffs), 1 - seq_along(diffs) / length(diffs))

  which.max(diffs * w^0.25)
}


describe_numeric_variable <- function(original_x, df, cutpoint, variables) {
  # Describes a numeric variable given a cutpoint

  # Different default value for ceteris_paribus and pdp
  default <- if (!(is.null(original_x))) original_x[1, variables] else mean(df[,'_yhat_'])

  # selected point is on the left from cutpoint
  if (default <= df[cutpoint, variables]) {
    # point is higher than the average
    if (mean(df[1:cutpoint, "_yhat_"]) > mean(df[, "_yhat_"])) {
      sufix <- paste0("Average model responses are *lower* for variable values *higher* than breakpoint ")
    } else {
      sufix <- paste0("Average model responses are *higher* for variable values *higher* than breakpoint ")
    }
  } else {
    # point is higher than the average
    if (mean(df[1:cutpoint, "_yhat_"]) > mean(df[, "_yhat_"])) {
      sufix <- paste0("Average model responses are *higher* for variable values *lower* than breakpoint ")
    } else {
      sufix <- paste0("Average model responses are *lower* for variable values *lower* than breakpoint")
    }
  }

  paste0(sufix, "(= ", round(df[cutpoint, variables], 3), ").")
}


describe_numeric_variable_average <- function(original_x, x_part, cutpoint, vname) {
  # selected point is on the left from cutpoint
  if (original_x[1, vname] <= x_part[cutpoint, vname]) {
    # point is higher than the average
    if (mean(x_part[1:cutpoint, "_yhat_"]) > mean(x_part[, "_yhat_"])) {
      sufix <- paste0("Model responses are *lower* for *higher* values of ", vname, " \n")
    } else {
      sufix <- paste0("Model responses are *higher* for *higher* values of ", vname, " \n")
    }
  } else {
    # point is higher than the average
    if (mean(x_part[1:cutpoint, "_yhat_"]) > mean(x_part[, "_yhat_"])) {
      sufix <- paste0("Model responses are *higher* for *lower* values of ", vname, " \n")
    } else {
      sufix <- paste0("Model responses are *lower* for *lower* values of ", vname, " \n")
    }
  }

  sufix
}
