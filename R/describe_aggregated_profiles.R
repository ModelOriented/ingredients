#' Natural language description of feature importance explainer
#'
#' @details Function \code{describe.ceteris_paribus()} generates a natural language description of
#' ceteris paribus profile. The description summarizes variable values, that would change
#' model's prediction at most. If a ceteris paribus profile for multiple variables is passed,
#' \code{variables} must specify a single variable to be described. Works only for a ceteris paribus profile
#' for one observation. In current version only categorical values are discribed. For `display_numbers = TRUE`
#' three most important variable values are displayed, while `display_numbers = FALSE` displays
#' all the important variables, however without further details.
#'
#'
#' @importFrom graphics plot
#' @importFrom stats quantile
#'
#' @examples
#' library("DALEX")
#' library("ingredients")
#' library("randomForest")
#'
#' titanic <- na.omit(titanic)
#'
#' model_titanic_rf <- randomForest(survived == "yes" ~ gender + age + class + embarked +
#'                                   fare + sibsp + parch,  data = titanic)
#' explain_titanic_rf <- explain(model_titanic_rf,
#'                              data = titanic[,-9],
#'                              y = titanic$survived == "yes",
#'                              label = "rf")
#'
#' selected_passangers <- select_sample(titanic, n = 10)
#' cp_rf <- ceteris_paribus(explain_titanic_rf, selected_passangers)
#' pdp <- aggregate_profiles(cp_rf, type = "partial", variable_type = "categorical")
#' describe(pdp, variables = "gender")
#'
#' @export
#' @rdname describe
describe.partial_dependency_explainer <- function(x,
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


  # Generating description
  if (class(x[ ,'_x_']) == "numeric") {
    description <- describe_aggregated_profiles_continuous(x = x,
                                                           nonsignificance_treshold = nonsignificance_treshold,
                                                           display_values = display_values,
                                                           display_numbers = display_numbers,
                                                           variables = variables,
                                                           label = label,
                                                           model_name = model_name)
  } else {
    x[ ,'_x_'] <- as.character(x[ ,'_x_'])

    description <- describe_aggregated_profiles_factor(x = x,
                                                      nonsignificance_treshold = nonsignificance_treshold,
                                                      display_values = display_values,
                                                      display_numbers = display_numbers,
                                                      variables = variables,
                                                      label = label,
                                                      model_name = model_name)
  }

  class(description) <- c("aggregated_profiles_description", "description", "character")
  description
}


describe_aggregated_profiles_factor <- function(x,
                                               nonsignificance_treshold,
                                               display_values,
                                               display_numbers,
                                               variables,
                                               label,
                                               model_name) {

  # Specifying a df for easier variable description

  df_list <- specify_df_aggregated(x = x,
                                   variables = variables,
                                   nonsignificance_treshold = nonsignificance_treshold)
  df <- df_list$df
  treshold <- df_list$treshold

  #Selecting model's prediction for the observation being explained
  baseline_prediction <- attr(x, "mean_prediction")

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

    introduction <- paste0(model_name,"'s mean ", label, " is equal to ", round(baseline_prediction, 3))

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

    introduction <- paste0(model_name, "'s mean " ,
                           label, " is equal to ",
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


describe_aggregated_profiles_continuous <- function(x,
                                                   nonsignificance_treshold,
                                                   display_values,
                                                   display_numbers,
                                                   variables,
                                                   label,
                                                   model_name) {

  # Specifying a df for variable description
  df <- specify_df_aggregated(x = x, variables = variables)$df

  baseline_prediction <- attr(x, "mean_prediction")

  introduction <- paste0(model_name, "'s mean ", label,
                         " is equal to ", round(baseline_prediction, 3), ".")

  # prefix
  max_name <- df[which.max(df$`_yhat_`), variables]
  min_name <- df[which.min(df$`_yhat_`), variables]
  cutpoint <- find_optimal_cutpoint(smooth(df$`_yhat_`))
  cut_name <- round(df[cutpoint, variables], 3)

  # Test if the break point is between max_name and min_name
  multiple_breakpoints <- ifelse((cut_name < min(min_name, max_name) | cut_name > max(min_name, max_name)),
                                 TRUE,
                                 FALSE)
  if (multiple_breakpoints) {
    df_additional <- df[which(df[ ,variables] == min(min_name, max_name)):which(df[ ,variables] == max(min_name, max_name)), ]
    cutpoint_additional <- find_optimal_cutpoint(smooth(na.omit(df_additional$`_yhat_`)))
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
specify_df_aggregated <- function(x, variables, nonsignificance_treshold) {

  baseline_prediction <- attr(x, "mean_prediction")

  df <- x[which(x[ ,'_vname_'] == variables), ]

  if (nrow(df) == 0) stop("There is no such variable.")

  df <- df[ ,c("_x_","_yhat_")]

  colnames(df)[1] <- variables

  df['variable_name'] <- paste0('"', df[ ,variables],'"')

  treshold <- NULL

  if (class(df[ ,variables]) == "factor" | class(df[ ,variables]) == "character") {

    df['importance'] <- sapply(df[ ,'_yhat_'], function(x) abs(x-baseline_prediction))
    df['importance'] <- round(df['importance'],3)
    df <- df[order(df[ ,'importance'], decreasing = TRUE), ]

    # Seting the treshold
    most_important_prediction <- max(df[ ,'importance'])
    treshold <- most_important_prediction * nonsignificance_treshold

    # Modifying names for better description display
    df['important'] <- sapply(df[ ,'importance'], function(x) ifelse(x < treshold, TRUE, FALSE))

  }

  list("df" = df, "treshold" = treshold)
}
