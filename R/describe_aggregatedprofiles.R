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
#' selected_passangers <- select_sample(titanic, n = 10)
#' cp_rf <- ceteris_paribus(explain_titanic_rf, selected_passangers)
#' pdp <- aggregate_profiles(cp_rf, type = "partial", only_numerical = FALSE)
#' describe(pdp, variables = "gender")
#'
#' @export
#' @rdname describe


describe.aggregated_profiles_explainer <- function(explainer,
                                                   nonsignificance_treshold = 0.15,
                                                   ...,
                                                   display_values = FALSE,
                                                   display_numbers = FALSE,
                                                   variables = NULL,
                                                   label = "prediction"){
  # ERROR HANDLING
  if (length(unique(explainer[ ,'_vname_'])) == 1) variables <- as.character(explainer[1,'_vname_'])
  if (is.null(variables)) stop("Choose a single variable to be described.")
  if (!class(variables) == "character") stop("Enter the single variable's name as character.")

  # Assigning model's name
  model_name <- as.character(explainer[1,'_label_'])
  model_name <- paste(toupper(substr(model_name, 1, 1)), substr(model_name, 2, nchar(model_name)), sep="")

  # Specifying a df for variable description
  baseline_prediction <- attr(explainer, "mean_prediction")
  df <- explainer[which(explainer[ ,'_vname_'] == variables), ]
  if (nrow(df) == 0) stop("There is no such variable.")
  df <- df[ ,c("_x_","_yhat_")]
  df['importance'] <- sapply(df[ ,'_yhat_'], function(x) abs(x-baseline_prediction))
  df['importance'] <- round(df['importance'],3)
  df <- df[order(df[ ,'importance'], decreasing = TRUE), ]

  # Seting the treshold
  most_important_prediction <- max(df[ ,'importance'])
  treshold <- most_important_prediction * nonsignificance_treshold

  # Modifying names for better description display
  df['variable_name'] <- sapply(df[ ,'_x_'], function(x) paste0('"',x,'"'))

  # Describing a factor variable
  if (class(df[ ,'_x_']) == "factor" | class(df[ ,'_x_']) == "character") {
    df['important'] <- sapply(df[ ,'importance'], function(x) if (x < treshold) FALSE else TRUE)

    # Choosing the mode of the explanation
    if (display_numbers) {
      sign1 <- if (df[1,'_yhat_'] > baseline_prediction) "increases" else "decreases"
      argument1 <- paste0("The most important change in ", model_name,
                          "'s prediction would occur for ", variables, " = ",
                          df[1,'variable_name'], ". It ",sign1,
                          " the prediction by ", df$importance[1], ".")
      if (nrow(df) == 1) argumentation <- argument1
      if (nrow(df) >= 2) {
        df[2, 'order'] <- "second"
        sign2 <- if (df[2,'_yhat_'] > baseline_prediction) "increases" else "decreases"
        argument2 <- paste0("The ", df$order[2], " most important change in ", "the",
                            " prediction would occur for ", variables, " = ",
                            df[2,'variable_name'], ". It ",sign2,
                            " the prediction by ", df$importance[2], ".")
      }
      if (nrow(df) == 2) {
        argumentation <- paste(argument1, argument2, sep = "\n")
      }
      if (nrow(df) > 2) {
        df[3, 'order'] <- "third"
        sign3 <- if (df[3,'_yhat_'] > baseline_prediction) "increases" else "decreases"
        argument3 <- paste0("The ", df$order[3], " most important change in ", "the",
                            " prediction would occur for ", variables, " = ",
                            df[3,'variable_name'], ". It ",sign3,
                            " the prediction by ", df$importance[3], ".")
        argumentation <- paste(argument1, argument2, argument3, sep = " \n")
      }

      introduction <- paste0(model_name," predicts that for the selected instance, ",
                             label, " is equal to ", round(baseline_prediction, 3))
      summary <- ifelse((nrow(df) > 3),
                        paste0("Other variable values are with less importance.",
                        " They do not change model's mean prediction by more than ",
                        df$importance[4],"."),
                        "All variables are being displayed.")


      description <- paste0(introduction," \n",
                            argumentation, "\n", summary)
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
      described_all <- (nrow(df_important) == nrow(df))
      summary <- ifelse(described_all,
                        "All the variables were displayed.",
                        paste0("Other variables are with less importance and they do not change ",
                               label, " by more than ", round(treshold,2), "%."))


      introduction <- paste0("The average, ",
                             label," for ", model_name, " is equal to ",
                             round(baseline_prediction,3), ".")
      if (!include_everything) {
        description <- paste0(introduction, "\n",
                              "Model's prediction would ", arguments_increasing,
                              arguments_decreasing,
                              ". The largest change would be marked if ",
                              variables, " variable would change to ", df_important[1,"variable_name"], ". \n",
                              summary)
      } else {
        description <- paste0(introduction, "\n",
                              "Model's prediction would ", arguments_increasing, ". On the other hand, for ",
                              model_name, label, " would ", arguments_decreasing,
                              ". The largest change would be marked if ",
                              variables, " variable would change to ", df_important[1,"variable_name"], ". \n",
                              summary)
      }
    }
  }


  if (class(df[ ,'_x_']) == "numeric") {
    df <- explainer[which(explainer[ ,'_vname_'] == variables), ]
    if (nrow(df) == 0) stop("There is no such variable.")
    if (length(unique(df[ ,'_x_'])) != length(df[ ,'_x_'])) {
      stop("Use aggregated_profile() function for describing a ceteris paribus explanation for more than one instance.")
    }
    df <- df[ ,c('_x_',"_yhat_")]

    #introduction
    #baseline_prediction <- attr(explainer, "observations")[ ,'_yhat_'][[1]]
    introduction <- paste0("The average ", label, " of ", model_name,
                           " is equal to ", round(baseline_prediction, 3), ".")

    # prefix
    max_name <- df[which.max(df$`_yhat_`), '_x_']
    min_name <- df[which.min(df$`_yhat_`), '_x_']
    cutpoint <- find_break(smooth(df$`_yhat_`))
    cut_name <- round(df[cutpoint, '_x_'], 3)

    # Test if the break point is between max_name and min_name
    multiple_breakpoints <- ifelse((cut_name < min(min_name, max_name) | cut_name > max(min_name, max_name)),
                                   TRUE,
                                   FALSE)
    if (multiple_breakpoints) {
      df_additional <- df[which(df[ ,'_x_'] == min(min_name, max_name)):which(df[ ,'_x_'] == max(min_name, max_name)), ]
      cutpoint_additional <- find_break(smooth(na.omit(df_additional$`_yhat_`)))
    }
    breakpoint_description <- ifelse(multiple_breakpoints,
                                     paste0("Breakpoints are identified at (",
                                            variables, " = ", cut_name, " and ",
                                            variables, " = ",
                                            round(df[cutpoint_additional, '_x_'], 3), ")."),
                                     paste0("Breakpoint is identified at (",
                                            variables, " = ", cut_name, ")."))

    prefix <- paste0("The highest prediction occurs for (", variables, " = ", max_name, "),",
                     " while the lowest for (", variables, " = ", min_name, ").\n",
                     breakpoint_description)

    #sufix
    cutpoint <- ifelse(multiple_breakpoints,
                       cutpoint_additional,
                       cutpoint)

    sufix <- describe_numeric_variable(original_x = attr(explainer, "observations"),
                                       df = df,
                                       cutpoint = cutpoint,
                                       variables = '_x_')


    description <- paste(introduction, prefix, sufix, sep = "\n")

  }
  class(description) <- c("ceteris_paribus_description", "character")
  description
}
