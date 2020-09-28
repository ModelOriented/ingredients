context("Check describe() function")

library("DALEX")
library("ranger")

model_titanic_rf <- ranger(survived ~., data = titanic_imputed, probability = TRUE)
explain_titanic_rf <- explain(model_titanic_rf,
                              data = titanic_imputed[,-8],
                              y = titanic_imputed$survived,
                              label = "rf", verbose = FALSE)

selected_passanger <- select_sample(titanic_imputed, n = 1)
cp_rf <- ceteris_paribus(explain_titanic_rf, selected_passanger, variable_type = "categorical")
description <- describe(cp_rf, variables = "gender", display_numbers = TRUE,
         label = "the predicted probability")

test_that("Output format", {
  expect_is(description, "ceteris_paribus_description")
})

variables <- c("gender", "class", "embarked")
n <- 2
test <- expand.grid(replicate(n, c(TRUE,FALSE), simplify = FALSE))
test_result <- sapply(variables, function(y) {
  apply(test, MARGIN = 1, function(x){
    selected_passanger <- select_sample(titanic, n = 1)
    cp_rf <- ceteris_paribus(explain_titanic_rf, selected_passanger)
    description <- describe(cp_rf,
                          variables = y,
                          display_values = x[[1]],
                          display_numbers = x[[2]])

    test_that("Output format", {
      expect_is(description, "ceteris_paribus_description")
    })
  })
})


test_result <- sapply(variables, function(y) {
    pdp <- aggregate_profiles(cp_rf, type = "partial", variable_type = "categorical")
    description <- describe(pdp,
                            variables = y,
                            display_numbers = TRUE)
    test_that("Output format", {
      expect_is(description, "aggregated_profiles_description")
    })
    description <- describe(pdp,
                            variables = y,
                            display_numbers = FALSE)
    test_that("Output format", {
      expect_is(description, "aggregated_profiles_description")
    })
})

fi_lm <- feature_importance(explain_titanic_rf)
test_that("Output format", {
  expect_is(describe(fi_lm), "feature_importance_description")
})

