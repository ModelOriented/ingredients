context("Check describe() functions")

test_that("describe cluster_profiles",{
  library("DALEX")
  library("randomForest")

  titanic_small <- na.omit(titanic[1:500,])

  rf_model <- randomForest(survived ~ gender + age + class + embarked +
                             fare + sibsp + parch,  data = titanic_small)

  explainer_rf <- explain(rf_model, data = titanic_small,
                          y = titanic_small$survived == "yes", label = "RF")

  selected_passangers <- select_sample(titanic_small, n = 1)
  cp_rf <- ceteris_paribus(explainer_rf, selected_passangers)
  desc_cp_rf <- describe(cp_rf, variables = "age")


  expect_true("ceteris_paribus_description" %in% class(desc_cp_rf))
})
