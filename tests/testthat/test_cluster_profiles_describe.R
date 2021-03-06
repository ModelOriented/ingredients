context("Check describe() functions")

test_that("describe cluster_profiles",{
  library("DALEX")
  library("ranger")

  titanic_small <- titanic_imputed[1:500,]

  rf_model <- ranger(survived ~.,  data = titanic_small, probability = TRUE)

  explainer_rf <- explain(rf_model, data = titanic_small,
                          y = titanic_small$survived, label = "RF",
                          verbose = FALSE)

  selected_passangers <- select_sample(titanic_small, n = 1)
  cp_rf <- ceteris_paribus(explainer_rf, selected_passangers)
  desc_cp_rf <- describe(cp_rf, variables = "age")


  expect_true("ceteris_paribus_description" %in% class(desc_cp_rf))
})
