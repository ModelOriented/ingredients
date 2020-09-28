context("Check aggregate_profiles() functions")

library("DALEX")
library("ranger")

titanic_small <- titanic_imputed[1:500,]

rf_model <- ranger(survived ~ gender + age + class + embarked +
                     fare + sibsp + parch,  data = titanic_small,
                   probability = TRUE)

explainer_rf <- explain(rf_model, data = titanic_small,
                        y = titanic_small$survived,
                        label = "RF", verbose = FALSE)

test_that("plot aggregate_profiles",{
  selected_passangers_10 <- select_neighbours(titanic_small, titanic_small[1,], n = 10)

  selected_passangers <- select_sample(titanic_small, n = 100)

  cp_rf <- ceteris_paribus(explainer_rf, selected_passangers)

  pdp_rf_p <- aggregate_profiles(cp_rf, variables = "age", type = "partial")
  pdp_rf_p$`_label_` <- "RF_partial"
  pdp_rf_c <- aggregate_profiles(cp_rf, variables = "age", type = "conditional")
  pdp_rf_c$`_label_` <- "RF_conditional"
  pdp_rf_a <- aggregate_profiles(cp_rf, variables = "age", type = "accumulated")
  pdp_rf_a$`_label_` <- "RF_accumulated"
  pl1 <- plot(pdp_rf_p, pdp_rf_c, pdp_rf_a, color = "_label_")

  expect_true("gg" %in% class(pl1))

  pdp_rf_p <- partial_dependence(explainer_rf, variables = "age")
  pdp_rf_p$`_label_` <- "RF_partial"
  pdp_rf_c <- conditional_dependence(explainer_rf, variables = "age")
  pdp_rf_c$`_label_` <- "RF_conditional"
  pdp_rf_a <- accumulated_dependence(explainer_rf, variables = "age")
  pdp_rf_a$`_label_` <- "RF_accumulated"
  pl2 <- plot(pdp_rf_p, pdp_rf_c, pdp_rf_a, color = "_label_")

  expect_true("gg" %in% class(pl2))

  expect_error(aggregate_profiles(explainer_rf, variable_type = "wrong"))
})


test_that("plot partial_dependence",{

  selected_passangers <- select_sample(titanic, n = 100)
  cp_rf <- ceteris_paribus(explainer_rf, selected_passangers)

  res <- partial_dependence(explainer_rf, N=50, variables = "gender", variable_type = "categorical")

  expect_true("aggregated_profiles_explainer" %in% class(res))
})
