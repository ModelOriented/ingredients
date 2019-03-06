context("Check aggregate_profiles() function")

test_that("test plot",{

  selected_passangers <- select_sample(titanic_small, n = 100)
 cp_rf <- ceteris_paribus(explainer_rf, selected_passangers)

   pdp_rf <- aggregate_profiles(cp_rf, selected_variables = "Age")
  pl <- plot(cp_rf, selected_variables = "Age") +
     show_observations(cp_rf, selected_variables = "Age") +
     show_rugs(cp_rf, selected_variables = "Age", color = "red") +
     show_aggreagated_profiles(pdp_rf, size = 2)

  expect_true("gg" %in% class(pl))
})

test_that("test ceteris_paribus",{

  selected_passangers <- select_sample(titanic_small, n = 100)
  cp_rf <- ceteris_paribus(explainer_rf, selected_passangers)
  pl <- plot(cp_rf, selected_variables = "Age") +
   show_observations(cp_rf, selected_variables = "Age") +
     show_rugs(cp_rf, selected_variables = "Age", color = "red")

  expect_true("gg" %in% class(pl))
})

