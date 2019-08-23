context("Check aggregate_profiles() function")

test_that("test plot",{

  selected_passangers <- select_sample(titanic_small, n = 100)
 cp_rf <- ceteris_paribus(explainer_rf, selected_passangers)

   pdp_rf <- aggregate_profiles(cp_rf, variables = "age")
  pl <- plot(cp_rf, variables = "age") +
     show_observations(cp_rf, variables = "age") +
     show_rugs(cp_rf, variables = "age", color = "red") +
     show_aggregated_profiles(pdp_rf, size = 2)

  expect_true("gg" %in% class(pl))
})

test_that("test ceteris_paribus",{

  selected_passangers <- select_sample(titanic_small, n = 100)
  cp_rf <- ceteris_paribus(explainer_rf, selected_passangers)
  pl <- plot(cp_rf, variables = "age") +
   show_observations(cp_rf, variables = "age") +
     show_rugs(cp_rf, variables = "age", color = "red")

  expect_true("gg" %in% class(pl))
})

