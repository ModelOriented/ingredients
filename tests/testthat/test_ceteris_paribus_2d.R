context("Check ceteris_paribus_2d() function")

test_that("Output rf",{
  library("DALEX")
  library("ranger")
  set.seed(59)
  apartments_rf_model <- ranger(m2.price ~., data = apartments)
  explainer_rf <- explain(apartments_rf_model,
       data = apartmentsTest[,2:6], y = apartmentsTest$m2.price, verbose = FALSE)
  new_apartment <- apartmentsTest[1, ]
  wi_rf_2d <- ceteris_paribus_2d(explainer_rf, observation = new_apartment,
          variables = c("surface", "floor", "no.rooms"))
  pl <- plot(wi_rf_2d)

  expect_true("ceteris_paribus_2d_explainer" %in% class(wi_rf_2d))
  expect_true("gg" %in% class(pl))
})

