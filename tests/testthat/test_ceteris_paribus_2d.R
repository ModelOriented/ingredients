context("Check ceteris_paribus_2d() function")

test_that("Output rf",{
  library("DALEX")
  library("randomForest")
  set.seed(59)
  apartments_rf_model <- randomForest(m2.price ~ construction.year + surface + floor +
       no.rooms + district, data = apartments)
  explainer_rf <- explain(apartments_rf_model,
       data = apartmentsTest[,2:6], y = apartmentsTest$m2.price)
  new_apartment <- apartmentsTest[1, ]
  wi_rf_2d <- ceteris_paribus_2d(explainer_rf, observation = new_apartment,
          variables = c("surface", "floor", "no.rooms"))
  pl <- plot(wi_rf_2d)

  expect_true("ceteris_paribus_2d_explainer" %in% class(wi_rf_2d))
  expect_true("gg" %in% class(pl))
})

