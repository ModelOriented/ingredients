context("Check calculate_oscillations() functions")

test_that("plot calculate_oscillations",{
  library("DALEX")
  library("randomForest")
  set.seed(59)
  apartments_rf_model <- randomForest(m2.price ~ construction.year + surface + floor +
                                        no.rooms + district, data = apartments)

  explainer_rf <- explain(apartments_rf_model,
                          data = apartmentsTest, y = apartmentsTest$m2.price, verbose = FALSE)

  apartment <- apartmentsTest[1,]

  cp_rf <- ceteris_paribus(explainer_rf, apartment)
  pl <- calculate_oscillations(cp_rf)
  expect_true("ceteris_paribus_oscillations" %in% class(pl))
})
