context("Check plotD3() functions")

test_that("plot plotD3",{
  library("DALEX")

  apartments_rf_model <- randomForest(m2.price ~ construction.year + surface + floor +
                                        no.rooms + district, data = apartments)

  explainer_rf <- explain(apartments_rf_model,
                          data = apartmentsTest, y = apartmentsTest$m2.price)

  fi_rf <- feature_importance(explainer_rf, loss_function = loss_root_mean_square)

  p1 <- plotD3(fi_rf)
  p2 <- plotD3(fi_rf, scaleHeight = TRUE)
  p3 <- plotD3(fi_rf, label = FALSE)
  p4 <- plotD3(fi_rf, label = FALSE, scaleHeight = TRUE)

  expect_true("r2d3" %in% class(p1))
  expect_true("r2d3" %in% class(p2))
  expect_true("r2d3" %in% class(p3))
  expect_true("r2d3" %in% class(p4))
})
