context("Check feature_importance() function")

test_that("Output glm",{
  vd_glm <- feature_importance(explainer_glm, type = "raw",
                                     loss_function = loss_root_mean_square)
  expect_true("feature_importance_explainer" %in% class(vd_glm))
})

test_that("Output rf",{
  vd_rf <- feature_importance(explainer_HR_rf,
                                     loss_function = loss_cross_entropy)
  expect_true("feature_importance_explainer" %in% class(vd_rf))
})

