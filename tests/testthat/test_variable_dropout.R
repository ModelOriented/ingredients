context("Check model_feature_importance() function")

test_that("Output glm",{
  vd_glm <- model_feature_importance(explainer_glm, type = "raw",
                                     loss_function = loss_root_mean_square)
  expect_true("model_feature_importance_explainer" %in% class(vd_glm))
})

test_that("Output rf",{
  vd_rf <- model_feature_importance(explainer_rf,
                                     loss_function = loss_cross_entropy)
  expect_true("model_feature_importance_explainer" %in% class(vd_rf))
})

