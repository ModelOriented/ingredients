context("Check model_feature_response_explainer() function")

test_that("test age",{
  expl_glm <- model_feature_response(explainer_glm, "age", "pdp")
  expect_true("model_feature_response_explainer" %in% class(expl_glm))

  expl_rf <- model_feature_response(explainer_rf, "age", "pdp")
  expect_true("model_feature_response_explainer" %in% class(expl_rf))
})

test_that("test ale",{
  expl_glm <- model_feature_response(explainer_glm, "age", "ale")
  expect_true("model_feature_response_explainer" %in% class(expl_glm))
})

test_that("test gender", {
  expl_glm <- model_feature_response(explainer_glm, "gender", which_class = 1)
  expl_glm <- model_feature_response(explainer_glm, "gender")
  expl_glm <- model_feature_response(explainer_glm, "gender", "factor")
  expect_true("model_feature_response_explainer" %in% class(expl_glm))

  expl_rf <- model_feature_response(explainer_rf, "gender", "factor")
  expect_true("factorMerger_list" %in% class(expl_rf))
})
