context("Check plot() functions")

test_that("plot feature_importance_explainer",{
  vd_glm <- feature_importance(explainer_glm, type = "raw",
                                     loss_function = loss_root_mean_square)
  pl <- plot(vd_glm)
  expect_true("gg" %in% class(pl))
})

test_that("plot model_feature_importance_explainer",{
  expl_glm <- model_feature_response(explainer_glm, "age", "pdp")
  pl <- plot(expl_glm)
  expect_true("gg" %in% class(pl))

  expl_glm <- model_feature_response(explainer_glm, "gender", "factor")
  pl <- plot(expl_glm)
  expect_true("gg" %in% class(pl))
})
