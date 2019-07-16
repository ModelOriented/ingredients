context("Check plot() functions")

test_that("plot feature_importance_explainer",{
  vd_glm <- feature_importance(explainer_glm, type = "raw",
                                     loss_function = loss_root_mean_square)
  pl <- plot(vd_glm)
  expect_true("gg" %in% class(pl))

  vd_glm <- feature_importance(explainer_glm, type = "raw",
                               loss_function = loss_root_mean_square)
  pl2 <- plot(vd_glm, max_vars = 3)
  expect_equal(nrow(pl2$data), 3)
})

test_that("plot ceteris_paribus_oscillations", {
  cp_glm <- ceteris_paribus(explainer_glm, titanic[10:12, ])
  os_glm <- calculate_oscillations(cp_glm)
  pl <- plot(os_glm)
  expect_true("gg" %in% class(pl))
})
