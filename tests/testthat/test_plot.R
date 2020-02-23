context("Check plot() functions")

test_that("plot feature_importance_explainer",{
  vd_glm <- feature_importance(explainer_glm, type = "raw",
                                     loss_function = loss_root_mean_square)
  pl <- plot(vd_glm)
  expect_true("gg" %in% class(pl))

  pl2 <- plot(vd_glm, max_vars = 3)
  expect_equal(nrow(pl2$data), 3)
})

test_that("plot ceteris_paribus_oscillations", {
  cp_rf <- ceteris_paribus(explainer_rf, titanic_small[1:2,])
  os_rf <- calculate_oscillations(cp_rf)
  pl <- plot(os_rf)
  expect_true("gg" %in% class(pl))
})

test_that("plot aggregated_profiles_explainer", {
  pdp_glm <- partial_dependency(explainer_glm)
  adp_glm <- accumulated_dependency(explainer_glm)

  pl <- plot(pdp_glm)
  expect_true("gg" %in% class(pl))
  expect_equal(pl[["labels"]][["title"]], "Partial Dependence profile")
  expect_equal(pl[["labels"]][["subtitle"]], "Created for the lm model")

  pl2 <- plot(pdp_glm, title = "test_title", subtitle = "test_subtitle")
  expect_equal(pl2[["labels"]][["title"]], "test_title")
  expect_equal(pl2[["labels"]][["subtitle"]], "test_subtitle")

  pl3 <- plot(adp_glm)
  expect_true("gg" %in% class(pl3))
  expect_equal(pl3[["labels"]][["title"]], "Accumulated Dependence profile")
  expect_equal(pl3[["labels"]][["subtitle"]], "Created for the lm model")

  pl4 <- plot(adp_glm, title = "test_title", subtitle = "test_subtitle")
  expect_equal(pl4[["labels"]][["title"]], "test_title")
  expect_equal(pl4[["labels"]][["subtitle"]], "test_subtitle")
})
