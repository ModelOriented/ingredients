context("Check plot() functions")

vd_glm <- feature_importance(explainer_glm, type = "raw",
                             loss_function = loss_root_mean_square)

test_that("plot feature_importance_explainer",{
  pl <- plot(vd_glm)
  expect_true("gg" %in% class(pl))
})

test_that("plot fi 2", {
  pl2 <- plot(vd_glm, max_vars = 3)
  warning(nrow(pl2$data), pl2[["labels"]][["subtitle"]])
  expect_equal(nrow(pl2$data), 3)
  expect_equal(pl2[["labels"]][["subtitle"]], "created for the lm model")
})

test_that("plot fi 3", {
  pl3 <- plot(vd_glm, subtitle = "Feature Importance of explainer_glm")
  expect_equal(pl3[["labels"]][["subtitle"]], "Feature Importance of explainer_glm")
  expect_equal(pl3[["labels"]][["title"]], "Feature Importance")
})

test_that("plot ceteris_paribus_oscillations", {
  cp_rf <- ceteris_paribus(explainer_rf, titanic_small[1:2,])
  os_rf <- calculate_oscillations(cp_rf)
  pl <- plot(os_rf)
  expect_true("gg" %in% class(pl))
})

test_that("plot ceteris_paribus_explainer", {
  cp_rf <- ceteris_paribus(explainer_rf, titanic_small[1:2,])

  # default title, subtitle
  pl <- plot(cp_rf)
  expect_true("gg" %in% class(pl))
  expect_equal(pl$labels$subtitle, "created for the RF model")
  expect_equal(pl$labels$title, "Ceteris Paribus profile")

  # custom title, subtitle
  test_subtitle <- "test subtitle"
  test_title <- "test title"
  pl2 <- plot(cp_rf, subtitle = test_subtitle, title = test_title)
  expect_equal(pl2$labels$subtitle, test_subtitle)
  expect_equal(pl2$labels$title, test_title)
})

test_that("plot aggregated_profiles_explainer", {
  pdp_glm <- partial_dependence(explainer_glm)
  adp_glm <- accumulated_dependence(explainer_glm)

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

