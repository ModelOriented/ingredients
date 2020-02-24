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
