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
  p5 <- plotD3(fi_rf, max_vars = 2, split = "feature")

  expect_true("r2d3" %in% class(p1))
  expect_true("r2d3" %in% class(p2))
  expect_true("r2d3" %in% class(p3))
  expect_true("r2d3" %in% class(p4))
  expect_true("r2d3" %in% class(p5))

  titanic <- na.omit(titanic)
  titanic$survived <- as.integer(as.factor(titanic$survived))
  model_titanic_rf <- randomForest(survived ~ gender + age + class + embarked +
                                     fare + sibsp + parch,  data = titanic)
  explainer_titanic_rf <- explain(model_titanic_rf,
                                  data = titanic[,-9],
                                  y = titanic$survived,
                                  label = "rf")

  selected_passanger <- select_sample(titanic, n = 10)
  cp_rf <- ceteris_paribus(explainer_titanic_rf, selected_passanger)

  selected_passangers2 <- select_sample(titanic, n = 1)
  cp_rf2 <- ceteris_paribus(explainer_titanic_rf, selected_passangers2)

  p6 <- plotD3(cp_rf, variables = c("age","parch","fare","sibsp"),
               size = 5, alpha = 0.5, show_rugs = TRUE, scale_plot = TRUE)
  p7 <- plotD3(cp_rf2, variables = c("class", "embarked", "gender", "sibsp"),
               facet_ncol = 2, only_numerical = FALSE, label_margin = 100, scale_plot = TRUE)

  expect_true("r2d3" %in% class(p6))
  expect_true("r2d3" %in% class(p7))

  expect_error(plotD3(cp_rf, variables = c("class", "embarked", "gender", "sibsp"),
                      facet_ncol = 2, only_numerical = FALSE, label_margin = 100, scale_plot = TRUE))
})
