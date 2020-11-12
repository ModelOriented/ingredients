context("Check plotD3() functions")

library("DALEX")
library("ranger")

apartments_rf_model <- ranger(m2.price ~ construction.year + surface + floor +
                                      no.rooms + district, data = apartments)

explainer_rf <- explain(apartments_rf_model,
                        data = apartmentsTest,
                        y = apartmentsTest$m2.price,
                        verbose = FALSE)

model_titanic_rf <- ranger(survived ~ gender + age + class + embarked +
                           fare + sibsp + parch,
                           data = titanic_imputed,
                           probability = TRUE)
explainer_titanic_rf <- explain(model_titanic_rf,
                                data = titanic_imputed[,-8],
                                y = titanic_imputed$survived,
                                label = "rf", verbose = FALSE)

test_that("plotD3 Feature Importance",{

  fi_rf <- feature_importance(explainer_rf, loss_function = loss_root_mean_square)

  p1 <- plotD3(fi_rf)
  p2 <- plotD3(fi_rf, scale_height = TRUE)
  p3 <- plotD3(fi_rf, bar_width = 20)
  p4 <- plotD3(fi_rf, chart_title = "HELLO", scale_height = TRUE)
  p5 <- plotD3(fi_rf, max_vars = 2, split = "feature")

  expect_true("r2d3" %in% class(p1))
  expect_true("r2d3" %in% class(p2))
  expect_true("r2d3" %in% class(p3))
  expect_true("r2d3" %in% class(p4))
  expect_true("r2d3" %in% class(p5))
})

test_that("plotD3 Ceteris Paribus and plotD3 Aggregated Profiles",{

  selected_passanger <- select_sample(titanic_imputed, n = 10)
  cp_rf <- ceteris_paribus(explainer_titanic_rf, selected_passanger)

  selected_passangers2 <- select_sample(titanic_imputed, n = 1)
  cp_rf2 <- ceteris_paribus(explainer_titanic_rf, selected_passangers2)

  p6 <- plotD3(cp_rf, variables = c("age","parch","fare","sibsp"),
               size = 5, alpha = 0.5, show_rugs = TRUE, scale_plot = TRUE)
  p7 <- plotD3(cp_rf2, variables = c("class", "embarked", "gender", "sibsp"),
               facet_ncol = 2, variable_type = "categorical", label_margin = 100, scale_plot = TRUE)

  expect_true("r2d3" %in% class(p6))
  expect_true("r2d3" %in% class(p7))

  expect_error(plotD3(cp_rf, variables = c("class", "embarked", "gender", "sibsp"),
                      facet_ncol = 2, variable_type = "categorical", label_margin = 100, scale_plot = TRUE))

  pdp_rf_p <- aggregate_profiles(cp_rf, type = "partial")
  pdp_rf_p$`_label_` <- "RF_partial"
  pdp_rf_c <- aggregate_profiles(cp_rf, type = "conditional")
  pdp_rf_c$`_label_` <- "RF_conditional"
  pdp_rf_a <- aggregate_profiles(cp_rf, type = "accumulated")
  pdp_rf_a$`_label_` <- "RF_accumulated"

  p8 <- plotD3(pdp_rf_p, pdp_rf_c, pdp_rf_a)

  pdp <- aggregate_profiles(cp_rf, type = "partial", variable_type = "categorical")
  pdp$`_label_` <- "RF_partial"

  p9 <- plotD3(pdp)


  expect_true("r2d3" %in% class(p8))
  expect_true("r2d3" %in% class(p9))
})
