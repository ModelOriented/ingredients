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






test_that("Variable groupings validation", {
  vd_rf <- feature_importance(explainer_rf,
                              loss_function = loss_root_mean_square,
                              variable_grouping = list("demographics" = c("Sex", "Age"),
                                                       "ticket_type" = c("Pclass", "Fare"))
                              )
  expect_is(vd_rf, "feature_importance_explainer")

})



test_that("Variable groupings input validation checks", {
  expect_warning(feature_importance(explainer_rf,
                                    loss_function = loss_root_mean_square,
                                    variable_grouping = list(c("Sex", "Age"),
                                                             c("Pclass", "Fare"))), "You have passed an unnamed list. The names of variable groupings will be created from variables names.")

  expect_error( feature_importance(explainer_rf,
                                   loss_function = loss_root_mean_square,
                                   variable_grouping = c("Sex", "Age")
  ), "Variable_grouping should be of class list")

   expect_error(feature_importance(explainer_rf,
                                   loss_function = loss_root_mean_square,
                                   variable_grouping = list("demographics" = c("wrong_name", "Age"))
   ), "You have passed wrong variables names in variable_grouping argument")

  expect_error(feature_importance(explainer_rf,
    loss_function = loss_root_mean_square,
  variable_grouping = list("demographics" = list("Sex", "Age"))
), "Elements of variable_grouping argument should be of class character")

})




