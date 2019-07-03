context("Check feature_importance() function")


# Usability - tests with improper inputs

test_that("feature_importance checks its inputs", {
  missing.data <- list()
  class(missing.data) <- "explainer"
  expect_error(feature_importance(missing.data), "data")
  missing.y <- list(data = data.frame(A=1:2))
  class(missing.y) <- "explainer"
  expect_error(feature_importance(missing.y), "y")
})


test_that("feature_importance stops with incorrect type", {
  # error message should provide hints on the allowed values for type
  expect_error(feature_importance(explainer_glm, type = "a"), "raw")
  expect_error(feature_importance(explainer_glm, type = "a"), "ratio")
})




# Correctness - tests with good inputs

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




# Permutations and subsampling

test_that("feature_importance can use sub-sampling", {
  # When using n_sample =1, the effective dataset is reduced to only one row.
  # A "permutation" of a one-row dataset is always equal to the original dataset.
  # Thus a model acting on the original data and on the permuted data gives
  # the same output for both. Thus, none of the features will appear "important".
  # Thus, all dropout_loss values should be equal.
  # Practically, this can be tested: sum to be equal to a multiple of the first item
  result <- feature_importance(explainer_rf, n_sample = 1)
  expect_equal(sum(result$dropout_loss), nrow(result)*result$dropout_loss[1],
               tolerance = 1e-12)
})


test_that("feature_importance gives slightly different output on subsequent runs", {
  result_1 <- feature_importance(explainer_rf)
  result_2 <- feature_importance(explainer_rf)
  change_12 <- abs(result_1$dropout_loss - result_2$dropout_loss)
  expect_gt(sum(change_12), 0)
})


test_that("feature_importance records number of permutations", {
  result <- feature_importance(explainer_rf, B = 2)
  expect_false(is.null(attr(result, "B")))
  expect_equal(attr(result, "B"), 2)
  expect_false(is.null(attr(result, "raw_permutations")))
  raw <- attr(result, "raw_permutations")
  expect_is(raw, "data.frame")
  # the raw permutations data frame will have:
  # - three columns: feature, permutation, dropout_loss
  # - many rows: one per permutation and per feature plus outcome
  n1 <- nrow(result) - 1
  expect_equal(dim(raw), c(n1*2, 3))
})


test_that("feature_importance performs at least one permutation", {
  result <- feature_importance(explainer_rf, B = 0.1)
  expect_false(is.null(attr(result, "B")))
  expect_equal(attr(result, "B"), 1)
})


test_that("feature_importance averaged over many permutations are stable", {
  # this test uses many permutations, so make a very small titanic dataset for speed
  tiny <- titanic_small[titanic_small$Age > 50,]
  tiny$Parch <- tiny$SibSp <- tiny$Embarked <- tiny$Sex <- NULL
  tiny_rf <- randomForest(Survived ~ Pclass + Fare + Age, data = tiny)
  tiny_explainer = explain(tiny_rf, data = tiny,
                           y = tiny$Survived=="1", label = "RF")
  # compute single permutations importance values
  result_1 <- feature_importance(tiny_explainer)
  result_2 <- feature_importance(tiny_explainer)
  # compute feature importance with several permutations 
  result_A <- feature_importance(tiny_explainer, B = 40)
  result_B <- feature_importance(tiny_explainer, B = 40)
  # two rounds with many permutation should give results closer together
  # than two rounds with single permutations 
  change_12 <- abs(result_1$dropout_loss - result_2$dropout_loss)
  change_AB <- abs(result_A$dropout_loss - result_B$dropout_loss)
  # this test should succeed most of the time... but in principle could fail by accident
  expect_lt(sum(change_AB), sum(change_12))
})




# Variable grouping

test_that("Variable groupings validation", {
  vd_rf <- feature_importance(explainer_rf,
                              loss_function = loss_root_mean_square,
                              variable_groups = list("demographics" = c("Sex", "Age"),
                                                       "ticket_type" = c("Pclass", "Fare"))
                              )
  expect_is(vd_rf, "feature_importance_explainer")
})


test_that("Variable groupings input validation checks", {
  expect_warning(feature_importance(explainer_rf,
                                    loss_function = loss_root_mean_square,
                                    variable_groups = list(c("Sex", "Age"),
                                                           c("Pclass", "Fare"))),
                 "You have passed an unnamed list. The names of variable groupings will be created from variables names.")
  
  expect_error( feature_importance(explainer_rf,
                                   loss_function = loss_root_mean_square,
                                   variable_groups = c("Sex", "Age")
                                   ),
               "variable_groups should be of class list")
  
  expect_error(feature_importance(explainer_rf,
                                  loss_function = loss_root_mean_square,
                                  variable_groups = list("demographics" = c("wrong_name", "Age"))
                                  ),
               "You have passed wrong variables names in variable_groups argument")
  
  expect_error(feature_importance(explainer_rf,
                                  loss_function = loss_root_mean_square,
                                  variable_groups = list("demographics" = list("Sex", "Age"))
                                  ),
               "Elements of variable_groups argument should be of class character")
})




# Output types

test_that("feature_importance with type ratio", {
  # type "ratio" gives $dropout_loss normalized by _full_model_
  result <- feature_importance(explainer_rf, type="ratio")
  expect_equal(result$dropout_loss[result$variable=="_full_model_"], 1)
})


test_that("feature_importance with type difference", {
  # type "difference" gives $dropout_loss with _full_model_ subtracted
  result <- feature_importance(explainer_rf, type="difference")
  expect_equal(result$dropout_loss[result$variable=="_full_model_"], 0)
})

