context("Check feature_importance() function")


# Basics - tests with improper and proper inputs

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

test_that('deprecated n_sample', {
  expect_silent(ingredients::feature_importance(explainer_glm, n_sample = 100))
  expect_silent(ingredients::feature_importance(explainer_glm, N = 100))
})


# Permutations and subsampling

test_that("feature_importance can use sub-sampling", {
  # When using N =1, the effective dataset is reduced to only one row.
  # A "permutation" of a one-row dataset is always equal to the original dataset.
  # Thus a model acting on the original data and on the permuted data gives
  # the same output for both. Thus, none of the features will appear "important".
  # Thus, all dropout_loss values should be equal.
  # Practically, this can be tested: sum to be equal to a multiple of the first item
  result <- feature_importance(explainer_rf, N = 1)
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
  expect_equal(max(result$permutation), 2)
  expect_equal(dim(result[result$permutation != 0,]), c(2*nrow(result)/3, 4))
  # because there is no sub-sampling, all the full-model results should be equal
  loss_full <- result[result$variable=="_full_model_",]
  expect_equal(length(unique(loss_full$dropout_loss)), 1)
})

#
# test_that("feature_importance avoids reporting permutations when only one performed", {
#   # by default, one permutation leads to no raw_permutations component
#   result_default <- feature_importance(explainer_rf, B = 1)
#   expect_true(is.null(attr(result_default, "raw_permutations")))
#   result_keep <- feature_importance(explainer_rf, B = 1, keep_raw_permutations = TRUE)
#   expect_false(is.null(attr(result_keep, "raw_permutations")))
# })
#
#
# test_that("feature_importance can avoid recording permutation details", {
#   result <- feature_importance(explainer_rf, B = 2, keep_raw_permutations = FALSE)
#   expect_true(is.null(attr(result, "raw_permutations")))
#   # when keep_raw_permutations is off, output should still signal number of permutations
#   expect_false(is.null(attr(result, "B")))
#   expect_equal(attr(result, "B"), 2)
# })


test_that("feature_importance with subsampling gives different full-model results ", {
  result <- feature_importance(explainer_rf, B = 2, N=200)
  # the full model losses should be different in the first and second round
  # because each round is based on different rows in the data...
  # but in principle there is a tiny probability the two rounds are based on the same rows
  loss_full <- result[result$variable=="_full_model_", ]
  expect_equal(length(unique(loss_full$dropout_loss)), 3)
})


test_that("feature_importance performs at least one permutation", {
  result <- feature_importance(explainer_rf, B = 0.1)
  expect_false(is.null(attr(result, "B")))
  expect_equal(attr(result, "B"), 1)
})


test_that("feature_importance averaged over many permutations are stable", {
  # this test uses many permutations, so make a very small titanic dataset for speed
  tiny <- titanic_small[titanic_small$age > 50,]
  tiny$country <- tiny$class <- tiny$sibsp <- tiny$embarked <- tiny$gender <- NULL

  tiny_rf <- ranger(survived ~ parch + fare + age, data = tiny, probability = TRUE)
  tiny_explainer = explain(tiny_rf, data = tiny,
                           y = tiny$survived == "yes", label = "RF")
  # compute single permutations importance values
  result_1 <- feature_importance(tiny_explainer, B = 1)
  result_2 <- feature_importance(tiny_explainer, B = 1)
  # compute feature importance with several permutations
  result_A <- feature_importance(tiny_explainer, B = 40)
  result_B <- feature_importance(tiny_explainer, B = 40)
  # two rounds with many permutation should give results closer together
  # than two rounds with single permutations
  change_12 <- abs(result_1[result_1$permutation == 0, "dropout_loss"] - result_2[result_2$permutation == 0, "dropout_loss"])
  change_AB <- abs(result_A[result_A$permutation == 0, "dropout_loss"] - result_B[result_B$permutation == 0, "dropout_loss"])
  # this test should succeed most of the time... but in principle could fail by accident
  expect_lt(sum(change_AB), sum(change_12))
})




# Variable grouping

test_that("Variable groupings validation with data frame", {
  vd_rf <- feature_importance(explainer_rf,
                              loss_function = loss_root_mean_square,
                              variable_groups = list(
                                siblings_souse = c("sibsp"),
                                demographics = c("gender", "age"),
                                ticket_type = c("class", "fare"))
                              )
  expect_is(vd_rf, "feature_importance_explainer")
})


test_that("Variable groupings validation with matrix", {
  result <- feature_importance(explainer_rf,
                               variable_groups = list(
                                 siblings_souse = c("sibsp"),
                                 demographics = c("gender", "age"),
                                 ticket_type = c("class", "fare"))
                               )
  expect_is(result, "feature_importance_explainer")
})


test_that("Variable groupings input validation checks", {
  expect_warning(feature_importance(explainer_rf,
                                    loss_function = loss_root_mean_square,
                                    variable_groups = list(c("gender", "age"),
                                                           c("class", "fare"))),
                 "You have passed an unnamed list. The names of variable groupings will be created from variables names.")

  expect_error( feature_importance(explainer_rf,
                                   loss_function = loss_root_mean_square,
                                   variable_groups = c("gender", "age")
                                   ),
               "variable_groups should be of class list")

  expect_error(feature_importance(explainer_rf,
                                  loss_function = loss_root_mean_square,
                                  variable_groups = list("demographics" = c("wrong_name", "age"))
                                  ),
               "You have passed wrong variables names in variable_groups argument")

  expect_error(feature_importance(explainer_rf,
                                  loss_function = loss_root_mean_square,
                                  variable_groups = list("demographics" = list("gender", "age"))
                                  ),
               "Elements of variable_groups argument should be of class character")
})




# Output types

test_that("feature_importance with type ratio", {
  # type "ratio" gives $dropout_loss normalized by _full_model_
  result <- feature_importance(explainer_rf, type="ratio")
  expect_equal(result$dropout_loss[result$variable=="_full_model_" & result$permutation == 0], 1)
})


test_that("feature_importance with type difference", {
  # type "difference" gives $dropout_loss with _full_model_ subtracted
  result <- feature_importance(explainer_rf, type="difference")
  expect_equal(result$dropout_loss[result$variable=="_full_model_" & result$permutation == 0], 0)
})

test_that("Inverse sorting of bars",{
  result <- feature_importance(explainer_rf, type="difference")

  expect_error(plot(result, desc_sorting = "desc"))
})
