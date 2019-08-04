context("Check aspect_importance() functions")

test_that("check output for aspects importance (glm, default)",{
  library("DALEX")
  library("ingredients")

  aspect_importance_titanic_glm <- aspect_importance(titanic_glm_model, titanic_data,
                                                     new_observation = titanic_new_observation,
                                                     aspects_list = titanic_aspects)

  expect_true("data.frame" %in% class(aspect_importance_titanic_glm))
  expect_true(dim(aspect_importance_titanic_glm)[1] == 4)
  expect_true(dim(aspect_importance_titanic_glm)[2] == 2)

})

test_that("check output for aspects importance (lm, binom)",{
  library("DALEX")
  library("ingredients")

  if (getRversion() >= "3.6")
  {
    suppressWarnings(set.seed(123, sample.kind = "Rounding"))
  } else {
    set.seed(123)
  }

  aspect_importance_apartments <- aspect_importance(apartments_lm_model, apartments,
                                                    new_observation = apartments_new_observation,
                                                    aspects_list =  apartments_aspects, method = "binom")
  expect_true("aspect_importance" %in% class(aspect_importance_apartments))
  expect_true(floor(aspect_importance_apartments[aspect_importance_apartments$aspects == "district",]$importance) == 279)
})

test_that("check aspects_importance for explainer",{
  library("DALEX")
  library("ingredients")


  titanic_explainer <- explain(model = titanic_glm_model,
                               data = titanic_data,
                               y = titanic_data$survived)

  aspect_importance_titanic_glm <- aspect_importance(titanic_explainer,
                                                     new_observation = titanic_new_observation,
                                                     aspects_list = titanic_aspects)

  expect_true("data.frame" %in% class(aspect_importance_titanic_glm))
})


test_that("check plot for aspects importance",{
  library("DALEX")
  library("ingredients")

  aspect_importance_apartments <- aspect_importance(apartments_lm_model, apartments,
                                                    new_observation = apartments_new_observation,
                                                    aspects_list =  apartments_aspects, method = "binom")

  expect_is(plot(aspect_importance_apartments), "gg")
})

test_that("check alias for aspect_importance",{
  library("DALEX")
  library("ingredients")


  aspect_importance_apartments <- lime(apartments_lm_model, apartments,
                                       new_observation = apartments_new_observation,
                                       aspects_list =  apartments_aspects, method = "binom")
  expect_true("aspect_importance" %in% class(aspect_importance_apartments))

})

test_that("plot for aspect_importance works",{
  library("DALEX")
  library("ingredients")


  aspect_importance_apartments <- aspect_importance(apartments_lm_model, apartments,
                                                    new_observation = apartments_new_observation,
                                                    aspects_list =  apartments_aspects, method = "binom")
  p <- plot(aspect_importance_apartments)
  expect_true(is.ggplot(p))
  expect_identical(p$labels$y, "Aspects importance")
  expect_error(plot.aspect_importance(apartments))
})

test_that("check for aspect_importance with lasso",{
  library("DALEX")
  library("ingredients")

  aspect_importance_apartments <- aspect_importance(apartments_lm_model, apartments,
                                                    new_observation = apartments_new_observation,
                                                    aspects_list =  apartments_aspects, n_var = 3)

  expect_true("aspect_importance" %in% class(aspect_importance_apartments))
  expect_true(sum(aspect_importance_apartments[,2] != 0) == 3)
})

test_that("check get_sample function with binom",{
  library("DALEX")
  library("ingredients")

  x <- get_sample(100,4,"binom")
  expect_true(ncol(x) == 4)
  expect_true(nrow(x) == 100)
  expect_true(max(x) == 1)
  expect_true(min(x) == 0)
  expect_error(get_sample(-100,4,"binom"))
})

test_that("check get_sample function with default sampling",{
  library("DALEX")
  library("ingredients")

  x <- get_sample(50,10,"default")
  expect_true(ncol(x) == 10)
  expect_true(nrow(x) == 50)
  expect_true(max(x) == 1)
  expect_true(min(x) == 0)
})

test_that("check group_variables function",{
  library("DALEX")
  library("ingredients")

  titanic_cont <- titanic_data[,unlist(lapply(titanic, is.numeric))]
  aspect_list <- group_variables(titanic_cont, 0.5)
  expect_true(length(aspect_list) == 3)
  expect_error(group_variables(titanic, 0.6))

})

test_that("check add_additional_information function",{
  library("DALEX")
  library("ingredients")

  aspect_importance_apartments <- aspect_importance(apartments_lm_model, apartments,
                                                    new_observation = apartments_new_observation,
                                                    aspects_list =  apartments_aspects, method = "binom")
  aspect_importance_apartments_info <- add_additional_information(aspect_importance_apartments, apartments, apartments_aspects, show_cor = T)
  aspect_importance_apartments_info_no_corr <- add_additional_information(aspect_importance_apartments, apartments, apartments_aspects, show_cor = F)

  expect_true(dim(aspect_importance_apartments_info)[2] == 5)
  expect_true(dim(aspect_importance_apartments_info_no_corr)[2] == 3)

})
