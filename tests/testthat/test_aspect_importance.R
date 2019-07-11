context("Check aspect_importance() functions")

test_that("check output for aspects importance (glm, default)",{
  library("DALEX")
  library("ingredients")
  set.seed(123)

  model <- glm(survived == "yes" ~ class+gender+age+sibsp+parch+fare+embarked,
               titanic, family = "binomial")

  new_observation <- data.frame(
    class = factor("1st", levels = c("1st", "2nd", "3rd", "deck crew",
                                     "engineering crew", "restaurant staff",
                                     "victualling crew")),
    gender = factor("male", levels = c("female", "male")),
    age = 8,
    sibsp = 0,
    parch = 0,
    fare = 72,
    embarked = factor("Southampton", levels = c("Belfast","Cherbourg",
                                                "Queenstown","Southampton"))
  )

  aspects <- list(wealth = c("class", "fare"),
                  family = c("gender", "sibsp", "parch"),
                  age = "age",
                  embarked = "embarked")

  aspect_importance_titanic_glm <- aspect_importance(model, titanic,
                                                     predict, new_observation,
                                                     aspects)

  expect_true("data.frame" %in% class(aspect_importance_titanic_glm))
  expect_true(dim(aspect_importance_titanic_glm)[1] == 4)
  expect_true(dim(aspect_importance_titanic_glm)[2] == 2)

})

test_that("check output for aspects importance (lm, binom)",{
  library("DALEX")
  library("ingredients")

  set.seed(123)
  model <- lm(m2.price ~ ., data = apartments)

  aspects <- list(space = c("surface", "no.rooms"),
                  construction.year = "construction.year",
                  floor = "floor",
                  district = "district")

  new_observation <- apartments_test[2,-1]

  aspect_importance_ap <- aspect_importance(model, apartments,
                                            predict, new_observation,
                                            aspects, method = "binom")
  expect_true("aspect_importance" %in% class(aspect_importance_ap))
  expect_true(floor(aspect_importance_ap[aspect_importance_ap$aspects == "district",]$importance) == 570)
})

test_that("check plot for aspects importance",{
  library("DALEX")
  library("ingredients")

  set.seed(123)
  model <- lm(m2.price ~ ., data = apartments)

  aspects <- list(space = c("surface", "no.rooms"),
                  construction.year = "construction.year",
                  floor = "floor",
                  district = "district")

  new_observation <- apartments_test[2,-1]

  aspect_importance_ap <- aspect_importance(model, apartments,
                                            predict, new_observation,
                                            aspects)
  expect_is(plot(aspect_importance_ap), "gg")
})

test_that("check alias for aspect_importance",{
  library("DALEX")
  library("ingredients")

  set.seed(123)
  model <- lm(m2.price ~ ., data = apartments)

  aspects <- list(space = c("surface", "no.rooms"),
                  construction.year = "construction.year",
                  floor = "floor",
                  district = "district")

  new_observation <- apartments_test[2,-1]

  aspect_importance_ap <- aspect_lime(model, apartments,
                                            predict, new_observation,
                                            aspects)
  expect_true("aspect_importance" %in% class(aspect_importance_ap))

})

test_that("plot for aspect_importance works",{
  library("DALEX")
  library("ingredients")

  set.seed(123)
  model <- lm(m2.price ~ ., data = apartments)

  aspects <- list(space = c("surface", "no.rooms"),
                  construction.year = "construction.year",
                  floor = "floor",
                  district = "district")

  new_observation <- apartments_test[2,-1]

  aspect_importance_ap <- aspect_importance(model, apartments,
                                      predict, new_observation,
                                      aspects)
  p <- plot(aspect_importance_ap)
  expect_true(is.ggplot(p))
  expect_identical(p$labels$y, "Aspects importance")
  expect_error(plot.aspect_importance(apartments))
})


test_that("check get_sample function with binom",{
  library("DALEX")
  library("ingredients")

  set.seed(123)
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

  set.seed(123)
  x <- get_sample(50,10,"default")
  expect_true(ncol(x) == 10)
  expect_true(nrow(x) == 50)
  expect_true(max(x) == 1)
  expect_true(min(x) == 0)
})

test_that("check group_variables function",{
  library("DALEX")
  library("ingredients")

  titanic <- na.omit(titanic)
  titanic_cont <- titanic[,unlist(lapply(titanic, is.numeric))]
  aspect_list <- group_variables(titanic_cont, 0.5)
  expect_true(length(aspect_list) == 3)
  expect_error(group_variables(titanic, 0.6))

})
