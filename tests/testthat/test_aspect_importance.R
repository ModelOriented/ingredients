context("Check aspect_importance() functions")

test_that("check output for aspects importance (glm)",{
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

  expect_true("lm" %in% class(aspect_importance_titanic_glm))
  expect_true(length(aspect_importance_titanic_glm$coefficients) == 5)
})

test_that("check output for aspects importance (lm)",{
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
  expect_true("lm" %in% class(aspect_importance_ap))
  expect_true(floor(coef(summary(aspect_importance_ap))["district","Estimate"]) == 342)
  expect_true(is.numeric(floor(coef(summary(aspect_importance_ap))["district","Estimate"])))

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
})

test_that("check get_sample functionw with default sampling",{
  library("DALEX")
  library("ingredients")

  set.seed(123)
  x <- get_sample(50,10,"default")
  expect_true(ncol(x) == 10)
  expect_true(nrow(x) == 50)
  expect_true(max(x) == 1)
  expect_true(min(x) == 0)
})
