context("Check show_residuals() functions")

test_that("plot show_residuals",{
 library("DALEX")
 library("randomForest")
 library("ingredients")

 titanic <- na.omit(titanic)
 model_titanic_rf <- randomForest(survived == "yes" ~ gender + age + class + embarked +
                                    fare + sibsp + parch,  data = titanic, ntree = 500)

 explain_titanic_rf <- explain(model_titanic_rf,
                               data = titanic[,-9],
                               y = titanic$survived == "yes",
                               label = "Random Forest v7", verbose = FALSE)

 johny_d <- data.frame(
   class = factor("1st", levels = c("1st", "2nd", "3rd", "deck crew", "engineering crew",
                                    "restaurant staff", "victualling crew")),
   gender = factor("male", levels = c("female", "male")),
   age = 8,
   sibsp = 0,
   parch = 0,
   fare = 72,
   embarked = factor("Southampton",
              levels = c("Belfast", "Cherbourg", "Queenstown", "Southampton"))
 )

 johny_neighbours <- select_neighbours(data = titanic,
                                       observation = johny_d,
                                       variables = c("age", "gender", "class",
                                                   "fare", "sibsp", "parch"),
                                       n = 10)

 cp_neighbours <- ceteris_paribus(explain_titanic_rf, johny_neighbours,
                          y = johny_neighbours$survived == "yes",
                          variable_splits = list(age = seq(0,70, length.out = 1000)))
 pl2 <- plot(cp_neighbours, variables = "age") +
   show_observations(cp_neighbours, variables = "age")

 cp_johny <- ceteris_paribus(explain_titanic_rf, johny_d,
                             variable_splits = list(age = seq(0,70, length.out = 1000)))

 pl1 <- plot(cp_johny, variables = "age", size = 1.5, color = "#8bdcbe") +
   show_profiles(cp_neighbours, variables = "age", color = "#ceced9") +
  show_observations(cp_johny, variables = "age", size = 5, color = "#371ea3") +
  show_residuals(cp_neighbours, variables = "age")


 expect_true("gg" %in% class(pl1))
 expect_true("gg" %in% class(pl2))

})



test_that("Multiple observatins",{
  library("randomForest")
  library("DALEX")

  titanic <- na.omit(titanic)
  # we predict embarked instead of survived for generating the bug
  model_titanic_rf <- randomForest(embarked ~ gender + age + class + survived +
                                     fare + sibsp + parch,  data = titanic)

  explain_titanic_rf <- explain(model_titanic_rf,
                                data = titanic[,-9],
                                y = titanic$survived == "yes",
                                label = "Random Forest v7", verbose = FALSE)

  # select few passangers
  selected_passangers <- select_sample(titanic, n = 20)

  expect_error(ceteris_paribus(explain_titanic_rf, selected_passangers))
})

