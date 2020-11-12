context("Check show_residuals() functions")

library("DALEX")
library("ranger")

model_titanic_rf <- ranger(survived ~ gender + age + class + embarked +
                             fare + sibsp + parch, data = titanic_imputed,
                           probability = TRUE)

explain_titanic_rf <- explain(model_titanic_rf,
                              data = titanic_imputed[,-8],
                              y = titanic_imputed$survived,
                              verbose = FALSE)

test_that("plot show_residuals",{

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

 johny_neighbours <- select_neighbours(data = titanic_imputed,
                                       observation = johny_d,
                                       variables = c("age", "gender", "class",
                                                   "fare", "sibsp", "parch"),
                                       n = 10)

 cp_neighbours <- ceteris_paribus(explain_titanic_rf, johny_neighbours,
                          y = johny_neighbours$survived,
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



test_that("wrong prediction bug",{
   # we predict embarked instead of survived for generating the bug
   model_titanic_rf <- ranger(embarked ~ gender + age + class + survived +
                                       fare + sibsp + parch,
                                    data = titanic_imputed, probability = TRUE)

   explain_titanic_rf <- explain(model_titanic_rf,
                                 data = titanic[,-8],
                                 y = titanic$survived, verbose = FALSE)

   # select few passangers
   selected_passangers <- select_sample(titanic_imputed, n = 20)

  expect_error(ceteris_paribus(explain_titanic_rf, selected_passangers))
})

