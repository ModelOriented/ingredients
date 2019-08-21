context("Check aggregate_profiles() functions")

test_that("plot aggregate_profiles",{
  library("DALEX")
  library("titanic")
  library("randomForest")

  titanic_small <- titanic_train[,c("Survived", "Pclass", "Sex", "Age",
                                    "SibSp", "Parch", "Fare", "Embarked")]
  titanic_small$Survived <- factor(titanic_small$Survived)
  titanic_small$Sex <- factor(titanic_small$Sex)
  titanic_small$Embarked <- factor(titanic_small$Embarked)
  titanic_small <- na.omit(titanic_small)
  rf_model <- randomForest(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
                           data = titanic_small)
  explainer_rf <- explain(rf_model, data = titanic_small,
                          y = titanic_small$Survived == "1", label = "RF")

  selected_passangers_10 <- select_neighbours(titanic_small, titanic_small[1,], n = 10)

  selected_passangers <- select_sample(titanic_small, n = 100)

  cp_rf <- ceteris_paribus(explainer_rf, selected_passangers)

  pdp_rf_p <- aggregate_profiles(cp_rf, variables = "Age", type = "partial", groups = "Sex")

  pdp_rf_p <- aggregate_profiles(cp_rf, variables = "Age", type = "partial")
  pdp_rf_p$`_label_` <- "RF_partial"
  pdp_rf_c <- aggregate_profiles(cp_rf, variables = "Age", type = "conditional")
  pdp_rf_c$`_label_` <- "RF_conditional"
  pdp_rf_a <- aggregate_profiles(cp_rf, variables = "Age", type = "accumulated")
  pdp_rf_a$`_label_` <- "RF_accumulated"
  pl1 <- plot(pdp_rf_p, pdp_rf_c, pdp_rf_a, color = "_label_")

  expect_true("gg" %in% class(pl1))

  pdp_rf_p <- partial_dependency(explainer_rf, variables = "Age")
  pdp_rf_p$`_label_` <- "RF_partial"
  pdp_rf_c <- conditional_dependency(explainer_rf, variables = "Age")
  pdp_rf_c$`_label_` <- "RF_conditional"
  pdp_rf_a <- accumulated_dependency(explainer_rf, variables = "Age")
  pdp_rf_a$`_label_` <- "RF_accumulated"
  pl2 <- plot(pdp_rf_p, pdp_rf_c, pdp_rf_a, color = "_label_")

  expect_true("gg" %in% class(pl2))

  expect_error(aggregate_profiles(explainer_rf, variable_type = "wrong"))
})


test_that("plot partial_dependency",{
  library("DALEX")
  library("randomForest")
  titanic <- na.omit(titanic)
  model_titanic_rf <- randomForest(survived ~ gender + age + class + embarked +
                                     fare + sibsp + parch,  data = titanic)

  explain_titanic_rf <- explain(model_titanic_rf,
                                data = titanic[,-9],
                                y = titanic$survived)

  selected_passangers <- select_sample(titanic, n = 100)
  cp_rf <- ceteris_paribus(explain_titanic_rf, selected_passangers)

  res <- partial_dependency(explain_titanic_rf, N=50, variables = "gender", variable_type = "categorical")

  expect_true("aggregated_profiles_explainer" %in% class(res))
})
