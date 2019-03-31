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

  selected_passangers <- select_sample(titanic_small, n = 100)
  cp_rf <- ceteris_paribus(explainer_rf, selected_passangers)

  pdp_rf_p <- aggregate_profiles(cp_rf, variables = "Age", type = "partial")
  pdp_rf_p$`_label_` <- "RF_partial"
  pdp_rf_c <- aggregate_profiles(cp_rf, variables = "Age", type = "conditional")
  pdp_rf_c$`_label_` <- "RF_conditional"
  pdp_rf_a <- aggregate_profiles(cp_rf, variables = "Age", type = "accumulated")
  pdp_rf_a$`_label_` <- "RF_accumulated"
  pl1 <- plot(pdp_rf_p, pdp_rf_c, pdp_rf_a, color = "_label_")

  expect_true("gg" %in% class(pl1))

  pdp_rf_p <- partial_profiles(explainer_rf, variables = "Age")
  pdp_rf_p$`_label_` <- "RF_partial"
  pdp_rf_c <- conditional_profiles(explainer_rf, variables = "Age")
  pdp_rf_c$`_label_` <- "RF_conditional"
  pdp_rf_a <- accumulated_profiles(explainer_rf, variables = "Age")
  pdp_rf_a$`_label_` <- "RF_accumulated"
  pl2 <- plot(pdp_rf_p, pdp_rf_c, pdp_rf_a, color = "_label_")

  expect_true("gg" %in% class(pl2))
})
