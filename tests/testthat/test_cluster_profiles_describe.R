context("Check describe() functions")

test_that("describe cluster_profiles",{
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

  selected_passangers <- select_sample(titanic_small, n = 1)
  cp_rf <- ceteris_paribus(explainer_rf, selected_passangers)
  desc_cp_rf <- describe(cp_rf, variables = "Age")
  res <- print(desc_cp_rf)
  res <- print(cp_rf)

  expect_true("ceteris_paribus_description" %in% class(desc_cp_rf))
})
