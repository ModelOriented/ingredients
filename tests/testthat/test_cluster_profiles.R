context("Check cluster_profiles() functions")

test_that("plot cluster_profiles",{
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

  pdp_rf <- aggregate_profiles(cp_rf, variables = "Age")
  clust_rf <- cluster_profiles(cp_rf, k = 3, variables = "Age")

  expect_true("aggregated_profiles_explainer" %in% class(clust_rf))

  pl1 <- plot(clust_rf, color = "_label_") +
    show_aggreagated_profiles(pdp_rf, color = "black", size = 3)

  pl2 <- plot(cp_rf, color = "grey", variables = "Age") +
    show_aggreagated_profiles(clust_rf, color = "_label_", size = 2)

  clust_rf <- cluster_profiles(cp_rf, k = 3, center = TRUE, variables = "Age")

  expect_true("aggregated_profiles_explainer" %in% class(clust_rf))
  expect_true("gg" %in% class(pl1))
  expect_true("gg" %in% class(pl2))
})
