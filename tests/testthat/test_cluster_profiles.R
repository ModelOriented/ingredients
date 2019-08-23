context("Check cluster_profiles() functions")

test_that("plot cluster_profiles",{
  library("DALEX")
  library("randomForest")

  titanic_small <- na.omit(titanic[1:500,])

  rf_model <- randomForest(survived ~ gender + age + class + embarked +
                             fare + sibsp + parch,  data = titanic_small)

  explainer_rf <- explain(rf_model, data = titanic_small,
                          y = titanic_small$survived == "yes", label = "RF", verbose = FALSE)

  selected_passangers <- select_sample(titanic_small, n = 100)
  cp_rf <- ceteris_paribus(explainer_rf, selected_passangers)

  pdp_rf <- aggregate_profiles(cp_rf, variables = "age")
  clust_rf <- cluster_profiles(cp_rf, k = 3, variables = "age")

  expect_true("aggregated_profiles_explainer" %in% class(clust_rf))

  pl1 <- plot(clust_rf, color = "_label_") +
    show_aggregated_profiles(pdp_rf, color = "black", size = 3)

  pl2 <- plot(cp_rf, color = "grey", variables = "age") +
    show_aggregated_profiles(clust_rf, color = "_label_", size = 2)

  pl3 <- plot(cp_rf, variables = "embarked", variable_type = "categorical")

  clust_rf <- cluster_profiles(cp_rf, k = 3, center = TRUE, variables = "age")

  expect_true("aggregated_profiles_explainer" %in% class(clust_rf))
  expect_true("gg" %in% class(pl1))
  expect_true("gg" %in% class(pl2))
  expect_true("gg" %in% class(pl3))
})


test_that("plot cluster_profiles",{
  set.seed(17)
  x <- runif(1000, -8, 8)
  y <- ifelse(x <= 1, (x + 3)^2 - 10 , -5*x + 11)
  molnar <- data.frame(x = x, y = y)

  true_model <- function(model, newdata) {
    ifelse(newdata$x <= 1, (newdata$x + 3)^2 - 10 , -5*newdata$x + 11)
  }

  library(DALEX)
  molnar_explainer <- explain(list(), molnar, y = y, predict_function = true_model, verbose = FALSE)

  profile <- ingredients::ceteris_paribus(molnar_explainer, new_observation = data.frame(x = -6),
                               variables = "x",
                               variable_splits = list(x = -10:10))

  expect_true("ceteris_paribus_explainer" %in% class(profile))
})
