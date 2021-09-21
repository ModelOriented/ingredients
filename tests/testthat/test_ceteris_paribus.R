library('DALEX')
library('ingredients')
library('ranger')


data('iris')
iris$Sepal.Length <- factor(iris$Sepal.Length > 5)
model_iris_rf <- ranger(Species ~., iris, probability = TRUE)
explain_iris_rf <- explain(model_iris_rf,
                           data = iris[, -5],
                           y = iris$Species,
                           verbose = FALSE)
cp_rf <- ceteris_paribus(explain_iris_rf, iris[1,])
p1 <- plot(cp_rf)
p2 <- plot(cp_rf, facet_ncol = 1, facet_scales = "free")
p3 <- plot(cp_rf, size = 2, alpha = 0.5, categorical_type = "bars")
p4 <- plot(cp_rf, variable_type = "categorical", categorical_type = "bars")

testthat::test_that("CP plot", {
  testthat::expect_true("gg" %in% class(p1))
  testthat::expect_true("gg" %in% class(p2))
  testthat::expect_true("gg" %in% class(p3))
  testthat::expect_true("gg" %in% class(p4))
})

testthat::test_that("Ceteris Paribus Profiles are computed for every class in a target variable", {
  testthat::expect_equal(unique(cp_rf$`_label_`), c('ranger.setosa', 'ranger.versicolor', 'ranger.virginica'))
})
