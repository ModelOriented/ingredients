library('DALEX')
library('ingredients')
library('ranger')


data('iris')
model_iris_rf <- ranger(Species ~., iris, probability = TRUE)
explain_iris_rf <- explain(model_iris_rf,
                              data = iris[, -5],
                              y = iris$Species,
                              verbose = FALSE)
cp_rf <- ceteris_paribus(explain_iris_rf, iris[1,])
plot(cp_rf)

testthat::test_that("Ceteris Paribus Profiles are computed for every class in a target variable", {
  testthat::expect_equal(unique(cp_rf$`_label_`), c('ranger.setosa', 'ranger.versicolor', 'ranger.virginica'))
})
