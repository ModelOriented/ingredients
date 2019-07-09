library('DALEX')
library('ingredients')
library('randomForest')


data('iris')
model_iris_rf <- randomForest(Species ~., iris)
explain_iris_rf <- explain(model_iris_rf,
                              data = iris[, -5],
                              y = iris$Species,
                              predict_function = function(m, x){predict(m, x, "prob")})
cp_rf <- ceteris_paribus(explain_iris_rf, iris[1,])
plot(cp_rf)

test_that("Ceteris Paribus Profiles are computed for every class in a target variable", {
  expect_equal(unique(cp_rf$`_label_`), c('randomForest.setosa', 'randomForest.versicolor', 'randomForest.virginica'))
})
