context("Check plotD3() functions")

test_that("plot plotD3",{
  library("DALEX")
  library("caret")

  regr_rf <- train(m2.price~., data = apartments, method="rf", ntree = 100)
  regr_svm <- train(m2.price~., data = apartments, method="svmLinear")
  explainer_rf <- explain(regr_rf, data = apartmentsTest[,2:6],
                          y = apartmentsTest$m2.price, label="rf")
  explainer_svm <- explain(regr_svm, data = apartmentsTest[,2:6],
                           y = apartmentsTest$m2.price, label="svm")
  fi_rf <- feature_importance(explainer_rf, loss_function = loss_root_mean_square)
  fi_svm <- feature_importance(explainer_svm, loss_function = loss_root_mean_square)

  p1 <- plotD3(fi_rf, fi_svm)
  p2 <- plotD3(fi_rf, fi_svm, scaleHeight = TRUE)
  p3 <- plotD3(fi_rf, fi_svm, label = FALSE)
  p4 <- plotD3(fi_rf, fi_svm, label = FALSE, scaleHeight = TRUE)

  expect_true("r2d3" %in% class(p1))
  expect_true("r2d3" %in% class(p2))
  expect_true("r2d3" %in% class(p3))
  expect_true("r2d3" %in% class(p4))
})
