context("Check select_neighbours() function")

test_that("Output rf",{
  library("DALEX")
  new_apartment <- apartments[1, 2:6]
  small_apartments <- select_neighbours(apartments_test, new_apartment, n = 10)

  expect_true(nrow(small_apartments) == 10)

  small_apartments <- select_sample(apartments_test, n = 10)

  expect_true(nrow(small_apartments) == 10)
})

