test_that("roll_mean works", {
  # example 1
  expect_equal(roll_mean(1:10,2), c(1,1.5:9.5))
  # dummy with all equal values
  expect_equal(roll_mean(rep(1,100), k = 2), rep(1,100))
})
test_that("character input gives error to roll_mean", {
  # example 3
  expect_error(roll_mean(letters,2))
})
