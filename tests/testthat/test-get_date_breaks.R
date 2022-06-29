# create vector of test data outside test_that()
vect_dates <- as.Date(seq(1, 100, by = 5), origin = as.Date("2022/01/01"))

test_that("get_date_breaks works", {
  # example 1, 1 months expected
  expect_equal(get_date_breaks(vect_dates, 10), "1 month")
  # dummy with all equal values
  expect_equal(get_date_breaks(vect_dates, 100), "1 week")
})
test_that("character input gives error to get_date_breaks", {
  # example 3
  expect_error(get_date_breaks(vect_dates, "100"))
  # this test gives an error,
  # the function can make the difference with character "100" due to the operation on Dates
  # get_date_breaks must be updated with a stop message in order to make this test pass
})

# updated function
get_date_breaks <- function(dates, threshold = 160) {
  if (is.character(threshold))
    stop("threshold must be numeric")
  ifelse(diff(range(dates)) < threshold, "1 week", "1 month")
}
