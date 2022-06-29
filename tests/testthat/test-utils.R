# if (FALSE) {
#   devtools::load_all()
# }
df <- read_data("case_time_series.csv","covid_data")

test_that("read_data works", {
  expect_true(is.data.frame(df),
              label = "wrong class")
  expect_equal(dim(df), c(620,4),
               label = "wrong dimension")
  expect_false(any(is.na(df)),
               label = "NA in data")
  expect_equal(names(df), c("Date", "Total.Confirmed", "Total.Recovered", "Total.Deceased"),
               label = "wrong column names")
  expect_false(any(df[,-1]<0),
               label = "data cannot be <0")

})

data <- process_data(df)

test_that("process_data works", {
  expect_true(is.data.frame(data),
              label = "wrong class")
  expect_equal(nrow(data), nrow(df),
               label = "wrong dimension")
  expect_true(sum(is.na(data)) == 0,
              label = "NAs found")
  expect_identical(data[, names(df)], df,
                   label = "data from read_data are not modified")

})

test_that("plot works", {
  pp <- line_plot_data(data, "Daily.Active")

  expect_equal(class(pp), c("gg","ggplot"),
               label = "wrong plot class")
  expect_equal(pp$data, data, label =
                 "data are different from input")

  pp <- bar_plot_data(data, "Daily.Active")

  expect_equal(class(pp), c("gg","ggplot"),
               label = "wrong plot class")
  expect_equal(pp$data, data,
               label = "data are different from input")
})
