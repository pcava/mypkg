# new test, if NAs in x roll_mean breaks
test_that("roll_mean breaks with NAs", {
  expect_error(roll_mean(c(1:10,NA),2)) 
})

# updated roll_mean function

roll_mean <- function(x, k) {
  if (!is.numeric(x)) {
    stop("wrong class of x: ", class(x))
  }
  if (any(is.na(x))) {
    stop("x contains: ", sum(is.na(x)), " NAs")
  }
  if (k %% 1 != 0) {
    stop("k is not an integer: ", k)
  }
  res <- NULL # will contain our result
  from <- 1
  # we stop when we hit the end
  while ((from + k - 1) <= length(x)) {
    to <- from + k - 1
    # add mean to our result
    res <- c(res, mean(x[from:to]))
    # shift forward
    from <- from + 1
  }
  c(x[1:(k - 1)], res) # to be modified
}
