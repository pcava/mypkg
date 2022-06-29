#' Function to compute the rolled mean of a vector over a certain window
#'
#' @param x \code{numeric}, vector of values.
#' @param k \code{integer}, size of window.
#'
#' @return numeric vector
#' @export
#'
#' @examples
#' roll_mean(1:10,2)
#' roll_mean(rnorm(10), k = 7)
#' # it will give an error
#' \dontrun{roll_mean(letters,4)}
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

