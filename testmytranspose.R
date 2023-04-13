install.packages("testthat")
library(testthat)
library(assertthat)

mytranspose <- function(x) {
  if (is.matrix(x) && all(dim(x) > 0)) {
    t(x)
  } else if (is.null(x) || length(x) == 0) {
    NA
  } else if (is.numeric(x) || is.logical(x)) {
    y <- matrix(1, nrow=ncol(x), ncol = nrow(x))
    for(i in 1:nrow(x)) {
      for(j in 1:ncol(x)) {
        y[j,i] <- x[i,j]
      }
    }
    return(y)
  } else if (is.data.frame(x)) {
    y_arr <- mytranspose(as.matrix(x))
    y_df <- as.data.frame(y_arr)
    names(y_df) <- 1:ncol(y_df)
    return(y_df)
  } else if (is.list(x)) {
    x_arr <- unlist(x)
    if (all(sapply(x_arr, is.numeric) | sapply(x_arr, is.logical))) {
      x_arr <- matrix(x_arr, nrow=length(x), byrow=TRUE)
      y_arr <- mytranspose(x_arr)
      y_list <- as.list(as.data.frame(y_arr))
      names(y_list) <- 1:ncol(y_arr)
      return(y_list)
    } else {
      stop("input should contain only numeric or logical values")
    }
  } else {
    stop("input should be a numeric vector, data frame, or list")
  }
}


# test 1
myvar1 <- matrix(1:10, nrow=5, ncol=2)
expected_result1 <- matrix(c(1,3,5,7,9,2,4,6,8,10), nrow=2, ncol=5, byrow=TRUE)
expect_equal(sort(mytranspose(myvar1)), sort(expected_result1))

myvar1 <- matrix(NA, nrow=0, ncol=0)
expected_result2 <- matrix(NA, nrow=0, ncol=0)
expect_equal(sort(mytranspose(myvar1)), sort(expected_result2))

myvar1 <- matrix(c(1,2), nrow=1, ncol=2)
expected_result3 <- matrix(c(1,2), nrow=2, ncol=1)
expect_equal(sort(mytranspose(myvar1)), sort(expected_result3))

myvar1 <- matrix(c(1,2), nrow=2, ncol=1)
expected_result4 <- matrix(c(1,2), nrow=1, ncol=2)
expect_equal(sort(mytranspose(myvar1)), sort(expected_result4))

# test 2
myvar2 <- c(1,2,NA,3)
expected_result1 <- matrix(c(1,2,NA,3), nrow=4, ncol=1)
expect_equal(sort(mytranspose(myvar2)), sort(expected_result1))

myvar2 <- c(NA)
expected_result2 <- matrix(NA, nrow=1, ncol=1)
expect_equal(sort(mytranspose(myvar2)), sort(expected_result2))

myvar2 <- c()
expected_result3 <- matrix(NA, nrow=0, ncol=0)
expect_equal(sort(mytranspose(myvar2)), sort(expected_result3))

# test 3
d <- c(1,2,3,4)
e <- c("red", "white", "red", NA)
f <- c(TRUE,TRUE,TRUE,FALSE)
mydata3 <- data.frame(d,e,f)

expected_result3 <- data.frame(
  `0` = c(1, "red", TRUE),
  `1` = c(2, "white", TRUE),
  `2` = c(3, "red", TRUE),
  `3` = c(4, NA, FALSE)
)

expect_equal(sort(mytranspose(mydata3)), sort(expected_result3))

