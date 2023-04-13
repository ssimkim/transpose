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


