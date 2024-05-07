#'
#' Callback counts on paired data
#'
#'@description
#' Computes the callback count statistics from the paired data sets.
#'
#' @param x A \code{callback} object.
#'
#' @return
#' A data frame containing the following variables:
#' \tabular{ll}{
#' \code{callback1}\tab number of callbacks for candidate 1 \cr
#' \code{callback2}\tab number of callbacks for candidate 2\cr
#' \code{c00}\tab number of test without a callback\cr
#' \code{c10}\tab number of tests with callbacks for candidate 1 only\cr
#' \code{c01}\tab number of tests with callbacks for candidate 2 only\cr
#' \code{c11}\tab number of tests with callbacks for both candidates\cr
#' \code{callback}\tab number of tests with at least one callback for either candidate\cr
#' \code{calldif}\tab difference in callback numbers (\code{callback1-callback2})\cr
#' }
#'
#' @examples
#' data(labour2)
#' x <- callback(data=labour2,cluster="offer",candid="hist",callback="callback")
#' stat_count(x)
#'
#'@export

stat_count <- function(x) {
  c00 <- c10 <- c01 <- c11 <- NULL
  z <- t(as.data.frame(lapply(x$pfds, colSums)))
  z <-
    z[, c("callback1",
          "callback2",
          "c00",
          "c10",
          "c01",
          "c11",
          "callback",
          "calldif")]
  z <- transform(z, tests = c00 + c10 + c01 + c11)
  z <-
    z[, c("tests",
          "callback",
          "callback1",
          "callback2",
          "c00",
          "c10",
          "c01",
          "c11",
          "calldif")]
  z <- list(counts=z)
  class(z) <- "stat_count"
  return (z)
}
