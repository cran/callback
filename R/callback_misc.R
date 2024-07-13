#'
#' Significance code of a p-value
#'
#' @param p A number between 0 and 1
#'
#' @return A character string
#'
#' @examples
#' stat_signif(0.045)
#'
#' @export

stat_signif <- function(p) {
  ifelse(p < 0, NA, ifelse(p <= 0.001, "***", ifelse(
    p <= 0.01,
    paste("**", " ", sep = ""),
    ifelse(
      p <= 0.05,
      paste("*", "  ", sep = ""),
      ifelse(p <= 0.10, paste(".", "  ", sep = ""), ifelse(p <= 1, paste("   "), NA))
    )
  )))
}

#'
#' Computational compatibility
#'
#' @param x A variable name
#'
#' @return A logical variable indicating whether the variable can be used for
#' computations (TRUE) or not (FALSE).
#'
#' @examples
#' data(origin1)
#' is.calc(origin1$cartime)
#' is.calc(origin1$callback)
#' is.calc(origin1$origin)
#'
#' @export

is.calc <- function(x) {
  is.logical(x) | is.numeric(x)
}

#'
#' Sums the numeric or logical columns in a data frame.
#'
#' @param x A data frame
#'
#' @return A data frame with the column sums, under their original names.
#'
#' @examples
#' data(labour1)
#' s <- callback(labour1,"offer","hist","callback","all")
#' stat_colsums(s$pfds[["LTC vs STC"]])
#'
#' @export

stat_colsums <- function(x) {
  colSums(x[, sapply(x, is.calc)])
}
