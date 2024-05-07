#'
#' Comparison statistics on matched data
#'
#' @param x A \code{callback} object.
#'
#' @return
#' A \code{stat_comp} objects including two data frames. The first data frame is \code{Student} and includes the following variables:
#' \tabular{ll}{
#' \code{p_callback1-p_callback2} \tab callback rates difference\cr
#' \code{Student}\tab Student statistics for \code{dif(cand1-cand2)}\cr
#' \code{p_value}\tab p-value for \code{Student}\cr
#' }
#' The second data frame is \code{binom} and includes the following variables:
#' \tabular{ll}{
#' \code{r_cand1}\tab proportion of callbacks received by the 1st candidate\cr
#' \code{r_dif}\tab \code{r_cand-0.5}\cr
#' \code{r_pvalue}\tab p-value of the binomial test \code{r_cand1=0.5} \cr
#' }
#'
#' @examples
#' data(labour1)
#' x <- callback(data=labour1,cluster="offer",candid="hist",callback="callback")
#' stat_comp(x)
#' @export
#'
#' @importFrom stats t.test

stat_comp <- function(x) {
  # asymptotic Student tests
  pfds <- x$pfds
  l_comp <- names(pfds)
  n_comp <- length(l_comp)
  s <- list()
  for (comp in l_comp) {
     s[[comp]] <- as.data.frame(
     t.test(pfds[[comp]][,"calldif"])[c("estimate", "statistic", "p.value")])
    }
  student <- s[[1]]
  if (n_comp >= 2) {
     for (i in 2:n_comp) {
     student <- rbind(student, s[[i]])
    }
    student <- as.data.frame(student)
    } else {
    student <- as.data.frame(t(student))
    }
    rownames(student) <- l_comp
    colnames(student) <- c("p_callback1-p_callback2", "Student", "p-value")

  # binomial tests
  counts <- stat_count(x)$counts
  binom <- with(
    counts,
    data.frame(
      r_cand1 = c10 / (c10 + c01),
      r_dif = c10 / (c10 + c01) - 0.5,
      r_pvalue = ifelse(
        c10 > c01,
        pbinom(c01, c10 + c01, 0.5)
        + pbinom(c10, c10 + c01, 0.5, lower.tail = FALSE)
        + dbinom(c10, c10 + c01, 0.5),
        pbinom(c10, c10 + c01, 0.5)
        + pbinom(c01, c10 + c01, 0.5, lower.tail = FALSE)
        + dbinom(c01, c10 + c01, 0.5)
      ),
      row.names = l_comp
    )
  )
  colnames(binom) <- c("r_cand1", "r_cand1-0.5", "p-value")

  z <- list(student=student,binom=binom)
  class(z) <- "stat_comp"
  return (z)
}
