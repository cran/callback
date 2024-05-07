#'
#' Callback rates on paired data
#'
#' @description
#' Computes the callback rates and their confidence intervals.
#'
#' @param x A \code{callback} object.
#'
#' @param level A number, containing the level of the confidence intervals (0.95
#' by default).
#'
#' @return
#' A data frame containing the following components (p refers to a proportion of
#' tests, r refers to a proportion of callbacks):
#'
#' \tabular{ll}{
#' \code{tests} \tab number of tests\cr
#' \code{Lxx_p_callback}\tab overall callback rate, lower bound\cr
#' \code{p_callback}\tab overall callback rate\cr
#' \code{Uxx_p_callback}\tab overall callback rate, upper bound\cr
#' \code{Lxx_p_cand1}\tab 1st candidate callback rate, lower bound\cr
#' \code{p_cand1}\tab 1st candidate callback rate \cr
#' \code{Uxx_p_cand1}\tab 1st candidate callback rate, upper bound\cr
#' \code{Lxx_p_cand2}\tab 2nd candidate callback rate, lower bound\cr
#' \code{p_cand2}\tab 2nd candidate callback rate \cr
#' \code{Uxx_p_cand2}\tab 2nd candidate callback rate, upper bound\cr
#' \code{callback}\tab number of callbacks\cr
#' \code{Lxx_r_cand1}\tab 1st candidate proportion of callbacks, lower bound\cr
#' \code{r_cand1}\tab 1st candidate proportion of callbacks \cr
#' \code{Uxx_r_cand1}\tab 1st candidate proportion of callbacks, upper bound\cr
#' \code{Lxx_r_cand2}\tab 2nd candidate proportion of callbacks, lower bound\cr
#' \code{r_cand2}\tab 2nd candidate proportion of callbacks \cr
#' \code{Uxx_r_cand2}\tab 2nd candidate proportion of callbacks, upper bound\cr
#' }
#'
#' @examples
#' data(labour1)
#' x <- callback(data=labour1,cluster="offer",candid="hist",callback="callback")
#' stat_prop(x,level=0.99)
#'
#' @export

stat_prop <- function(x, level=0.95) {
  if (level<=0|level>=1) {level = 0.95}
  alpha <- 1 - level
  l_num <- round(level * 100, 1)
  l_inf <- paste("L", l_num, sep = "")
  l_sup <- paste("U", l_num, sep = "")
  counts <- stat_count(x)$counts
  out_prop <- with(
    counts,
    data.frame(
      tests = tests,
      inf_p_callback = qbeta(alpha / 2, callback, tests - callback + 1),
      p_callback = callback / tests,
      sup_p_callback = qbeta(1 - alpha / 2, callback + 1, tests - callback),
      inf_p_cand1 = qbeta(alpha / 2, callback1, tests - callback1 + 1),
      p_cand1 = callback1 / tests,
      sup_p_cand1 = qbeta(1 - alpha / 2, callback1 + 1, tests - callback1),
      inf_p_cand2 = qbeta(alpha / 2, callback2, tests - callback2 + 1),
      p_cand2 = callback2 / tests,
      sup_p_cand2 = qbeta(1 - alpha / 2, callback2 + 1, tests - callback2),
      row.names = rownames(counts)
    )
  )
  colnames(out_prop)[c(1, 2, 4, 5, 7, 8, 10)] <- c(
    "tests",
    paste(l_inf, "p_callback", sep =
            "_"),
    paste(l_sup, "p_callback", sep =
            "_"),
    paste(l_inf, "p_cand1", sep =
            "_"),
    paste(l_sup, "p_cand1", sep =
            "_"),
    paste(l_inf, "p_cand2", sep =
            "_"),
    paste(l_sup, "p_cand2", sep =
            "_")
  )


  out_prop_r <- with(
    counts,
    data.frame(
      callback = callback,
      inf_r_cand1 = qbeta(alpha / 2, c10, c01 + 1),
      r_cand1 = c10 / (c10 + c01),
      sup_r_cand1 = qbeta(1 - alpha / 2, c10 + 1, c01),
      inf_r_cand2 = qbeta(alpha / 2, c01, c10 + 1),
      r_cand2 = c01 / (c10 + c01),
      sup_r_cand2 = qbeta(1 - alpha / 2, c01 + 1, c10)
    )
  )

  out_prop <- cbind(out_prop, out_prop_r)

  colnames(out_prop)[12:17] <- c(
    paste(l_inf, "r_cand1", sep = "_"),
    "r_cand_1",
    paste(l_sup, "r_cand1", sep = "_"),
    paste(l_inf, "r_cand2", sep = "_"),
    "r_cand_2",
    paste(l_sup, "r_cand2", sep = "_")
  )
  z <- list(props=out_prop,level=level)
  class(z) <- "stat_prop"
  return (z)
}
