#'
#' Prints the structure of the experiment
#'
#' @description
#' Computes the number of tests available for each pair of candidates
#'
#' @param x A \code{stat_count} object.
#' @param ... further arguments passed to or from other methods.
#'
#' @return Printed output.
#' @examples
#' data(labour1)
#' x <- callback(data=labour1,cluster="offer",candid="hist",callback="callback",
#' comp = "all")
#' print(x)
#' @rdname print.callback
#' @export

print.callback <- function(x,...) {
  cat(
    "\n",
    "Structure of the experiment \n",
    "---------------------------\n",
    "\n",
    "Candidates defined by:",
    x[["candid"]],
    "\n",
    "Callback variable:",
    x[["callback"]],
    "\n",
    "\n Number of tests for each candidate:\n"
  )
  single <- table(x[["fds"]][, "candid"])
  print(single)

  paired <- as.data.frame(lapply(x[["pfds"]], nrow), row.names = "")
  cat("\n", "\n Number of tests for each pair of candidates:\n\n")
  print(paired, row.names = FALSE)
  z <- list(single = single, paired = paired)
}

#'
#' Print the callback counts analysis
#'
#' @description
#' Computes the callback numbers for each candidate.
#'
#' @param x A \code{stat_count} object.
#' @param ... further arguments passed to or from other methods.
#'
#' @return Printed output.
#'
#' @examples
#' data(labour1)
#' print(stat_count(callback(labour1, "offer", "hist", "callback",
#' comp = "all")))
#' @rdname print.stat_count
#' @export

print.stat_count <- function(x,...) {
  m <- x$counts
  cat("\n", "Callback counts:\n",
      "----------------\n")
  colnames(m)[c(5:9)] <-
    c("Neither", "Only 1", "Only 2", "Both", "Difference")
  print(m)
}

#'
#' Print the callback proportions analysis
#'
#' @description
#' Computes the callbacks rates for each candidate, with their confidence intervals.
#'
#' @param x A \code{stat_prop} object.
#' @param digits minimal number of significant digits.
#' @param ... further arguments passed to or from other methods.
#'
#' @return Printed output.
#'
#' @examples
#' data(labour1)
#' x <- callback(labour1, "offer", "hist", "callback", comp = "all")
#' print(stat_prop(x,level=0.99))
#' @rdname print.stat_prop
#' @export


print.stat_prop <- function(x,digits=3,...){
  m <- x$props
  l_num <- round(x$level * 100, 1)
  cat(
    "\n",
    "\n Analysis of proportions - fractions of tests:\n",
    "---------------------------------------------",
    "\n",
    "\n",
    "At least one callback:\n\n"
  )
  out_prop_p1 <- m[, 1:4]
  print(out_prop_p1, digits = digits)
  cat("(Clopper-Pearson confidence intervals at the ",
      l_num,
      "% level)\n\n")


  cat("\n Callback rates of the candidates (cand1 vs cand2): \n\n")
  out_prop_p2 <- m[, 5:10]
  print(out_prop_p2, digits = digits)
  cat("(Clopper-Pearson confidence intervals at the ",
      l_num,
      "% level)\n\n")

  cat(
    "\n Analysis of proportions - fractions of exclusive callbacks:\n",
    "-----------------------------------------------------------\n"
  )

  out_prop_p3 <- m[, c(11:17)]
  cat("\n Callback rates of the candidates (cand1 vs cand2): \n\n")
  print(out_prop_p3, digits = digits)
  cat("(Clopper-Pearson confidence intervals at the ",
      l_num,
      "% level)\n\n")
}

#'
#' Print the callback comparison statistics
#'
#' @description
#' Print the results of the the Student and binomial tests of equality between the callback rates.
#'
#' @param x A \code{stat_comp} object.
#' @param digits minimal number of significant digits.
#' @param ... further arguments passed to or from other methods.
#'
#' @return Printed output.
#'
#' @examples
#' data(labour1)
#' print(stat_comp(callback(data=labour1, cluster="offer",candid="hist",
#' callback="callback", comp = "all")))
#' @rdname print.stat_comp
#' @export

print.stat_comp <- function(x,digits=5,...) {
  cat(
    "\n",
    "\n Equality of proportions - fractions of tests:\n",
    "---------------------------------------------",
    "\n"
  )
  print(x$student, digits = digits)
  cat("(Asymptotic Student tests)\n")

  cat(
    "\n",
    "\n Equality of proportions - fractions of exclusive callbacks:\n",
    "-----------------------------------------------------------",
    "\n"
  )
  print(x$binom, digits = digits)
  cat("(Exact binomial tests)")
}
