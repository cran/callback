#'
#' Prints the structure of the experiment
#'
#' @description
#' Computes the number of tests available for each pair of candidates
#'
#' @param x a \code{stat_count} object.
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

print.callback <- function(x, ...) {
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

  if (is.null(x$ofds)) {
    cat("\n", "\n Duplication detected")
  } else {
    cat("\n",
        "\n Number of tests with all the candidates:",
        nrow(x$ofds))
  }
}

#'
#' Prints the global callback statistics
#'
#' @description
#' Prints the global callback numbers and proportions for each candidate.
#'
#' @param x a \code{stat_glob} object.
#' @param method the type of confidence interval: "cp" for Clopper-Pearson,
#' "wilson" for Wilson (the default), "student" for Student.
#' @param digits minimal number of significant digits
#' @param ... further arguments passed to or from other methods.
#'
#' @return Printed output.
#'
#' @examples
#' data(labour1)
#' print(stat_glob(callback(labour1, "offer", "hist", "callback",
#' comp = "all")))
#' @rdname print.stat_glob
#' @export

print.stat_glob <-
  function(x = NULL,
           method = "wilson",
           digits = 3,
           ...) {
    l_num <- round(x$level * 100, 1)
    l_method <- c("cp", "wilson", "student")
    method <- tolower(method)
    if (!(method %in% l_method)) {
      method <- "wilson"
    }
    m_name <- ifelse(
      method == "wilson",
      "Wilson",
      ifelse(method == "student", "Student", "Clopper-Pearson")
    )
    p_callback <- inf <- sup <- NULL
    props <- x[["props"]]
    m <- x[[method]]
    pr <- data.frame(
      inf = m$inf_p_callback,
      p_callback = props$p_callback,
      sup = m$sup_p_callback,
      row.names = rownames(m)
    )

    cat(
      "\n",
      "Global callback rates \n",
      "---------------------\n",
      "\n",
      paste(
        m_name,
        "confidence intervals at the",
        round(x$level * 100, 1),
        "percent level"
      ),
      "\n\n"
    )
    print(pr)

  }

#'
#' Print the callback counts analysis
#'
#' @description
#' Computes the callback numbers for each candidate.
#'
#' @param x a \code{stat_count} object.
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

print.stat_count <- function(x, ...) {
  m <- x$counts
  cat("\n", "Callback counts:\n", "----------------\n")
  colnames(m)[c(5:9)] <-
    c("Neither", "Only 1", "Only 2", "Both", "Difference")
  print(m)
}

#'
#' Prints the callback proportions analysis
#'
#' @description
#' Prints the statistics about the callback rates
#'
#' @param x a \code{stat_mcr} object.
#' @param digits minimal number of significant digits.
#' @param ... further arguments passed to or from other methods.
#'
#' @return Printed output.
#'
#' @examples
#' data(labour1)
#' x <- callback(labour1, "offer", "hist", "callback", comp = "all")
#' print(stat_mcr(x,level=0.90))
#' @rdname print.stat_prop
#' @export


print.stat_mcr <- function(x, digits = 3, ...) {
  m <- x$props
  l_num <- round(x$level * 100, 1)
  cat(
    "\n",
    "\n Equality of proportions - matched callback rates\n",
    "------------------------------------------------",
    "\n",
    "\n",
    "Fisher test:",
    "\n"
  )
  print(x$t.fisher)
  cat("\n Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.10 ' ' 1")
  cat("\n", "\n", "Chi-squared test:", "\n")
  print(x$t.pearson)
  cat("\n Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.10 ' ' 1")
  cat("\n", "\n", "Student test:", "\n")
  print(x$t.student)
  cat("\n Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.10 ' ' 1")
}



#'
#' Prints the proportions of the total callback shares
#'
#' @description
#' Prints the statistics from stat_tcs() output
#'
#' @param x a \code{stat_tcs} object.
#' @param digits minimal number of significant digits.
#' @param ... further arguments passed to or from other methods.
#'
#' @return Printed output.
#'
#' @examples
#' data(labour1)
#' x <- callback(labour1, "offer", "hist", "callback", comp = "all")
#' print(stat_tcs(x,level=0.90))
#' @rdname print.stat_ecs
#' @export

print.stat_tcs <- function(x, digits = 3, ...) {
  m <- x$props
  l_num <- round(x$level * 100, 1)
  cat(
    "\n",
    "\n Equality of proportions - total callback shares\n",
    "-----------------------------------------------",
    "\n",
    "\n",
    "Fisher test:",
    "\n"
  )
  print(x$t.fisher)
  cat("\n Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.10 ' ' 1")
  cat("\n", "\n", "Chi-squared test:", "\n")
  print(x$t.pearson)
  cat("\n Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.10 ' ' 1")
  cat("\n", "\n", "Student test:", "\n")
  print(x$t.student)
  cat("\n Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.10 ' ' 1")
}

#'
#' Prints the proportions of the excluvive callback shares
#'
#' @description
#' Prints the statistics from stat_ecs() output
#'
#' @param x a \code{stat_ecs} object.
#' @param digits minimal number of significant digits.
#' @param ... further arguments passed to or from other methods.
#'
#' @return Printed output.
#'
#' @examples
#' data(labour1)
#' x <- callback(labour1, "offer", "hist", "callback", comp = "all")
#' print(stat_ecs(x,level=0.90))
#' @rdname print.stat_ecs
#' @export

print.stat_ecs <- function(x, digits = 3, ...) {
  m <- x$props
  l_num <- round(x$level * 100, 1)
  cat(
    "\n",
    "\n Equality of proportions - exclusive callback shares\n",
    "---------------------------------------------------",
    "\n",
    "\n",
    "Fisher test:",
    "\n"
  )
  print(x$t.fisher)
  cat("\n Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.10 ' ' 1")
  cat("\n", "\n", "Chi-squared test:", "\n")
  print(x$t.pearson)
  cat("\n Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.10 ' ' 1")
  cat("\n", "\n", "Student test:", "\n")
  print(x$t.student)
  cat("\n Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.10 ' ' 1")
}
