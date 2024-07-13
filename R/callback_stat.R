#'
#' Callback counts on paired data
#'
#'@description
#' Computes the callback count statistics from the paired data sets.
#'
#' @param x A \code{callback} object.
#'
#' @return
#' A list with class \code{"stat_count"} containing two data frames: counts and
#' props.
#'
#' \code{counts:} a data frame with the callback counts.
#' \itemize{
#' \item \bold{tests:} number of tests.
#' \item \bold{callback:} number of tests with at least one callback for either
#' candidate.
#' \item \bold{callback1:} number of callbacks for candidate 1.
#' \item \bold{callback2:} number of callbacks for candidate 2.
#' \item \bold{c00:} number of test without a callback.
#' \item \bold{c10:} number of tests with callbacks for candidate 1 only.
#' \item \bold{c01:} number of tests with callbacks for candidate 2 only.
#' \item \bold{c11:} number of tests with callbacks for both candidates
#' \item \bold{calldif:} difference in callback numbers.
#'  }
#'
#' \code{props:} a data frame with the following variables.
#' \itemize{
#' \item \bold{p_callback:} callback/tests.
#' \item \bold{p_cand1:} callback1/tests.
#' \item \bold{p_cand2:} callback2/tests.
#' \item \bold{p_c00:} c00/tests.
#' \item \bold{p_c10:} c10/tests.
#' \item \bold{p_c01:} c01/tests.
#' \item \bold{p_c11:} c11/tests.
#' \item \bold{p_cand_dif:} calldif/tests.
#' }
#'
#' @author Emmanuel Duguet
#'
#' @examples
#' data(labour2)
#' x <- callback(data=labour2,cluster="offer",candid="hist",callback="callback")
#' stat_count(x)
#'
#'@export

stat_count <- function(x) {
  c00 <- c10 <- c01 <- c11 <- NULL
  z <- t(as.data.frame(lapply(x$pfds, stat_colsums)))
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
  counts <-
    z[, c("tests",
          "callback",
          "callback1",
          "callback2",
          "c00",
          "c10",
          "c01",
          "c11",
          "calldif")]

  props <- with(
    z,
    data.frame(
      p_callback = callback / tests,
      p_cand1 = callback1 / tests,
      p_cand2 = callback2 / tests,
      p_c00 = c00 / tests,
      p_c10 = c10 / tests,
      p_c01 = c01 / tests,
      p_c11 = c11 / tests,
      p_cand_dif = calldif / tests,
      row.names = rownames(z)
    )
  )

  z <- list(counts = counts, props = props)

  class(z) <- "stat_count"
  return (z)
}

#'
#' Exclusive callback shares
#'
#' @description
#' Computes the callback shares and their confidence intervals. The analysis is
#'  restricted to the tests with discrimination cases.
#'
#' @param x a \code{callback} object.
#'
#' @param level a number, containing the level of the confidence intervals (0.95
#' by default).
#'
#' @return
#' A list with class \code{"stat_ecs"} containing 8 components : level, shares,
#' cp, wilson, student, t.student, t.pearson and t.fisher.
#'
#' \code{level}: the level of the confidence intervals.
#'
#' \code{shares}: a data frame containing the following variables.
#' \itemize{
#' \item \bold{disc:} number of discrimination cases.
#' \item \bold{c10:} number of tests with the 1st candidate preferred (2nd candidate
#'  discriminated against).
#' \item \bold{c01:} number of tests with the 2nd candidate preferred (1st candidate
#'  discriminated against).
#' \item \bold{cdif:} net discrimination c10-c01.
#' \item \bold{p_cand1:} 1st candidate callback share (c10/disc).
#' \item \bold{p_cand2:} 2nd candidate callback share (c01/disc).
#' \item \bold{p_cand_dif:} p_cand1-1/2.
#' }
#'
#' \code{cp}: a data frame containing the Clopper-Pearson confidence intervals,
#' from binom.test(), and the p-value of the Fisher test of independence between
#' the candidate type and the callback variable, from fisher.test().
#' \itemize{
#' \item \bold{inf_p_cand1:} 1st candidate callback rate, lower bound.
#' \item \bold{sup_p_cand1:} 1st candidate callback rate, upper bound.
#' \item \bold{inf_p_cand2:} 2nd candidate callback rate, lower bound.
#' \item \bold{sup_p_cand2:} 2nd candidate callback rate, upper bound.
#' }
#'
#' \code{wilson}: a data frame containing the Wilson confidence intervals
#'  and the p-value of the equality test of callback shares between the two
#'  candidates, from prop.test().
#' \itemize{
#' \item \bold{inf_p_cand1:} 1st candidate callback share, lower bound.
#' \item \bold{sup_p_cand1:} 1st candidate callback share, upper bound.
#' \item \bold{inf_p_cand2:} 2nd candidate callback share, lower bound.
#' \item \bold{sup_p_cand2:} 2nd candidate callback share, upper bound.
#' \item \bold{inf_cand_dif:} p_c10-p_c01, lower bound.
#' \item \bold{sup_cand_dif:} p_c10-p_c01, upper bound.
#' }
#'
#' \code{student}: a data frame containing the Student confidence intervals
#'  and the p-value of the equality test of callback shares between the two
#'  candidates.
#' \itemize{
#' \item \bold{inf_p_cand1:} 1st candidate callback share, lower bound.
#' \item \bold{sup_p_cand1:} 1st candidate callback share, upper bound.
#' \item \bold{inf_p_cand2:} 2nd candidate callback share, lower bound.
#' \item \bold{sup_p_cand2:} 2nd candidate callback share, upper bound.
#' \item \bold{inf_cand_dif:} p_c10-p_c01, lower bound.
#' \item \bold{sup_cand_dif:} p_c10-p_c01, upper bound.
#' }
#'
#' \code{t.fisher}: a data frame containing the statistics of the Fisher test.
#'  \itemize{
#'   \item\bold{p_cand_dif:} 1st candidate callback share - 1/2.
#'   \item\bold{p_Fisher:} the p-value of the Fisher test.
#'   \item\bold{s_Fisher:} the significance code of the Fisher test.
#'    }
#'
#' \code{t.pearson}: a data frame containing the statistics of the Pearson test.
#'  \itemize{
#'   \item\bold{p_cand_dif:} 1st candidate callback share - 1/2.
#'   \item\bold{Pearson:} the value of Pearson's chi-squared test statistic.
#'   \item\bold{p_Pearson:} the p-value of the Pearson test.
#'   \item\bold{s_Pearson:} the significance code of the Pearson test.
#'    }
#'
#' \code{t.student}: A data frame containing the statistics of the Student test.
#'  \itemize{
#'   \item\bold{p_cand_dif:} 1st candidate callback share - 1/2.
#'   \item\bold{Student:} the value of Student's test statistic.
#'   \item\bold{p_Student:} the p-value of the Student test.
#'   \item\bold{s_Student:} the significance code of the Student test.
#'    }
#'
#'
#' @author Emmanuel Duguet
#'
#' @references
#' Clopper, C. J. & Pearson, E. S. (1934). The use of confidence or fiducial
#' limits illustrated in the case of the binomial. Biometrika, 26, 404–413.
#' doi:10.2307/2331986.
#'
#' Wilson, E.B. (1927). Probable inference, the law of succession, and
#' statistical inference. Journal of the American Statistical Association, 22,
#' 209–212. doi:10.2307/2276774.
#'
#' @examples
#' data(labour1)
#' x <- callback(data=labour1,cluster="offer",candid="hist",callback="callback")
#' str(stat_ecs(x,level=0.9))
#'
#' @importFrom stats binom.test prop.test fisher.test
#'
#' @export

stat_ecs <- function(x, level = 0.95) {
  c10 <-
    disc <-
    c01 <- p_cand1 <- p_cand2 <- inf_p_cand1 <- inf_p_cand2 <- NULL
  sup_p_cand1 <-
    sup_p_cand2 <- p_cand_dif <- Student <- Pearson <- NULL
  p_Fisher <-
    p_Student <- inf_cand_dif <- sup_cand_dif <- p_Pearson <-
    s_Fisher <- s_Student <- s_Pearson <- NULL

  if (level <= 0 | level >= 1) {
    level = 0.95
  }
  alpha <- 1 - level

  counts <- stat_count(x)$counts

  props <- data.frame(
    tests = counts$tests,
    disc = counts$c10 + counts$c01,
    c10 = counts$c10,
    c01 = counts$c01,
    row.names = rownames(counts)
  )

  props <- transform(props,
                     p_cand1 = c10 / pmax(disc, 1),
                     p_cand2 = c01 / pmax(disc, 1))

  props <-
    transform(props, p_cand_dif = ifelse(disc == 0, 0, p_cand1 - p_cand2))

  # Clopper-Pearson/Fisher exact test
  l_comp <- rownames(props)
  n_comp <- length(l_comp)
  ct <- c1 <- c2 <- c3 <- list()
  for (comp in l_comp) {
    z <- subset(props, c10 + c01 > 0)
    c1 <- binom.test(z[comp, "c10"], z[comp, "disc"], conf.level =
                       level)["conf.int"]
    c2 <- binom.test(z[comp, "c01"], z[comp, "disc"], conf.level =
                       level)["conf.int"]
    #contingency table in matrix form
    m <- matrix(
      c(z[comp, "disc"] - z[comp, "c10"], z[comp, "c10"], z[comp, "disc"] - z[comp, "c01"], z[comp, "c01"]),
      nrow = 2,
      ncol = 2,
      byrow = TRUE
    )
    c3 <- fisher.test(m)["p.value"]

    ct[[comp]] <- c(c1[[1]], c2[[1]], c3[[1]])
  }

  cp <- ct[[1]]
  if (n_comp >= 2) {
    for (i in 2:n_comp) {
      cp <- rbind(cp, ct[[i]])
    }
    cp <- as.data.frame(cp)
  } else {
    cp <- as.data.frame(t(cp))
  }
  rownames(cp) <- l_comp
  colnames(cp) <- c("inf_p_cand1",
                    "sup_p_cand1",
                    "inf_p_cand2",
                    "sup_p_cand2",
                    "p_Fisher")
  cp$s_Fisher = stat_signif(cp$p_Fisher)


  # Student
  pfds <- x$pfds
  l_comp <- names(pfds)
  n_comp <- length(l_comp)
  st <- s1 <- s2 <- s3 <- list()
  for (comp in l_comp) {
    z <- subset(pfds[[comp]], c10 + c01 > 0)
    s1 <- t.test(z[, "c10"], conf.level = level)["conf.int"]
    s2 <- t.test(z[, "c01"], conf.level = level)["conf.int"]
    s3 <-
      t.test(z[, "c10"] - z[, "c01"], conf.level = level)["p.value"]
    s4 <-
      t.test(z[, "c10"] - z[, "c01"], conf.level = level)["statistic"]
    s5 <-
      t.test(z[, "c10"] - z[, "c01"], conf.level = level)["conf.int"]
    st[[comp]] <- c(s1[[1]], s2[[1]], s3[[1]], s4[[1]], s5[[1]])
  }
  student <- st[[1]]
  if (n_comp >= 2) {
    for (i in 2:n_comp) {
      student <- rbind(student, st[[i]])
    }
  }
  rownames(student) <- l_comp
  colnames(student) <- c(
    "inf_p_cand1",
    "sup_p_cand1",
    "inf_p_cand2",
    "sup_p_cand2",
    "p_Student",
    "Student",
    "inf_cand_dif",
    "sup_cand_dif"
  )
  student <- transform(
    student,
    inf_p_cand1 = pmax(0, inf_p_cand1),
    inf_p_cand2 = pmax(0, inf_p_cand2),
    sup_p_cand1 = pmin(1, sup_p_cand1),
    sup_p_cand2 = pmin(1, sup_p_cand2),
    s_Student = stat_signif(p_Student)
  )

  # Wilson
  l_comp <- rownames(props)
  n_comp <- length(l_comp)
  wt <- w1 <- w2 <- w3 <- list()
  for (comp in l_comp) {
    z <- subset(props, c10 + c01 > 0)
    w1 <- prop.test(z[comp, "c10"], z[comp, "disc"], conf.level =
                      level)["conf.int"]
    w2 <- prop.test(z[comp, "c01"], z[comp, "disc"], conf.level =
                      level)["conf.int"]
    w3 <-
      prop.test(c(z[comp, "c10"], z[comp, "c01"]), c(z[comp, "disc"], z[comp, "disc"]))["p.value"]
    w4 <-
      prop.test(c(z[comp, "c10"], z[comp, "c01"]), c(z[comp, "disc"], z[comp, "disc"]))["statistic"]
    w5 <-
      prop.test(c(z[comp, "c10"], z[comp, "c01"]), c(z[comp, "disc"], z[comp, "disc"]))["conf.int"]

    wt[[comp]] <- c(w1[[1]], w2[[1]], w3[[1]], w4[[1]], w5[[1]])
  }

  wilson <- wt[[1]]
  if (n_comp >= 2) {
    for (i in 2:n_comp) {
      wilson <- rbind(wilson, wt[[i]])
    }
    wilson <- as.data.frame(wilson)
  } else {
    wilson <- as.data.frame(t(wilson))
  }
  rownames(wilson) <- l_comp
  colnames(wilson) <- c(
    "inf_p_cand1",
    "sup_p_cand1",
    "inf_p_cand2",
    "sup_p_cand2",
    "p_Pearson",
    "Pearson",
    "inf_cand_dif",
    "sup_cand_dif"
  )
  wilson$s_Pearson = stat_signif(wilson$p_Pearson)

  #comparisons
  comp <- data.frame(p_cand_dif = props$p_cand_dif,
                     row.names = rownames(props))

  comp <-
    cbind(comp, cp[, 5:6], wilson[, c(5, 6, 9)], student[, c(5, 6, 9)])

  m <- list(
    level = level,
    props = props,
    cp = cp[, 1:4],
    wilson = wilson[c(1:4, 7, 8)],
    student = student[c(1:4, 7, 8)],
    t.fisher = comp[, 1:3],
    t.pearson = comp[, c(1, 5, 4, 6)],
    t.student = comp[, c(1, 8, 7, 9)]
  )
  class(m) <- "stat_ecs"
  return (m)
}

#'
#' Unmatched callback rates
#'
#'@description
#' Number and proportion of callbacks for all the candidates.
#'
#' @param x A \code{callback} object.
#'
#' @param level A number, containing the level of the confidence intervals (0.95
#' by default).
#'
#' @return
#' A \code{stat_glob} object with 5 components : \code{level}, \code{props},
#' \code{cp}, \code{wilson} and \code{student}.
#'
#' \code{level}: the level of the confidence intervals
#'
#' \code{props}: a data frame containing the following variables.
#' \itemize{
#' \item\bold{tests:} number of tests.
#' \item\bold{callback:} number of callbacks.
#' \item\bold{p_callback:} callback rate.
#' }
#'
#' \code{cp}, \code{wilson} and \code{student} are data frames containing the
#' following variables:
#' \itemize{
#' \item\bold{inf_p_callback:} callback rate lower bound.
#' \item\bold{sup_p_callback:} callback rate upper bound.
#' }
#'
#' @author Emmanuel Duguet
#'
#' @references
#' Clopper, C. J. & Pearson, E. S. (1934). The use of confidence or fiducial
#' limits illustrated in the case of the binomial. Biometrika, 26, 404–413.
#' doi:10.2307/2331986.
#'
#' Wilson, E.B. (1927). Probable inference, the law of succession, and
#' statistical inference. Journal of the American Statistical Association, 22,
#' 209–212. doi:10.2307/2276774.
#'
#' @examples
#' data(labour2)
#' x <- callback(data=labour2,cluster="offer",candid="hist",callback="callback")
#' str(stat_glob(x))
#'
#' @importFrom stats aggregate binom.test prop.test t.test
#'
#' @export

stat_glob <- function(x, level = 0.95) {
  candid <- NULL
  if (level <= 0 | level >= 1) {
    level = 0.95
  }
  tests <- NULL
  alpha <- 1 - level
  m1 <- aggregate(data = x$fds, callback ~ candid, FUN = sum)
  m2 <- aggregate(data = x$fds, callback ~ candid, FUN = length)
  m <- merge(m1, m2, by = "candid")
  colnames(m)[2:3] <- c("callback", "tests")
  rownames(m) <- m$candid
  m <- m[, 2:3]
  m$p_callback <- m$callback / m$tests

  l_cand <- rownames(m)
  n_cand <- length(l_cand)
  s0 <- s1 <- s2 <- st <- NULL
  for (cand in l_cand) {
    s0 <-
      binom.test(m[cand, "callback"], m[cand, "tests"], conf.level = level)["conf.int"]
    s1 <-
      prop.test(m[cand, "callback"], m[cand, "tests"], conf.level = level)["conf.int"]
    s2 <-
      t.test(subset(x$fds, candid == cand)[["callback"]], conf.level = level)["conf.int"]
    st[[cand]] <- c(s0[[1]], s1[[1]], s2[[1]])
  }

  stats <- st[[1]]
  if (n_cand >= 2) {
    for (i in 2:n_cand) {
      stats <- rbind(stats, st[[i]])
    }
    stats <- as.data.frame(stats)
  } else {
    stats <- as.data.frame(t(stats))
  }
  rownames(stats) <- l_cand


  cp <- stats[, 1:2]
  colnames(cp) <- c("inf_p_callback", "sup_p_callback")

  wilson <- stats[, 3:4]
  colnames(wilson) <- c("inf_p_callback", "sup_p_callback")

  student <- stats[, 5:6]
  colnames(student) <- c("inf_p_callback", "sup_p_callback")
  student$inf_p_callback <- pmax(0, student$inf_p_callback)
  student$sup_p_callback <- pmin(1, student$sup_p_callback)

  m <- list(
    level = level,
    props = m,
    cp = cp,
    wilson = wilson,
    student = student
  )
  class(m) <- "stat_glob"
  return(m)
}

#'
#' Matched callback rates
#'
#' @description
#' Computes the callback rates and their confidence intervals.
#'
#' @param x a \code{callback} object.
#'
#' @param level a number, containing the level of the confidence intervals (0.95
#' by default).
#'
#' @return
#'
#' A list with class \code{"stat_mcr"} containing 8 components: level, props, cp,
#'  wilson, student, t.fisher, t.pearson and t.student.
#'
#'
#'  \code{level:} the level of the confidence intervals.
#'
#'  \code{props}: a data frame containing the following variables:
#'  \itemize{
#'   \item\bold{tests:} number of tests.
#'   \item\bold{p_callback:} overall callback rate.
#'   \item\bold{p_cand1:} 1st candidate callback rate.
#'   \item\bold{p_cand_dif:} p_cand1-p_cand2.
#'  }
#'
#' \code{cp}: A data frame containing the Clopper-Pearson confidence intervals,
#' from binom.test().
#'  \itemize{
#'   \item\bold{inf_p_callback:} overall callback rate, lower bound.
#'   \item\bold{p_callback:} overall callback rate.
#'   \item\bold{sup_p_callback:} overall callback rate, upper bound.
#'   \item{inf_p_cand1:} 1st candidate callback rate, lower bound.
#'   \item\bold{p_cand1:} 1st candidate callback rate.
#'   \item\bold{sup_p_cand1:} 1st candidate callback rate, upper bound.
#'   \item\bold{inf_p_cand2:} 2nd candidate callback rate, lower bound.
#'   \item\bold{p_cand2:} 2nd candidate callback rate.
#'   \item\bold{sup_p_cand2:} 2nd candidate callback rate, upper bound.
#'   \item\bold{inf_cand_dif:} p_cand1-p_cand2, lower bound.
#'   \item\bold{sup_cand_dif:} p_cand1-p_cand2, upper bound.
#'  }
#'
#' \code{wilson}: a data frame containing the Wilson confidence intervals,
#'  from prop.test().
#'  \itemize{
#'  \item\bold{inf_p_callback:} overall callback rate, lower bound.
#'  \item\bold{p_callback:} overall callback rate.
#'  \item\bold{sup_p_callback:} overall callback rate, upper bound.
#'  \item\bold{inf_p_cand1:} 1st candidate callback rate, lower bound.
#'  \item\bold{p_cand1:} 1st candidate callback rate.
#'  \item\bold{sup_p_cand1:} 1st candidate callback rate, upper bound.
#'  \item\bold{inf_p_cand2:} 2nd candidate callback rate, lower bound.
#'  \item\bold{p_cand2:} 2nd candidate callback rate.
#'  \item\bold{sup_p_cand2:} 2nd candidate callback rate, upper bound.
#'  \item\bold{inf_cand_dif:} p_cand1-p_cand2, lower bound.
#'  \item\bold{sup_cand_dif:} p_cand1-p_cand2, upper bound.
#'  }
#'
#' \code{student}: a data frame containing the Student confidence intervals,
#' from t.test().
#'  \itemize{
#'   \item\bold{inf_p_callback:} overall callback rate, lower bound.
#'   \item\bold{p_callback:} overall callback rate.
#'   \item\bold{sup_p_callback:} overall callback rate, upper bound.
#'   \item\bold{inf_p_cand1:} 1st candidate callback rate, lower bound.
#'   \item\bold{p_cand1:} 1st candidate callback rate.
#'   \item\bold{sup_p_cand1:} 1st candidate callback rate, upper bound.
#'   \item\bold{inf_p_cand2:} 2nd candidate callback rate, lower bound.
#'   \item\bold{p_cand2:} 2nd candidate callback rate.
#'   \item\bold{sup_p_cand2:} 2nd candidate callback rate, upper bound.
#'   \item\bold{inf_cand_dif:} p_cand1-p_cand2, lower bound.
#'   \item\bold{sup_cand_dif:} p_cand1-p_cand2, upper bound.
#'  }
#'
#' \code{t.fisher}: a data frame containing the statistics of the Fisher test.
#'  \itemize{
#'   \item\bold{p_cand_dif:} callback proportion difference between the candidates.
#'   \item\bold{p_Fisher:} the p-value of the Fisher test.
#'   \item\bold{s_Fisher:} the significance code of the Fisher test.
#'    }
#'
#' \code{t.pearson}: a data frame containing the statistics of the Pearson test.
#'  \itemize{
#'   \item\bold{p_cand_dif:} callback proportion difference between the candidates.
#'   \item\bold{Pearson:} the value of Pearson's chi-squared test statistic.
#'   \item\bold{p_Pearson:} the p-value of the Pearson test.
#'   \item\bold{s_Pearson:} the significance code of the Pearson test.
#'    }
#'
#' \code{t.student}: A data frame containing the statistics of the Student test.
#'  \itemize{
#'   \item\bold{p_cand_dif:} callback proportion difference between the candidates.
#'   \item\bold{Student:} the value of Student's test statistic.
#'   \item\bold{p_Student:} the p-value of the Student test.
#'   \item\bold{s_Student:} the significance code of the Student test.
#'    }
#'
#' @author Emmanuel Duguet
#'
#' @references
#' Clopper, C. J. & Pearson, E. S. (1934). The use of confidence or fiducial
#' limits illustrated in the case of the binomial. Biometrika, 26, 404–413.
#' doi:10.2307/2331986.
#'
#' Wilson, E.B. (1927). Probable inference, the law of succession, and
#' statistical inference. Journal of the American Statistical Association, 22,
#' 209–212. doi:10.2307/2276774.
#'
#' @examples
#' data(labour1)
#' x <- callback(data=labour1,cluster="offer",candid="hist",callback="callback")
#' str(stat_mcr(x,level=0.9))
#'
#' @importFrom stats binom.test prop.test t.test fisher.test
#'
#' @export

stat_mcr <- function(x, level = 0.95) {
  inf_p_callback <-
    inf_p_cand1 <- inf_p_cand2 <- sup_p_callback <- NULL
  sup_p_cand1 <-
    sup_p_cand2 <- dif_p_value <- Student <- Pearson <- NULL
  p_Fisher <-
    p_Student <-
    p_Pearson <- s_Fisher <- s_Student <- s_Pearson <- NULL
  if (level <= 0 | level >= 1) {
    level = 0.95
  }
  alpha <- 1 - level

  counts <- stat_count(x)$counts

  out_prop <- with(
    counts,
    data.frame(
      tests = tests,
      p_callback = callback / tests,
      p_cand1 = callback1 / tests,
      p_cand2 = callback2 / tests,
      p_cand_dif = (callback1 - callback2) / tests,
      row.names = rownames(counts)
    )
  )

  # Clopper-Pearson/Fisher exact test
  l_comp <- rownames(counts)
  n_comp <- length(l_comp)
  ct <- c0 <- c1 <- c2 <- c3 <- list()
  for (comp in l_comp) {
    c0 <-
      binom.test(counts[comp, "callback"], counts[comp, "tests"], conf.level =
                   level)["conf.int"]
    c1 <-
      binom.test(counts[comp, "callback1"], counts[comp, "tests"], conf.level =
                   level)["conf.int"]
    c2 <-
      binom.test(counts[comp, "callback2"], counts[comp, "tests"], conf.level =
                   level)["conf.int"]
    #contingency table in matrix form
    m <- matrix(
      c(counts[comp, "tests"] - counts[comp, "callback1"], counts[comp, "callback1"], counts[comp, "tests"] -
          counts[comp, "callback2"], counts[comp, "callback2"]),
      nrow = 2,
      ncol = 2,
      byrow = TRUE
    )
    c3 <- fisher.test(m)["p.value"]

    ct[[comp]] <- c(c0[[1]], c1[[1]], c2[[1]], c3[[1]])
  }

  cp <- ct[[1]]
  if (n_comp >= 2) {
    for (i in 2:n_comp) {
      cp <- rbind(cp, ct[[i]])
    }
    cp <- as.data.frame(cp)
  } else {
    cp <- as.data.frame(t(cp))
  }
  rownames(cp) <- l_comp
  colnames(cp) <- c(
    "inf_p_callback",
    "sup_p_callback",
    "inf_p_cand1",
    "sup_p_cand1",
    "inf_p_cand2",
    "sup_p_cand2",
    "p_Fisher"
  )

  cp$s_Fisher = stat_signif(cp$p_Fisher)

  # Student
  pfds <- x$pfds
  l_comp <- names(pfds)
  n_comp <- length(l_comp)
  st <- s0 <- s1 <- s2 <- s3 <- list()
  for (comp in l_comp) {
    s0 <-
      (t.test(pfds[[comp]][, "callback"], conf.level = level)["conf.int"])
    s1 <-
      (t.test(pfds[[comp]][, "callback1"], conf.level = level)["conf.int"])
    s2 <-
      (t.test(pfds[[comp]][, "callback2"], conf.level = level)["conf.int"])
    s3 <-
      (t.test(pfds[[comp]][, "calldif"], conf.level = level)["p.value"])
    s4 <-
      (t.test(pfds[[comp]][, "calldif"], conf.level = level)["statistic"])
    s5 <-
      (t.test(pfds[[comp]][, "calldif"], conf.level = level)["conf.int"])
    st[[comp]] <-
      c(s0[[1]], s1[[1]], s2[[1]], s3[[1]], s4[[1]], s5[[1]])
  }
  student <- st[[1]]
  if (n_comp >= 2) {
    for (i in 2:n_comp) {
      student <- rbind(student, st[[i]])
    }
    student <- as.data.frame(student)
  } else {
    student <- as.data.frame(t(student))
  }
  rownames(student) <- l_comp
  colnames(student) <- c(
    "inf_p_callback",
    "sup_p_callback",
    "inf_p_cand1",
    "sup_p_cand1",
    "inf_p_cand2",
    "sup_p_cand2",
    "p_Student",
    "Student",
    "inf_cand_dif",
    "sup_cand_dif"
  )
  student <- transform(
    student,
    inf_p_callback = pmax(0, inf_p_callback),
    inf_p_cand1 = pmax(0, inf_p_cand1),
    inf_p_cand2 = pmax(0, inf_p_cand2),
    sup_p_callback = pmin(1, sup_p_callback),
    sup_p_cand1 = pmin(1, sup_p_cand1),
    sup_p_cand2 = pmin(1, sup_p_cand2),
    s_Student = stat_signif(p_Student)
  )

  # Wilson/Pearson
  l_comp <- rownames(counts)
  n_comp <- length(l_comp)
  wt <- w0 <- w1 <- w2 <- w3 <- list()
  for (comp in l_comp) {
    w0 <-
      prop.test(counts[comp, "callback"], counts[comp, "tests"], conf.level =
                  level)["conf.int"]
    w1 <-
      prop.test(counts[comp, "callback1"], counts[comp, "tests"], conf.level =
                  level)["conf.int"]
    w2 <-
      prop.test(counts[comp, "callback2"], counts[comp, "tests"], conf.level =
                  level)["conf.int"]
    w3 <-
      prop.test(c(counts[comp, "callback1"], counts[comp, "callback2"]), c(counts[comp, "tests"], counts[comp, "tests"]))["p.value"]
    w4 <-
      prop.test(c(counts[comp, "callback1"], counts[comp, "callback2"]), c(counts[comp, "tests"], counts[comp, "tests"]))["statistic"]
    w5 <-
      prop.test(c(counts[comp, "callback1"], counts[comp, "callback2"]), c(counts[comp, "tests"], counts[comp, "tests"]))["conf.int"]

    wt[[comp]] <-
      c(w0[[1]], w1[[1]], w2[[1]], w3[[1]], w4[[1]], w5[[1]])
  }

  wilson <- wt[[1]]
  if (n_comp >= 2) {
    for (i in 2:n_comp) {
      wilson <- rbind(wilson, wt[[i]])
    }
    wilson <- as.data.frame(wilson)
  } else {
    wilson <- as.data.frame(t(wilson))
  }
  rownames(wilson) <- l_comp
  colnames(wilson) <- c(
    "inf_p_callback",
    "sup_p_callback",
    "inf_p_cand1",
    "sup_p_cand1",
    "inf_p_cand2",
    "sup_p_cand2",
    "p_Pearson",
    "Pearson",
    "inf_cand_dif",
    "sup_cand_dif"
  )
  wilson$s_Pearson = stat_signif(wilson$p_Pearson)

  #comparisons
  comp <- data.frame(p_cand_dif = out_prop$p_cand_dif,
                     row.names = rownames(out_prop))

  comp <-
    cbind(comp, cp[, 7:8], wilson[, c(8, 7, 11)], student[, c(8, 7, 11)])

  z <- list(
    level = level,
    props = out_prop,
    cp = cp[, 1:6],
    wilson = wilson[, c(1:6, 9:10)],
    student = student[, c(1:6, 9:10)],
    t.fisher = comp[, 1:3],
    t.pearson = comp[, c(1, 4:6)],
    t.student = comp[, c(1, 7:9)]
  )
  class(z) <- "stat_mcr"
  return (z)
}

#'
#' Total callback shares
#'
#' @description
#' Computes the callback shares and their confidence intervals. The analysis is
#'  restricted to the tests with at least one callback. It is the definition
#'  used in Riach and Rich (2006).
#'
#' @param x a \code{callback} object.
#'
#' @param level a number, containing the level of the confidence intervals (0.95
#' by default).
#'
#' @return
#' A list with class \code{"stat_tcs"} containing 8 components : level, props,
#' cp, wilson, student, t.student, t.pearson and t.fisher.
#'
#' \code{level}: the level of the confidence intervals.
#'
#' \code{props}: a data frame containing the following variables.
#' \itemize{
#' \item\bold{ncall:} number of callbacks.
#' \item\bold{c10:} number of tests with the 1st candidate preferred (2nd candidate
#'  discriminated against).
#' \item\bold{c01:} number of tests with the 2nd candidate preferred (1st candidate
#'  discriminated against).
#' \item\bold{c11:} number of tests with both candidates called back.
#' \item\bold{p_cand1:} 1st candidate total callback share (c10/ncall).
#' \item\bold{p_cand2:} 2nd candidate total callback share (c01/ncall).
#' \item\bold{p_equal:} equal treatment callback share (c11/ncall).
#' \item\bold{p_cand_dif:} p_cand1-p_cand2.
#' }
#'
#' \code{cp}: a data frame containing the Clopper-Pearson confidence intervals,
#' from binom.test(), and the p-value of the Fisher test of independence between
#' the candidate type and the callback variable, from fisher.test().
#' \itemize{
#' \item\bold{inf_p_cand1:} 1st candidate total callback share, lower bound.
#' \item\bold{sup_p_cand1:} 1st candidate total callback share, upper bound.
#' \item\bold{inf_p_cand2:} 2nd candidate total callback share, lower bound.
#' \item\bold{sup_p_cand2:} 2nd candidate total callback share, upper bound.
#' \item\bold{inf_p_equal:} equal treatment total callback rate, lower bound.
#' \item\bold{sup_p_equal:} equal treatment total callback rate, upper bound.
#' }
#'
#' \code{wilson}: a data frame containing the Wilson confidence intervals
#'  and the p-value of the equality test of callback shares between the two
#'  candidates, from prop.test().
#' \itemize{
#' \item\bold{inf_p_cand1:} 1st candidate total callback share, lower bound.
#' \item\bold{sup_p_cand1:} 1st candidate total callback share, upper bound.
#' \item\bold{inf_p_cand2:} 2nd candidate total callback share, lower bound.
#' \item\bold{sup_p_cand2:} 2nd candidate total callback share, upper bound.
#' \item\bold{inf_p_equal:} equal treatment total callback rate, lower bound.
#' \item\bold{sup_p_equal:} equal treatment total callback rate, upper bound.
#' \item\bold{inf_cand_dif:} p_c10-p_c01, lower bound.
#' \item\bold{sup_cand_dif:} p_c10-p_c01, upper bound.
#' }
#'
#' \code{student}: a data frame containing the Student confidence intervals
#'  and the p-value of the equality test of callback shares between the two
#'  candidates.
#' \itemize{
#' \item\bold{inf_p_cand1:} 1st candidate total callback share, lower bound.
#' \item\bold{sup_p_cand1:} 1st candidate total callback share, upper bound.
#' \item\bold{inf_p_cand2:} 2nd candidate total callback share, lower bound.
#' \item\bold{sup_p_cand2:} 2nd candidate total callback share, upper bound.
#' \item\bold{inf_p_equal:} equal treatment total callback rate, lower bound.
#' \item\bold{sup_p_equal:} equal treatment total callback rate, upper bound.
#' \item\bold{inf_cand_dif:} p_c10-p_c01, lower bound.
#' \item\bold{sup_cand_dif:} p_c10-p_c01, upper bound.
#' }
#'
#' \code{t.fisher}: a data frame containing the statistics of the Fisher test.
#'  \itemize{
#'   \item\bold{p_cand_dif:} p_cand1-p_cand2.
#'   \item\bold{p_Fisher:} the p-value of the Fisher test.
#'   \item\bold{s_Fisher:} the significance code of the Fisher test.
#'    }
#'
#' \code{t.pearson}: a data frame containing the statistics of the Pearson test.
#'  \itemize{
#'   \item\bold{p_cand_dif:} p_cand1-p_cand2.
#'   \item\bold{Pearson:} the value of Pearson's chi-squared test statistic.
#'   \item\bold{p_Pearson:} the p-value of the Pearson test.
#'   \item\bold{s_Pearson:} the significance code of the Pearson test.
#'    }
#'
#' \code{t.student}: A data frame containing the statistics of the Student test.
#'  \itemize{
#'   \item\bold{p_cand_dif:} p_cand1-p_cand2.
#'   \item\bold{Student:} the value of Student's test statistic.
#'   \item\bold{p_Student:} the p-value of the Student test.
#'   \item\bold{s_Student:} the significance code of the Student test.
#'    }
#'
#'
#' @author Emmanuel Duguet
#'
#' @references
#' Clopper, C. J. & Pearson, E. S. (1934). The use of confidence or fiducial
#' limits illustrated in the case of the binomial. Biometrika, 26, 404–413.
#' doi:10.2307/2331986.
#'
#' Riach, P. A., & Rich, J. (2006). An experimental investigation of sexual
#' discrimination in hiring in the English labor market. The BE Journal of
#' Economic Analysis & Policy, 6(2),
#'
#' Wilson, E.B. (1927). Probable inference, the law of succession, and
#' statistical inference. Journal of the American Statistical Association, 22,
#' 209–212. doi:10.2307/2276774.
#'
#' @examples
#' data(labour1)
#' x <- callback(data=labour1,cluster="offer",candid="hist",callback="callback")
#' str(stat_ecs(x,level=0.9))
#'
#' @importFrom stats binom.test prop.test fisher.test
#'
#' @export

stat_tcs <- function(x, level = 0.95) {
  c10 <-
    c11 <-
    c01 <- ncall <- p_cand1 <- inf_p_cand1 <- inf_p_cand2 <- NULL
  p_cand2 <-
    sup_p_cand1 <-
    sup_p_cand2 <- p_cand_dif <- Student <- Pearson <- NULL
  p_Fisher <-
    p_Student <-
    p_Pearson <- s_Fisher <- s_Student <- s_Pearson <- NULL
  inf_p_equal <- sup_p_equal <- inf_cand_dif <- sup_cand_dif <- NULL

  if (level <= 0 | level >= 1) {
    level = 0.95
  }
  alpha <- 1 - level

  counts <- stat_count(x)$counts

  props <- with(
    counts,
    data.frame(
      tests = tests,
      ncall = c11 + c10 + c01,
      c10 = c10,
      c01 = c01,
      c11 = c11,
      row.names = rownames(counts)
    )
  )

  props <- transform(
    props,
    p_cand1 = c10 / pmax(ncall, 1),
    p_cand2 = c01 / pmax(ncall, 1),
    p_equal = c11 / pmax(ncall, 1)
  )

  props <-
    transform(props, p_cand_dif = ifelse(ncall == 0, 0, p_cand1 - p_cand2))

  # Clopper-Pearson/Fisher exact test
  l_comp <- rownames(props)
  n_comp <- length(l_comp)
  ct <- c1 <- c2 <- c3 <- c4 <- list()
  for (comp in l_comp) {
    z <- subset(props, ncall > 0)
    c1 <- binom.test(z[comp, "c10"], z[comp, "ncall"], conf.level =
                       level)["conf.int"]
    c2 <- binom.test(z[comp, "c01"], z[comp, "ncall"], conf.level =
                       level)["conf.int"]
    c3 <- binom.test(z[comp, "c11"], z[comp, "ncall"], conf.level =
                       level)["conf.int"]
    #contingency table in matrix form
    m <- matrix(
      c(z[comp, "ncall"] - z[comp, "c10"], z[comp, "c10"], z[comp, "ncall"] - z[comp, "c01"], z[comp, "c01"]),
      nrow = 2,
      ncol = 2,
      byrow = TRUE
    )
    c4 <- fisher.test(m)["p.value"]

    ct[[comp]] <- c(c1[[1]], c2[[1]], c3[[1]], c4[[1]])
  }

  cp <- ct[[1]]
  if (n_comp >= 2) {
    for (i in 2:n_comp) {
      cp <- rbind(cp, ct[[i]])
    }
    cp <- as.data.frame(cp)
  } else {
    cp <- as.data.frame(t(cp))
  }
  rownames(cp) <- l_comp
  colnames(cp) <- c(
    "inf_p_cand1",
    "sup_p_cand1",
    "inf_p_cand2",
    "sup_p_cand2",
    "inf_p_equal",
    "sup_p_equal",
    "p_Fisher"
  )
  cp$s_Fisher = stat_signif(cp$p_Fisher)


  # Student
  pfds <- x$pfds
  l_comp <- names(pfds)
  n_comp <- length(l_comp)
  st <- s1 <- s2 <- s3 <- s4 <- list()
  for (comp in l_comp) {
    z <- subset(pfds[[comp]], c11 + c10 + c01 > 0)
    s1 <- t.test(z[, "c10"], conf.level = level)["conf.int"]
    s2 <- t.test(z[, "c01"], conf.level = level)["conf.int"]
    s3 <- t.test(z[, "c11"], conf.level = level)["conf.int"]
    s4 <-
      t.test(z[, "c10"] - z[, "c01"], conf.level = level)["p.value"]
    s5 <-
      t.test(z[, "c10"] - z[, "c01"], conf.level = level)["statistic"]
    s6 <-
      t.test(z[, "c10"] - z[, "c01"], conf.level = level)["conf.int"]
    st[[comp]] <-
      c(s1[[1]], s2[[1]], s3[[1]], s4[[1]], s5[[1]], s6[[1]])
  }
  student <- st[[1]]
  if (n_comp >= 2) {
    for (i in 2:n_comp) {
      student <- rbind(student, st[[i]])
    }
  }
  rownames(student) <- l_comp
  colnames(student) <- c(
    "inf_p_cand1",
    "sup_p_cand1",
    "inf_p_cand2",
    "sup_p_cand2",
    "inf_p_equal",
    "sup_p_equal",
    "p_Student",
    "Student",
    "inf_cand_dif",
    "sup_cand_dif"
  )
  student <- transform(
    student,
    inf_p_cand1 = pmax(0, inf_p_cand1),
    inf_p_cand2 = pmax(0, inf_p_cand2),
    inf_p_equal = pmax(0, inf_p_equal),
    sup_p_cand1 = pmin(1, sup_p_cand1),
    sup_p_cand2 = pmin(1, sup_p_cand2),
    sup_p_equal = pmin(1, sup_p_equal),
    s_Student = stat_signif(p_Student)
  )

  # Wilson
  l_comp <- rownames(props)
  n_comp <- length(l_comp)
  wt <- w1 <- w2 <- w3 <- w4 <- w5 <- list()
  for (comp in l_comp) {
    z <- subset(props, ncall > 0)
    w1 <- prop.test(z[comp, "c10"], z[comp, "ncall"], conf.level =
                      level)["conf.int"]
    w2 <- prop.test(z[comp, "c01"], z[comp, "ncall"], conf.level =
                      level)["conf.int"]
    w3 <- prop.test(z[comp, "c11"], z[comp, "ncall"], conf.level =
                      level)["conf.int"]
    w4 <-
      prop.test(c(z[comp, "c10"], z[comp, "c01"], z[comp, "c11"]), c(z[comp, "ncall"], z[comp, "ncall"], z[comp, "ncall"]))["p.value"]
    w5 <-
      prop.test(c(z[comp, "c10"], z[comp, "c01"], z[comp, "c11"]), c(z[comp, "ncall"], z[comp, "ncall"], z[comp, "ncall"]))["statistic"]
    w6 <-
      prop.test(c(z[comp, "c10"], z[comp, "c01"]), c(z[comp, "ncall"], z[comp, "ncall"]))["conf.int"]

    wt[[comp]] <-
      c(w1[[1]], w2[[1]], w3[[1]], w4[[1]], w5[[1]], w6[[1]])
  }

  wilson <- wt[[1]]
  if (n_comp >= 2) {
    for (i in 2:n_comp) {
      wilson <- rbind(wilson, wt[[i]])
    }
    wilson <- as.data.frame(wilson)
  } else {
    wilson <- as.data.frame(t(wilson))
  }
  rownames(wilson) <- l_comp
  colnames(wilson) <- c(
    "inf_p_cand1",
    "sup_p_cand1",
    "inf_p_cand2",
    "sup_p_cand2",
    "inf_p_equal",
    "sup_p_equal",
    "p_Pearson",
    "Pearson",
    "inf_cand_dif",
    "sup_cand_dif"
  )
  wilson$s_Pearson = stat_signif(wilson$p_Pearson)


  #comparisons
  comp <- data.frame(p_cand_dif = props$p_cand_dif,
                     row.names = rownames(props))

  comp <-
    cbind(comp, cp[, 7:8], wilson[, c(7, 8, 11)], student[, c(7, 8, 11)])

  m <- list(
    level = level,
    props = props,
    cp = cp[, 1:6],
    wilson = wilson[, c(1:6, 9, 10)],
    student = student[, c(1:6, 9, 10)],
    t.fisher = comp[, 1:3],
    t.pearson = comp[, c(1, 5, 4, 6)],
    t.student = comp[, c(1, 8, 7, 9)]
  )
  class(m) <- "stat_tcs"
  return (m)
}
