#'
#' Data formatting
#'
#' @description
#' Creates the data set used in the callback package.
#'
#' @param data
#' A data frame.
#'
#' @param cluster
#' A variable name, identifying the test (e.g. a job offer number).
#'
#' @param candid
#' A list of factor names defining the candidates (e.g., gender, origin).
#'
#' @param callback
#' A Boolean variable, equal to TRUE for non negative callbacks.
#'
#' @param comp
#' An option, equal to "all" or "ref" (the default). "ref" give the comparisons with the reference candidate, and "all" the pairwise comparisons.
#'
#' @return
#' A 'callback' object containing the formatted data sets (\code{fds}), the list of the paired formatted data sets (\code{pfds}), the list of the clusters retained in the paired formatted data sets (\code{cfds}), the names of the candidate variables (\code{candid}) and the name of the callback variable (\code{callback}).
#' \code{fds} contains the following variables:
#' \tabular{ll}{
#' \code{cluster} \tab the cluster variable.\cr
#' \code{candid}\tab the concatenation of the candidate variables.\cr
#' \code{callback}\tab the callback variables.\cr
#' }
#' \code{pfds} data frames containing the following variables:
#' \tabular{ll}{
#' \code{callback1}\tab TRUE if candidate 1 had a callback.\cr
#' \code{callback2}\tab TRUE if candidate 2 had a callback.\cr
#' \code{c00}\tab TRUE if neither candidate was called back.\cr
#' \code{c10}\tab TRUE if candidate 1 was the only one called back.\cr
#' \code{c01}\tab TRUE if candidate 2 was the only one called back.\cr
#' \code{c11}\tab TRUE if both candidates were called back.\cr
#' \code{callback}\tab TRUE if either candidate was called back.\cr
#' \code{calldif}\tab callback difference (\code{callback1-callback2}).\cr
#' }
#' \code{cfds} data frames containing the \code{cluster} variable.
#'
#' @export
#'
#' @examples
#' data(inter1)
#' callback(data=inter1,cluster="offer",candid=c("gender","origin"), callback="callback",comp = "ref")


callback <- function(data,
                     cluster,
                     candid,
                     callback,
                     comp = "ref") {
  c00 <- c01 <- c10 <- c11 <- NULL
  # no missing callback or candidate variables
  m <-
    subset(data[, c(cluster, candid, callback)],!is.na(callback))
  for (x in candid) {
    m <- subset(m,!is.na(x))
  }

  # concatenation of candidate variables
  if (length(candid) > 1) {
    m[, "candid"] <- interaction(m[, candid], drop = TRUE)
  } else {
    m[, "candid"] <- m[, candid, drop = FALSE]
  }

  # formatted data set
  m <- m[, c(cluster, "candid", callback)]
  colnames(m)[c(1, 3)] <- c("cluster", "callback")

  # paired formatted data sets
  callback1 <- callback2 <- cluster <- NULL
  l_cand <- levels(m[, "candid"])
  n_cand <- length(l_cand)
  pfds <- list()
  cfds <- list()

  if (comp == "all") {
    for (i1 in 1:(n_cand - 1)) {
      for (i2 in (i1 + 1):n_cand) {
        cand1 <- l_cand[i1]
        cand2 <- l_cand[i2]
        m1 <- subset(m, candid == cand1)
        m2 <- subset(m, candid == cand2)
        #matching
        mi <-
          merge(m1[, c("cluster", "callback")], m2[, c("cluster", "callback")],
                by = "cluster")
        colnames(mi) <- c("cluster", "callback1", "callback2")
        mi1 <- mi[, "cluster", drop = FALSE]
        mi2 <- mi[, c("callback1", "callback2")]
        mi2 <- transform(
          mi2,
          c00 = !callback1 & !callback2,
          c10 = callback1 & !callback2,
          c01 = !callback1 & callback2,
          c11 = callback1 & callback2,
          callback = callback1 | callback2,
          calldif = callback1 - callback2
        )
        pfds[[paste(cand1, cand2, sep = " vs ")]] <- mi2
        cfds[[paste(cand1, cand2, sep = " vs ")]] <- mi1
      }#i2
    }#i1

  } else {
    cand1 <- l_cand[1]
    for (i2 in 2:n_cand) {
      cand2 <- l_cand[i2]
      m1 <- subset(m, candid == cand1)
      m2 <- subset(m, candid == cand2)
      #matching
      mi <-
        merge(m1[, c("cluster", "callback")], m2[, c("cluster", "callback")],
              by = "cluster")
      colnames(mi) <- c("cluster", "callback1", "callback2")
      mi1 <- mi[, "cluster", drop = FALSE]
      mi2 <- mi[, c("callback1", "callback2")]
      mi2 <- transform(
        mi2,
        c00 = !callback1 & !callback2,
        c10 = callback1 & !callback2,
        c01 = !callback1 & callback2,
        c11 = callback1 & callback2,
        callback = callback1 | callback2,
        calldif = callback1 - callback2
      )
      pfds[[paste(cand1, cand2, sep = " vs ")]] <- mi2
      cfds[[paste(cand1, cand2, sep = " vs ")]] <- mi1
    }#i2
  }#else

  z <-
    list(
      candid = candid,
      callback = callback,
      fds = m,
      pfds = pfds,
      cfds = cfds
    )

  class(z) <- "callback"

  return(z)
}
