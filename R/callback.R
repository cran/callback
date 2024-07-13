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
#' A variable name, identifying the test (e.g., a job offer number).
#'
#' @param candid
#' A list of factor names defining the candidates (e.g., gender, origin).
#'
#' @param callback
#' A Boolean variable, equal to TRUE for non negative callbacks.
#'
#' @param comp
#' An option, equal to "all" or "ref" (the default). "ref" give the comparisons
#' with the reference candidate, and "all" the pairwise comparisons.
#'
#' @return
#' A 'callback' object containing the formatted data set (\code{fds}), the
#' averaged formatted data set (\code{afds}, for multiple tests), the list of the
#' paired formatted data sets (\code{pfds}), the offer-level formatted data set
#' (\code{ofds}), the names of the candidate variables (\code{candid}) and the
#' name of the callback variable (\code{callback}).
#'
#' \code{fds} contains the following variables:
#' \itemize{
#' \item \bold{cluster:} the cluster variable.
#' \item \bold{candid:} the concatenation of the candidate variables.
#' \item \bold{callback:} the callback variable.
#' }
#'
#' \code{afds} contains the following variables:
#' \itemize{
#' \item \bold{cluster:} the cluster variable.
#' \item \bold{candid:} the concatenation of the candidate variables.
#' \item \bold{callback:} the aggregated callback dummy.
#' }
#'
#' \code{pfds} data frames containing the following variables:
#' \itemize{
#' \item \bold{callback1:} TRUE if candidate 1 had a callback.
#' \item \bold{callback2:} TRUE if candidate 2 had a callback.
#' \item \bold{c00:} TRUE if neither candidate was called back.
#' \item \bold{c10:} TRUE if candidate 1 was the only one called back.
#' \item \bold{c01:} TRUE if candidate 2 was the only one called back.
#' \item \bold{c11:} TRUE if both candidates were called back.
#' \item \bold{callback:} TRUE if either candidate was called back.
#' \item \bold{calldif:} callback difference.
#' }
#'
#' \code{ofds} contains variables corresponding to the candidates' codes and the
#' following variables:
#' \itemize{
#' \item \bold{cluster:} the cluster variable.
#' \item \bold{ncall:} the number of callback for the current offer.
#' }
#'
#' @author Emmanuel Duguet
#'
#' @examples
#' data(inter1)
#' m <- callback(data=subset(origin1,reput=="U"),cluster="offer",candid="origin",
#'      callback="callback",comp = "ref")
#' str(m)
#' @export

callback <-
  function(data, cluster, candid, callback, comp = "ref") {
    c00 <- c01 <- c10 <- c11 <- NULL
    # no missing callback or candidate variables
    m <-
      subset(data[, c(cluster, candid, callback)],!is.na(callback))
    for (x in candid) {
      m <- subset(m,!is.na(x))
    }

    # concatenation of candidate variables
    if (length(candid) > 1) {
      m[, "candid"] <-
        interaction(m[, candid], drop = TRUE, lex.order = TRUE)
    } else {
      m[, "candid"] <- m[, candid, drop = FALSE]
    }

    # formatted data set
    m <- m[, c(cluster, "candid", callback)]
    colnames(m)[c(1, 3)] <- c("cluster", "callback")

    # averaged formatted data set
    am1 <- aggregate(data = m, callback ~ candid + cluster, mean)
    am2 <- aggregate(data = m, callback ~ candid + cluster, length)
    am <- merge(am1, am2, by = c("candid", "cluster"))
    colnames(am)[3:4] <- c("callback", "n")
    am <- am[, c("cluster", "candid", "callback", "n")]


    # paired formatted data sets (with max rule)
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
          #allows for multiple identical applicants:best answer kept
          m1 <- subset(m, candid == cand1)
          m1 <- aggregate(data = m1, callback ~ cluster, max)
          m2 <- subset(m, candid == cand2)
          m2 <- aggregate(data = m2, callback ~ cluster, max)
          #matching
          mi <-
            merge(m1[, c("cluster", "callback")], m2[, c("cluster", "callback")], by = "cluster")
          #cat("\n mi=",nrow(mi))
          #cat(colnames(mi))
          colnames(mi) <- c("cluster", "callback1", "callback2")
          mi <- transform(
            mi,
            c00 = !callback1 & !callback2,
            c10 = callback1 & !callback2,
            c01 = !callback1 & callback2,
            c11 = callback1 & callback2,
            callback = callback1 | callback2,
            calldif = callback1 - callback2
          )
          pfds[[paste(cand1, cand2, sep = " vs ")]] <- mi
        }#i2
      }#i1

    } else {
      cand1 <- l_cand[1]
      for (i2 in 2:n_cand) {
        cand2 <- l_cand[i2]
        #allows for multiple identical applicants:best answer kept
        m1 <- subset(m, candid == cand1)
        m1 <- aggregate(data = m1, callback ~ cluster, max)
        m2 <- subset(m, candid == cand2)
        m2 <- aggregate(data = m2, callback ~ cluster, max)
        #matching
        mi <-
          merge(m1[, c("cluster", "callback")], m2[, c("cluster", "callback")], by = "cluster")
        colnames(mi) <- c("cluster", "callback1", "callback2")
        mi <- transform(
          mi,
          c00 = !callback1 & !callback2,
          c10 = callback1 & !callback2,
          c01 = !callback1 & callback2,
          c11 = callback1 & callback2,
          callback = callback1 | callback2,
          calldif = callback1 - callback2
        )
        pfds[[paste(cand1, cand2, sep = " vs ")]] <- mi
      }#i2
    }#else

    #offer level formatted data sets
    #block transposition - offers with all the candidates only
    l_candid  <- levels(m$candid)
    n_candid  <- length(l_candid)
    n_cand <- aggregate(data = m, candid ~ cluster, length)
    colnames(n_cand)[2] <- "n_cand"
    om <- merge(m, n_cand, by = "cluster")
    test <- sum(om$n_cand == n_candid)
    if (test > 0) {
      om <- subset(om, n_cand == n_candid)
      om$cluster <- droplevels(om$cluster)
      l_cluster <- levels(om$cluster)
      n_cluster <- length(l_cluster)
      x <- om[om$cluster == l_cluster[[1]], c("candid", "callback")]
      rownames(x) <- x$candid
      x$candid  <- NULL
      x <- as.data.frame(t(x))
      x$ncall <- rowSums(x)
      sx <- x
      for (i in 2:n_cluster) {
        x <- om[om$cluster == l_cluster[[i]], c("candid", "callback")]
        rownames(x) <- x$candid
        x$candid  <- NULL
        n_x <- nrow(x)
        x <- as.data.frame(t(x))
        x$ncall <- rowSums(x)
        sx <- rbind(sx, x)
      }
      rownames(sx) <- l_cluster
    }
    else {
      sx <- NULL
    }

    z <-
      list(
        candid = candid,
        callback = callback,
        fds = m,
        afds = am,
        pfds = pfds,
        ofds = sx
      )

    class(z) <- "callback"

    return(z)
  }
