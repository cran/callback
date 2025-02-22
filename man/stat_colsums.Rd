% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/callback_misc.R
\name{stat_colsums}
\alias{stat_colsums}
\title{Sums the numeric or logical columns in a data frame.}
\usage{
stat_colsums(x)
}
\arguments{
\item{x}{A data frame}
}
\value{
A data frame with the column sums, under their original names.
}
\description{
Sums the numeric or logical columns in a data frame.
}
\examples{
data(labour1)
s <- callback(labour1,"offer","hist","callback","all")
stat_colsums(s$pfds[["LTC vs STC"]])

}
