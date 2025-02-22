% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/callback_graph.R
\name{g_tcs}
\alias{g_tcs}
\title{Total callback shares plot}
\usage{
g_tcs(data = NULL, col = c("gray75", "#F564E3", "#7CAE00"), ...)
}
\arguments{
\item{data}{a \code{stat_tcs} object.}

\item{col}{A list of colors (three colors needed).}

\item{...}{further arguments passed to or from other methods.}
}
\value{
A ggplot2 object
}
\description{
Total callback shares plot
}
\examples{
data(origin1)
m <- callback(labour1,"offer","hist","callback","all")
s <- stat_tcs(m)
g_tcs(s)

}
\author{
Emmanuel Duguet
}
