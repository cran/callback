% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/callback_graph.R
\name{graph.stat_mcr}
\alias{graph.stat_mcr}
\title{Matched callback rates plots}
\usage{
\method{graph}{stat_mcr}(
  data = NULL,
  method = "wilson",
  col = c("#F564E3", "#7CAE00", "#00BFC4"),
  dif = TRUE,
  ...
)
}
\arguments{
\item{data}{a \code{stat_mcr} object.}

\item{method}{the type of confidence interval: "cp" for Clopper-Pearson,
"wilson" for Wilson (the default), "student" for Student.}

\item{col}{A list of colors (three colors needed).}

\item{dif}{TRUE for the difference in proportions (the default), FALSE for a
comparison of confidence intervals}

\item{...}{further arguments passed to or from other methods.}
}
\value{
a ggplot2 object
}
\description{
Matched callback rates plots
}
\examples{
data(origin1)
m <- callback(labour1,"offer","hist","callback","all")
s <- stat_mcr(m)
graph(data=s,method="student")

}
\author{
Emmanuel Duguet
}
