% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/funcao_plotUD.R
\name{plotUD}
\alias{plotUD}
\title{Plot Uniform Discrete Probability Distribution}
\usage{
plotUD(a = 0, b = 4, prob = FALSE, i = 2, f = 4)
}
\arguments{
\item{a}{lower limits of the distribution}

\item{b}{upper limits of the distribution}

\item{prob}{logical (default = FALSE), if TRUE highlight some values of the probability of random variable from 'i' to 'f'}

\item{i}{highlight probability from this intial value of distribution}

\item{f}{highlight probability from 'i' to this final value of distribution}
}
\description{
Plot Uniform Discrete Probability Distribution and highlight probability of some values from i to f. There are some default to values
to each parameter in function.
}
\examples{
plotUD(a = 0, b = 4, prob = TRUE, i = 2, f = 4)
}
