\name{waic}
\alias{waic}
\alias{waic,matrix-method}
\alias{waic-method}
\title{Widely Applicable Information Criterion (WAIC)}
\arguments{
  \item{x}{\code{matrix} Containing log likelihood values.
  Each row is an observation. Each column is an iteration.}

  \item{method}{Method to use when calculing the effective
  number of parameters.}
}
\value{
  Object of class \code{\linkS4class{WAIC}}.
}
\description{
  Widely Applicable Information Criterion (WAIC)
}

