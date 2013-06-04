\docType{methods}
\name{rhat}
\alias{rhat}
\alias{rhat,list-method}
\alias{rhat,matrix-method}
\alias{rhat,numeric-method}
\alias{rhat-method}
\title{Gelman-Rubin Criteria}
\arguments{
  \item{x}{Object with parameters}

  \item{chain_id}{chain identifier. Used to
  \code{\link{split}} \code{x} when \code{x} is a
  \code{numeric} or \code{matrix}.}
}
\description{
  Gelman-Rubin univariate potential reduction in scale
  factor (PRSF), generalized for chains of differing
  lengths.
}
\references{
  Stan Manual, Section 27.2.

  \emph{Stan Reference Manual}, version 1.0.2.
}
\keyword{methods}

