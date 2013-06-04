\docType{methods}
\name{rhat_split}
\alias{rhat_split}
\alias{rhat_split,matrix-method}
\alias{rhat_split,numeric-method}
\alias{rhat_split-method}
\title{Gelman-Rubin Diagnostic with a Split Single Chain}
\arguments{
  \item{x}{Object containing the MCMC samples.}

  \item{frac1}{\code{numeric}. Fraction to use from
  beginning of chain}

  \item{frac2}{\code{numeric}. Fraction to use from end of
  chain}
}
\description{
  The single chain is split in half, and then
  \code{gelman_diag} is run on the two halves.
}
\references{
  Stan Manual, Section 27.2.
}
\keyword{methods}

