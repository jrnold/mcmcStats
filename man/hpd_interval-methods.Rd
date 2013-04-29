\docType{methods}
\name{hpd_interval}
\alias{hpd_interval}
\title{Highest Posterior Density Interval}
\arguments{
  \item{object}{The object containing the samples.}

  \item{prob}{A numeric scalar in the interval (0,1) giving
  the target probability content of the intervals. The
  nominal probability content of the intervals is the
  multiple of 1/nrow(obj) nearest to prob.}

  \item{...}{Optional additional arguments for methods.}
}
\value{
  An object of class \code{\linkS4class{HPD}} (extends
  matrix).
}
\description{
  Calculate Highest Posterior Density (HPD) intervals for
  samples.
}
\section{Details}{
  For each parameter the interval is constructed from the
  empirical cdf of the sample as the shortest interval for
  which the difference in the ecdf values of the endpoints
  is the nominal probability. Assuming that the
  distribution is not severely multimodal, this is the HPD
  interval.
}

\section{Notes}{
  This function was adapted from the function
  \code{\link[coda]{HPDinterval}} in the \pkg{coda}
  package.
}
\author{
  Douglas Bates, Jeffrey Arnold
}

