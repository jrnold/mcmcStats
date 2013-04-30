\name{InfoCriterion}
\alias{DIC}
\alias{DIC-class}
\alias{InfoCriterion}
\alias{InfoCriterion-class}
\alias{WAIC}
\alias{WAIC-class}
\title{Information Criterion Objects}
\description{
  Class for information criteria (AIC, DIC, WAIC)
}
\section{Details}{
  Information criteria of the form \eqn{-2 (L - b)}, where
  \eqn{L} is an the log-likelihood, and \eqn{b} is a bias
  correction term.
}

\section{Slots}{
  \describe{ \item{\code{.Data}}{Value of the information
  criterion. On the deviance scale.}
  \item{\code{loglik}}{Log likelihood.}
  \item{\code{b}}{bias correction. Varies with the class.}
  \item{\code{n}}{number of observations.} }
}

