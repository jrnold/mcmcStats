\name{dic}
\alias{dic}
\alias{dic,numeric-method}
\alias{dic-method}
\title{Deviance Information Criterion (DIC)}
\arguments{
  \item{x}{\code{numeric} vector of deviances, -2 *
  log-likelihood.}

  \item{dhat}{\code{numeric} \eqn{-2 * \log(p(y |
  \hat{\theta}))}. The deviance evaluated at point-estimate
  (usually the mean) of the parameters of the model.}
}
\value{
  Object of class \code{\linkS4class{DIC}}.
}
\description{
  Deviance Information Criterion (DIC)
}
\references{
  Spiegelhalter DJ, Best NG, Carlin BP and Van der Linde A,
  "Bayesian Measures of Model Complexity and Fit (with
  Discussion)", Journal of the Royal Statistical Society,
  Series B, 2002 64(4):583-616.

  Gelman, Andrew and Jessica Hwang and Aki Vehtari (2013),
  "Understanding Predictive Information Criteria for
  Bayesian Models,"
  \url{http://www.stat.columbia.edu/~gelman/research/unpublished/waic_understand_final.pdf}.

  \url{http://www.mrc-bsu.cam.ac.uk/bugs/winbugs/dicpage.shtml}
}
\seealso{
  \code{\link{waic}}
}

