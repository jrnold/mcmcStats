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
\section{Notes}{
  The WAIC is constructed as \deqn{WAIC = -2 * (lppd -
  p_{WAIC})}

  The lppd is the log pointwise predictive density, defined
  as \deqn{lppd = \sum_{i=1}^n \log \left(\frac{1}{S}
  \sum_{s=1}^S p(y_i | \theta^s)\right)}

  The value of \eqn{p_WAIC} can be calculated in two ways,
  the method used is determined by the \code{method}
  argument.

  Method 1 is defined as, \deqn{p_{WAIC1} = 2
  \sum_{i=1}^{n} (\log (\frac{1}{S} \sum_{s=1}^{S} p(y_i \
  \theta^s)) - \frac{1}{S} \sum_{s = 1}^{S} \log p(y_i |
  \theta^s))} Method 2 is defined as, \deqn{p_{WAIC2} = 2
  \sum_{i=1}^{n} V_{s=1}^{S} (\log p(y_i | \theta^s))}
  where \eqn{V_{s=1}^{S}} is the sample variance.
}
\references{
  Gelman, Andrew and Jessica Hwang and Aki Vehtari (2013),
  "Understanding Predictive Information Criteria for
  Bayesian Models,"
  \url{http://www.stat.columbia.edu/~gelman/research/unpublished/waic_understand_final.pdf}.

  Watanabe, S. (2010). "Asymptotic Equivalence of Bayes
  Cross Validation and Widely Applicable Information
  Criterion in Singular Learning Theory", Journal of
  Machine Learning Research,
  \url{http://www.jmlr.org/papers/v11/watanabe10a.html}.
}

