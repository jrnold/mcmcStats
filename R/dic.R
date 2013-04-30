#' @include mcmcstats-package.R
#' @include infocriterion-class.R
#' @exportClass DIC
#' @exportMethod dic
NULL

#' @rdname InfoCriterion-class
#' @aliases DIC-class
setClass("DIC", contains = "InfoCriterion")

#' @rdname dic-methods
#' @aliases dic-method
#' @title Deviance Information Criterion (DIC)
#'
#' @param x \code{numeric} vector of deviances, -2 * log-likelihood.
#' @param dhat \code{numeric} \eqn{-2 * \log(p(y | \hat{\theta}))}. The deviance
#' evaluated at point-estimate (usually the mean) of the parameters of the model.
#' @return Object of class \code{\linkS4class{DIC}}.
#'
#' @references
#'
#' Spiegelhalter DJ, Best NG, Carlin BP and Van der Linde A, "Bayesian Measures of Model Complexity and Fit (with Discussion)", Journal of the Royal Statistical Society, Series B, 2002 64(4):583-616.
#' 
#' Gelman, Andrew and Jessica Hwang and Aki Vehtari (2013), "Understanding Predictive Information Criteria for Bayesian Models," \url{http://www.stat.columbia.edu/~gelman/research/unpublished/waic_understand_final.pdf}.
#' 
#' \url{http://www.mrc-bsu.cam.ac.uk/bugs/winbugs/dicpage.shtml}
#'
#' @seealso \code{\link{waic}}
setGeneric("dic",
           function(x, ...) {
             standardGeneric("dic")
           })

dic.numeric <- function(x, dhat=NULL) {
  dbar <- mean(x)
  if (is.null(dhat)) {
    p <- var(x) / 2
  } else {
    p <- dbar - dhat
  }
  new("DIC", dbar + p, loglik = -0.5 * dbar , b = p)
}

#' @rdname dic-methods
#' @aliases dic,numeric-method
setMethod("dic", "numeric", dic.numeric)
