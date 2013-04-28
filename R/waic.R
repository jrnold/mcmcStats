#' @include mcmcstats-package.R
#' @include infocriterion-class.R
NULL

#' @rdname waic-methods
#' @aliases waic-method
#' @title Widely Applicable Information Criterion (WAIC)
#'
#' @param x \code{matrix} Containing log likelihood values. Each row is an observation.
#' Each column is an iteration.
#' @param method Method to use when calculing the effective number of parameters.
#' @return Object of class \code{\linkS4class{WAIC}}.
setGeneric("waic",
           function(x, ...) {
             standardGeneric("waic")
           })

waic.matrix <- function(x, method=2) {
  if (length(method) > 1) {
    method <- method[1]
    warning("'method' has length > 1 and only the first element will be used")
  }
  if (! method %in% 1:2) {
    stop("option 'method' must equal 1 or 2")
  }
  # \sum_{i=1}^n \log (\frac{1}{S} \sum_{i=1}^S p(y_i | theta^S))
  lppd <- sum(aaply(x, 1, function(y) mean(exp(y))))
  if (method == 2) {
    p <- sum(aaply(x, 1, var))
  } else {
    p <- 2 * sum(aaply(x, 1, function(y) log(mean(exp(y))) - mean(y)))
  }
  list(waic = -2 * (lppd - p), loglik = lppd, p = p, n = nrow(x))
}

#' @rdname waic-methods
#' @aliases waic,matrix-method
setMethod("waic", "matrix", waic.matrix)
