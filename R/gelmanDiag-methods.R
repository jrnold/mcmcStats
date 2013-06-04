#' @include mcmcstats-package.R
#' @exportMethod rhat
NULL

## Sum of squared errors
sumsqe <- function(x)  sum((x - mean(x))^2)

#' Gelman-Rubin Criteria
#'
#' Gelman-Rubin univariate potential reduction in scale factor
#' (PRSF), generalized for chains of differing lengths.
#'
#' @param x Object with parameters
#' @param chain_id chain identifier. Used to \code{\link{split}}
#' \code{x} when \code{x} is a \code{numeric} or \code{matrix}.
#'
#' @references Stan Manual, Section 27.2.
#'
#' @aliases rhat
#' @aliases rhat-method
#' @aliases rhat,list-method
#' @aliases rhat,numeric-method
#' @aliases rhat,matrix-method
#' @keywords methods
#' @docType methods
#' @references \emph{Stan Reference Manual}, version 1.0.2.
#' @export
setGeneric("rhat", function(x, ...) standardGeneric("rhat"))

rhat_list <- function(x)  {
    m <- length(x)
    n <- sapply(x, length)
    w_mean <- sapply(x, mean)
    w_sumsqe <- sapply(x, sumsqe)
    W <- mean(w_sumsqe / (n - 1))
    b_var <- sumsqe(w_mean)
    varplus <- mean(w_sumsqe / n) + b_var
    Rhat <- max(1, sqrt(varplus / W))
}
setMethod("rhat", "list", rhat_list)


rhat_numeric <- function(x, chain_id) {
    rhat(split(x, chain_id))
}
setMethod("rhat", "numeric", rhat_numeric)

setMethod("rhat", "matrix",
          function(x, chain_id) {
              apply(x, 2, rhat, chain_id=chain_id)
          })
