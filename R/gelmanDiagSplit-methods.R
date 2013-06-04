#' @include mcmcstats-package.R
#' @exportMethod rhat_split
NULL

#' Gelman-Rubin Diagnostic with a Split Single Chain
#'
#' The single chain is split in half, and then
#' \code{gelman_diag} is run on the two halves.
#' 
#' @param x Object containing the MCMC samples.
#' @param frac1 \code{numeric}. Fraction to use from beginning of chain
#' @param frac2  \code{numeric}. Fraction to use from end of chain
#'
#' @references Stan Manual, Section 27.2.
#'
#' @aliases rhat_split
#' @aliases rhat_split-method
#' @aliases rhat_split,numeric-method
#' @aliases rhat_split,matrix-method
#' @docType methods
#' @keywords methods
setGeneric("rhat_split",
           function(x, ...) {
               standardGeneric("rhat_split")
           })

setMethod("rhat_split", "numeric",
          function(x, frac1=0.5, frac2=0.5) {
              n <- length(x)
              i1 <- 1:floor(frac1 * n)
              i2 <- ceiling((1 - frac2) * n):n
              rhat(list(x[i1], x[i2]))
          })

setMethod("rhat_split", "matrix",
          function(x, ...) {
              apply(x, 2, rhat_split, ...)
          })

