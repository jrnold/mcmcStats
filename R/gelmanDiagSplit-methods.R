#' @include mcmcstats-package.R
#' @exportMethod gelmanDiagSplit
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
#' @aliases gelmanDiagSplit
#' @aliases gelmanDiagSplit-method
#' @aliases gelmanDiagSplit,numeric-method
#' @aliases gelmanDiagSplit,matrix-method
#' @docType methods
#' @keywords methods
setGeneric("gelmanDiagSplit",
           function(x, ...) {
               standardGeneric("gelmanDiagSplit")
           })

setMethod("gelmanDiagSplit", "numeric",
          function(x, frac1=0.5, frac2=0.5) {
              n <- length(x)
              i1 <- 1:floor(frac1 * n)
              i2 <- ceiling((1 - frac2) * n):n
              gelmanDiag(list(x[i1], x[i2]))
          })

setMethod("gelmanDiagSplit", "matrix",
          function(x, ...) {
              apply(x, 2, gelmanDiagSplit, ...)
          })

