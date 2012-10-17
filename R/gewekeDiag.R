geweke_diag_default <- function(x, frac1 = 0.1, frac2 = 0.5) {
    if (frac1 < 0 || frac1 > 1) {
        stop("frac1 invalid")
    }
    if (frac2 < 0 || frac2 > 1) {
        stop("frac2 invalid")
    }
    if (frac1 + frac2 > 1) {
        stop("start and end sequences are overlapping")
    }
    n <- length(x)
    xstart <- c(1, ceiling((1 - frac2) * n))
    xend <- c(floor(frac1 * n), n)
    y <- mapply(function(start, end) {
        y <- x[(start:end)]
        list(mean=mean(y),
             variance=spectrum0(y)$spec/length(y))
    }, xstart, xend, SIMPLIFY=FALSE)
    z <- (y[[1]]$mean - y[[2]]$mean) / sqrt(y[[1]]$variance + y[[2]]$variance)
    out <- structure(z, frac = c(frac1, frac2))
    return(out)
}


##' Geweke Diagnostic
##'
##' @aliases gewekeDiag-methods
##' @aliases gewekeDiag,numeric-method
##' @aliases gewekeDiag,matrix-method
##' @aliases gewekeDiag,list-method
##' @export
setGeneric("gewekeDiag", function(x, ...) standardGeneric("gewekeDiag"))

setMethod("gewekeDiag", "numeric", geweke_diag_default)

setMethod("gewekeDiag", "matrix",
          function(x, ...) apply(x, 2, gewekeDiag, ...))

setMethod("gewekeDiag", "list",
          function(x, ...) lapply(x, gewekeDiag, ...))
          
