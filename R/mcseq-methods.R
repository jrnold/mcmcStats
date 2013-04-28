#' @include mcmcstats-package.R
#' @exportMethod mcseq
NULL

#' Monte Carlo standard error for Quantiles
setGeneric("mcseq", function(x, ...) {
    ret <- mcmcse::mcse.q(x, ...)
    ifelse(is.na(ret), c(est=NA, se=NA), unlist(ret))
})

setMethod("mcseq", "matrix", function(x, ...) {
    t(apply(x, 2, mcseq, ...))
})

setMethod("mcseq", "array",
          function(x, MARGIN=seq_along(dim(x))[-1], ...)
      {
          apply(x, MARGIN, mcseq)
      })

setMethod("mcseq", "list", function(x, ...) {
    lapply(x, mcseq, ...)
})
