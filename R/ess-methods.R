#' @include mcmcstats-package.R
#' @exportMethod ess
NULL

#' Effective Sample Size (ESS)
setGeneric("ess", function(x, ...) {
    mcmcse::ess(x, ...)
})

setMethod("ess", "matrix", function(x, ...) {
    t(apply(x, 2, ess, ...))
})

setMethod("ess", "array",
          function(x, MARGIN=seq_along(dim(x))[-1], ...)
      {
          apply(x, MARGIN, ess, ...)
      })

setMethod("ess", "list", function(x, ...) {
    lapply(x, ess, ...)
})

