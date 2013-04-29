#' @include mcmcstats-package.R
#' @exportMethod mcse
NULL

#' Monte Carlo Standard Error Methods
setGeneric("mcse", function(x, ...) {
  standardGeneric("mcse")
})

setMethod("mcse", "numeric",
          function(x, ...) {
            ret <- mcmcse::mcse(x, ...)
            ifelse(is.na(ret), c(est=NA, se=NA), unlist(ret))
          })

setMethod("mcse", "matrix", function(x, ...) {
    t(apply(x, 2, mcse, ...))
})

setMethod("mcse", "array",
          function(x, MARGIN=seq_along(dim(x))[-1], ...)
      {
          apply(x, MARGIN, mcse_q)
      })

setMethod("mcse", "list", function(x, ...) {
    lapply(x, mcse, ...)
})
