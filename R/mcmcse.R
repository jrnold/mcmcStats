## create generic functions for functions in the mcmcse package

##' Monte Carlo Standard Error Methods
##' @export
setGeneric("mcse", function(x, ...) {
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


##' Monte Carlo standard error for Quantiles
##' @export
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

setMethod("mcseq", "mcmc", function(x, ...) {
    mcseq(as.matrix(x), ...)
})

setMethod("mcseq", "list", function(x, ...) {
    lapply(x, mcseq, ...)
})

##' Effective Sample Size (ESS)
##'
##' @export
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

