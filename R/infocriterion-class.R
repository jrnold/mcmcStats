#' @include mcmcstats-package.R
#' @exportClass InfoCriterion
#' @exportClass WAIC
#' @exportClass DIC
NULL

#' @rdname InfoCriterion-class
#' @aliases InfoCriterion-class
#' @title Information Criterion Objects
#'
#' @description Class for information criteria (AIC, DIC, WAIC)
#'
#' @section Slots:
#'
#' \describe{
#' \item{\code{.Data}}{Value of the information criterion}
#' \item{\code{loglik}}{Log likelihood.}
#' \item{\code{b}}{bias correction.}
#' \item{\code{n}}{number of observations.}
#' }
setClass("InfoCriterion",
         contains = "numeric",
         representation =
         representation(loglik = "numeric",
                        b = "numeric",
                        n = "integer"))

setValidity("InfoCriterion",
            function(object) {
              if (length(object@loglik) != length(object)) {
                return("length(object@loglik) != length(object)")
              }
              if (length(object@b) != length(object)) {
                return("length(object@b) != length(object)")
              }
              if (length(object@n) != length(object)) {
                return("length(object@n) != length(object)")
              }

              TRUE
            })

#' @rdname InfoCriterion-class
#' @aliases WAIC-class
setClass("WAIC", contains = "InfoCriterion")

#' @rdname InfoCriterion-class
#' @aliases DIC-class
setClass("DIC", contains = "InfoCriterion")
