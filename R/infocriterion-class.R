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
#'
#' @section Details:
#'
#' Information criteria of the form \eqn{-2 (L - b)}, where \eqn{L} is
#' an the log-likelihood, and \eqn{b} is a bias correction term.
#' 
#' @section Slots:
#'
#' \describe{
#' \item{\code{.Data}}{Value of the information criterion. On the deviance scale.}
#' \item{\code{loglik}}{Log likelihood.}
#' \item{\code{b}}{bias correction. Varies with the class.}
#' \item{\code{n}}{number of observations.}
#' }
setClass("InfoCriterion",
         contains = "numeric",
         representation =
         representation(loglik = "numeric",
                        b = "numeric"))

setValidity("InfoCriterion",
            function(object) {
              if (length(object@loglik) != length(object)) {
                return("length(object@loglik) != length(object)")
              }
              if (length(object@b) != length(object)) {
                return("length(object@b) != length(object)")
              }
              TRUE
            })

#' @rdname InfoCriterion-class
#' @aliases DIC-class
setClass("DIC", contains = "InfoCriterion")
