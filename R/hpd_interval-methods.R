#' @docType class
#' @title HPD object
#'
#' @description Object containing highest posterior density intervals.
#'
#' @section Slots:
#'
#' \describe{
#'   \item{\code{.Data}}{\code{"matrix"} object. The matrix has two columns, with the lower and upper
#'   values of the HPD. Each row corresponds to a sample. In some cases the rows will be named.}
#'   \item{\code{probability}}{\code{"numeric"} object. The probability within the interval}
#' }
#' 
#' @section Extends:
#'
#' \code{matrix}, directly.
#'
#' @seealso \code{\link{hpd_interval}} which 
setClass("HPD", contains = "matrix",
         representation = representation(probability = "numeric"))


#' @rdname hpd_interval-methods
#' @docType methods 
#' @title Highest Posterior Density Interval
#'
#' @description Calculate Highest Posterior Density (HPD) intervals for samples.
#'
#' @param object The object containing the samples.
#' @param prob A numeric scalar in the interval (0,1) giving the target probability content of the intervals. The nominal probability content of the intervals is the multiple of 1/nrow(obj) nearest to prob.
#' @param ... Optional additional arguments for methods.
#' @return An object of class \code{\linkS4class{HPD}} (extends matrix).
#'
#' @section Details:
#'
#' For each parameter the interval is constructed from the empirical cdf of the sample as the shortest interval for which the difference in the ecdf values of the endpoints is the nominal probability. Assuming that the distribution is not severely multimodal, this is the HPD interval.
#' 
#' @section Notes:
#'
#' This function was adapted from the function \code{\link[coda]{HPDinterval}} in the \pkg{coda} package.
#'
#' @author Douglas Bates, Jeffrey Arnold
setGeneric("hpd_interval",
           function(object, ...) {
             standardGeneric("hpd_interval")
           })
           
hpd_interval.numeric <- function (object, prob = 0.95, ...) {
  if (length(prob) > 1) {
    prob <- prob[1]
    warning("'prob' has length > 1 and only the first element will be used")
  }
  vals <- sort(object)
  nsamp <- length(vals)
  gap <- max(1, min(nsamp - 1, round(nsamp * prob)))
  init <- 1:(nsamp - gap)
  inds <- which.min(vals[init + gap] - vals[init])
  ans <- matrix(vals[c(inds, inds + gap)], nrow=1)
  colnames(ans) <- c("lower", "upper")
  attr(ans, "Probability") <- gap/nsamp
  ans
}

setMethod("hpd_interval", "numeric", hpd_interval.numeric)

hpd_interval.matrix <- function (object, prob = 0.95, ...) {
  ans <- apply(object, 2, hpd_interval.numeric)
  colnames(ans) <- c("lower", "upper")
  rownames(ans) <- rownames(obj)
  attr(ans, "Probability") <- gap/nsamp
  ans
}

setMethod("hpd_interval", "matrix", hpd_interval.matrix)
