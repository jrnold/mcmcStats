#' @include mcmcstats-package.R
NULL

#' Predictive Concordance
#'
#' @param y Response variable
#' @param yrep Draws from the posterior predictive distribution of
#' \code{y}.
#' @param qmin Minimum quantile of the range.
#' @param qmax Maximum quantile of the range.
#' 
#' @export
predconcord <- function(y, yrep, qmin=0.025, qmax=0.975) {
    yrange <- apply(yrep, 2, quantile, probs=c(qmin, qmax))
    sum(between(y, yrange[1, ], yrange[2, ])) / length(y)
}

#' L-criterion
#'
#' @param y Response variable
#' @param yrep Draws from the posterior predictive distribution of
#' \code{y}.
#' 
#' @export
lcriterion <- function(y, yrep) {
    yrep_var <- apply(yrep, 2, var)
    yrep_mean <- apply(yrep, 2, mean)
    Li <- sqrt(yrep_var + (y - yrep_mean)^2)
    list(L = sum(Li), SL = sd(Li))
}

#' Posterior Chi squared (method 1)
#'
#' @param y Response variable
#' @param yrep Draws from the posterior predictive distribution of
#' \code{y}.
#' 
#' @export
posterior_chisq_1 <- function(y, yrep) {
    yrep_var <- apply(yrep, 2, var)
    yrep_mean <- apply(yrep, 2, mean)
    chisq <- (y - yrep_mean)^2 / yrep_var
    list(individual = chisq,
         total = sum(chisq))
}


#' Posterior p-value for Chi Squared discrepancy (method 2)
#'
#' @param y Data
#' @param yrep Draws from the posterior predictive distribution of
#' \code{y}.
#' @param yhat Estimated value of \code{y}
#' 
#' @export
posterior_chisq_2 <- function(y, yrep, yhat) {
    chisq_obs <- apply(t(apply(yhat, 1, `-`, y=y)^2) / yhat, 1, sum)
    chisq_rep <- apply((yhat - yrep)^2 / yhat, 1, sum)
    p <- sum(chisq_rep > chisq_obs) / length(chisq_obs)
    list(obs = chisq_obs, rep = chisq_rep, pval = p)
}

#' Posterior p-value for Freeman-Tukey discrepancy
#'
#' @param y Data
#' @param yrep Draws from the posterior predictive distribution of
#' \code{y}.
#' @param yhat Estimated value of \code{y}
#' 
#' @export
posterior_freeman_tukey <- function(y, yrep, yhat) {
    d_obs <- apply(t(apply(sqrt(yhat), 1, `-`, y=sqrt(y))^2), 1, sum)
    d_rep <- apply((sqrt(yhat) - sqrt(yrep))^2, 1, sum)
    p <- sum(d_rep > d_obs) / length(d_obs)
    list(obs = d_obs, rep = d_rep, pval = p)
}

