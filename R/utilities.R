#' @include mcmcstats-package.R
NULL

between <- function(x, x.min=-Inf, x.max=Inf,
                    include.left = TRUE, include.right = TRUE) {
    if (include.left) {
        left_fun = `>=`
    } else {
        left_fun = `>`
    }
    if (include.right) {
        right_fun = `<=`
    } else {
        right_fn = `<`
    }
    left_fun(x, x.min) & right_fun(x, x.max)
}
