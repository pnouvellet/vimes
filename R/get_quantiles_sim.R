#' Get quantiles for simulations
#'
#' These functions approximate quantiles for a probability density function or
#' mass function given by a function \code{f}.
#'
#' @author SH
#'
#'
#' @aliases get_quantiles_sim
#'
#' @export
#'
#' @param f A function returning the probability density function (for
#'   \code{get_quantiles_pdf}) or the probability mass function (for
#'   \code{get_quantiles_pmf}.
#'
#' @param p A vector of probabilities.
#'
#' @param continuous A logical indicating if the distribution computed by
#'   \code{f} is continuous (i.e., a probability density function) or discrete
#'   (i.e. a probability mass function). Automatically detected if the function
#'   provided is an instance of \code{\link{fpaircase}}.
#'
#' @param precision The size of the step used to discretise the continuous
#'   distribution. Defaults to 0.01.
#'
#' @param n_steps The number steps used to discretise the continuous
#'   distribution. Defaults to 1000. Note that if \code{n_steps} intervals are
#'   not enough to attain the largest value in \code{p}, batches of
#'   \code{n_steps} are iteratively added.
#'
#' @param maxit The maximum number of batches of \code{n_steps} considered.
#'   Defaults to 1000. Avoids infinite looping when \code{p} is close to 1.
#'
#' @param ... Further arguments passed to the function \code{f}
#'
#' @examples
#'
#' ## reference: exponential of rate 1
#' qexp(c(.4, .5, .95))
#'
#' ## approximation
#' get_quantiles(f = dexp, c(.4,.5,.95), TRUE)
#'
#' ## better precision
#' get_quantiles(f = dexp, c(.4,.5,.95), precision = 0.001, TRUE)
#'
#'
#' ## example with fpaircase
#' f <- fpaircase("spatial", sd_spatial=10)
#' plot(f)
#' plot(f, xlim = c(0, 100))
#' plot(f, xlim = c(0, 100), pi = 0.4)
#'
#' q <- get_quantiles(f, c(.9, .95, .99))
#' q
#' abline(v = q)
#'
#'


## Main function to compute the quantiles

get_quantiles_sim <- function() {
 
  # out <- vapply(p, find_one_quantile, double(1))
  # 
  # return(out)
}

