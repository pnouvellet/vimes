library(vimes)

# Vimes uses it's own system to get the quantile. 

get_quantiles_pdf <- function(f, p, precision = 0.01, n_steps = 1000, maxit = 1000, ...) {
  p <- check_proba(p)
  
  ## we approximate the empirical cdf by stepsize precision, over first n_steps
  ## steps
  x <- seq(from = 0, by = precision, length = n_steps)
  x <- seq(from = 0, to = 130, by = precision)
  csm <- cumsum(f(x, pi=1)) * precision
#  dsm <- f(x, pi=1)
 # csm2 <- pgamma(q = x, scale = gam_parms$scale, shape = gam_parms$shape)
  #dsm2 <- dgamma(x = x, scale = gam_parms$scale, shape = gam_parms$shape)
  
  iter <- 0
  
  ## if we didn't go far enough to catch the quantile p (or at least one of
  ## them) we reiterate the process as long as needed, by n_steps at a time
  
  while ( (max(p) > max(csm)) && (iter < maxit) ) {
    iter <- iter + 1
    new_x <- seq(from = max(x)+precision, by = precision, length = n_steps)
    x <- c(x, new_x)
    new_cms <- cumsum(f(new_x, ...))*precision + max(csm)
    csm <- c(csm, new_cms)
  }
  
  if (iter == maxit) {
    warning("maxit reached, quantile estimation may be unreliable")
  }
  
  ## now we have the approximate cdf up to far enough, we find where the
  ## quantile(s) p lie in the recorded vector of cdf quantile defined as the
  ## smallest recorded cdf which is > p
  
  find_one_quantile <- function(e) { # e is a probability
    ## corner cases
    
    if (e == 0 ) {
      return(0.0)
    }
    
    if (e == 1) {
      return(Inf)
    }
    
    
    ## normal cases
    
    temp <- csm > e
    if (any(temp)) {
      out_idx <- min(which(temp))
      out <- x[out_idx]
      return(out)
    } else {
      return(NA_real_)
    }
  }
  
  out <- vapply(p, find_one_quantile, double(1))
  
  return(out)
}


