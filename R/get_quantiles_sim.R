
## The function get_quantiles_sim is [[a wrapper around 2 non exported functions
## which approximate quantiles for a given probability density / mass
## function. The main function is get_quantiles_pdf, on which the 'pmf'
## equivalent relies. It creates a 1D regular grid iteratively, which it uses to
## compute the cummulative density/mass, until it reaches the highest quantile
## requested. This grid has a mesh resolution of 'precision' and a size
## 'n_steps'.]]





## Main function to compute the quantiles

get_quantiles_sim <- function() {
 
  # out <- vapply(p, find_one_quantile, double(1))
  # 
  # return(out)
}

