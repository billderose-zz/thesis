# inverse_sample takes the inverse function of a CDF and produces sample_size
# samples by mapping uniform random values to samples from the distribution
# whose inverse CDF is given. args are additional arguments that need 
# be passed to the inverse.
inverse_sample <- function(cdf_inverse, args, sample_size) {
  return(cdf_inverse(runif(sample_size), args))
}

# inverse_exponential is the inverse of an exponential distribution with
# parameter lambda
inverse_exponential <- function(x, lambda) {
  return(-1/lambda * log(1 - x))
}
