library(ggplot2)
library(animation)
col.range <- heat.colors(15)


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
  return(-1 / lambda * log(1 - x))
}

MH.Target <- function(x) {
  return((1 / 3.96) * (0.3 * exp(-0.2 * x ^ 2 ) + 0.7 * exp(-0.2*(x - 10) ^ 2)))
}

ani.options(interval = .05)
setwd("~/Dropbox/thesis")
Metropolis.Hastings <- function(g) {
  x <- c(5)
  saveGIF({
    for (t in 2:1500) {
      u <- runif(1)
      proposal <- rnorm(1, x, 10)
      if (u < min(1, (g(proposal) * dnorm(x[t - 1], proposal, 10)) / (g(x[t - 1]) * dnorm(proposal, x[t - 1], 10)))) {
        x[t] <- proposal
      } else {
        x[t] <- x[t - 1]
      }
      hist(x, xlim = c(-10, 20), ylim = c(0, 1), prob = TRUE, breaks = 30)
      curve(MH.Target, from = -10, to = 20, add = TRUE)
      ani.pause()
    }
  })
  return(x)
}

Metropolis.Hastings(MH.Target)

