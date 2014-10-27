library(hash)
library(randomForest)
map <- hash()

Learn.Interval <- function(u, l, model) {

} 


dist.x <- seq(-10, 10, length = 1000)
Slice.Sample <- function(x0, f, nsample, step = 1) {
  x <- c(x0)
  interval <- data.frame()
  for (i in 2:nsample) {
    u <- runif(1, 0, f(x[i - 1]))
    lower <- x[i - 1] - 1
    upper <- x[i - 1] + 1
    if (i > 10) {
      lowerModel <- randomForest(x = interval[ , c("u", "x")], y = interval[ , "lower"])
      lower <- predict(lowerModel, data.frame(u = u, x = x[i - 1]))
      upperModel <- randomForest(x = interval[ , c("u", "x")], y = interval[ , "upper"])
      upper <- predict(upperModel, data.frame(u = u, x = x[i - 1]))
    } else {
      lower <- x[i - 1] - 1
      upper <- x[i - 1] + 1
      while (u < f(lower)) {
        lower <- lower - step
      }
      while (u < f(upper)) {
        upper <- upper + step
      }
    }
    while (TRUE) {
      x.proposal <- runif(1, lower, upper)
      if (u < f(x.proposal)) {
        x[i] <- x.proposal
        break
      } else { # drew sample above curve, must shrink slice
        if (x.proposal < lower) {
          lower <- x.proposal
        } else if (x.proposal > upper) {
          upper <- x.proposal
        }
      }
    }
    interval[i - 1, "u"] <- u
    interval[i - 1, "x"] <- x[i - 1]
    interval[i - 1, "lower"] <- lower
    interval[i - 1, "upper"] <- upper
  }
  return(x)
}

