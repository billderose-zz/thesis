library(animation)

dist.x <- seq(-10, 10, length = 1000)
ani.options(interval = .05, nmax = 1000)


Slice.Sample <- function(x0, f, nsample, step = 1, min, max) {
  x <- c(x0)
  saveGIF({
  for (i in 2:ani.options("nmax")) {
    hist(x, xlim = c(-4, 4), ylim = c(0, 1), prob = TRUE, breaks = 30)
    u <- runif(1, 0, f(x[i - 1]))
    lines(dist.x, f(dist.x), type = "l")
    segments(x[i - 1], 0, x[i - 1], f(x[i - 1]))
    l <- x[i - 1] - 1
    r <- x[i - 1] + 1
    while (u < f(l)) {
      l <- l - step
    }
    while (u < f(r)) {
      r <- r + step
    }
    while (TRUE) {
      x.proposal <- runif(1, l, r)
      if (u < f(x.proposal)) {
        x[i] <- x.proposal
        points(x.proposal, u, pch = 20)
        break
      } else { # drew sample above curve, must shrink slice
        if (x.proposal < l) {
          l <- x.proposal
        } else if (x.proposal > r) {
          r <- x.proposal
        }
      }
    }
    points(c(l, r), c(u, u), pch = 4, col = "red")
    segments(l, u, r, u)
    segments(x.proposal, u, x.proposal, 0, lty = 2)
    ani.pause()
  }}, movie.name = "slice.gif")
  return(x)
}
hist(Slice.Sample(1, dnorm, ani.options("nmax"), 1, -100, 100))

