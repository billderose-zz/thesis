library(animation)
ani.options(interval = .05, nmax = 10)

# Some variables defined
x <- seq(0, 10, length = 500)
mu  <- c(100)
sigma <- c(sqrt(1.0))
p <- c(0.5)
colour <- c("blue", "red", "gold", "darkgreen")

labels <- c("pdf", "cdf")

# PDFs
plot(x, dbinom(x, size = mu[1], prob = p[1]),
     lwd=2, col="blue", type = "l", ylim = c(0, 1),
     main = "Standard Normal Distribution",
     xlab = expression(X),
     ylab = expression(p(X)))
# CDF
lines(x, pbinom(x, size = mu[1], prob = p[1]), lwd=2, col="red", type = "l", ylim = c(0, 1),
     main = "Cumulative distribution function",
     xlab = expression(chi),
     ylab = expression(phi[mu ~ "," ~ sigma^2](chi)))
legend("topleft", inset=.05, labels, lwd=2, lty=c(1, 1, 1, 1), cex = 0.8, col=colour)

setwd("~/Dropbox/thesis")
sample <- c()
saveLatex({
for (i in 1:ani.options("nmax")) {
  u <- runif(1)
  sample[i] <- qbinom(u, mu[1], p[1])
  hist(sample, breaks = 30, prob = TRUE, xlim = c(-4, 4), 
       ylim = c(0, 1), main = "Empirical Distribution", xlab = "X")
  lines(x, dbinom(x, size = mu[1], prob = p[1]), 
       lwd=2, col="blue", type = "l", ylim = c(0, 1),
       main = "Standard Normal Distribution",
       xlab = expression(X),
       ylab = expression(p(X)))
  # CDF
  lines(x, pbinom(x, size = mu[1], prob = p[1]), lwd=2, col="red", type = "l", ylim = c(0, 1),
        main = "Cumulative distribution function",
        xlab = expression(chi),
        ylab = expression(phi[mu ~ "," ~ sigma^2](chi)))
  
  segments(-7, u, sample[i], u)
  segments(sample[i], u, sample[i], 0, lty= 2)
  # Adds a legend to the top-left
  ani.pause()
}
}, movie.name = "inverse_transform_latex")
