library(ggplot2)
library(reshape2)

simulate <- function(f, n) {
  x <- sort(runif(n, -1, 1))
  epsilon <- rnorm(n)
  y <- f(x) + epsilon
  estimator.linear <- lm(y ~ x)
  estimator.polynomial2 <- lm(y ~ poly(x, 2))
  estimator.polynomial3 <- lm(y ~ poly(x, 3))
  estimator.polynomial4 <- lm(y ~ poly(x, 4))
  estimator.polynomial5 <- lm(y ~ poly(x, 5))
  estimator.box <- ksmooth(x=x, y=y, kernel="box", bandwidth=n^(-1/5), x.points=x)
  estimator.gaussian <- ksmooth(x=x, y=y, kernel="normal", bandwidth=n^(-1/5), x.points=x)
  data.frame(
    linear=mean((predict(estimator.linear, newdata=data.frame(x=x)) - y)^2),
    polynomial2=mean((predict(estimator.polynomial2, newdata=data.frame(x=x)) - y)^2),
    polynomial3=mean((predict(estimator.polynomial3, newdata=data.frame(x=x)) - y)^2),
    polynomial4=mean((predict(estimator.polynomial4, newdata=data.frame(x=x)) - y)^2),
    polynomial5=mean((predict(estimator.polynomial4, newdata=data.frame(x=x)) - y)^2),
    box=mean((estimator.box$y - y)^2),
    gaussian=mean((estimator.gaussian$y - y)^2))    
}

f.linear <- function(x) {
  2 * x
}

f.sin <- function(x) {
  sin(x * pi)
}

f.polynomial <- function(x) {
  x3 <- x * x * x
  2 * x + x3 - 6 * x3 * x
}

f.inverse <- function(x) {
  1 / (1 + 25 * x * x)
}

mse.linear <- do.call(rbind, replicate(100, simulate(f.linear, 1000), simplify=FALSE))
mse.sin <- do.call(rbind, replicate(100, simulate(f.sin, 1000), simplify=FALSE))
mse.polynomial <- do.call(rbind, replicate(100, simulate(f.polynomial, 1000), simplify=FALSE))
mse.inverse <- do.call(rbind, replicate(100, simulate(f.inverse, 1000), simplify=FALSE))

pdf("mse_linear.pdf", width=6, height=3)
ggplot(melt(mse.linear, value.name="mse", variable.name="estimator"),
       aes(estimator, mse)) + geom_boxplot() + ggtitle('MSE of Linear')
dev.off()

pdf("mse_sin.pdf", width=6, height=3)
ggplot(melt(mse.sin, value.name="mse", variable.name="estimator"),
       aes(estimator, mse)) + geom_boxplot() + ggtitle('MSE of Sin')
dev.off()

pdf("mse_polynomial.pdf", width=6, height=3)
ggplot(melt(mse.polynomial, value.name="mse", variable.name="estimator"),
       aes(estimator, mse)) + geom_boxplot() + ggtitle('MSE of Polynomial')
dev.off()

pdf("mse_inverse.pdf", width=6, height=3)
ggplot(melt(mse.inverse, value.name="mse", variable.name="estimator"),
       aes(estimator, mse)) + geom_boxplot() + ggtitle('MSE of Inverse')
dev.off()
