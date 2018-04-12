#
## Explore data
###

plot(data1$x,data1$y)

require(MASS)
data(Boston, package='MASS')
y <- Boston$nox
x <- Boston$dis
n <- length(x)

cvs <- rep(0, n)
for(j in seq(n)){
  ys <- y[-j]
  xs <- x[-j]
  d <- nls(ys ~ A + B * exp(C * xs), start=list(A=0.5, B=0.5, C=-0.5))
  cvs[j] <- (y[j] - predict(d, data.frame(xs=x[j])))^2
  print(paste0(j, " of ", n, " finished (", round(j/n*100), "%)"))
}

plot(y~x, pch=19, col='gray', cex=1.5, xlab='dis', ylab='nox')
d <- nls(y~ A + B * exp(C * x), start=list(A=0.5, B=0.5, C=-0.5))
lines(predict(d)[order(x)]~sort(x), lwd=4, col='black')
usr <- par("usr")
text(usr[1] + 0.9*(usr[2]-usr[1]), usr[3] + 0.9*(usr[4]-usr[3]), paste("LOO MSE", "=", round(mean(cvs), 5)), pos=2)
text(usr[1] + 0.9*(usr[2]-usr[1]), usr[3] + 0.8*(usr[4]-usr[3]), paste("MSE", "=", round(mean(resid(d)^2), 5)), pos=2)