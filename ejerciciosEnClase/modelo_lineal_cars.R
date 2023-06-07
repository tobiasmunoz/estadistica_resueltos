autos <- cars
x <- autos$speed
y <- autos$dist
xm <- matrix(x)
ym <- matrix(y)
plot(x,y)

beta0 <- mean(y) - cov(x,y)*mean(x)/var(x)
beta1 <- cov(x,y)/var(x)

plot(x,y)
grilla <- seq(0,25,0.1)
lines(grilla,beta0 + beta1*grilla, col = "red")
points(x,xm %*% solve( t(xm)%*%xm ) %*% t(xm) %*% ym, col = "orange")

p <- xm %*% solve( t(xm)%*%xm ) %*% t(xm)
sigma2_estimado <- t(ym) %*% (diag(x=1, nrow = 50, ncol = 50) - p) %*% ym / (50-1)
sigma2_estimado

X <- cbind( rep(1,50), xm, xm^2)

theta <- solve( t(X)%*%X ) %*% t(X) %*% ym

plot(x,y)
lines( x, theta[1] + theta[2]*x + theta[3]*x^2, col = "magenta")
