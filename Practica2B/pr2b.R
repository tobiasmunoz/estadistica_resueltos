# Ejercicio 1

# Cantidad de lluvia (mm) que cae en una tormenta es una v.a. con distribucion Gamma(alpha,lambda)
# Se seleccionan al azar 200 tormentas, datos en tormentas.txt

datos <- scan('tormenta.txt')

# a)

alpha_emm <- mean(datos)^2 / ( mean(datos^2) - mean(datos)^2 )
lambda_emm <- mean(datos) / ( mean(datos^2) - mean(datos)^2 )

library(MASS)
values_emv <- fitdistr(datos, 'gamma')
values_emv
alpha_emv <- 1.9615040
lambda_emv <- 0.19548582

# b)
# P(X) > 20 (mm)

paste('P(X) > 20 =(emv)' ,1 - pgamma(20, shape = alpha_emv, rate = lambda_emv) )
paste('P(X) > 20 =(emm)' ,1 - pgamma(20, shape = alpha_emm, rate = lambda_emm) )

hist(datos, probability = TRUE, ylim = c(0,0.07))
lines(density(datos), col = 'blue')
points(datos,dgamma(datos,alpha_emm,lambda_emm), col = 'red')
points(datos,dgamma(datos,alpha_emv,lambda_emv), col = 'magenta')

# Ejercicio 2

# fX(x,θ) = (θ^4)*x*exp(−x*θ^2)*I{x > 0}, θ > 0

# fY(y,θ)= 2y/θ^2I{0<y<θ}, θ>0

# a) Estimadores de maxima verosimilitud (hecho en papel)

# θx = sqrt(2n/sum(xi)), i = 1,...,n
# θy = max(yi) i=1,...,n

# E(X) = 2 / θ^2
# E(Y) = 2θ/3

datos_a <- c(1.49, 1.12, 1.08, 0.65, 0.98, 0.86, 0.37, 0.41, 0.91, 0.85,
0.25, 0.78, 0.30, 1.41, 0.18, 0.52, 0.40, 0.69, 0.73, 0.81)

datos_b <- c(1.32, 0.59, 1.41, 1.15, 0.34, 1.16, 1.29, 1.25, 1.18, 0.27,
1.31, 0.96, 0.57, 0.66, 0.67, 1.34, 0.47, 1.07, 1.13, 0.25)

theta_A <- sqrt( 2*length(datos_a) / sum(datos_a))
theta_B <- max(datos_b)

# Basados en el estimador de maxima verosimilitud

paste('Tiempo esperado de respuesta para el servidor A', 2/theta_A^2)

paste('Tiempo esperado de respuesta para el servidor B', 2*theta_B/3)

# Ejercicio 3

# a) y b) en papel

# c)


