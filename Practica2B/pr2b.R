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

# i)

unif_datos <- runif(10,0,1)

theta_emv <- max(unif_datos)
theta_emm <- 2*mean(unif_datos)

verif_emv <- abs(theta_emv-1) < 0.01
verif_emm <- abs(theta_emm-1) < 0.01

pmom <- rep(0,1000)
pemv <- rep(0,1000)
for (i in 1:1000){
  temp_datos <- runif(10,0,1)
  pmom[i] <- 2*mean(temp_datos)
  pemv[i] <- max(temp_datos)
}

pmom_bool <- abs(pmom-1) < 0.01
pemv_bool <- abs(pemv-1) < 0.01

prob_mom <- mean(pmom_bool)
prob_emv <- mean(pemv_bool)

prob_mom
prob_emv
# ii)

# n = 100

pmom2 <- rep(0,1000)
pemv2 <- rep(0,1000)
for (i in 1:1000){
  temp_datos <- runif(100,0,1)
  pmom2[i] <- 2*mean(temp_datos)
  pemv2[i] <- max(temp_datos)
}

pmom_bool2 <- abs(pmom2-1) < 0.01
pemv_bool2 <- abs(pemv2-1) < 0.01

prob_mom2 <- mean(pmom_bool2)
prob_emv2 <- mean(pemv_bool2)

prob_mom2
prob_emv2

# n = 500

pmom3 <- rep(0,1000)
pemv3 <- rep(0,1000)
for (i in 1:1000){
  temp_datos <- runif(500,0,1)
  pmom3[i] <- 2*mean(temp_datos)
  pemv3[i] <- max(temp_datos)
}

pmom_bool3 <- abs(pmom3-1) < 0.01
pemv_bool3 <- abs(pemv3-1) < 0.01

prob_mom3 <- mean(pmom_bool3)
prob_emv3 <- mean(pemv_bool3)

prob_mom3
prob_emv3

# n = 1000

pmom4 <- rep(0,1000)
pemv4 <- rep(0,1000)
for (i in 1:1000){
  temp_datos <- runif(1000,0,1)
  pmom4[i] <- 2*mean(temp_datos)
  pemv4[i] <- max(temp_datos)
}

pmom_bool4 <- abs(pmom4-1) < 0.01
pemv_bool4 <- abs(pemv4-1) < 0.01

prob_mom4 <- mean(pmom_bool4)
prob_emv4 <- mean(pemv_bool4)

prob_mom4
prob_emv4

# analogo con n = 2000, 5000, 10000, 100000

# A medida que aumenta n, la probabilidad es cada vez mejor (tiende a 1), pero el EMV posee una proba mayor,
# i.e. el estimador de maxima verosimilitud es mejor, pues converge mas rapido al valor real

# Ejercicio 5

cpu <- scan('cpu.txt')

# Estimacion de la ventana de Silverman, utilizando el nucleo gaussiano.

hsil <- 1.06*min(sd(cpu),IQR(cpu)/1.349)*length(cpu)^(-1/5)

plot(density(cpu, kernel = 'gaussian'), col = 'blue') # Con ventana por default
lines(density(cpu, kernel = 'gaussian', bw = hsil), col = 'green') # Con ventana dada por Silverman

# Conclusion: Son casi iguales

# Ejercicio 6

debernardi <- read.csv('debernardi.csv')

# Calculo de la ventana de Silverman segun los niveles de  la variable DIAGNOSIS. 
hsil_db <- 1.06*min(sd(debernardi$diagnosis),IQR(debernardi$diagnosis)/1.349)*length(debernardi$diagnosis)^(-1/5)

plot(density(debernardi$LYVE1, kernel = 'gaussian'), col = 'blue')
lines(density(debernardi$LYVE1, kernel = 'gaussian', bw = hsil_db), col = 'green')

plot(density(debernardi$LYVE1, kernel = 'epanechnikov'), col = 'blue')
lines(density(debernardi$LYVE1, kernel = 'epanechnikov', bw = hsil_db), col = 'green')

