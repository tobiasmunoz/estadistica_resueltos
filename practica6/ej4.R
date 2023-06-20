datos <- read.csv('glakes.csv')

# a)

LT <- log( datos$Time )

W <- ( datos$Tonnage )^0.25

modelo <- lm( LT ~ W )

# b)

modelo$coefficients #θ_0 = 1.1884 y θ_1 = 0.3091

# c)

summary(modelo) # σ^2 = 0.3034^2 = 0.092
sigma2 <-  0.3034^2

# d) 

# Como σ^2 es desconocido el test es |θ_1|/ σ*sqrt(dii) > t_31-2,alfa/2 (si vale, se rechaza H0)
# H0: θ_1 != 0
# D = solve( t( cbind(1,matrix(W)) ) %*% cbind(1,matrix(W)) )

D <- solve( t( cbind(1,matrix(W)) ) %*% cbind(1,matrix(W)) )

res_test_cero <- modelo$coefficients[2] / ( sqrt(sigma2 * D[2,2]) ) > qt(1 - 0.01/2, 31-2)
res_test_cero
# res_test = TRUE, por lo tanto se rechaza H0 a nivel 0.01

# e)

# H0: Intercept = 10 vs H1: Intercept > 10 ( No seria intercept = 1, en vez de 10?? )
# Hay evidencia suficiente a nivel 0.05 para rechazar H0? p-valor?

res_test_uno <- abs( ( modelo$coefficients[1] - 1 ) / ( sqrt(sigma2 * D[1,1]) ) ) > qt(1 - 0.05/2, 31-2)
res_test_uno

# Como res_test_uno es falso --> no rechazo H0

# f)

# Intervalo de confianza para θ_0 de nivel 0.95

ic_theta0 <- c( -qt( 1 - 0.05/2, 31 - 2 ) * sqrt(sigma2 * D[1,1]) + 1, qt( 1 - 0.05/2, 31 - 2 ) * sqrt(sigma2 * D[1,1]) + 1  )
ic_theta0

# R-squared = 0.8158
# Dado que es un numero bastante cercano a 1, el ajuste parece ser bastante bueno.

# g)

# Una empresa envia habitualmente cargamentos de peso 625 toneladas y esta interesada
# en obtener una estimacíon de la media del logaritmo del tiempo que se tarda
# en descargar cada uno de ellos. ¿Como estimarıa este valor?
# Calcular un intervalo de confianza de nivel 0.95 para esta cantidad.

# Cual es la diferencia con el item siguiente? Preg...

# h)

# Llega un nuevo cargamento con peso 625 toneladas.
# ¿Cual es su prediccion del logaritmo del tiempo que se tardara en descargarlo?
# Calcular un intervalo de prediccion de nivel 0.95. Comparelo con el intervalo del ıtem anterior.

x_obs <- 625^0.25

estimacion_tiempo <- modelo$coefficients[1] + modelo$coefficients[2]*x_obs
estimacion_tiempo

x0 <- matrix( data = c(1,x_obs), nrow = 2, ncol = 1 )
ic_tiempo_peso625 <- c( estimacion_tiempo - qt( 1 - 0.05/2, 31-2 )*sqrt( sigma2 * t(x0) %*% D %*% x0 ), estimacion_tiempo + qt( 1 - 0.05/2, 31-2 )*sqrt( sigma2 * t(x0) %*% D %*% x0 ))
ic_tiempo_peso625 # En escala logaritmica

# i)

# Para el cargamento del ıtem anterior
# ¿cual es su prediccion del tiempo que se tardara en descargarlo?
# Calcular un intervalo de prediccion de nivel 0.95

# Aplicando la inversa del logaritmo para convertir el tiempo a la escala original:

exp(estimacion_tiempo)
exp(ic_tiempo_peso625)

