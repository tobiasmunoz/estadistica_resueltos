# Ejercicio 1
datos <- read.table('bajoPeso.txt', header = TRUE)

# a) Realizar un diagrama de dispersion del indice Apgar (eje de abscisas) contra la presion sistolica.
plot( datos$apgar5, datos$presSist)

# b) Usando la presion sistolica como variable respuesta y la edad gestacional y el indice Apgar como
# variables explicativas, ajustar el modelo lineal Y = β0 + β1·edadG+β2·apgar5 + ϵ. Brindar todos
# los coeficientes estimados.
modelo <- lm( datos$presSist ~ datos$edadG + datos$apgar5)
modelo$coefficients

# c) Cual es la presion media estimada para la poblacion de niños de bajo peso cuya edad gestacional
# es 31 semanas y cuyo indice Apgar es 7 ?
modelo$coefficients[1] + modelo$coefficients[2]*31 + modelo$coefficients[3]*7 # Real = 64

# d) Decidir si la relacion lineal es estadisticamente significativa, justificando. Indicar las hipotesis que se contrastan
# Chequeo si los coeficientes sean cero (hipotesis nula)

summary(modelo) # Para no hacerlo a mano, sin embargo probar hacerlo a mano

# Hipotesis β1 = 0 vs != 0 --> estadistico = 3.492 > t_n-p,0.05/2 = 1.98. Entonces rechazo.
# Hipetiss β2 = 0 vs != 0 --> estadistico = 9.574 > t_n-p,0.05/2 = 1.98. Entonces rechazo.
# i.e. la relacion lineal es estadisticamente significativa

# e) Construir un intervalo de confianza de nivel 0,95 para β1. Calcular la estimacion por intervalo
# con el conjunto de datos brindados.

X <- cbind(1,datos$edadG,datos$apgar5)
Y <- datos$presSist
P <- X %*% solve( t(X) %*% X ) %*% t(X)
sigma2 <- t(Y) %*% ( diag(1, 100, 100) - P ) %*% Y / (100-3)
sigma2

a <- matrix(data = c(0,1,0))
denominador <- t( a ) %*% solve( t(X) %*% X ) %*% a # Del estadistico que distribuye como una Student
intervalo_conf <- c( modelo$coefficients[2] - sqrt(sigma2*denominador)*qt(1-0.05,100-3), modelo$coefficients[2] + sqrt(sigma2*denominador)*qt(1-0.05,100-3) ) # Nivel 95%

# f) Informar la significatividad de las variables, indicando cada uno de los tests que devuelve el
# resumen ( summary ) del ajuste del item b). ¿Como se calcula el p-valor en cada test?
# Rta: coeficiente mas grande en apgar5, tiene mayor significatividad que edadG. P-valores muy bajos y casi nulo para apgar5

# g) ¿Cuales supuestos tuvo que asumir para responder los tres items anteriores?
# Rta:  Normalidad e i.i.d. de los errores ϵ

# h) Comentar la magnitud de R2
# Rta: R2 = 0.55 No demuestra que el ajuste es muy bueno ni muy malo.

# Calculo de los coeficientes, t-value, p-value, R-squared, F-statistic, residuos, etc.

coeficientes <- solve( t(X) %*% X ) %*% t(X) %*% Y
coeficientes

Y_hat <- X %*% coeficientes
residuos <- Y - Y_hat
res_se <- sqrt( sum(residuos^2) / (100-3) ) # Residual Standard Error
res_se

# dii = elemento de la diagonal de la matriz t(a) * ( t(x) * X )^-1 * a
d11 <- t( matrix(data = c(0,1,0)) ) %*% solve( t(X) %*% X ) %*% matrix(data = c(0,1,0))
d22 <- t( matrix(data = c(0,0,1)) ) %*% solve( t(X) %*% X ) %*% matrix(data = c(0,0,1))

t_edadG <- coeficientes[2] / (res_se * sqrt(d11))
t_edadG
t_apgar5 <- coeficientes[3] / (res_se * sqrt(d22))
t_apgar5

p_value_edadG <- 1 - pt(t_edadG, 100-3)
p_value_edadG
p_value_apgar5 <- 1 - pt(t_apgar5, 100-3)
p_value_apgar5

r_squared <- sum( ( Y_hat - mean(Y) )^2 ) / sum( ( Y - mean(Y) )^2 )
r_squared
