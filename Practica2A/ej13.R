# Ejercicio a)

# Generar n = 100 datos como si provenieran de una muestra aleatoria X1, ... , Xn donde Xi
# es una variable aleatoria con distribucion Exp(λ) con λ = 1.
# Graficar su funcion de distribucion empirica junto con la verdadera funcion de distribucion acumulada.

datos <- rexp(100)

plot(pexp, col = "green")
lines(ecdf(datos))

# Ejercicio b)

# Para las 100 observaciones obtenidas en la parte a), calcular el estimador de λ
# por el metodo de los momentos y por maxima verosimilitud.

# Tanto el estimador de momentos como el de maxima verosimilitud proponen que el esimador de λ es:
# 1 / x̄ por ejercicio 1 y ejercicio 5 de la guia.

lambda_estimation <- 1 / mean(datos)
lambda_estimation # 1.018929 ~ 1

# Ejercicio c)

# Proponer un estimador de la funcion de distribucion acumulada distinto al obtenido en a)
# Superponerlo en el graficoo anterior.
# ¿Que observa? ¿Parece su propuesta mejorar la estimacion dada en a)?

# Uso la estimacion de lambda para calcular la funcion de distribucion acumulada
# Usando que Fx(x) = 1 - Exp(-λx), calculo la funcion para cada dato obtenido.

probas_estimadas <- 1 - exp(-lambda_estimation * datos)

plot(pexp, col = 'green')
lines(ecdf(datos), col = 'yellow')
points(datos,probas_estimadas, col = 'purple')

# La estimacion parece mejorar considerablemente con la empirica dada por R. Es muy similar a la original.

# Repetir los items anteriores para n = 10, n = 1000

# n = 10 

datos2 <- rexp(10)
plot(pexp, col = "green")
lines(ecdf(datos2))

lambda_estimation2 <- 1 / mean(datos2)
lambda_estimation2 # 1.119923

probas_estimadas2 <- 1 - exp(-lambda_estimation2 * datos2)

plot(pexp, col = 'green')
lines(ecdf(datos2), col = 'yellow')
points(datos2,probas_estimadas2, col = 'purple')

# n = 1000

datos3 <- rexp(1000)
plot(pexp, col = "green")
lines(ecdf(datos3))

lambda_estimation3 <- 1 / mean(datos3)
lambda_estimation3 # 1.041827

probas_estimadas3 <- 1 - exp(-lambda_estimation3 * datos3)

plot(pexp, col = 'green')
lines(ecdf(datos3), col = 'yellow')
points(datos3,probas_estimadas3, col = 'purple')


