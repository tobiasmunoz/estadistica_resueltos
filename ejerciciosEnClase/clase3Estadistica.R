# Actividad - Clase 3 (Estadistica)
buffalo <- scan("buffalo.txt")

# Ejercicio 1

hist(buffalo)

# a)
par(mfrow=c(2,2))
hist( buffalo, breaks = seq(20,130,10) )
# b)
hist( buffalo, breaks = seq(22,132,10) )
# c)
hist( buffalo, breaks = seq(24,134,10) )
# d)
hist( buffalo, breaks = seq(24,136,10) )

# Ejercicio 2
par(mfrow=c(1,1))

hist( buffalo, breaks = seq(10,130,10))
# La eleccion permite visualizar mejor la distribucion de los datos, en algunos es mas notorio que la distribucion parece ser normal

# Ejercicio 3

prob_caer_en_intervalo <- function(x,h,datos){
  n <- length(datos)
  res <- sum( datos <= (x+h) & datos >= (x-h) )  / n
  return(res)
}

# Ejercicio 4

estimacion_h1 <- c()
estimacion_h2 <- c()
estimacion_h3 <- c()

for ( i in 1:length(buffalo) ) {
  value <- prob_caer_en_intervalo(buffalo[i],10,buffalo)
  estimacion_h1[i] <- value
}

for ( i in 1:length(buffalo) ) {
  value <- prob_caer_en_intervalo(buffalo[i],20,buffalo)
  estimacion_h2[i] <- value
}

for ( i in 1:length(buffalo) ) {
  value <- prob_caer_en_intervalo(buffalo[i],30,buffalo)
  estimacion_h3[i] <- value
}

# Ejercicio 5
densidad.est.parzen <- function(datos,h,x){
  n <- length(datos)
  en_rango <- sum( (x - datos) >= -1 & (x-datos)  <= 1 )
  res <- en_rango / (2*h*n)
  return(res)
}

# Ejercicio 6

values1 <- c()
for ( i in seq(25,126.4,length.out = 200) ){
  val <- densidad.est.parzen(buffalo,10,i)
  values1 = c(values1,val)
}

values2 <- c()
for ( i in seq(25,126.4,length.out = 200) ){
  val2 <- densidad.est.parzen(buffalo,20,i)
  values2 = c(values2,val2)
}

values3 <- c()
for ( i in seq(25,126.4,length.out = 200) ){
  val3 <- densidad.est.parzen(buffalo,30,i)
  values3 = c(values3,val3)
}

plot(values1, type = "l", col = "blue")
lines(values2, col = "green")
lines(values3, col = "red")

# Ejercicio 7

hist(buffalo, freq = FALSE, ylim = c(0,0.03))
lines(density(buffalo, kernel = "rectangular", width = 5) )
lines(density(buffalo,kernel="gaussian", width = 5), col = "green")
lines(density(buffalo,kernel="epanechnikov", width = 5), col = "purple")

# Ejercicio 8

hist(buffalo, freq = FALSE, ylim = c(0,0.06))
lines(density(buffalo, kernel = "rectangular", width = 1) )
lines(density(buffalo,kernel="gaussian", width = 1), col = "green")
lines(density(buffalo,kernel="epanechnikov", width = 1), col = "purple")

hist(buffalo, freq = FALSE, ylim = c(0,0.03))
lines(density(buffalo, kernel = "rectangular", width = 10) )
lines(density(buffalo,kernel="gaussian", width = 10), col = "green")
lines(density(buffalo,kernel="epanechnikov", width = 10), col = "purple")

hist(buffalo, freq = FALSE, ylim = c(0,0.03))
lines(density(buffalo, kernel = "rectangular", width = 50) )
lines(density(buffalo,kernel="gaussian", width = 50), col = "green")
lines(density(buffalo,kernel="epanechnikov", width = 50), col = "purple")
