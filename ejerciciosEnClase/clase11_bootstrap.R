set.seed(20)

# Ejemplo 1
datos <- rcauchy(21)
medianas <- c()
for (i in 1:5000){  # Bootstrap No parametrica 
  muestra_temp <- sample(datos,20,replace =TRUE)
  mediana_temp <- median(muestra_temp)
  medianas[i] <- mediana_temp
}

mean_medianas <- mean(medianas)
print( (1/5000)* sum( (medianas - mean_medianas)^2) )

# Ejemplo 2
# a)

datos_exp <- rexp(500,1)

# b)

media_estimada <- mean(datos_exp)
mediana_estimada <- median(datos_exp)
c(media_estimada, mediana_estimada)

# c)

medias_exp <- c()
medianas_exp <- c()
for ( i in 1:1000){
  muestr_exp <- sample(datos_exp, replace = TRUE)
  medias_exp[i] <- mean(muestr_exp)
  medianas_exp[i] <- median(muestr_exp)
}

# d)

hist(medias_exp, probability = TRUE)
lines(density(medias_exp, kernel = "gaussian"), col = "blue")
hist(medianas_exp, probability = TRUE)
lines(density(medianas_exp, kernel = "gaussian"), col = "blue")

# e)

# Intervalo de confianza bootstrap parametrico nivel 0.95

intervalo_media <- c(media_estimada -1.96*sd(medias_exp), media_estimada + 1.96*sd(medias_exp) )
intervalo_mediana <- c(mediana_estimada -1.96*sd(medianas_exp), mediana_estimada + 1.96*sd(medianas_exp))
intervalo_media
intervalo_mediana

# Intervalo de confianza bootstrap no parametrico nivel 0.95

intervalo_media_percentil <- c( quantile(medias_exp,0.05/2), quantile(medias_exp,1 - 0.05/2))
intervalo_mediana_percentil <- c( quantile(medianas_exp,0.05/2), quantile(medianas_exp,1 - 0.05/2))
intervalo_media_percentil
intervalo_mediana_percentil

# Adicionales:
# f) calcular intervalo exacto para la media y comparar con lo anterior. Obs: una clase vimos que la exp es una gamma
# g) deducir el intervalo asintotico para la media