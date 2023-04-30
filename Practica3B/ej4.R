# Ejercicio 4

library('Lock5withR')

data('MustangPrice')

# a)

median_price_bootstrap <- c()

set.seed(123)
for ( i in 1:5000){
  temp <- sample( MustangPrice$Price, 25, replace = TRUE )
  median_temp <- median(temp)
  median_price_bootstrap[i] <- median_temp
}

# b)

hist(median_price_bootstrap)

# A partir de este grafico explicar por que no seria  apropiado usar ninguna de las dos estrategias
# basadas en bootstrap para construir un intervalo de confianza para la mediana.

# El histograma presente mucho huecos entre los distintos valores

# c)
# Podra usarse bootstrap si en lugar de interesarnos la mediana nos interesa la media?
# Veamos...

mean_price_bootstrap <- c()

for ( i in 1:5000){
  temp <- sample( MustangPrice$Price, 25, replace = TRUE )
  mean_temp <- mean(temp)
  mean_price_bootstrap[i] <- mean_temp
}

hist(mean_price_bootstrap) # Tiene distribuciion normal. Parece que ha de poder usarse bootstrap.

# Calculemos entonces el intervalo de confianza normal de nivel 0.95, simplemente para ver como se comporta.

c( quantile(mean_price_bootstrap, 0.05/2), quantile(mean_price_bootstrap, 1 - 0.05/2) )
