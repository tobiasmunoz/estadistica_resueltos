# Ejercicio 2

library('Lock5withR')

data('MustangPrice')

# a)

plot(MustangPrice$Price, MustangPrice$Miles)
cor(MustangPrice$Price, MustangPrice$Miles) # Negativamente correlacionados

# b)
# Describir un mecanismo que permita computar un estimador bootstrapeado de la correlacion entre Price y Miles.

# Dada la formula para estimar la correlacion muestral, se utiliza para una muestra con reposicion de
# los datos a pares de los originales con el mismo size. Bootstrap no parametrico 

# c)

cor.bootstrap <- c()

for ( i in 1:5000 ) { # Pongo set.seed(i) antes de cada sample para que me de los pares correctos Xi, Yi.
  set.seed(i)
  price_temp <- sample(MustangPrice$Price, 25 , replace = TRUE)
  set.seed(i)
  miles_temp <- sample(MustangPrice$Miles, 25 , replace = TRUE)
  temp_cor <- cor(price_temp, miles_temp)
  cor.bootstrap[i] <- temp_cor
}

hist(cor.bootstrap) # Forma acampanada, con gran cantidad de valores en -0.8 lo cual tiene sentido pues esta cerca del valor real

# d)

intervalo_bootstrap_percentil <- c( quantile(cor.bootstrap, 0.05/2), quantile(cor.bootstrap, 1 - 0.05/2) )
intervalo_bootstrap_percentil

# El valor original -0.8246164 se encuentra dentro del intervalo. Parece que el estimador bootstrap es bastante bueno.