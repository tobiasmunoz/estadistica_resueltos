# Ejercicio 3

# Encuesta Permanente de Hogares

eph <- read.csv('eph21_3T.csv')

# a)

eph_hombres <- eph[ eph$mujer == 0,]
length(eph_hombres$ingresototal) # 20386

eph_mujeres <- eph[ eph$mujer == 1,]
length(eph_mujeres$ingresototal) # 21962


median(eph_hombres$ingresototal.conruido) - median(eph_mujeres$ingresototal.conruido) # La mediana de los hombres es mayor

# b)

set.seed(10)
bootstrap_mujeres <- sample(eph_mujeres$ingresototal.conruido, 21962, replace = TRUE)
bootstrap_hombres <- sample(eph_hombres$ingresototal.conruido, 20386, replace = TRUE)

dif_bootstrap_ruido <- median(bootstrap_hombres) - median(bootstrap_mujeres)
dif_bootstrap_ruido

# c)

dif_medianas_bootstrap <- c()
for ( i in 1:5000){
  temp_mujeres <- sample(eph_mujeres$ingresototal.conruido, 21962, replace = TRUE)
  temp_hombres <- sample(eph_hombres$ingresototal.conruido, 20386, replace = TRUE)
  dif_mediana_temp <- median(temp_hombres) - median(temp_mujeres)
  dif_medianas_bootstrap[i] <- dif_mediana_temp
}

hist(dif_medianas_bootstrap) # Tres modas, en todos los casos son positivas las diferencias, i.e. la mediana del ingreso total de hombres es mayor
sd(dif_medianas_bootstrap)

# d)

intervalo_confianza_normal <- c( mean(dif_medianas_bootstrap) - qnorm(1 - 0.05/2), mean(dif_medianas_bootstrap) + qnorm(1 - 0.05/2) )
intervalo_confianza_percentil <- c( quantile(dif_medianas_bootstrap, 0.05/2), quantile(dif_medianas_bootstrap, 1 - 0.05/2))

intervalo_confianza_normal
intervalo_confianza_percentil

# En base a estos datos se observa que los ingresos medianos de los hombres es mayor que el de las mujeres.