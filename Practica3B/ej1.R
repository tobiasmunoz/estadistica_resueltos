# Ejercicio 1

dados <- c(2,2,4,6,1,3,1,3,2,4,4,4,4,4,6,3,3,4,1,2,1,6,3,2,3,4,1,1,5,4,1,4,6,4,1,2,1,5,4,3,3,1,3,1,6,5,1,3,2,3,6,2,4,2,6,6,5,2,4,4,1,4,3,1,2,1,6,1,1,3,1,6,6,1,2,6,1,1,4,5,4,1,5,2,2,1,6,6,1,2,1,3,1,3,3,4,3,3,3,5)

# a)
# Queremos estudiar la probabilidad de que un resultado del dado sea par.
# Llamemos θ a dicha probabilidad. ¿Cuanto deberia valer θ si el dado fuera equilibrado? --> 1/2

# i)

set.seed(123)
datos_sample <- sample(dados,100, replace = TRUE)
sum <- 0
for (i in 1:length(datos_sample) ){
  if ( datos_sample[i] %% 2 == 0) {
    sum <- sum + 1
  }
}
print(sum/100)

# ii )

titas.boot <- c()

for ( j in 1:5000 ){
  sum_temp <- 0
  temp <- sample(dados,100, replace = TRUE)
  for (i in 1:100 ){
    if ( temp[i] %% 2 == 0) {
      sum_temp <- sum_temp + 1
    }
  }
  titas.boot[j] <- sum_temp / 100
}

# iii)

hist(titas.boot) # Tiene forma acampanada, pero no centrada en 0.5

# iv)

sd(titas.boot)

# v)

ic_a <- c( mean(titas.boot) - qnorm(1 - 0.05/2)*sd(titas.boot), mean(titas.boot) + qnorm(1 - 0.05/2)*sd(titas.boot))
ic_a

ic_b <- c( quantile(titas.boot, 0.05/2), quantile(titas.boot, 1 - 0.05/2))
ic_b

# No parece ser equilibrado el dado en base a los resultados.

# b)
# Probabilidad de que un resultado del dado sea exactamente igual a 5 (prob = tita)
# Cuanto vale tita si es equilibrado el dado? --> 1/6 = 0.1666667
# Repetir el analisis anterior

sum <- 0
for (i in 1:length(datos_sample) ){
  if ( datos_sample[i] == 5) {
    sum <- sum + 1
  }
}
print(sum/100)


titas.boot_2 <- c()

for ( j in 1:5000 ){
  sum_temp <- 0
  temp <- sample(dados,100, replace = TRUE)
  for (i in 1:100 ){
    if ( temp[i] == 5) {
      sum_temp <- sum_temp + 1
    }
  }
  titas.boot_2[j] <- sum_temp / 100
}

hist(titas.boot_2)
abline( v = 1/6, col = 'red')

sd(titas.boot_2)

ic_a_2 <- c( mean(titas.boot_2) - qnorm(1 - 0.05/2)*sd(titas.boot_2), mean(titas.boot_2) + qnorm(1 - 0.05/2)*sd(titas.boot_2))
ic_a_2

ic_b_2 <- c( quantile(titas.boot_2, 0.05/2), quantile(titas.boot_2, 1 - 0.05/2))
ic_b_2

# No es equilibrado pues ni siquiera pertence al rango de valores el valor 1/6 que es lo que deberia valer tita.
