n <- c(20,50,100)
p <- c(0.10,0.50)

set.seed(123)

# Nivel asintotico 0.95 cuando sustituyo p en la varianza por X_bar
longitudes <- c()
sum <- 0
for (i in 1:2000){ # n = 20, p = 0.10
  temp <- rbinom(n[1],1,p[1])
  a <- mean(temp) - 1.96*sqrt( mean(temp)*( 1-mean(temp) ) ) / sqrt(n[1])
  b <- mean(temp) + 1.96*sqrt( mean(temp)*( 1-mean(temp) ) ) / sqrt(n[1])
  longitud <- b - a
  longitudes[i] <- longitud
  if ( p[1] >= a & p[1] <= b) sum = sum + 1 
}

print(sum/2000)
print(mean(longitudes))

longitudes <- c()
sum <- 0
for (i in 1:2000){ # n = 50, p = 0.10
  temp <- rbinom(n[2],1,p[1])
  a <- mean(temp) - 1.96*sqrt( mean(temp)*( 1-mean(temp) ) ) / sqrt(n[2])
  b <- mean(temp) + 1.96*sqrt( mean(temp)*( 1-mean(temp) ) ) / sqrt(n[2])
  longitud <- b - a
  longitudes[i] <- longitud
  if ( p[1] >= a & p[1] <= b) sum = sum + 1 
}

print(sum/2000)
print(mean(longitudes))

longitudes <- c()
sum <- 0
for (i in 1:2000){ # n = 100, p = 0.10
  temp <- rbinom(n[3],1,p[1])
  a <- mean(temp) - 1.96*sqrt( mean(temp)*( 1-mean(temp) ) ) / sqrt(n[3])
  b <- mean(temp) + 1.96*sqrt( mean(temp)*( 1-mean(temp) ) ) / sqrt(n[3])
  longitud <- b - a
  longitudes[i] <- longitud
  if ( p[1] >= a & p[1] <= b) sum = sum + 1 
}

print(sum/2000)
print(mean(longitudes))

longitudes <- c()
sum <- 0
for (i in 1:2000){ # n = 20, p = 0.50
  temp <- rbinom(n[1],1,p[2])
  a <- mean(temp) - 1.96*sqrt( mean(temp)*( 1-mean(temp) ) ) / sqrt(n[1])
  b <- mean(temp) + 1.96*sqrt( mean(temp)*( 1-mean(temp) ) ) / sqrt(n[1])
  longitud <- b - a
  longitudes[i] <- longitud
  if ( p[2] >= a & p[2] <= b) sum = sum + 1 
}

print(sum/2000)
print(mean(longitudes))

longitudes <- c()
sum <- 0
for (i in 1:2000){ # n = 50, p = 0.50
  temp <- rbinom(n[2],1,p[2])
  a <- mean(temp) - 1.96*sqrt( mean(temp)*( 1-mean(temp) ) ) / sqrt(n[2])
  b <- mean(temp) + 1.96*sqrt( mean(temp)*( 1-mean(temp) ) ) / sqrt(n[2])
  longitud <- b - a
  longitudes[i] <- longitud
  if ( p[2] >= a & p[2] <= b) sum = sum + 1 
}

print(sum/2000)
print(mean(longitudes))

longitudes <- c()
sum <- 0
for (i in 1:2000){ # n = 100, p = 0.50
  temp <- rbinom(n[3],1,p[2])
  a <- mean(temp) - 1.96*sqrt( mean(temp)*( 1-mean(temp) ) ) / sqrt(n[3])
  b <- mean(temp) + 1.96*sqrt( mean(temp)*( 1-mean(temp) ) ) / sqrt(n[3])
  longitud <- b - a
  longitudes[i] <- longitud
  if ( p[2] >= a & p[2] <= b) sum = sum + 1 
}

print(sum/2000)
print(mean(longitudes))
