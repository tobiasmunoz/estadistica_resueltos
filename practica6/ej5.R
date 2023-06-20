datos <- read.table('credit.txt', sep = ',', header = TRUE)
datos <- datos[,-1] # Indices que no aportan nada
datos <- datos[, -10] # Elimino la etnia del modelo

# a)

ajuste <- lm( Balance ~., datos)

# b) Predigo el valor para la segunda observacion:

predict( ajuste, newdata = datos[2,]) # Predicho = 913.8 vs Real = 903
predict( ajuste, newdata = datos[2,]) - datos[2,]$Balance # Residuo = 10.8

# c)

summary(ajuste) # Sigma  = 98.72 --> sigma^2 = 9745.6
sigma <- 98.72

# d)

# Variables estadisticamente mas relevantes: 
# Income, cards, Gender, Married, Student.

# e)

# coeficiente de Age = -0.63468
# p-valor para H0: coef = 0 vs H1: coef != 0 ---> 0.031

# f)

# coef de Education = -1.11503
# Intervalo de confianza para el coef de Education de nivel 0.9

D <- solve( t(model.matrix(ajuste)) %*% model.matrix(ajuste) ) # Matriz de diseño

beta_education <- -1.11503

ic_education <- c( abs(beta_education) - qt(1 - 0.01/2, 390)*sigma*sqrt(D[7,7]), abs(beta_education) + qt(1 - 0.01/2, 390)*sigma*sqrt(D[7,7]) )

# g)

# Si comparten las mismas variablex explicativas a excepcion de la edad, se tiene que

# Bj - Bv = beta_edad*( edadJ - edadV ) = beta_edad*( edadJ - (edadJ+3) ) = beta_edad*(-3) = -0.63468*(-3) = 1.90404

# Pues se anulan: la intercept y los terminos que no contienen a la edad con sus respectivos coeficientes.

# Si sigma fuese conocida entonces Bj - Bv distribuye Normal( 0, sigma^2 * 2 * diag_age ) 

# Como es desconocida distribuye como una student.

beta_age <- -0.63468
  
ic_diferencia <- c( beta_age - qt(1 - 0.05/2, 390)*sigma*sqrt(2*D[6,6]), beta_age + qt(1 - 0.05/2, 390)*sigma*sqrt(2*D[6,6]) ) # Nivel 0.95 