# Practica 1 - Estadistica descriptiva.

# Ejercicio 1

cancer_pancreas <- read.csv('Debernardi.csv')

diagnosis <- table(cancer_pancreas$diagnosis) / length(cancer_pancreas$diagnosis)
diagnosis # a)
barplot(diagnosis, col = c('blue','green','yellow')) # b)

# Ejercicio 2

titanic <- read.csv('datos_titanic.csv')

# Probabilidad de ser mujer sabiendo que sobrevivio.
# A = ser mujer
# B = sobrevivir
# P(A|B) = P(B|A)*P(A) / P(B)

prob_ser_mujer <- length( titanic$Sex[titanic$Sex == 'female']) / length( titanic$Sex )
prob_sobrevivir <- length( titanic$Survived[ titanic$Survived == 1 ]) / length( titanic$Survived) # 1 for survived
prob_sobrevivir_siendo_mujer <- length( titanic$Sex[ titanic$Survived == 1 & titanic$Sex == 'female']) / length(titanic$Sex[titanic$Sex == 'female'])

prob_ser_mujer_habiendo_sobrevivido <- prob_sobrevivir_siendo_mujer * prob_ser_mujer / prob_sobrevivir
prob_ser_mujer_habiendo_sobrevivido # a)

# Pclass (clase del ticket) 1: Primera clase, 2: Segunda clase, 3: Tercera clase.
contingency_table <- table(titanic$Survived,titanic$Pclass)
contingency_table # b)
barplot(contingency_table, col = c(3,4), legend = TRUE, beside = TRUE, args.legend = list(x='top')) # c)

# Ejercicio 3

rodio <- read.table('rodio.txt', header=TRUE)
rodio <- rodio[,1]
iridio <- read.table('iridio.txt', header=TRUE)
iridio <- iridio[,1]

# a)

hist(rodio) # Toma valores mas chicos, concentrados los datos a izquierda
hist(iridio) # Valores mas grandes y con mayor presencia de datos a izquierda, "uniforme" en dicho intervalo

boxplot(rodio,iridio, names = c('Rodio','Iridio'), col = c('red','green'))

# b)

mean(rodio)
mean(iridio)

median(rodio)
median(iridio)

mean(rodio,0.1) # Media podada 10%
mean(rodio,0.2) # Media podada 20%

mean(iridio,0.1) # Media podada 10%
mean(iridio,0.2)# Media podada 20%

# Valores mas chicos para el rodio

# c)

sd(rodio) # Desvio estandar mas alto
sd(iridio) # sd mas bajo

IQR(rodio) # Rango mas grande
IQR(iridio) # Rango mas chico

mad(rodio) # Valor mas alto
mad(iridio) # Valor mas bajo

# d)

quantile(rodio,0.9)
quantile(rodio,0.75)
quantile(rodio,0.5)
quantile(rodio,0.25)
quantile(rodio,0.1)


quantile(iridio,0.9)
quantile(iridio,0.75)
quantile(iridio,0.5)
quantile(iridio,0.25)
quantile(iridio,0.1)

# Ejercicio 4

salchichasA <- read.table('salchichas_A.txt', header = TRUE)
salchichasB <- read.table('salchichas_B.txt', header = TRUE)
salchichasC <- read.table('salchichas_C.txt', header = TRUE)

# a)
# Histogramas Calorias 
hist(salchichasA[,1]) # Tres grupos, el del medio uniforme.
hist(salchichasB[,1]) # Un solo grupo, distribucion casi uniforme con los valores mas grandes. Valores mas altos
hist(salchichasC[,1]) # Un grupo con dos picos. Rango de valores mas chico

# b)
# Histogramas Sodio
hist(salchichasA[,2]) # Dos grupos, mayoria de valores en el rango 300 a 500 (bastante uniforme)
hist(salchichasB[,2]) # Outlier a izquierda con valor alrededor de 100. Mayoria de datos concentrados a derecha
hist(salchichasC[,2]) # Dos grupos igualmente distribuidos. Exactamente mismo patron/forma.

# c)

# No se observan los grupos como antes. Salchichas A y Salchichas B en el histograma se ven considerablemente distintos 
# pero en el rango de valores son similares. En el boxplot se ven casi iguales.
# Salchichas C es el que se nota mas la diferencia por tomar valores mas chicos con respecto a los otros dos (menor cantidad
#de calorias)

boxplot(salchichasA[,1],salchichasB[,1],salchichasC[,1], col = c('blue','red','green'), names = c('Salchichas A', 'Salchichas B', 'Salchichas C'))

# Ejercicio 5

estudiantes <- read.table('estudiantes.txt',header =TRUE)

# a)

grupo1 <- estudiantes[,1]
grupo2 <- estudiantes[,2]

hist(grupo1, main = 'Grupo1') # Grupo 1. Distribucion normal

x1_values <- seq(min(grupo1), max(grupo1), length = 100)
y1_values <- dnorm(x1_values, mean = mean(grupo1), sd = sd(grupo1)) 
y1_values <- y1_values * diff(hist(grupo1)$mids[1:2]) * length(grupo1) 
lines(x1_values, y1_values, lwd = 2)

hist(grupo2, main = 'Grupo2') # Grupo 2. Distribucion normal

x2_values <- seq(min(grupo2), max(grupo2), length = 100)
y2_values <- dnorm(x2_values, mean = mean(grupo2), sd = sd(grupo2)) 
y2_values <- y2_values * diff(hist(grupo2)$mids[1:2]) * length(grupo2) 
lines(x2_values, y2_values, lwd = 2)

qqnorm(estudiantes[,1])
qqline(estudiantes[,1], col = 'red')

qqnorm(estudiantes[,2])
qqline(estudiantes[,2], col = 'red')

# b)
# La media y mediana para cada grupo presentan valores muy similares.
mean(grupo1)
mean(grupo2)

median(grupo1) 
median(grupo2)

# Ambos presentan desviacion estandar relativamente baja
sd(grupo1)
sd(grupo2)

# El rango de valores del grupo 2 es mas extenso. 
IQR(grupo1)
IQR(grupo2)

# Ambos grupos de datos representan una distribucion normal pero el grupo 1 posee valores mas bajos
# comparado con el grupo 2. Ademas de que el grupo 1 presenta un rango de valores mas acotado.
boxplot(grupo1,grupo2, names = c('Grupo1', 'Grupo2'), col = c(3,4))

# Ejercicio 6
# Nubes no tratados = Nubes 'Control'
# Cantidad de agua caida para cada tipo de nube.

nubes <- read.table('nubes.txt', header = TRUE)
nubes_control <- nubes[,1]
nubes_tratadas <- nubes[,2]

# a)

boxplot(nubes_control,nubes_tratadas, names = c('Nubes control','Nubes tratadas'), col = c('yellow','green'))

# b)

# Se observa que para las nubes tratadas la cantidad de agua caida es mayor en ellas, con respecto a aquelas no tratradas (nubes control)

hist(nubes_control)
hist(nubes_tratadas)

# En ninguno de los dos histogramas parece haber distribucion normal de los datos.

qqnorm(nubes_control)
qqline(nubes_control, col = 'red')

qqnorm(nubes_tratadas)
qqline(nubes_tratadas, col = 'red')

# Tampoco se observa que el qqplot represente una distribucion normal de los datos (No ajusta bien la recta)

# c)

transf_log_control <- log(nubes_control)
transf_log_tratadas <- log(nubes_tratadas)

hist(transf_log_control) # Se puede ver un indicio de distribucion normal, pero no tan notorio como las nubes tratadas.
hist(transf_log_tratadas) # Se observa claramente una distribucion normal.

qqnorm(transf_log_control)
qqline(transf_log_control, col = 'red') # La linea parece ajustar mejor que en las nubes tratadas

qqnorm(transf_log_tratadas) 
qqline(transf_log_tratadas, col = 'red') # A pesar de verse claramente la distribucion normal en el histograma, la linea no parece ajustar "tan bien"

boxplot(transf_log_control, transf_log_tratadas, names = c('Nubes control (log)', 'Nubes tratadas (log)'), col = c(3,4))

# Con los boxplots de ambos tipos de nubes pero en escala logaritmica se observan menos datos atipicos que en el item a)
# Ademas se puede comparar mucho mejor que en el otro caso, pues los numeros son mas chicos y las cajas no estan demasiado aplastadas y concentradas en valores bajos.

# Ejercicio 7

cpu <- read.table('cpu.txt', header = TRUE)

# a)

cpu_list <- cpu[,1]

mean(cpu_list)
median(cpu_list)
mean(cpu_list,0.1)

# b)

sd(cpu_list)
IQR(cpu_list)
mad(cpu_list)

# c)

hist(cpu_list) # Cola a derecha, datos concentrados en valores chicos
plot(density(cpu_list)) 
boxplot(cpu_list) # Varios valores atipicos cercanos al valor 20 

# d)

# No parece ser una distribucion normal, a medida que me alejo por derecha hay menor cantidad de datos.
# Aunque podria ser algo asi como media distribucion normal.

# No se ve un patron de que la distribucion sea normal. El qqplot muestra una curva, i.e. no una recta (indicio distribucion normal)
qqnorm(cpu_list)
qqline(cpu_list, col = 'red')

# e) ---> Como la distribucion de los datos presenta varios outliers a derecha a medida que nos alejamos, la mediana
# resulta mas apropiada para describir el centro de los datos pues no es tan sensible a valores atipicos como la media.

# Ejercicio 8

islander <- read.csv('Islander_data.csv')

# a)

hist(islander$Diff) # Distribucion muy similar a la normal

# b) -- Estimacion P(diff <= 1)

menores_a_1 <- islander$Diff[ islander$Diff <= 1]
length(menores_a_1) / length(islander$Diff) # Casos menores o iguales a 1 sobre el total de los datos.

# c) -- Funcion de distribucion empirica

plot(ecdf(islander$Diff), col = 'blue')

# d)
# Estimacion de la densidad de DIFF usando estimadores basados en nucleos, utilizando
# diferentes ventanas ( h = 0.5, 1.5, 2.5 ) y nucleos ( rectangular, gaussiano y de Epanechnikov )
# Que se observa?

# Demasiado "serruchado" con valores de ventana bajos,
# mejora al aumentar el valor de h ( se suaviza para h = 1.5 y 2.5 pero siguen siendo picos agudos ).
plot( density( islander$Diff, kernel = 'rectangular', width = 0.5 )  )
plot( density( islander$Diff, kernel = 'rectangular', width = 1.5 )  )
plot( density( islander$Diff, kernel = 'rectangular', width = 2.5 )  )

# Analogo con rectangular para valores de h = 0.5 y 1.5 pero mucho mas suave para h = 2.5
plot( density( islander$Diff, kernel = 'gaussian', width = 0.5 )  )
plot( density( islander$Diff, kernel = 'gaussian', width = 1.5 )  )
plot( density( islander$Diff, kernel = 'gaussian', width = 2.5 )  )

# Muy similar a gaussian, picos agudos para h = 0.5 y 1.5, pero muy suave para h = 2.5 (casi igual que gaussian)
plot( density( islander$Diff, kernel = 'epanechnikov', width = 0.5 )  )
plot( density( islander$Diff, kernel = 'epanechnikov', width = 1.5 )  )
plot( density( islander$Diff, kernel = 'epanechnikov', width = 2.5 )  )

# Ejercicio 9

# Considerar nuevamente el conjunto de datos del ejercicio 1 --> cancer_pancreas

# a)

# Diagnosis:
# 1: Control (sin enfermedad pancreatica)
# 2: Enfermedad hepatobiliar benigna
# 3: Adenocarcinoma pancreatico ductal (i.e. cancer de pancreas)

# LYVE1: Niveles urinarios en ng/ml del receptor 1 de hialuronato linfatico endotelial,
# una proteina que puede desempeñar un rol en la metastasis tumoral

diagnosis1 <- cancer_pancreas[ cancer_pancreas$diagnosis == 1,]
diagnosis2 <- cancer_pancreas[ cancer_pancreas$diagnosis == 2,]
diagnosis3 <- cancer_pancreas[ cancer_pancreas$diagnosis == 3,]

hist(diagnosis1$LYVE1) # Valores concentrados a izquierda, cola a derecha con menor cantidad de datos.
hist(diagnosis2$LYVE1) # Analogo a diagnosis1 pero con mas valores a derecha del pico 
hist(diagnosis3$LYVE1) # Distribucion casi uniforme para valores chicos de LYVE1,
# no tiene una cola tan definida como los anteriores. Presenta al menos un outlier.
# Gran cantidad de valores en el rango 0 a 10.

# b)

plot( ecdf(diagnosis1$LYVE1), col = 'blue', xlab = 'LYVE1 (ng/ml)'  )
lines( ecdf(diagnosis2$LYVE1), col = 'purple' )
lines( ecdf(diagnosis3$LYVE1), col = 'red' )
legend( 'topleft', legend = c('Diagnosis 1','Diagnosis 2','Diagnosis 3'), col = c("blue","purple","red"), cex = 0.6, lty = 1 )

# Afirmacion: “los valores de la variable LYVE1 tienden a ser mas altos entre quienes tienen
# cancer de pancreas que entre quienes sufren otras enfermedades asociadas al pancreas”.

# Dicha afirmacion segun las funciones de distribucion empirica se puede deducir que es verdadera pues:
# La curva roja representa a aquellos que tienen cancer de pancreas y al estar por debajo de las demas
# se deduce que para valores mas chicos los valores de LYVE1 para los otros diagnosticos son mas probables,
# a comparacion del diagnostico3 (cancer) que indica mayor presencia de valores mas elevados de LYVE1.

# c)

# Afirmacion: “en terminos generales, el sexo del paciente no afecta los niveles
# de la proteina que se mide en la variable LYVE1”

# La afirmacion es falsa al menos para los datos del diagnostico 1 y 2, puesto que se observa
# en ambos casos que los niveles de LYVE1 son mayores en hombres que mujeres.
# Presentan un rango de valores mas amplio que las mujeres.
# A su vez se observan una gran cantidad de outliers en mujeres para niveles de LYVE1 mas altos
# en comparacion con los hombres.

# Para el caso de diagnostico 1 (cancer) se observa que los niveles tanto para hombres como para mujeres
# son muy similares. Con solo un caso atipico para los hombres.

boxplot(diagnosis1$LYVE1 ~ diagnosis1$sex, col = c('pink','lightblue'))
boxplot(diagnosis2$LYVE1 ~ diagnosis2$sex, col = c('pink','lightblue'))
boxplot(diagnosis3$LYVE1 ~ diagnosis3$sex, col = c('pink','lightblue'))

# d)

# Se observa en el grafico de las densidades estimadas superpuestas que para el caso de Diagnosis 1
# la mayoria de los datos se encuentra concentrada en niveles bajos de LYVE1 y muy pocos en valores altos.
# Para el caso de Diagnosis 2, hay menor cantidad de valores en niveles bajos pero aun asi presenta tambien
# pocos valores en niveles altos (aunque mayor cantidad en comparacion con diagnosis 1).
# Ademas ambos diagnosticos (1 y 2) presentan una distribucion similar (en forma) a comparacion del diagnosis 3.
# Y finalmente para el caso de Diagnosis 3 (cancer) se puede ver una distribucion totalmente distinta a los otros
# casos. La mayoria de los datos presenta niveles mas elevados con respecto a los otros diagnosticos.
# El grueso de los datos se concentra entre 2 y 6. Indicando que presenta mayor cantidad de datos
# con niveles altos de LYVE1 y una muy baja cantidad de ellos en niveles bajos.
# No disminuyen considerablemente los datos para niveles altos de LYVE1 como se observa en los demas diagnosticos. 

plot( density(diagnosis1$LYVE1), col = 'blue' )
lines( density(diagnosis2$LYVE1), col = 'purple' )
lines( density(diagnosis3$LYVE1), col = 'red' )
legend( 'topright', legend = c('Diagnosis 1', 'Diagnosis 2', 'Diagnosis 3'), col = c('blue', 'purple','red'), lty = 1, cex=0.8 )

