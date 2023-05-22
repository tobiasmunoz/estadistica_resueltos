grilla <- seq(70,130,0.1)
n <- 16

plot(grilla, 1 - pnorm( qnorm( 1 - 0.05 ) + sqrt(n)*( 100 - grilla)/20 ), col = 'red', xlab = "mu", ylab = "Potencia", type = "l" )
lines(grilla, pnorm( qnorm(0.05) + sqrt(n)*( 100 - grilla)/20 ), col = 'blue' )
lines(grilla, pnorm( qnorm(0.025) + sqrt(n)*( 100 - grilla)/20 ) + 1 - pnorm( qnorm(1 - 0.025) + sqrt(n)*( 100 - grilla)/20 ), col = 'green'  )
legend( 70, 0.40, legend = c("Phi 1","Phi 2","Phi 3"), col = c('red','blue','green'), lty = 1  )