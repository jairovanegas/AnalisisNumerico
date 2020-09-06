q_test <- function(x) ((x^51)-1)/(x-1)
horner <- function(x, coeficientes) {
  acumulado <- 1
  multiplicaciones <- 0
  for(i in 1:length(coeficientes)){
    acumulado = (acumulado * x) + coeficientes[i]
    multiplicaciones = multiplicaciones + 1
  }
  cat("Algoritmo completado con ", multiplicaciones, " multiplicaciones: ", acumulado, "\n")
  return(acumulado)
}
h_test <- function(x) horner(x, rep(1, 50))
test <- function(floor, roof, step){
  e <- seq(floor, roof, step)
  n <- seq(floor, roof, step)
  for(i in 1:length(n)){
    e[i] <- abs(q_test(n[i]) - h_test(n[i]))
  }
  plot(n, e)
}
test(0, 3, 0.0001)
