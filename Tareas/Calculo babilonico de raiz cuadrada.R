raiz.iterative <- function(n, b, a, e){
  nb <- (b+a)/2
  na <- n/nb
  if(na <= nb+e & na >= nb-e) return(nb)
  #Calcular el error y que este por debajo del umbral.
  else return(raiz.iterative(n, nb, na, e))
}

raiz <- print(function(n, e) raiz.iterative(n, n, 1, e))