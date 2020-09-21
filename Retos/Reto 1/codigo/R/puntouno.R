library("Rmpfr")
library("ggplot2")
library("reshape2")
setwd("C:/Users/jhvan/OneDrive - Pontificia Universidad Javeriana/2020-03/Analisis Numerico/Reto 1/codigo")
source("./methods.R")
source("./utils.R")

polinomios.generador <- function(grado) {
    coeficientes <- runif(grado, min = -1000, max = 1000)
    coeficientes <- ceiling(coeficientes)
    return(coeficientes)
}

polinomios.presentador <- function(polinomio) {
    polinomio_string <- "f -> "
    i <- length(polinomio) - 1
    for (p in polinomio) {
        # print(polinomio[j])
        if (p > 0) {
            polinomio_string <- paste(polinomio_string, "+", abs(p), "*x^", i, " ", sep = "")
        } else {
            polinomio_string <- paste(polinomio_string, "-", abs(p), "*x^", i, " ", sep = "")
        }
        i <- i - 1
    }
    # polinomio_string <- paste(polinomio_string, polinomio[length(polinomio)], sep = "")
    return(polinomio_string)
}

experimentoA <- function() {
    print("Iniciando experimento")
    coeficientes <- c(1, -5, -9, 155, -250)
    x <- runif(50, min = -50, max = 50)
    tolerancia <- toleranciaFactory(-16)
    coeficientes <- sapply(coeficientes, mpfr, precBits = tolerancia$bits)
    coeficientes <- mpfr2array(coeficientes, dim = length(coeficientes))
    laguerre.r <- c()
    newton.r <- c()
    laguerre.iter <- c()
    newton.iter <- c()
    laguerre.multi <- c()
    newton.multi <- c()
    for (xi in x) {
        xn <- mpfr(xi, tolerancia$bits)
        laguerre <- methods.laguerre(coeficientes, xn, tolerancia$tol)
        newton <- methods.newton_horner(coeficientes, xn, tolerancia$tol)
        laguerre.r <- c(laguerre.r, formatMpfr(laguerre$r))
        newton.r <- c(newton.r, formatMpfr(newton$r))
        laguerre.iter <- c(laguerre.iter, laguerre$iter)
        newton.iter <- c(newton.iter, newton$iter)
        laguerre.multi <- c(laguerre.multi, laguerre$multi)
        newton.multi <- c(newton.multi, newton$multi)
    }
    experimento.data <- data.frame(x, laguerre.iter, laguerre.multi, newton.iter, newton.multi, laguerre.r, newton.r)
    experimento.multi <- data.frame(x = x, laguerre = laguerre.multi, newton = newton.multi)
    df <- melt(experimento.multi ,  id.vars = 'x', variable.name = 'Multiplicaciones', value.name = 'multiplicaciones')
    print(ggplot(df, aes(x, multiplicaciones)) + geom_line(aes(colour = Multiplicaciones), size=1.4) + theme(legend.text=element_text(size=14), legend.title = element_text(size = 16), axis.text = element_text(size = 10), axis.title = element_text(size = 16, face = "bold")))
}

experimentoA()