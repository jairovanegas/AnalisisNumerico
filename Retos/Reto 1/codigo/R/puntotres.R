library("Rmpfr")
library("ggplot2")
library("reshape2")
library("pracma")
setwd("C:/Users/jhvan/OneDrive - Pontificia Universidad Javeriana/2020-03/Analisis Numerico/Reto 1/codigo")
source("./methods.R")
source("./utils.R")

# Funcion a aproximar
f <- function(x) exp(sin(x) - cos(x^2))
# Rango en el que se va a aproximar
range <- c(-(2^(-8)), 2^(-8))
# Tolerancia maxima con la que se aproxima la funcion
tol <- mpfr(2^-90, 90)

# Coeficientes del polinomio aproximado por el algoritmo de Remez mejorado
remezPolinomio <- c(
    5320394595779079 * 2^-58,
    5559725200690211 * 2^-59,
    589077943038783 * 2^-57,
    6516674741954513 * 2^-56,
    358969371405011 * 2^-51,
    4970345142530923 * 2^-55,
    0,
    119383704169626743428436621385363 * 2^-109,
    29845926042406685857117349204375 * 2^-106,
    119383704169626743428469396878343 * 2^-108
)
# Convertimos los coeficientes en multi precision
remezPolinomio <- sapply(remezPolinomio, mpfr, precBits = 120)
# Convertimos la lista anterior en un vector
remezPolinomio <- mpfr2array(remezPolinomio, dim = length(remezPolinomio))

# Polinomio de taylor aproximado por la libreria pracma de grado 4
taylorPolinomio <- taylor(f, 0, n = 4)

# Evaluacion de las dos aproximaciones por el algoritmo de Horner
x <- runif(100, min = range[1], max = range[2])
x <- sort(x, decreasing = FALSE)
errorRemez <- c()
errorTaylor <- c()
errorMax <- 0
for (xn in x) {
    xi <- mpfr(xn, 120)
    v <- f(xi)
    r <- methods.horner(remezPolinomio, xi)
    t <- methods.horner(taylorPolinomio, xi)
    errorRemez <- c(errorRemez, formatMpfr(abs(v - r$p)))
    if(errorRemez > errorMax){
        errorMax <- errorRemez
    }
    errorTaylor <- c(errorTaylor, formatMpfr(abs(v - t$p)))
    if(errorTaylor > errorMax){
        errorMax <- errorTaylor
    }
}
comparacion <- data.frame("x" = x, "e.remez" = errorRemez, "e.taylor" = errorTaylor)
plot(comparacion$x, comparacion$e.remez, type = "b", xlab = "x", ylab = "Error Absoluto", main = "Error Absoluto Aproximaciones", ylim = c(0, as.numeric(errorMax)))
#plot(comparacion$x, comparacion$e.taylor, type = "b", xlab = "x", ylab = "Error Absoluto", main = "ErrorAbsoluto Taylor")
lines(comparacion$x, comparacion$e.taylor, type = "b", col = "red")
legend("topright",
      legend = c("Remez", "Taylor"),
      col = c("black", "red"), lty = 1:1, cex = 0.8,
      title = "Leyenda", text.font = 4, bg = "lightblue"
    )
plot(comparacion$x, comparacion$e.remez, type = "b", xlab = "x", ylab = "Error Absoluto", main = "Error Absoluto Remez")
plot(comparacion$x, comparacion$e.taylor, type = "b", xlab = "x", ylab = "Error Absoluto", main = "Error Absoluto Taylor")
print(comparacion)