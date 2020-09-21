methods.horner <- function(coeficientes, x) {
    n <- length(coeficientes) - 1
    p <- coeficientes[1]
    d1 <- 0
    d2 <- 0
    multi.p <- 0
    multi.d1 <- 0
    multi.d2 <- 0
    for (i in 1:n) {
        d2 <- d2 * x + 2 * d1
        multi.d2 <- multi.d2 + 2
        d1 <- d1 * x + p
        multi.d1 <- multi.d1 + 1
        p <- p * x + coeficientes[i + 1]
        multi.p <- multi.p + 1
    }
    return(list(
        "p" = p,
        "d1" = d1,
        "d2" = d2,
        "multi.p" = multi.p,
        "multi.d1" = multi.p + multi.d1,
        "multi.d2" = multi.p + multi.d1 + multi.d2
    ))
}

methods.laguerre <- function(coeficientes, x, tolerancia) {
    multi <- 0
    i <- 0
    p <- 10000
    n <- length(coeficientes) - 1
    while (abs(p) > tolerancia) {
        horner <- methods.horner(coeficientes, x)
        p <- horner$p
        pD1 <- horner$d1
        pD2 <- horner$d2
        multi <- multi + horner$multi.d2
        G <- pD1 / p
        H <- G^2 - (pD2 / p)
        factorInterno <- (n - 1) * ((n * H) - G^2)
        raiz <- sqrt(as.complex(as.numeric(factorInterno)))
        x2 <- 0
        if (abs(G + raiz) > abs(G - raiz)) {
            x2 <- x - (n / (G + raiz))
        } else {
            x2 <- x - (n / (G - raiz))
        }
        x <- x2
        i <- i + 1
        horner <- methods.horner(coeficientes, x)
        p <- horner$p
        multi <- multi + horner$multi.p
    }
    return(list(
        "r" = x,
        "iter" = i,
        "multi" = multi
    ))
}

methods.newton_horner <- function(coeficientes, x, tolerancia) {
    i <- 0
    multi <- 0
    p <- 10000
    while (abs(p) > tolerancia) {
        horner <- methods.horner(coeficientes, x)
        p <- horner$p
        pD1 <- horner$d1
        multi <- multi + horner$multi.d1
        x2 <- x - (p / pD1)
        x <- x2
        horner <- methods.horner(coeficientes, x)
        p <- horner$p
        multi <- multi + horner$multi.d1
        i <- i + 1
    }
    return(list(
        "r" = x,
        "iter" = i,
        "multi" = multi
    ))
}