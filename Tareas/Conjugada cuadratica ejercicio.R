raiz_b <- function(a, b, c) sqrt(b^2-4*a*c)
q_mas <- function(a, b, c) (-4*a*c)/(2*a*raiz_b(a, b, c)+2*a*b)
q_menos <- function(a, b, c) (-b-raiz_b(a,b,c))/(2*a)
#Verificar el denominador con una estructura
q_menos_que_estalla <- function(a, b, c) (-1*((-4*a*c)/(2*a*raiz_b(a, b, c)-2*a*b)))
