# Calcule a media dos tempos do pendulo e insira em uma nova coluna tmedia no data frame
data.Pendulo$tmedia <- (data.Pendulo$t1 + data.Pendulo$t2 + data.Pendulo$t3) / 3

# Realize a regressao linear tmedia vs. comprimento
lm.pendulo <- lm(data.Pendulo$tmedia ~ data.Pendulo$comprimento)

# Desenhe o grafico tmedia em funcao do comprimento
plot(data.Pendulo$comprimento, data.Pendulo$tmedia, main="Comprimento x Período", xlab="L(m)", ylab="T(s)")

# Insira a linha da regressao linear ao grafico
abline(lm.pendulo)

# Se voce olhar mais a fundo a regressao linear no terminal do R...
#
# > lm.pendulo
# Call:
# lm(formula = data.Pendulo$tmedia ~ data.Pendulo$comprimento)
#
# Coefficients:
#             (Intercept)  data.Pendulo$comprimento
#                   2.829                     5.356
#
# A reta desenhada sera Y = 2.829 + 5.356x
reta_lm <- function(x)
{
  return(2.829+(5.356*x))
}

# Peguei dois pontos da reta e calculei as abscissas
x1 <- 0.2
x2 <- 1.0
y1 <- reta_lm(x1)
y2 <- reta_lm(x2)

# > x1
# [1] 0.2
# > y1
# [1] 3.9002
# > x2
# [1] 1
# > y2
# [1] 8.185

# alpha <- Dy^2 / Dx
alpha <- ((y2-y1)*(y2-y1))/(x2-x1)

# > alpha
# [1] 22.94939

# alpha = (4 * pi^2) / g, logo
# g = (4 * pi^2) / alpha
G <- (4 * pi^2) / alpha

# > G
# [1] 1.720238
# E nao faz sentido
