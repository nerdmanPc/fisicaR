# Massa do pendulo (Kg)
pendulo.massa <- 0.0671

# Angulo de inclinacao do pendulo (rad)
theta <- (5*pi)/180

# Valor da aceleracao da gravidade na UFBA
G <- 9.7833

# Banco de dados do experimento
pendulo.data <- read.csv(file="pendulo.csv",sep=";")

# Adicione uma coluna com as medias dos tempos
pendulo.data$tmedia <- (pendulo.data$t1 + pendulo.data$t2 + pendulo.data$t3)/3

# Funcao horaria do pendulo em funcao do tempo:
# x(t) = A * cos(w*t + o) onde:
# A: amplitude; w:vel.angular; o:var. mudanca de fase
# Vamos construir as funcoes com base nas medidas da ultima iteracao
# Tomando o maior comprimento de pendulo

# Calculando amplitude
# A = L * sin(theta)
A <- pendulo.data$comprimento[10] * sin(theta)

# Calculando velocidade angular
# w = (2*pi)/T
w = (2*pi)/pendulo.data$tmedia[10]

# Quanto a variavel de mudanca de fase
# Vamos assumir um valor desprezivel (proxima de zero)

# De posse desses dados, podemos construir a funcao da posicao em funcao do tempo:
pendulo.Posicao <- function(t){
  return(A*cos(w*t))
}

# Para obter as funcoes que descrevem a velocidade e a aceleracao em funcao do tempo
# Pode-se derivar a funcao pendulo.Posicao()

# No terminal do R, executou-se as linhas de codigo abaixo
# para obtencao das derivadas.
# D(expression(A*cos(w*t)),"t")
# D(expression(-(A*w*sin(w*t))),"t")

# Retornam, respectivamente:
# -(A * (sin(w * t) * w))
# -(A * w * (cos(w * t) * w))

# Numericamente as funcoes horarias sao:
# x(t) = 0.0939*cos(0.7531*t)
# v(t) = -0.0707*sin(0.7531*t)
# a(t) = -0.0533*cos(0.7531*t)

# Copiou-se e usou-se o resultado para construcao das outras funcoes:
pendulo.Velocidade <- function(t){
  return(-(A * (sin(w * t) * w)))
}

pendulo.Aceleracao <- function(t){
  return(-(A * w * (cos(w * t) * w)))
}

# Desenhe cada um dos graficos separadamente
jpeg("pendulo_posicao.png")
plot(pendulo.Posicao, col="red", main="Pêndulo: Posição x Tempo", xlab="Tempo(s)", ylab="Posição(m)", xlim=c(0,10))
dev.off()

jpeg("pendulo_velocidade.png")
plot(pendulo.Velocidade, col="green", main="Pêndulo: Velocidade x Tempo", xlab="Tempo(s)", ylab="Velocidade(m/s)", xlim=c(0,10))
dev.off()

jpeg("pendulo_aceleracao.png")
plot(pendulo.Aceleracao, col="blue", main="Pêndulo: Aceleração x Tempo", xlab="Tempo(s)", ylab="Aceleração(m/s²)", xlim=c(0,10))
dev.off()

# Desenhe os graficos das tres funcoes simultaneamente
jpeg("pendulo_funcoes.png")
plot(pendulo.Posicao, col="red", main="Pêndulo: Sobreposicao Posição, Velocidade e Aceleração", xlab="Tempo(s)", ylab="Posição(m) | Velocidade(m/s) | Aceleração(m/s²)", xlim=c(0,10))
plot(pendulo.Velocidade, col="green",  xlim=c(0,10), add=T)
plot(pendulo.Aceleracao, col="blue",  xlim=c(0,10), add=T)
legend("bottomright", c("Posição", "Velocidade", "Aceleração"), cex=0.8, col=c("red", "green", "blue"), lty=1)
dev.off()

# Executando summary() para Posicao, Velocidade e Aceleracao nos tempos t=0:10 tem-se que:
# max_pendulo.Posicao = 0.09390m (Amplitude do movimento)
# max_pendulo.Velocidade = 0.069377m/s (Velocidade maxima do movimento)
# max_pendulo.Aceleracao = 0.052856m/s^2 (Maxima aceleracao do movimento)

# Defina uma funcao para calcular a Energia Cinetica do pendulo em funcao do tempo
pendulo.Ecin <- function(t){
  vel <- pendulo.Velocidade(t)
  return ((pendulo.massa*(vel^2))/2)
}

# Com o angulo theta e a velocidade maxima
# podemos extrair a energia potencial maxima e a energia cinetica maxima
# Quanto a energia potencial, tomamos como referencia um ponto extremamente proximo do solo
# quando o pendulo se encontra em repouso. Sejam:
# * h0 a altura do pendulo em relacao ao solo quando em repouso
# * Li a altura do pendulo em relacao ao solo quando distendido
# Nesse referencial, h0 -> 0
# Com isso, Epot = m*g*h, onde h = Li + ho
# Como h0 ->0 tempos Epot = m*g*Li
# Conforme mostrado em sala, Li=L*(1-cos(theta)), portanto
# Epot = m*g*L*(1-cos(theta))

# Em relacao aos valores da 10a iteracao do experimento
# Energia potencial maxima do sistema:
pendulo.maxEpot <- pendulo.massa * G * pendulo.data$comprimento[10] * (1 - cos(theta))
# Energia cinetica maxima do sistema:
pendulo.maxEcin <- (pendulo.massa * (0.069377^2))/2

# Imprima na tela os valores
print("pendulo.maxEpot")
print(pendulo.maxEpot)
print("pendulo.maxEcin")
print(pendulo.maxEcin)

# Desenhe o grafico da energia cinetica
jpeg("pendulo_ecin.png")
plot(pendulo.Ecin, col="red", main="Pêndulo: Energia Cinética x Tempo", xlab="Tempo(s)", ylab="Energia Cinética(J)", xlim=c(0,10))
dev.off()

# Duvida para discussao:
# Discrepancia entre valores de energia: porque nao sao iguais? Ha algo errado?
