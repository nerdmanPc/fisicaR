# ==============================
# Calculos
# ==============================

G <- 9.7833 # Valor da aceleracao da gravidade na UFBA
pendulo.massa <- 0.0671 # Massa do pendulo (Kg)
pendulo.theta <- (5*pi)/180 # Angulo de inclinacao do pendulo (rad)
pendulo.data <- read.csv(file="pendulo.csv",sep=";") # Banco de dados do experimento
pendulo.data$tmedia <- (pendulo.data$t1 + pendulo.data$t2 + pendulo.data$t3)/15 # Adicione uma coluna com as medias dos tempos
pendulo.data$tmedia2 <- pendulo.data$tmedia * pendulo.data$tmedia # Adicione uma coluna com os tempos linearizados
pendulo.data$amplitude <- pendulo.data$comprimento * sin(pendulo.theta) # Adicione uma coluna com as amplitudes {A=L*sin(theta)}
pendulo.data$omega <- (2*pi)/pendulo.data$tmedia # Adicione uma coluna com as velocidades angulares {w=(2*pi)/T}

# Quanto a variavel de mudanca de fase
# Vamos assumir um valor desprezivel (proxima de zero)

# Realize a regressao linear T^2 vs. L
pendulo.lm <- lm(pendulo.data$tmedia2 ~ pendulo.data$comprimento)

# Funcao horaria do pendulo em funcao do tempo definida como:
# x(t) = A * cos(w*t + o) onde:
# A: amplitude; w:vel.angular; o:var. mudanca de fase
# Vamos construir as funcoes com base nas medidas da ultima iteracao
# Tomando o maior comprimento de pendulo (pendulo.data$comprimento[10])

# De posse desses dados, podemos construir a funcao da posicao em funcao do tempo:
pendulo.Posicao <- function(t){
return(pendulo.data$amplitude[10]*cos(pendulo.data$omega[10]*t))
}

# Para obter as funcoes que descrevem a velocidade e a aceleracao em funcao do tempo
# Pode-se derivar a funcao pendulo.Posicao()

# No terminal do R, executou-se as linhas de codigo abaixo para obtencao das derivadas.
# D(expression(A*cos(w*t)),"t")
# D(expression(-(A*w*sin(w*t))),"t")

# Retornam, respectivamente:
# -(A * (sin(w * t) * w))
# -(A * w * (cos(w * t) * w))

# Copiou-se e usou-se o resultado para construcao das outras funcoes:
pendulo.Velocidade <- function(t){
return(-(pendulo.data$amplitude[10]*pendulo.data$omega[10]*sin(pendulo.data$omega[10]*t)))
}

pendulo.Aceleracao <- function(t){
return(-(pendulo.data$amplitude[10]*pendulo.data$omega[10]*pendulo.data$omega[10]*cos(pendulo.data$omega[10]*t)))
}

# A Energia Cinetica eh definida como Ec = (m*v^2)/2
# Onde m:massa do pendulo; v:velocidade
# Defina uma funcao para calcular a Energia Cinetica do pendulo em funcao do tempo
pendulo.Ecin <- function(t){
vel <- pendulo.Velocidade(t)
return((pendulo.massa*(vel^2))/2)
}

# A energia potencial eh definida como Ep = m*g*h
# Onde m:massa do pendulo; g:aceleracao da gravidade: h:altura do pendulo em relacao ao referencial (geralmente, o chao)
# A altura h eh composta de dois componentes:
# * h0: a altura do pendulo em relacao ao solo quando em repouso
# * li: a altura do pendulo em relacao ao solo quando peturbado (i.e, quando deslocado para o inicio do movimento)
# Dai h = h0 + li
# O referencial usado foi um ponto onde, quando em repouso, o pendulo se encontra muito proximo do solo
# Nesse caso, h0 -> 0 (h0 eh um valor muito proximo de zero e, portanto, desprezivel)
# Portanto h = h0. Dai
# Ep = m*g*li, onde li = L*(1-cos(theta)), onde L representa o comprimento do pendulo e theta o angulo de perturbacao
# Substituindo li na formula temos, enfim, que:
# Ep = m*g*L*(1-cos(theta))

# Durante todo o movimento, o angulo do pendulo varia em funcao do tempo e da velocidade angular do mesmo
# Com isso, defina uma funcao para calcular a Energia Potencial do pendulo em funcao do tempo
# (Agradecimentos a Alisson, Fabio, Giusepe e Lucas pela ideia da funcao)
pendulo.Epot <- function(t){
var_theta <- pendulo.theta*cos(pendulo.data$omega[10]*t)
return(pendulo.massa*G*pendulo.data$comprimento[10]*(1-cos(var_theta)))
}

# ==============================
# Desenho dos graficos
# ==============================

# Dados obtidos: T x L
jpeg("pendulo_TxL.png")
plot(pendulo.data$comprimento, pendulo.data$tmedia, main="Pêndulo: Comprimento x Período", xlab="L(m)", ylab="T(s)")
dev.off()

# Dados obtidos: Regressao linear
jpeg("pendulo_lm.png")
plot(pendulo.data$comprimento, pendulo.data$tmedia2, main="Pêndulo: Comprimento x Período²", xlab="L(m)", ylab="T²(s²)")
abline(pendulo.lm)
dev.off()

# Posicao em funcao do tempo
jpeg("pendulo_posicao.png")
plot(pendulo.Posicao, col="red", main="Pêndulo: Posição x Tempo", sub=expression(paste(omega, "=3.765393")), xlab="Tempo(s)", ylab="Posição(m)", xlim=c(0,3))
dev.off()

# Velocidade em funcao do tempo
jpeg("pendulo_velocidade.png")
plot(pendulo.Velocidade, col="green", main="Pêndulo: Velocidade x Tempo", sub=expression(paste(omega, "=3.765393")), xlab="Tempo(s)", ylab="Velocidade(m/s)", xlim=c(0,3))
dev.off()

# Aceleracao em funcao do tempo
jpeg("pendulo_aceleracao.png")
plot(pendulo.Aceleracao, col="blue", main="Pêndulo: Aceleração x Tempo", sub=expression(paste(omega, "=3.765393")), xlab="Tempo(s)", ylab="Aceleração(m/s²)", xlim=c(0,3))
dev.off()

# Posicao + Velocidade + Aceleracao
jpeg("pendulo_funcoes.png")
plot(pendulo.Posicao, col="red", main="Pêndulo: Posição, Velocidade e Aceleração", sub=expression(paste(omega, "=3.765393")), xlab="Tempo(s)", ylab="Posição(m) | Velocidade(m/s) | Aceleração(m/s²)", yaxt='n', xlim=c(0,3), ylim=c(-1.5,1.5))
plot(pendulo.Velocidade, col="green",  xlim=c(0,3), add=T)
plot(pendulo.Aceleracao, col="blue",  xlim=c(0,3), add=T)
legend("bottomright", c("Posição", "Velocidade", "Aceleração"), cex=0.8, col=c("red", "green", "blue"), lty=1)
dev.off()

# Energia Cinetica
jpeg("pendulo_ecin.png")
plot(pendulo.Ecin, col="red", main="Pêndulo: Energia Cinética x Tempo", sub=expression(paste(omega, "=3.765393")), xlab="Tempo(s)", ylab="Energia Cinética(J)", xlim=c(0,3))
dev.off()

# Energia Potencial
jpeg("pendulo_Epot.png")
plot(pendulo.Epot, col="green", main="Pêndulo: Energia Potencial x Tempo", sub=expression(paste(omega, "=3.765393")), xlab="Tempo(s)", ylab="Energia Potencial(J)", xlim=c(0,3))
dev.off()

# Variacao da energia do pendulo
jpeg("pendulo_Energia.png")
plot(pendulo.Ecin, col="red", main="Pêndulo: Variação da Energia do Sistema", sub=expression(paste(omega, "=3.765393")), xlab="Tempo(s)", ylab="Energia(J)", xlim=c(0,3), ylim=c(0,0.005))
plot(pendulo.Epot, col="green", xlim=c(0,3), ylim=c(0,0.005), add=T)
legend("topright", c("Energia Cinética", "Energia Potencial"), cex=0.8, col=c("red", "green"), lty=1)
dev.off()
