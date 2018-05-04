# Importar os dados do repositório como "data_pendulo"

# Calcule a media dos tempos do pendulo e insira em uma nova coluna tmedia no data frame
data_pendulo$`Periodo (s)` <- (data_pendulo$`Tempo1 (s)` + data_pendulo$`Tempo2 (s)` + data_pendulo$`Tempo3 (s)`) / 15

# Cria uma coluna com os tempos linearizados( T²(L) )
data_pendulo$`Periodo2 (s)` <- data_pendulo$`Periodo (s)` * data_pendulo$`Periodo (s)`

# Realize a regressao linear T² vs. L
lm_pendulo <- lm(data_pendulo$`Periodo2 (s)` ~ data_pendulo$`comprimento (m)` )

# Desenhe o grafico T em funcao de L
plot(data_pendulo$`comprimento (m)`, data_pendulo$`Periodo (s)`, main="Comprimento x Período", xlab="L(m)", ylab="T(s)")

# Desenhe o gráfico T² em função de L
plot(data_pendulo$`comprimento (m)`, data_pendulo$`Periodo2 (s)`, main="Comprimento x Período²", xlab="L(m)", ylab="T²(s)")

# Insira a linha da regressao linear ao grafico
abline(lm_pendulo) 
