# Importar dataset "data_mru.csv"

data_mru[6, "Sensor1_s"] <- sum(data_mru[1:5, "Sensor1_s"]) / 5
data_mru[6, "Sensor2_s"] <- sum(data_mru[1:5, "Sensor2_s"]) / 5
data_mru[6, "Sensor3_s"] <- sum(data_mru[1:5, "Sensor3_s"]) / 5
data_mru[6, "Sensor4_s"] <- sum(data_mru[1:5, "Sensor4_s"]) / 5

tempos <- unlist(data_mru[6, 1:4])
posicoes <- unlist(data_mru[1, 5:8])
regressao <- lm(formula = posicoes ~ tempos)
velocidade <- regressao$coefficients[2]

plot(tempos, posicoes, main="Função Horária", xlab="Tempo (s)", ylab="Posição (m)")
abline(regressao)