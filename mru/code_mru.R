data_mru[6, "Sensor1_s"] <- sum(data_mru[1:5, "Sensor1_s"]) / 5
data_mru[6, "Sensor2_s"] <- sum(data_mru[1:5, "Sensor2_s"]) / 5
data_mru[6, "Sensor3_s"] <- sum(data_mru[1:5, "Sensor3_s"]) / 5
data_mru[6, "Sensor4_s"] <- sum(data_mru[1:5, "Sensor4_s"]) / 5

tempos <- c(data_mru[6, 1:4])
posicoes <- c(data_mru[1, 5:8])

plot(tempos, posicoes, main="Função Horária", xlab="Tempo (s)", ylab="Posição (s)")
abline(lm(c(tempos, posicoes)))