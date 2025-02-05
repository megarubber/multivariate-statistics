raw_data1 <- read.csv("./dados1_correspondencia.csv", header=T, sep=";")
#raw_data2 <- read.csv("./dados2_correspondencia.csv", header=T, sep=";")

dados <- data.frame(
  raw_data$menos_2000,
  raw_data$entre_2000_5000,
  raw_data$mais_5000
)

tabela <- table(dados$menos_2000,dados$mais_5000)

chisq.test(tabela)
