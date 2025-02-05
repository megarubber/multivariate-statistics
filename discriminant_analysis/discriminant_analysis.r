library(MASS)
library(caret)
# 1 - empresa que faliu
# 2 - empresa que não faliu

data1 <- read.csv("/home/aluno/Downloads/dados_discriminante_grupo1.csv", header=T)
data2 <- read.csv("/home/aluno/Downloads/dados_discriminante_grupo2.csv", header=T)
d1 <- data.frame(
  data1$x1,
  data1$x2,
  data1$x3,
  data1$x4
)
d2 <- data.frame(
  data2$x1,
  data2$x2,
  data2$x3,
  data2$x4
)

cov_1 <- cov(d1[,-5])
cov_2 <- cov(d2[,-5])

t1 <- length(d1[,1]) - 1
t2 <- length(d2[,1]) - 1
matriz_s <- ((t1 * cov_1) + (t2 * cov_2)) / (t1 + t2 - 2)

inversa_matriz_s <- solve(matriz_s)

x1 <- c(mean(d1[,1]), mean(d1[,2]), mean(d1[,3]), mean(d1[,4]))
x2 <- c(mean(d2[,1]), mean(d2[,2]), mean(d2[,3]), mean(d2[,4]))

b_linha = (x1 - x2) %*% inversa_matriz_s
cal <- b_linha * d1[1,]
fischer <- 0
for(i in 1:length(cal)) {
  fischer <- fischer + cal[1, i]
}

v <- (x1 + x2) / 2
c <- v * b_linha
const_delimitacao <- 0
for(i in 1:length(c)) {
  const_delimitacao <- const_delimitacao + c[1, i]
}

# Método da substituição
n_1_2 <- 0
for(i in 1:length(d1[,1])) {
  cal <- b_linha * d1[i,]
  fischer <- 0
  for(j in 1:length(cal)) {
    fischer <- fischer + cal[1, j]
  }
  if(fischer < const_delimitacao) {
    n_1_2 <- n_1_2 + 1
  }
}
p_2_1 <- n_1_2 / t1

n_2_1 <- 0
for(i in 1:length(d2[,1])) {
  cal <- b_linha * d2[i,]
  fischer <- 0
  for(j in 1:length(cal)) {
    fischer <- fischer + cal[1, j]
  }
  if(fischer >= const_delimitacao) {
    n_2_1 <- n_2_1 + 1
  }
}
p_1_2 <- n_2_1 / t2

p_acerto_global <- ((t1 - n_1_2) + (t2 - n_2_1)) / (t1 + t2)
p_acerto_1 <- (t1 - n_1_2) / t1
p_acerto_2 <- (t2 - n_2_1) / t2

# Método da validação cruzada
for(i in (t1 + t2)) {
  
  cov_1 <- cov(d1[,-5])
  cov_2 <- cov(d2[,-5])
  
  t1 <- length(d1[,1]) - 1
  t2 <- length(d2[,1]) - 1
  matriz_s <- ((t1 * cov_1) + (t2 * cov_2)) / (t1 + t2 - 2)
  
  inversa_matriz_s <- solve(matriz_s)
  
  x1 <- c(mean(d1[,1]), mean(d1[,2]), mean(d1[,3]), mean(d1[,4]))
  x2 <- c(mean(d2[,1]), mean(d2[,2]), mean(d2[,3]), mean(d2[,4]))
  
  b_linha = (x1 - x2) %*% inversa_matriz_s
  cal <- b_linha * d1[1,]
  fischer <- 0
  for(i in 1:length(cal)) {
    fischer <- fischer + cal[1, i]
  }
  
  v <- (x1 + x2) / 2
  c <- v * b_linha
  const_delimitacao <- 0
  for(i in 1:length(c)) {
    const_delimitacao <- const_delimitacao + c[1, i]
  }
}

d <- c(data1, data2)

# lda -> função de análise Linear
cruzada <- lda(d[,-4], d[,4], CV = TRUE)
