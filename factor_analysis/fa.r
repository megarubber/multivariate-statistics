library(psych)
d <- read.csv("./diabetes_prediction_dataset.csv", header=T, sep=";")
data_scaled <- data.frame(
  as.numeric(d$age),
  as.numeric(d$hypertension),
  as.numeric(d$heart_disease),
  as.numeric(d$bmi),
  as.numeric(d$HbA1c_level),
  as.numeric(d$blood_glucose_level)
)

cov_matrix <- cov(data_scaled)
eigen <- eigen(cov_matrix)
cor_matrix <- cor(data_scaled)

a <- fa(data_scaled, nfactors=2)
fl <- factanal(data_scaled, factors = 2) 

# Calculando Matriz Residual
Lambda <- fa$loadings
Psi <- diag(fa$uniquenesses)
S <- fa$correlation
Sigma <- Lambda %*% t(Lambda) + Psi
round(S - Sigma, 6)

'''
array_to_LaTeX(round(matriz_residual, 3))

L_matrix = matrix(0, nrow=nrow(cor_matrix), ncol=2)

for(i in 1:length(nrow(cor_matrix))) {
    L_matrix[, i] <- sqrt(eigen$values[i]) * eigen$vectors[,i]
}

LL_M = matrix(0, nrow=nrow(cor_matrix), ncol=ncol(cor_matrix))

for(i in 1:length(nrow(cor_matrix))) {
  LL_M <- LL_M + (eigen$values[i] * eigen$vectors[,i] * t(eigen$vectors)[,i])
}
'''
