install.packages("ggplot2")
library(ggplot2)
library(ggfortify)

raw_data<-read.csv2("./diabetes_prediction_dataset.csv",header=T)
data <- data.frame(
  scale(as.numeric(d$age)),
  scale(as.numeric(d$hypertension)),
  scale(as.numeric(d$heart_disease)),
  scale(as.numeric(d$bmi)),
  scale(as.numeric(d$HbA1c_level)),
  scale(as.numeric(d$blood_glucose_level)),
  scale(as.numeric(d$diabetes))
)

colnames(data) <- c("age", "hypertension", "heart_disease", "bmi", "HbA1c_level", "blood_glucose_level", "diabetes")

covariance_matrix <- cov(data)
eigen_v <- eigen(covariance_matrix)

round(eigen_v$vectors,3)

array_to_LaTeX(round(eigen_v$vectors,3))

array_to_LaTeX <- function(arr){
  rows <- apply(arr, MARGIN=1, paste, collapse = " & ")
  matrix_string <- paste(rows, collapse = " \\\\ ")
  return(paste("\\begin{bmatrix}", matrix_string, "\\end{bmatrix}"))
}

for (i in 1:ncol(eigen_v$vectors)) {
  latex_eq <- paste0("PC_", i, " = ", 
                     paste0(sprintf("%.3f", eigen_v$vectors[, i]), "X_", 1:ncol(eigen_v$vectors), collapse = " + "))
  cat("\\[ ", latex_eq, " \\] \n")
}

ggplot(as.data.frame(pca_result$x))

pca_result <- prcomp(data, center = TRUE, scale. = TRUE)
summary(pca_result)

autoplot(
  pca_result, 
  loadings = TRUE, 
  loadings.label = TRUE, 
  loadings.label.size = 3,
  loadings.label.color="blue")

knitr::kable(as.data.frame(summary(pca_result)$importance))
