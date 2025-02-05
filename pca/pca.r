install.packages("ggplot2")
library(ggplot2)
library(ggfortify)

raw_data<-read.csv2("./diabetes_prediction_dataset.csv",header=T)
data <- data.frame(
	#as.numeric(raw_data$age),
	scale(as.numeric(raw_data$heart_disease)),
	scale(as.numeric(raw_data$bmi)),
	scale(as.numeric(raw_data$HbA1c_level)),
	scale(as.numeric(raw_data$blood_glucose_level)),
	scale(as.numeric(raw_data$diabetes))
)

colnames(data) <- c("heart_disease", "bmi", "HbA1c_level", "blood_glucose_level", "diabetes")

covariance_matrix <- cov(data)
eigen_v <- eigen(covariance_matrix)

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
autoplot(
  pca_result, 
  loadings = TRUE, 
  loadings.label = TRUE, 
  loadings.label.size = 3,
  loadings.label.color="blue")

knitr::kable(as.data.frame(summary(pca_result)$importance))
array_to_LaTeX(eigen_values$vectors)
