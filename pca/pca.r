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

pca_result <- prcomp(data, scale. = TRUE)
summary(pca_result)
