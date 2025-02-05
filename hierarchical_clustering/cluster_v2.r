library(txtplot)

raw_data <- read.csv2("./diabetes_prediction_dataset.csv",header=T)

data <- data.frame(
  as.numeric(raw_data$heart_disease),
  as.numeric(raw_data$bmi),
  as.numeric(raw_data$HbA1c_level),
  as.numeric(raw_data$blood_glucose_level),
  as.numeric(raw_data$diabetes)
)

euclidean_distance <- dist(data, method="euclidean")

print(euclidean_distance)

h_clust_avg <- hclust(euclidean_distance, method='average')
plot(h_clust_avg)

