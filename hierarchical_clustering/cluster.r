library(txtplot)

d <- read.csv("./dados_agrupamento.csv", header=T, sep=",")

data <- data.frame(d$renda, d$idade)

euclidean_distance <- dist(data, method="euclidean")

print(euclidean_distance)

h_clust_avg <- hclust(euclidean_distance, method='average')
plot(h_clust_avg)
