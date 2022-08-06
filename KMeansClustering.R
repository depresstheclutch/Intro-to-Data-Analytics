HR_input <- read.csv("HR_Data.csv")

#Format data for use with km function
kmdata <- as.matrix(HR_input[, c("Satisfaction", "Evaluation")])

#Calculate and plot WSS for a series of k values
wss <- numeric(15)
#set iteration to 15 cause 10 didnot cover
for (k in 1:15) wss[k] <- sum(kmeans(kmdata, centers = k, iter.max = 15, nstart = 25)$withinss)
plot(1:15, wss, type = "b", xlab = "Number of Clusters", ylab = "Within Sum of Squares (WSS)")

#Generate and review the results of a k-means analysis with k=3
km <- kmeans(kmdata, 4, nstart = 25)
km
library(ggplot2)
library(grid)
library(gridExtra)
plotdata <- HR_input[, c("Satisfaction", "Evaluation")]
plotdata$Cluster <- km$cluster
centers <- as.data.frame(km$centers)

g1 <- ggplot(data = plotdata, aes(x = Satisfaction, y = Evaluation, color = Cluster)) +
  geom_point() +
  theme(legend.position = "right") +
  geom_point(data = centers,
             aes(x = Satisfaction, y = Evaluation, color = c(1, 2, 3, 4)),
             size = 10, alpha = 0.3, show.legend = FALSE)
g1



#Plot the data
autoplot(km,HR_input,frame=TRUE, label.qty = "Test")
km$centers