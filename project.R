## Reading the Data Set
boa.data <- read.csv("BOA.csv.csv")

##Using Dim, Summary and Str to read the data
dim(boa.data)
summary(boa.data)
str(boa.data)

## Coverting CUST_ID into Factor
boa.data$CUST_ID <- factor(boa.data$CUST_ID)

## Visualizing Purchases using Histogram
hist(boa.data$PURCHASES, 
     col = "skyblue", 
     border = "black",
     main = "Distribution of Purchases",
     xlab = "Purchases Amount",
     ylab = "Frequency")

## Removing CustID
boa_data <- boa.data[, -2]

## Using Library Cluster cause of mixed data types
library(cluster)
boa.dist <- daisy(boa_data)
## Clustering Using Hclust
boa.hc<-hclust(boa.dist,method="complete")
boa.hc
## Plotting
plot(boa.hc)
plot(cut(as.dendrogram(boa.hc),h=50000)$lower[[1]])


## Using K-means
set.seed(123)
boa_kmeans <- kmeans(boa_data, centers = 4)
boa.summ<-function(data,groups){
  aggregate(data,list(groups),function(x)mean(as.numeric(x)))
}

boa.summ(boa_data,boa_kmeans$cluster)

## Plotting the data

library(cluster)
clusplot(boa_data, boa_kmeans$cluster, color = TRUE, shade = TRUE,
         labels = 5, main = "K-means Cluster Plot")

## Using MClust for 5 groups

library(mclust)
boa.data.new <- boa.data
boa.mc<-Mclust(boa.data.new) #use all defaults
summary(boa.mc)

#Mclust for 4 groups
boa.data5<-Mclust(boa.data.new,G=4) 
summary(boa.data5)
BIC(boa.mc,boa.data5)
boa.summ(boa_data,boa.mc$class)

##plot for Mclust model 
clusplot(boa_data,boa.mc$class,color = TRUE,shade = TRUE,labels = 4,
         main = "Model-based cluster plt")

##plot for Mclust model for 4 labels
clusplot(boa_data,boa.data5$class,color = TRUE,shade = TRUE,labels = 4,
         main = "Model-based cluster plt")

