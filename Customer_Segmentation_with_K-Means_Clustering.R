# Codeclause Internship - Task I
## Project Title - Customer Segmentation with K-Means Clustering

## Loading the dataset

customer <- read.csv("C:\\Users\\PREET PAUL\\Desktop\\Presidency University M.Sc. Notes\\Codeclause\\Mall_Customers.csv")
dim(customer)   # dimension of the dataset
View(customer)
str(customer)
summary(customer)   # summary of the dataset

## Checking for Missing Values

sum(is.na(customer))

## Eliminating the "Genre" and "CustomerID" column to perform K-Means Clustering 
## since they are categorical variables

customer <- customer[,-c(1,2)]
View(customer)

## Hence, there are no missing values present in the dataset.

## Loading Required Packages 

library(clValid)
library(cluster)
library(ggplot2)
library(factoextra)

# Performing K-Means Algorithm

## Finding 'K' by Elbow Plot

wssplot <- function(customer, nc = 15, seed = 19)
{
  wss <- (nrow(customer)-1)*sum(apply(customer, 2, var))
  for (i in 2:nc)
  {
    set.seed(seed)
    wss[i] = sum(kmeans(customer, centers = i)$withinss)
  }
  plot(1:nc, wss, type = "b", col = "blue", xlab = "Number of Clusters",
       ylab = "Within Groups Sum of Squares", main = "Elbow Plot of the data")
}

## Plotting the Elbow Plot

wssplot(customer)

## From the plot, we can clearly see that, there is an elbow at 6.
## So, we can select 5 as our number of clusters.

## Performing K-Means

KM <- kmeans(customer, centers = 6, iter.max = 100)
KM

## Vizualizing through Cluster Plot

fviz_cluster(KM, data = customer)


# Cluster Validation Technique

## Dunn Index

data <- dist(customer)   # distance matrix of the dataset

dunn_index <- NULL
for (i in 1:10)
{
  set.seed(19)
  K <- kmeans(customer, centers = i, iter.max = 100)
  dunn_index <- rbind(dunn_index, c(i, dunn(data, K$cluster, method = "euclidean")))
}

colnames(dunn_index) <- c("No. of Clusters", "Dunn Index")
dunn_index

## From the above, we can clearly see that for No. of Clusters = 6, Dunn Index
## will be maximum. 
## Hence, 6 clusters are the best possible clustering in this case.


## Gap Statistic

set.seed(19)
GapStat <- clusGap(customer, FUNcluster =  kmeans, K.max = 10)
GapStat

## Plotting the Gap Statistic

fviz_gap_stat(GapStat)




















