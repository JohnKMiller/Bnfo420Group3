#New state clustering
library(tidyverse)
library(rio)
library(factoextra)

#reading in and formatting data
urlState <-"https://media.githubusercontent.com/media/uclalawcovid19behindbars/data/master/latest-data/latest_state_counts.csv"
DataState <- read.csv(urlState)
StateNames <- DataState[,1]

#Selecting data to be tested 
StateRelvData <- DataState[,c(2,4,17)] #select the columns just by editing list
StateRelvData<- t(StateRelvData)
colnames(StateRelvData) <- StateNames

#removing NA values
StateRelvData <- StateRelvData[ , colSums(is.na(StateRelvData)) == 0]

#actually performing clustering
k <- ncol(StateRelvData)
kdata <- t(StateRelvData)
kmeans_ge <- kmeans(x=kdata,centers=3)
fviz_cluster(kmeans_ge, data = kdata) #if the data has more than two dimensions, PCA is performed automatically 
