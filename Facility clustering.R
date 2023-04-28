library(tidyverse)
library(rio)
library(factoextra)

#running for national counts
urlNat <- "https://media.githubusercontent.com/media/uclalawcovid19behindbars/data/master/latest-data/latest_national_counts.csv"

natData <- read.csv(urlNat)
library(writexl)
write_xlsx(natData, "C://Users/homsl/Documents/rProjects/national_data.xlsx")



#running for state counts
data6 <- read.csv("C://Users/homsl/Documents/rProjects/prison_death.csv")

states <- data6[1]
states <- t(states)
states <- states[,-54:-64]

data5 <- data6[,-1]
data5 <-t(data5)
data5 <- data5[,-54:-64]
colnames(data5) <- states
data5 <- data5[ , colSums(is.na(data5)) == 0]


k <- ncol(data5)
kdata <- t(data5)
kmeans_ge <- kmeans(x=kdata,centers=2)
fviz_cluster(kmeans_ge, data = kdata, main="total deaths")

library(Rtsne)
tsne <- Rtsne(X=t(data5), perplexity = 2)
plot(tsne$Y, col="black", bg = colors, pch=21, cex=3, main = "TSNE")

#running for state counts for individual facilities 

urlFacil <-"https://media.githubusercontent.com/media/uclalawcovid19behindbars/data/master/latest-data/latest_facility_counts.csv"

DataFacil <- read.csv(urlFacil)

FacilIDs <- DataFacil[,4]

FacilDataRelevent <- DataFacil[,12:13]
FacilDataRelevent <- t(FacilDataRelevent)
colnames(FacilDataRelevent) <- FacilIDs
FacilDataRelevent <- FacilDataRelevent[ , colSums(is.na(FacilDataRelevent)) == 0]

k <- ncol(FacilDataRelevent)
kdata <- t(FacilDataRelevent)
kmeans_ge <- kmeans(x=kdata,centers=3)
fviz_cluster(kmeans_ge, data = kdata, main="total deaths")

library(Rtsne)
tsne <- Rtsne(X=t(FacilDataRelevent), perplexity = 2)
plot(tsne$Y, col="black", bg = colors, pch=21, cex=3, main = "TSNE")


library(writexl)
write_xlsx(FacilDataRelevent, "C://Users/homsl/Documents/rProjects/Facility_data2.xlsx")
