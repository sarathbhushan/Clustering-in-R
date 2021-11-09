rm(list=ls(all=T))
setwd("C:\\Users\\abhushan\\OneDrive - Electronic Arts\\Hackathon ideas")

dataset <- read.csv("one_day_data.csv", header = T, sep = ",",na.strings=c("","NA"))

#require(readxl)

#dataset <- read_excel("one_day_datav2.xlsx")
#colnames(data1)

options(scipen = 999)
head(dataset)
colnames(dataset)

d1 <- data.frame(dataset$Player.ID,dataset$Level,dataset$Feature, dataset$Count_Feature)

nrow(d1)
colnames(d1) <- c("PID","Level","Feature","Count")

dt1 <- d1[!is.na(d1$PID), ]

colnames(dt1)

#completeFun <- function(data, desiredCols) {
#  completeVec <- complete.cases(data[, desiredCols])
#  return(data[completeVec, ])
#}

#completeFun(dt1,"Feature")

sum(is.na(dt1["Feature"]))

dt2 <- dt1[!is.na(dt1$Feature), ]

colnames(dt2)
sum(is.na(dt2["Feature"]))
nrow(dt2)

str(dt2)
require(reshape2)


d2 <- dcast(dt2, PID + Level ~...)

require(ggplot2)

head(d2)
tail(d2)
nrow(d2)
str(d2)

colnames(d2)
d3<-d2[!(d2$PID=="1000000000000"),]

d3$Level <- as.character(as.numeric(d3$Level))


str(d3)
head(d3)

#d4 <- d3[(d3$Level == "14"), ]



#my_boxplot <- ggplot(d4,aes(x=Level,y= BakeOff)) + geom_boxplot() + xlab('Levels') + ylab('BakeOff Events')
#my_boxplot


#####################
require(ggplot2)
d3[is.na(d3)] <- 0
sum(is.na(d3))
head(d3)

require(DMwR)
require(BBmisc)

d4 <- normalize(d3, method = "standardize", range = c(0, 1), margin = 1L, on.constant = "quiet")

d5 <- d4[,-c(1,2)]
head(d5)
head(d5)

set.seed(1234)

k <- kmeans(d5, centers = 4)
k$centers
table(k$cluster)

aggregate(d5 ,by=list(k$cluster), FUN=mean)

d51 <- data.frame(d5, k$cluster)
head(d51)


colors <- c("#999999", "#E69F00", "#56B4E9","#82E0AA")
colors <- colors[as.numeric(d51$k.cluster)]
require(scatterplot3d)
scatterplot3d(d51[,1:3], pch = 16, color=colors, surface = F)



write.csv(k$centers,"centroids.csv", row.names = F)
# Cluster based on PCA 1

require

D_PCA <- prcomp(d5, center = T, scale. = T)
summary(D_PCA)
require(factoextra)
fviz_eig(D_PCA)

head(D_PCA)
nrow(d5)

d6 <- D_PCA$x

head(d6)

d7 <- d6[,1:4]

k2 <- kmeans(d7, centers = 4, nstart = 30)
k2$centers
table(k2$cluster)

write.csv(k2$centers,"centroids2.csv", row.names = F)

aggregate(d7 ,by=list(k2$cluster), FUN=mean)

d8 <- data.frame(d7, k2$cluster)
head(d8)


colors <- c("#999999", "#E69F00", "#56B4E9","#82E0AA")
colors <- colors[as.numeric(d8$k2.cluster)]
require(scatterplot3d)
scatterplot3d(d8[,1:3], pch = 16, color=colors)
