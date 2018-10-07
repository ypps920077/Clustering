#--Tool
install.packages("fastcluster"); install.packages("cluster"); install.packages("useful"); install.packages("factoextra")
library(fastcluster); library(cluster); library(useful); library(factoextra)

#--Code
orgData <- read.csv("Repos500.csv")
summary(orgData)
head(orgData)
preData <- subset(orgData, select = c(forks,watchers))   #--取forks、watchers欄位
n <- nrow(preData) 

#--Sampling_1
t_idx <- sample(seq_len(n), size = round(0.0004 * n)) #--取50筆資料
s1 <- preData[t_idx,] 

dataCluster1 <- hclust(dist(s1), method = "centroid")      #--進行階層式分群
plot(dataCluster1, labels = FALSE, main = "Centroid")      #--以圖像顯示

plot(dataCluster1)
rect.hclust(dataCluster1, k = 3, border = "red")

plot(dataCluster1)
rect.hclust(dataCluster1, k = 4, border = "red")

#===> 3 groups

#--Sampling_2
t_idx <- sample(seq_len(n), size = round(0.008 * n)) #--取1000筆資料
s2 <- preData[t_idx,] 

dataCluster2 <- hclust(dist(s2), method = "centroid")      #--進行階層式分群
plot(dataCluster2, labels = FALSE, main = "Centroid")      #--以圖像顯示

#===> 3~4 groups

#--Sampling_3
t_idx <- sample(seq_len(n), size = round(0.04 * n)) #--取5000筆資料
s3 <- preData[t_idx,] 
orgS3 <- orgData[t_idx,]    #--原始資料(比對用)

dataCluster3 <- hclust(dist(s3), method = "centroid")      #--進行階層式分群
plot(dataCluster3, labels = FALSE, main = "Centroid")      #--以圖像顯示

#--Sampling_4
t_idx <- sample(seq_len(n), size = round(0.08 * n)) #--取10000筆資料
s4 <- preData[t_idx,] 
orgS4 <- orgData[t_idx,]   #--原始資料(比對用)

dataCluster4 <- hclust(dist(s4), method = "centroid")      #--進行階層式分群
plot(dataCluster4, labels = FALSE, main = "Centroid")      #--以圖像顯示

##---------------------------------------------------------------------------------------------------------
#--Result1 ===> for sample 3, sample size = 5000
set.seed(280411)
resultCluster3v1 <- kmeans(x = s3, centers = 3, nstart = 25)  # 分成三群
resultCluster3v1

set.seed(280411)
resultCluster3v2 <- kmeans(x = s3, centers = 4, nstart = 25)  # 分成四群
resultCluster3v2

resultCluster3v2$totss          #--各個點的離均差平方和
resultCluster3v2$withinss       #--每個單一群內部的離均差平方和 (群內點跟點之間)
resultCluster3v2$tot.withinss   #--每個單一群內部的離均差平方和的總和
resultCluster3v2$betweenss      #--各個群之間的離均差平方和 (群跟群之間)
resultCluster3v2$size           #--每個群的點個數

#library(cluster) 
clusplot(s3, resultCluster3v2$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
table(resultCluster3v2$cluster, orgS3$language)   #--分群的結果與程式語言配對之間的關係

#library(factoextra)
fviz_cluster(resultCluster3v2, data = s3, geom = "point", stand = FALSE)

#guess the number of clusters
s3Best1 <- FitKMeans(s3, max.clusters=20, nstart=50, seed=280411)
PlotHartigan(s3Best1)
s3Best1

set.seed(280411)
resultCluster3EX <- kmeans(x = s3, centers = 12, nstart = 25)  # 分成12群
resultCluster3EX
fviz_cluster(resultCluster3EX, data = s3, geom = "point", stand = FALSE)
table(resultCluster3EX$cluster, orgS3$language)

##---------------------------------------------------------------------------------------------------------
#--Result2 ===> for sample 4, sample size = 10000
set.seed(280411)
resultCluster4v1 <- kmeans(x = s4, centers = 3, nstart = 25)  # 分成三群
resultCluster4v1

set.seed(280411)
resultCluster4v2 <- kmeans(x = s4, centers = 4, nstart = 25)  # 分成四群
resultCluster4v2

#library(cluster) 
clusplot(s4, resultCluster4v2$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
table(resultCluster4v2$cluster, orgS4$language)   #--分群的結果與程式語言配對之間的關係

#library(factoextra)
fviz_cluster(resultCluster4v2, data = s4, geom = "point", stand = FALSE)

#guess the number of clusters
s4Best1 <- FitKMeans(s4, max.clusters=30, nstart=50, seed=280411)
PlotHartigan(s4Best1)
s4Best1