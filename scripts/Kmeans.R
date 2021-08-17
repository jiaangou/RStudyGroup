iris_kmeans <- kmeans(iris[,-5],centers=3)
library(vegan)

?prcomp
iris_pca <- rda(iris[,-5])
plot(iris_pca,type='n',display='si')
points(iris_pca,col=iris_kmeans$cluster)

error <- data.frame(cluster=as.factor(iris_kmeans$cluster),species=iris$Species)
library(dplyr)
error_sum <- error%>%
  group_by(species,cluster,.drop=FALSE)%>%
  summarise(count = n())

PCA_prcomp <- iris%>%
  select(-Species)%>%
  prcomp()%>%
  .$x%>%
  as.data.frame()


PCA_prcomp%>%
  ggplot(aes(x=PC1,y=PC2))+geom_point()+
  geom_point(data=PCA_prcomp[1:3,],col='red')

mat <- matrix(runif(10),nrow=5)
vector <- matrix(runif(2),nrow=1)

apply(mat,MARGIN=1,FUN=function(x)dist(rbind(vector,x)))

?which
which(vector[1,]==min(vector[1,]))
?summarise_all
library(ggplot2)
ggplot(error_sum,aes(x=species,y=count,fill=cluster))+geom_bar(stat='identity')

#K means algorithm
dist(iris[,-5])






