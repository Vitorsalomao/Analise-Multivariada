









### outras funções para Análise de Agrupamento
### biblioteca {cluster}

###################################################

install.packages("cluster")
require(cluster)

?agnes

head(mtcars)
pairs(mtcars, pch=19)

colorir=as.factor(mtcars$vs)
pairs(mtcars[,-mtcars$vs], pch=19, col=colorir)

mtcars.sing <- agnes(mtcars,metric = "euclidean", 
                     method = "single")

plot(mtcars.sing)
plot(mtcars.sing, hang=-1)

mtcars.sing.hclust <- hclust(dist(mtcars), method = "single")
plot(mtcars.sing.hclust, hang=-1)
cutree(mtcars.sing.hclust, k=4)
rect.hclust(mtcars.sing.hclust, k=4)


###### k-means:

?kmeans

kmeans.mtcars=kmeans(mtcars, 4)
names(kmeans.mtcars)

table(kmeans.mtcars$cluster)

cor(mtcars)

plot(mtcars$cyl, mtcars$disp, col=kmeans.mtcars$cluster, pch=19)
mtcars["Maserati Bora",]

cbind(kmeans.mtcars$centers,kmeans.mtcars$size)

#####################################

dv <- diana(mtcars, metric = "euclidean")
plot(dv, hang=-1)

####################################

?daisy

d.mtcars <- daisy(mtcars, metric = "euclidean")
dim(d.mtcars)
