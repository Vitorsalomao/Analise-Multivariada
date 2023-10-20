
#### Cluster e Componentes Principais:

dados=USArrests
head(dados)
ls()

saida.pc.corr=princomp(dados, cor = TRUE)
saida.pc.corr
names(saida.pc.corr)
summary(saida.pc.corr)

saida.pc.corr$loadings

A=saida.pc.corr$scores[,1:3]


par(mfrow=c(2,2))
saida_single=hclust(dist(A),method = "single")
plot(saida_single, hang = -1, 
     main="Método single")

saida_complete=hclust(dist(A),method = "complete")
plot(saida_complete, hang = -1,
     main="Método complete") 

saida_average=hclust(dist(A),method = "average")
plot(saida_average, hang = -1, 
     main="Método average")

saida_median=hclust(dist(A),method = "median")
plot(saida_median, hang = -1, 
     main="Método median")


d1 <- dist(A)
hc <- hclust(d1, "ave")
d2 <- cophenetic(saida_average)

cor(dist(A), cophenetic(saida_average)) # 
cor(dist(A), cophenetic(saida_single))
cor(dist(A), cophenetic(saida_median))
cor(dist(A), cophenetic(saida_complete))

    
saida_average=hclust(dist(A),method = "average")

par(mfrow=c(1,2))

## Recortando pelo número de grupos, ou seja, k:
plot(saida_average, hang = -1, 
     main="Método average")
rect.hclust(saida_average, k = 4, border = "red")

plot(saida_average, hang = -1, 
     main="Método average")
rect.hclust(saida_average, k = 5, border = "blue")



## Recortando pela medida da altura, ou seja, h (eixo vertical):
plot(saida_average, hang = -1, 
     main="Método average")
rect.hclust(saida_average, h = 2.3, border = "red")

plot(saida_average, hang = -1, 
     main="Método average")
rect.hclust(saida_average, h = 2, border = "blue")


install.packages("cstab")

a=c(4,5)
cDistance(A, kseq=2:5, method = "hierarchical", 
          linkage = "average")
