

### Análise dos dados de FORÇA DE TRABLHO,  livro do Manly, 3a edição

> Manly_Tabela_1_5 <- read.csv2("C:/Users/aluno/Downloads/Profa Silvia/2023-2/Manly_Tabela_1_5.txt", encoding="UTF-8", row.names=1, sep="")
>   View(Manly_Tabela_1_5)



#### Descritiva:

dados=data.frame(Manly_Tabela_1_5)
head(dados)
ls()

dim(dados)

pairs(dados[,-1], pch=19, col=as.factor(dados$Grupo))

require(psych)

describe(dados[,-1])
describeBy(dados[,-1], dados[,1])

table(dados$Grupo)

dados[dados$Grupo=="Outro",]

par(mfrow=c(1,2))
boxplot(dados$AGR ~ dados$Grupo, col=c(2,3,4,5))
### fazer os demais, alterar o par(mfrow)

boxplot(dados$FE ~ dados$Grupo, col=c(2,3,4,5))


#### Cluster e Componentes Principais:

saida.pc.corr=princomp(dados[,-1], cor = TRUE)
saida.pc.corr
names(saida.pc.corr)
summary(saida.pc.corr)

saida.pc.corr$loadings

A=saida.pc.corr$scores[,1:4]; A

biplot(saida.pc.corr)
biplot(saida.pc.corr, choices = c(1,3))
biplot(saida.pc.corr, choices = c(2,3))

par(mfrow=c(1,1))
pairs(A, pch=19, col=as.factor(dados$Grupo))


### Cluster:

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

## Correlação Cofenética:

d1 <- dist(A)
hc <- hclust(d1, "ave")
d2 <- cophenetic(saida_average)

cor(dist(A), cophenetic(saida_average)) # 
cor(dist(A), cophenetic(saida_single))
cor(dist(A), cophenetic(saida_median))
cor(dist(A), cophenetic(saida_complete))


saida_average=hclust(dist(A),method = "average")

par(mfrow=c(1,1))

## Recortando pelo número de grupos, ou seja, k:

plot(saida_average, hang = -1, 
     main="Método average")
rect.hclust(saida_average, k = 8, border = "blue")

plot(saida_average, hang = -1, 
     main="Método average")
rect.hclust(saida_average, k = 7, border = "blue")

plot(saida_average, hang = -1, 
     main="Método average")
rect.hclust(saida_average, k = 6, border = "blue")

table(cutree(saida_average, k = 6))
G=cutree(saida_average, k = 6)

plot(A[, 1:2], col=G, pch=19)

cbind(dados, G)
dados2=cbind(dados, G)

arrange(dados2, G)
