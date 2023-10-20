rm(list = ls())
library(corrplot)
library(HSAUR2)
library(ggplot2)
library(stats)
data("pottery", package = "HSAUR2")

View(pottery)
nomes <- c("Al2O3", "Fe2O3", "MgO", "CaO", "Na2O", "K2O", "TiO2", "MnO", "BaO")
pottery = pottery * 100
View(pottery)
# Carregar pacotes
# Boxplot com legenda
boxplot(pottery, names=nomes, main="Boxplot dos tipos de óxidos", ylab="Valores", xlab="Tipos de óxidos")

#Os dados do arquivo “pottery” são referentes à composição química de 
#45 amostras de cerâmica Romano-britânica, determinadas por espectrofotometria 
#de absorção atômica, para nove tipos de óxidos, encontrados em cinco sítios arqueológicos:

#(a) Faça uma análise descritiva com os dados em geral e outra por sítio arqueológico.
pottery <- as.data.frame(sapply(pottery, as.numeric)) #Transformando o dataset em numérico
View(pottery)
oxidos <- subset(pottery, select = -kiln) # Remove a coluna desnecessária
M <- cor(oxidos)
corrplot(M, method = "number", type = "lower", bg="black",
         title = "Correlação entre os óxidos")
medias <- colMeans(oxidos)
print(medias)

#GRÁFICO DE BARRAS PARA A MÉDIA DE CADA ÓXIDO
df_medias <- data.frame(Oxido = names(medias), Media = medias)
ggplot(df_medias, aes(x = Oxido, y = Media)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Presença média de cada óxido na cerâmica.", x = "Óxido", y = "Média")
# BOX
boxplot(oxidos, main="Boxplot de Todos os Óxidos", ylab="Valores", names=c("Al2O3", "Fe2O3", "MgO", "CaO", "Na2O", "K2O", "TiO2", "MnO", "BaO"))
#ANOVA
aov <- lm(soma_linhas ~ .,data = oxidos)
print(aov)
soma_linhas <- rowSums(oxidos)
print(soma_linhas)

#ANALISE DE COMPONETES PRINCIPAIS (PCA)
pca_result <- prcomp(oxidos, scale. = TRUE)
print(pca_result)
#Redução da dimensionalidade para 3 componentes:
reduced_pca <- pca_result$x[, 1:3]
#Resultados
print(summary(pca_result))
print(pca_result$rotation)
screeplot(pca_result) #Curva do cotovelo

Dp = scale() #Padronizacao dos dados


AV = eigen(var(#X)) #Autovalores
AVL =AV$values
AVT = AV$vectors

100*AVL/sum(AVL)
cumsum(100*AVL/sum(AVL))

#OBTENCAO DE COMPONENTES PRINCIPAIS
CP = Dp%*%AVT

#OBTENDO GRAFICO
plot(CP[,1],CP[,2],xlab="CP1",ylab="CP2",main="Componentes principais",col=0)
text(CP[,1],CP[,2],rownames(D))

#########################  OUTRA MANEIRA É IMPORTANTE QUE OS DADOS ESTEJAM NORMALIZADOS ##########################
###################################################################
# install.packages("MultivariateAnalysis")
library(MultivariateAnalysis)

CPb=princomp(Dp)
plot(CPb)
biplot(CPb)


####### CLUSTER 
library(FactoMineR) # Realizar os clster
library(factoextra) # K-MEANS
library(cluster) #Plotar o grafico
cluster <- hclust(dist(#dados)) #Distância entre as observações
rect.hclust(cluster, k=3) #Número de clusters

fviz_nbclust(dados, kmeans, method = "") #Definir quantidade ótima de cluster
#####KMEANS
dados_kmeans <- kmeans(dados, 3)
fviz_cluster(dados_kmeans, data= dados) #PLOTA OS CLUSTERS

lista <- dados_kmeans$cluster #LISTA DE CLUSTERS

