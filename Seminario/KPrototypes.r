rm(list = ls())

library(dplyr)
library(cluster)
library(clustMixType)
library(writexl)
library(ggplot2)
dados = read.csv("C:/Users/vitor/Documents/UFC/Multivariada/Seminário/saopaulo.csv")

dados <- dados %>%
  mutate(Elevator= as.factor(Elevator),
         Furnished= as.factor(Furnished),
         Swimming.Pool = as.factor(Swimming.Pool),
         New = as.factor(New),
         Sale = Negotiation.Type == 'sale')
dados <- dados %>%
  select(-Property.Type, -Negotiation.Type)

dados <- dados %>%
  select(1:11, 15, 12:14)

set.seed(15011998);dados <- dados[sample(nrow(dados)), ] #RANDONIZANDO AS LINHAS
dados <- dados[1:10000,] # SELECIONANDO APENAS AS 1000 PRIMEIRAS LINHAS, EVITANDO ALTO PROCESSAMENTO
gower_dist <- daisy(dados[, -13:-15],
                    metric = "gower",
                    type = )
tempo_execucao <- system.time({
  gower_dist
})[3]

inicio <- Sys.time();gower_dist <- daisy(dados[, -12:-15],
                    metric = "gower",
                    type = ); fim <- Sys.time();tempo_execucao <- fim - inicio

cat("Tempo de execução:", tempo_execucao, "\n")


summary(gower_dist)

'''
O Summary para o atributo type -> (A) representa uma variável binária Assimétrica,
                                      já (I) representa uma variável Intervalar,
                                      já (N) representa uma variável Nominal(factor)
'''

gower_mat <- as.matrix(gower_dist)
dados[
  which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
        arr.ind = TRUE)[1, ], ]
'''
Os imóveis que apresentaram as características menos dissimilares são de um imóvel localizado em X e em Y
'''

dados[
  which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]),
        arr.ind = TRUE)[1, ], ]
'''
Os imóveis que apresentaram as características mais dissimilares são de um imóvel localizado em X e Y
'''

#DETERMINANDO O NÚMERO IDEAL DE CLUSTER POR MÉTODO
set.seed(150198);val_cindex <- validation_kproto(method = "cindex", data = dados[,-12:-15], k = 3:10) #Leva Muito tempo
set.seed(150198);val_dunn <- validation_kproto(method = "dunn", data = dados[,-12:-15], k = 3:10) #Leva Muito tempo
set.seed(150198);val_silhouette <- validation_kproto(method = "silhouette", data = dados[,-12:-15], k = 3:10)

#NÚMEROS IDEIAS DE CLUSTER
val_silhouette$k_opt
val_silhouette$indices

val_dunn$k_opt
val_dunn$indices

val_cindex$k_opt
val_cindex$indices

val_silhouette$k_opt
val_silhouette$indices

View(dados)


#GRÁFICO DE DECISÃO DO MELHOR NÚMERO DE CLUSTERS
plot(3:10,val_silhouette$indices,
     xlab = "Número de clusters",
     ylab = "Silhouette Amplitude");lines(3:10, val_silhouette$indices);points(val_silhouette$k_opt,
                                           val_silhouette$indices[val_silhouette$k_opt - 2],
                                           col = "blue", pch = 16);text(val_silhouette$k_opt,
                                                                       val_silhouette$indices[val_silhouette$k_opt - 2],
                                                                       labels = paste("Ideal(Silhoueta) =", val_silhouette$k_opt),
                                                                       pos = 1,
                                                                       col = "blue")





plot(3:10,val_cindex$indices,
     xlab = "Number of clusters",
     ylab = "Cindex Width");lines(3:10,val_cindex$indices);points(val_cindex$k_opt,
                                                                  val_cindex$indices[val_cindex$k_opt - 2],
                                                                  col = "red", pch = 16);text(val_cindex$k_opt,
                                                                                              val_cindex$indices[val_cindex$k_opt - 2],
                                                                                              labels = paste("Ideal(Cindex) =", val_cindex$k_opt),
                                                                                              pos = 2,
                                                                                              col = "red")


plot(3:10,val_dunn$indices,
     xlab = "Number of clusters",
     ylab = "Dunn Width");lines(3:10, val_dunn$indices);points(val_dunn$k_opt,
                                                               val_dunn$indices[val_dunn$k_opt - 2],
                                                               col = "blue", pch = 16);text(val_dunn$k_opt,
                                                                                           val_dunn$indices[val_dunn$k_opt - 2],
                                                                                           labels = paste("Ideal(Dunn) =", val_dunn$k_opt),
                                                                                           pos = 1,
                                                                                           col = "blue")


#DIVIDINDO EM CLUSTERS
set.seed(150198);kpt_silhouette <- kproto_gower(dados, val_silhouette$k_opt)
set.seed(150198);kpt_cindex <- kproto_gower(dados[,-12:-15], val_cindex$k_opt)
set.seed(150198);kpt_dunn <- kproto_gower(dados[,-12:-15], val_dunn$k_opt)


#ACRESCENTANDO QUAL CLUSTER CADA UM PERTENCE (PELA MEDIDA) NO DATAFRAME
dados$Cluster_Silhouette <- kpt_silhouette$cluster
dados$C_Dunn <- kpt_dunn$cluster
dados$C_Cindex <- kpt_cindex$cluster

View(dados)

kpt_silhouette$centers
kpt_dunn$centers
kpt_cindex$centers


h_complete <- hclust(gower_dist, method = 'complete')
h_single <- hclust(gower_dist, method = 'single')
h_average <- hclust(gower_dist, method = 'average')

write.csv(dados,"C:/Users/vitor/Documents/UFC/Multivariada/Seminário/dados.csv")
