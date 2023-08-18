#TRABALHANDO COM A S
dim(USArrests)
S = var(USArrests); S
eigen(S) # Auto valores e Auto Vetores
sum(eigen(S)$values) # Soma dos Auto(valores e vetores)
sum(diag(S)) # O traço da Matriz

Gama = eigen(S)$vectors # Todos os valores
dim(Gama)
round(Gama%*%t(Gama), 4)
#TRABALAHANDO COM A R
cor(USArrests)
eigen(cor(USArrests))
