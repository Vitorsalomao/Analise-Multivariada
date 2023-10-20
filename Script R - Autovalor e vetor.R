
######### Aula de autovalor e autovetor:
######### trabalhando com a S:

dim(USArrests)
S=var(USArrests)
S
eigen(S)

sum(eigen(S)$values)
sum(diag(S))

Gama=eigen(S)$vectors
dim(Gama)
round(Gama%*%t(Gama), 4)

######### trabalhando com a R:

cor(USArrests)

eigen(cor(USArrests))
