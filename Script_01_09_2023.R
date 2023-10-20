
## Exercicio 8.10, Livro do Johnson, usa dados da Tabela 8.4
## Weekly rate stocks

setwd("C:/Users/aluno/Downloads/Profa Silvia/2023-2")
dados=read.table(file.choose(), header = F)
head(dados)
ls()

attach(dados)

pairs(dados, pch=19)

install.packages("psych")
require(psych)

describe(dados)

par(mfrow=c(2,3))
hist(V1, breaks=20)
hist(V2, breaks=20)
hist(V3, breaks=20)
hist(V4, breaks=20)
hist(V5, breaks=20)

par(mfrow=c(2,3))
boxplot(V1)
boxplot(V2)
boxplot(V3)
boxplot(V4)
boxplot(V5)

par(mfrow=c(1,1))
boxplot(dados, pch=19)



###### CP´s usando amatriz S:var-cov amostral:

?princomp

saida.pc=princomp(dados, cor = FALSE)
saida.pc
names(saida.pc)
summary(saida.pc)

saida.pc$loadings
eigen(var(dados))$vectors

saida.pc$scores
plot(saida.pc$scores[,1:2], pch=19)

biplot(saida.pc, main="CP com a matriz S")

dados[c(56,63,70),]

eigen(var(dados))
eigen(var(dados))$values
sqrt(eigen(var(dados))$values)
VT=sum(eigen(var(dados))$values);VT
sum(diag(var(dados)))


###### CP´s usando amatriz R:corr amostral:

saida.pc=princomp(dados, cor = TRUE)
saida.pc
names(saida.pc)
summary(saida.pc)

saida.pc$loadings
eigen(cor(dados))$vectors

saida.pc$scores
plot(saida.pc$scores[,1:2], pch=19)

biplot(saida.pc, main="CP com a matriz R")

dados[c(56,63,70),]



