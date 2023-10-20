
##### Exemplo com dos da Tabela 9-12:

rm(list=ls())

names(Dados)
dim(Dados)

pairs(Dados, pch=19)

round(cor(Dados), 4)

install.packages("psych")
library(psych)

eigen(cor(Dados))
eigen(cor(Dados))$values/7

cbind(sqrt(eigen(cor(Dados))$values[1])*eigen(cor(Dados))$vectors[,1],
sqrt(eigen(cor(Dados))$values[2])*eigen(cor(Dados))$vectors[,2])

KMO(cor(Dados))

cortest.bartlett(cor(Dados), n = nrow(Dados),diag=TRUE)

#principal compon
pc <- principal(cor(Dados),7,rotate="none")   
summary(pc)
pc

#principal compon
pc <- principal(cor(Dados),2,rotate="none")   
names(pc)
biplot.psych(pc)


pc <- principal(Dados,2,corr=TRUE ,rotate="none")   
names(pc)
biplot.psych(pc)
pc

pc_rot <- principal(Dados,2,corr=TRUE ,rotate="varimax")   
names(pc_rot)
biplot.psych(pc_rot)
pc_rot


