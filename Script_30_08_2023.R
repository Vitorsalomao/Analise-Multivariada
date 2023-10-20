
iris
plot(iris[,-ncol(iris)])
pairs(iris[,-ncol(iris)], col=iris$Species, pch=19)

length(iris)
names(iris)
plot(iris[,1],iris[,2], pch=19, col=iris$Species)

summary(iris)

par(mfrow=c(2,2))
hist(iris[,1]);hist(iris[,2]);hist(iris[,3]);hist(iris[,4])

## acrescentar as demais MEDIDAS DESCRITIVAS

install.packages("psych")
require(psych)

describe(iris[,-5])
describe.by(iris[,-5], group=iris[,5])

###### CP´s usando amatriz S:var-cov amostral:

S=var(iris[,-5]); S
eigen(S)

E_12=eigen(S)$vectors[,1:2]
dim(E_12)

X=as.matrix(iris[,-5])

Y=X%*%E_12
dim(Y)

plot(Y[,1], Y[,2], pch=19, col=iris$Species)

dotchart(Y[,1])
stripchart(Y[,1], method="jitter", col=iris$Species)

hist(Y[,1])


###### CP´s usando amatriz R: correlação amostral:

R=cor(iris[,-5]); R
eigen(R)

eigen(R)$values/4

R_12=eigen(R)$vectors[,1:2]
dim(R_12)
R_12


R12=scale(iris[,1:2], center = TRUE)
dim(R12)
var(R12)

cor(iris[,1:2])



