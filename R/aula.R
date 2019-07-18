#listar objetos no workspace

ls()

#para remover
rm(list = ls())

# qual e a pasta de trabalho
getwd()


a <-c(5,4,3,2,1)
a

# terça aula diogo

#git pull
#git status
#git add
#git commit -m "mensagem"
#git push


#tipos de vetores
#num <- c(1,2,3



#num

# num <- 1:3
 # num

#[1] 1 3

#class(v1)


# > v1
# [1] 1 2 3 4 5
# v1 [2] <- 0; v1
#[1]

#v1 <-  v1 [1:4]; v1

#length (v1)
#[1] 4

#matriz
m1 <- matrix (1:9, nrow =3, ncol =3)
m1

#cbind outra maneira de criar matriz
riq <- c(12,15,18,10)
abun <- c(40,52,60,37,22)
m1 <- cbind(riq, abund)
m1



#dim( ) str() head( )

#cbind( )

#rbind ( )

# read.csv() => header = TRUE sep "," ,  dec = "."

# read.csv2() => header = TRUE sep ";"  , dec = ","

# read.table( )

head(dados)

#aula <- read.csv ("./DATA/aulacsv.csv", sep = "." head = TRUE)
aula
aula = aula[[1:6]]


data("anscombe")

dim("anscombe")
head(anscombe)
class(anscombe)
str(anscombe)

mean(anscombe$x1)
mean(anscombe$x2)
mean(anscombe$x3)
mean(anscombe$x4)

apply(anscombe[,1:4], 2, mean)

apply(anscombe[,5:8], 2, mean)

apply(anscombe, 2, var)

cor(anscombe$x1, anscombe$y1)
cor(anscombe$x2, anscombe$y2)
cor(anscombe$x3, anscombe$y3)
cor(anscombe$x4, anscombe$y4)

m1 <- lm(anscombe$y1 ~ anscombe$x1)
m2 <- lm(anscombe$y2 ~ anscombe$x2)
m3 <- lm(anscombe$y3 ~ anscombe$x3)
m4 <- lm(anscombe$y4 ~ anscombe$x4)

m1 <- lm (y1 ~x1, data= anscombe)

mlist <- list(m1, m2, m3, m4)
mlist[[1]]
summary(mlist[[1]])
coef(mlist[[1]])



lapply(mlist, coef)

anscombe

# funcao par para definir as configuracoes da janela grafica entre em ?par
par(mfrow=c(2,2), #abre uma janela gráfica com 2 linhas  e 2 colunas
    las=1, # deixa as legendas dos eixos na vertical
    bty="l") # tipo do box do grafico em L
plot(anscombe$y1 ~ anscombe$x1) #plot das variaveis
abline(mlist[[1]]) # adicionando a reta prevista pelo modelo de regressao
plot(anscombe$y2 ~ anscombe$x2)
abline(mlist[[2]])
plot(anscombe$y2 ~ anscombe$x3)
abline(mlist[[3]])
plot(anscombe$y3 ~ anscombe$x4)
abline(mlist[[4]])


head(iris)
summary(iris)

table(iris$Species)
#calcular a media
tapply(X = iris$Sepal.Length, INDEX = list(iris$Species), FUN = mean)
aggregate(x = iris$Sepal.Length, by = list(iris$Species), FUN = mean)
aggregate(Sepal.Length ~ Species, data=iris, mean)

#outro modo
aggregate(Sepal.Length ~ Species, data=iris, mean)
aggregate(Sepal.Width ~ Species, data=iris, mean)
aggregate(Petal.Length ~ Species, data=iris, mean)

#desvio padrao
tapply(X = iris$Sepal.Length, INDEX = list(iris$Species), FUN = sd)
tapply(X = iris$Sepal.Width, INDEX = list(iris$Species), FUN = sd)
tapply(X = iris$Petal.Length, INDEX = list(iris$Species), FUN = sd)
tapply(X = iris$Petal.Width, INDEX = list(iris$Species), FUN = sd)


# criando matriz de 3 colunas - uma para cada sp - e 4 linhas - uma para cada metrica
medias <- matrix(NA, ncol=3, nrow=4)
# definindo o nome das colunas e das linhas da matriz
colnames(medias) <- unique(iris$Species)
rownames(medias) <- names(iris)[-5]
for (i in 1:4){
    medias[i,] <- tapply(iris[,i], iris$Species, mean)
}

vars <- iris[,-5]
apply(vars, 2, mean)

apply(vars, 2, median)

#Moda: valor mais frequente na amostra
freq_sl <- sort(table(iris$Sepal.Length), decreasing = TRUE)
freq_sl[1]

#desvio padrao
sd01 <- apply(vars, 2, sd)
# outra forma:
sd02 <- apply(vars, 2, function(x) sqrt(var(x)))
sd01
sd02
sd01==sd02

#Coeficiente de variação: medida relativa de desvio padrao
cv <- function(x){
    sd(x)/mean(x)*100
}
apply(vars, 2, cv)



# sumario de 5 numeros
apply(vars, 2, quantile)
# 5%, 50% e 95%
apply(vars, 2, quantile, probs=c(0.5, 0.5, 0.95))

# a funcao range nos retorna os valores minimo e maximo
apply(vars, 2, range)
# aplicando a funcao diff ao resultado do range, temos o valor desejado
# uma boa pratica é nunca sobrescrever um objeto já existente no R, por isso
# nunca nomeie um objeto com um nome já existente
my_range <- function(x){
    diff(range(x))
}
apply(vars, 2, my_range)

apply(vars, 2, IQR)

#correlacao
cor(vars)

#grafico de barras
barplot(table(iris$Species))

par(mfrow=c(2,2))
hist(iris$Sepal.Length)
hist(iris$Sepal.Width)
hist(iris$Petal.Length)
hist(iris$Petal.Length)

par(mfrow=c(1,1))

par(mfrow=c(1,2))
hist(iris$Sepal.Width)
hist(iris$Sepal.Width, breaks = 4)

par(mfrow=c(1,2))
hist(iris$Sepal.Width)
hist(iris$Sepal.Width, freq = FALSE)

par(mfrow=c(1,1))

par(mfrow=c(1,2))
# plot da curva de densidade
plot(density(iris$Sepal.Width))
# plot da curva de densidade sobre o histograma de densidade
hist(iris$Sepal.Width, freq = FALSE)
lines(density(iris$Sepal.Width), col="blue") # note que agora estamos usando a funcao o comando add=TRUE

par(mfrow=c(1,1))

boxplot(iris$Sepal.Length)
boxplot(iris$Sepal.Width)
boxplot(iris$Petal.Length)
boxplot(iris$Petal.Width)

boxplot(Sepal.Length ~ Species, data=iris)
boxplot(Sepal.Width ~ Species, data=iris)
boxplot(Petal.Length ~ Species, data=iris)
boxplot(Petal.Width ~ Species, data=iris)

boxplot(iris$Sepal.Width)

my_boxplot <- boxplot(iris$Sepal.Width, plot=FALSE)
my_boxplot

outliers <- my_boxplot$out
#qual a posicao dos outliers
which(iris$Sepal.Width %in% outliers)
# vamos usar a posicao para indexar o objeto
iris[which(iris$Sepal.Width %in% outliers), c("Sepal.Width", "Species")]

boxplot(Sepal.Width ~ Species, data=iris)
my_boxplot2 <- boxplot(Sepal.Width ~ Species, data=iris, plot=FALSE)
my_boxplot2
# o objeto é uma lista e os valores outliers estão guardados no elemento $out da lista
outliers2 <- my_boxplot2$out
# neste caso, queremos apenas os outliers da especie setosa
# vamos usar a posicao para indexar o objeto
iris[iris$Sepal.Width %in% outliers2 &
         iris$Species=="setosa",
     c("Sepal.Width", "Species")]

par(mfrow=c(1,3))
qqnorm(iris$Sepal.Length[iris$Species=="setosa"],
       main="setosa")
qqline(iris$Sepal.Length[iris$Species=="setosa"])
qqnorm(iris$Sepal.Length[iris$Species=="versicolor"],
       main="versicolor")
qqline(iris$Sepal.Length[iris$Species=="versicolor"])
qqnorm(iris$Sepal.Length[iris$Species=="virginica"],
       main="virginica")
qqline(iris$Sepal.Length[iris$Species=="virginica"])

par(mfrow=c(1,1))

pairs(vars)

# carregando o pacote GGally
## se você não tiver o pacote usar:
install.packages("GGally")
## se já tiver o pacote, apenas carregue
library(GGally)
ggpairs(vars)

library(GGally)
ggpairs(vars)


registros = read.csv("trees.csv", h = T, sep = ",")
#registros$sp <- as.character(registros$sp)

# sumario de 5 numeros
apply(registros, 2, quantile)
# 5%, 50% e 95%
apply(registros, 2, quantile, probs=c(0.5, 0.5, 0.95))

#desvio padrao
sd01 <- apply(registros, 2, sd)
# outra forma:
sd02 <- apply(registros, 2, function(x) sqrt(var(x)))
sd01
sd02
sd01==sd02

#histograma
par(mfrow=c(2,2))
hist(Girth$.Height.volume)
hist(Girth$Height.volume)
hist(Girth$Height.volume)
hist(Girth$Height.Volume)
