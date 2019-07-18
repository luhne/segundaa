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


#aula tutorial 07

# lendo os dados
sal <- read.csv("salarios.csv")
# explore os dados com as funções head e summary
# criando objetos para auxiliar a construção do gráfico
# criando modelos lineares
mh <- lm(salario ~ experiencia, data=sal[sal$sexo=="H",])
mm <- lm(salario ~ experiencia, data=sal[sal$sexo=="M",])
coefh <- coef(mh)
coefm <- coef(mm)
# definindo os limites dos eixos
limy <- c(min(sal$salario),max(sal$salario))
limx <- c(min(sal$experiencia),max(sal$experiencia))
## definindo os nomes dos eixos
labx <- "Experiência (anos)"
laby <- "Salário (R$)"

# define parametros graficos
par(mfrow=c(1,2), las=1, bty="l") # aqui estamos usando las e bty dentro do par para fixar para todas as janelas
# plot dos valores de salario dos homens
plot(salario ~ experiencia, data=sal[sal$sexo=="H",],
     col="tomato",
     ylim=limy, xlim=limx,
     ylab=laby, xlab=labx)
# linha do previsto pelo modelo
## a + b*x
abline(a=coefh[1], b=coefh[2],
       col='tomato', lwd=2)
mtext("A", 3, adj=0, font=2)
## plot do salario das mulheres
plot(salario ~ experiencia, data=sal[sal$sexo=="M",],
     col="navy",
     ylim=limy, xlim=limx,
     ylab="", xlab=labx)
mtext("B", 3, adj=0, font=2)
# linha do previsto pelo modelo
## a + b*x
abline(a=coefm[1], b=coefm[2],
       col='navy', lwd=2)

# a funcao png cria o arquivo, daqui pra frente você não vai mais ver o gráfico
png("figs/figura01.png", res=300, width=2400, height=1200)
# define parametros graficos
par(mfrow=c(1,2), las=1, bty="l") # aqui estamos usando las e bty dentro do par para fixar para todas as janelas
# plot dos valores de salario dos homens
plot(salario ~ experiencia, data=sal[sal$sexo=="H",],
     col="tomato",
     ylim=limy, xlim=limx,
     ylab=laby, xlab=labx)
# linha do previsto pelo modelo
## a + b*x
abline(a=coefh[1], b=coefh[2],
       col='tomato', lwd=2)
mtext("A", 3, adj=0, font=2)
## plot do salario das mulheres
plot(salario ~ experiencia, data=sal[sal$sexo=="M",],
     col="navy",
     ylim=limy, xlim=limx,
     ylab="", xlab=labx)
mtext("B", 3, adj=0, font=2)
# linha do previsto pelo modelo
## a + b*x
abline(a=coefm[1], b=coefm[2],
       col='navy', lwd=2)
# para finalizar o gráfico e gerar o arquivo, precisamos rodar o dev.off()
dev.off()


# plot dos valores de salario dos homens
plot(salario ~ experiencia, data=sal[sal$sexo=="H",],
     col="tomato",
     ylim=limy, xlim=limx,
     ylab=laby, xlab=labx)
# linha do previsto pelo modelo
## a + b*x
abline(a=coefh[1], b=coefh[2],
       col='tomato', lwd=2)
## usando points para adicionar os pontos do salario das mulheres
points(salario ~ experiencia, data=sal[sal$sexo=="M",],
       col="navy")
# linha do previsto pelo modelo das mulheres
## a + b*x
abline(a=coefm[1], b=coefm[2],
       col='navy', lwd=2)
# incluindo a legenda
legend("topleft", legend=c("homens", "mulheres"),
       col=c("tomato", "navy"),
       lty=1, bty='n')

# criando vetor de cores
cores <- c("#3B9AB2", "#EBCC2A", "#F21A00")
# criando vetor com o nome das espécies
sp <- paste("I.", unique(iris$Species), sep=" ")

par(mfrow=c(2,2), bty='l', las=1)
boxplot(Sepal.Length ~ Species, data=iris, xlab="", col=cores,
        xaxt="n")
axis(1, at=1:3, labels=sp, font=3)
boxplot(Sepal.Width ~ Species, data=iris, xlab="", col=cores,
        xaxt="n")
axis(1, at=1:3, labels=sp, font=3)
boxplot(Petal.Length ~ Species, data=iris,  col=cores,
        xaxt="n")
axis(1, at=1:3, labels=sp, font=3)
boxplot(Petal.Width ~ Species, data=iris, col=cores,
        xaxt="n")
axis(1, at=1:3, labels=sp, font=3)

# fixando uma semente de numeros aleatorios para manter o mesmo resultado no sample
set.seed(42)
# criando um data frame com valores medios e desvio padrão de uma variável
d2 <- data.frame(name=letters[1:5],
                 value=sample(seq(4,15),5),
                 sd=c(1,0.2,3,2,4))

plot(x=1:5, d2$value, las=1, bty='l', ylim=c(0, 18), pch=19, xaxt='n',
     xlab="names", ylab="value")
axis(1, at=1:5, labels=d2$name)
arrows(x0=1:5,
       y0=d2$value+d2$sd,
       y1=d2$value-d2$sd, angle=90, length=0.05, code=3)
