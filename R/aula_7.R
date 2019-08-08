
install.packages('latticeExtra')#soon!
install.packages('ggplot2')#soon!
install.packages('wesanderson')#soon!
install.packages('RColorBrewer')#soon!
install.packages('plotrix')#soon!
install.packages('rgdall')


#### Carregando pacotes ####
## pacote necessarios para rodar todos os graficos
# pacotes para o ggplot2
library(latticeExtra)
library(ggplot2)
# pacotes para paleta de cores
library(wesanderson)
library(RColorBrewer)
# pacote para grafico 3D
library(plotrix)
# pacote para dotchart
require(graphics)


#aula tutorial 07

# lendo os dados
sal <- read.csv("../salarios.csv")
head(sal)
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
