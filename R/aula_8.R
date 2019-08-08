# ====== aula 8 mapas === #

library(rgdal)

westeros <- readOGR("political.shp", encoding = "UTF-8")

## OGR data source with driver: ESRI Shapefile
## Source: "C:\Users\Diogo\Documents\GitHub\analise_de_dados_ENBT_2019\aula08\data\GoTRelease\political.shp", layer: "political"
## with 12 features
## It has 3 fields
## Integer64 fields read as strings:  id

plot(westeros, las = 1, axes = T)
abline(h = 0, lty = 2, col = "tomato") #plotando a linha do 'equador'

names(westeros)

## [1] "id"        "name"      "ClaimedBy"

westeros$ClaimedBy

##  [1] Night's Watch Tully         Wildlings     Night's Watch Stark
##  [6] Greyjoy       Martell       Baratheon     Arryn         Lannister
## [11] Targaryen     Tyrell
## 11 Levels: Arryn Baratheon Greyjoy Lannister Martell ... Wildlings

stark <- westeros[westeros$ClaimedBy == "Stark",]

plot(stark, axes = T, las = 1)

pontos <- spsample(stark, 10, 'random')

plot(stark, axes = T)
points(pontos, pch = "+", col = "tomato", cex = 1.5)


pontos.buffer <- buffer(pontos, width = 200000, dissolve = TRUE)

## Loading required namespace: rgeos

plot(stark, axes = T)
plot(pontos.buffer, add = T, col = "grey60")
points(pontos, col = 'red', pch = 16)


stark.buffer <- buffer(stark, width = 2, dissolve = TRUE)
plot(stark.buffer, col = "grey80", axes = T)
plot(stark, add = T, col = "lightblue")

westeros

westeros$regiao <- c(rep(1:3, each = 4))
westeros

westeros_contorno = aggregate(westeros)
plot(westeros_contorno, axes = T)

new_westeros = aggregate(westeros, by = "regiao")
plot(westeros, axes = T, col = terrain.colors(12))

plot(new_westeros, axes = T, col = terrain.colors(4))

#odemos exportar os shapes que criamos com a função writeOGR do pacote sp. Um detalhe importante: a função writeOGR só exporta objetos do formato SpatialPointsDataFrame,  SpatialLinesDataFrame ou SpatialPolygonsDataFrame object.

wastereosn1 <- westeros[westeros$ClaimedBy == "Stark",]

writeOGR(
    westeros_contorno, westeros_contorno$n1,   #nome do objeto a ser salvo
    dsn = "./RESULTS/meushape", #diretorio a serem salvos os resultados
    layer = "westeros_contorno", #nome do arquivo
    driver = "ESRI Shapefile" #formato pretendido para exportação
)

westeros_raster <- raster(westeros_contorno, res = 0.08)
westeros_raster <- rasterize(westeros_contorno, westeros_raster) #deixando com o mesmo extent
plot(westeros_raster)



var1 <- raster("./data/vars/var_1.tif")
var1

plot(var1)

lista <- list.files("./data", pattern = "tif$", full.names = T)
vars <- stack(lista)
plot(vars)


vars <- stack("./data/vars.tif")
plot(vars)

writeRaster(var1, "output.tif")

media <- mean(vars)
plot(media)

westeros <- readOGR("./data/GoTRelease/political.shp", encoding = "UTF-8")

stark <- westeros[westeros$ClaimedBy == "Stark",]
stark

plot(var1)
plot(westeros, add = T)

var1_croped <- crop(var1, stark)
var1_croped

plot(var1_croped)
plot(stark, add = T)

var1_masked = mask(var1, stark)
var1_masked

plot(var1_masked)
plot(stark, add = T)

var1.masked2 = mask(crop(var1,stark), stark)
var1.masked2

plot(var1.masked2)
plot(stark, add = T)

var1.aggregated = aggregate(var1, fact = 5, fun = "mean")
var1.aggregated

plot(var1.aggregated)




