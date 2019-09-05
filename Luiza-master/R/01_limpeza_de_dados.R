
library(modleR)

registros <- read.table("./registros/registros.csv", h = T, sep = ";", stringsAsFactors = F)

head(registros)
str(registros)

species <- sort(unique(registros$sp))
species

env = brick("./env/pca_5km.tif")
env = env[[1:6]]

reg.clean = c()
for (i in species) {
  temp  = modleR::clean(
    registros[registros$sp == i, 2:3], #registros
    predictors = env, #variáveis preditoras
    clean_dupl = T, #retira os registros duplicados
    clean_nas = T,
    clean_uni = T
  )
  temp$sp = i
  reg.clean = rbind(reg.clean, temp)
}

table(registros$sp)
table(reg.clean$sp)

write.csv(reg.clean, "./registros/registros_limpos.csv")
