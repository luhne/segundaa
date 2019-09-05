#Lendo diretamente dos arquivos de excel

lista <- list.files('originais', ".xls", full.names = T)
xlsx::read.xlsx2(lista[5], sheetIndex = 1)

dados <- c()
for(i in 1:length(lista)){
  temp <- xlsx::read.xlsx2(lista[i], sheetIndex = 1)
  dados <- rbind(dados,temp)
  }
dados

write.csv(dados, "./registros/registros.csv", row.names = F)


#Lendo os csv, mas estão errados
lista <- list.files('originais', ".csv", full.names = T)
read.table(lista[5], sep = ",", h = T)
