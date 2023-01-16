library(data.table)
library(sf)

# consolidar tabelas censo

ler.tabs <- function(lista) {
  tabs <- fread(lista, select = c("Cod_setor", "V002", "V003", "V004", "V005", "V006"))
  return(tabs)
}

endereco <- "C:/ACELERADOR/DadosCenso2010/Pessoa03/"

tabelas <- list.files(path = endereco, pattern = "*.csv", full.names = TRUE)

LTabPes03 <- lapply(tabelas, ler.tabs)

TabPessoas03 <- rbindlist(LTabPes03, use.names = TRUE, fill = TRUE)

setDT(TabPessoas03)
setnames(TabPessoas03, c("Cod_setor" ,"V002", "V003", "V004", "V005", "V006"), c("CD_GEOCODI", "branca", "preta", "amarela", "parda", "indigena"))

# st_write(TabPessoas03, dsn = "C:/ACELERADOR/Bases/BASE_2010.gpkg", layer = "TabCorRaca", append = FALSE)
