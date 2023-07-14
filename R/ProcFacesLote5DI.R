install.packages("readr")
install.packages("tidyverse")
install.packages("stringr")
install.packages("sf")

library(readr)
library(tidyverse)
library(stringr)
library(sf)

pastaRaiz <- "/home/joaquim/Downloads/lote_5/"

arquivo_entrada <- paste(pastaRaiz, as.character(CodUF), ".csv", sep = "")
arquivo_saida <- paste(pastaRaiz, as.character(CodUF), ".ods", sep = "")

tabela_enderecos <- read_fwf(arquivo_entrada, 
                fwf_widths(c(15,
                             1, 20, 30, 60, 8,
                             7, 20, 10, 20, 10,
                             20, 10, 20, 10, 20,
                             10, 20, 10, 15, 15,
                             60, 60, 2, 40, 1,
                             30, 3, 3, 8),
                           col_names = c("CD_SETOR",
                                         "SIT_SETOR", "TIP_LOG", "TIT_LOG", "NOM_LOG", "NUM_LOG",
                                         "MOD_NUM", "COMPLTO", "ELEMENTO1", "VALOR1", "ELEMENTO2",
                                         "VALOR2", "ELEMENTO3", "VALOR3", "ELEMENTO4", "VALOR4",
                                         "ELEMENTO5", "VALOR5", "ELEMENTO6", "LATITUDE", "LONGITUDE",
                                         "LOCALIDADE", "NULO", "ESPEC_ENDC", "ID_ESTAB", "IND_ENDC",
                                         "ID_DOM_COL", "QUADRA", "FACE", "CEP")),
                trim_ws = FALSE,
                na = character()) %>%
  filter(LATITUDE != "               " & LONGITUDE != "               ")