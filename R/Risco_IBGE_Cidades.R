#
##

# Script para construção de tabela com dados de população em áreas de risco para a plataforma IBGE Cidades

library(tidyverse)
library(readxl)
library(writexl)

# Carrega as tabelas com as variáveis das BATERs.
# Observe que não existe BATER no Distrito Federal, por isso não tem "DF" na lista de UFs.

M_BATER <- read_excel("C:/ACELERADOR/IBGE_Cidades/M_gBATER_Definitiva_modificadoNatal.xls",
                      col_types = c("text", "text", "text",
                                    "text", "text", "numeric", 
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric")) %>%
  mutate(GdReg = factor(as.numeric(substr(GEO_MUN, 1, 1)), labels = c("N", "NE", "SE", "S", "CO")),
         CodUF = factor(as.numeric(substr(GEO_MUN, 1, 2))),
         UF = factor(CodUF, labels = c("RO", "AC", "AM", "RR", "PA", "AP", "TO",
                                       "MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", "BA",
                                       "MG", "ES", "RJ", "SP", 
                                       "PR", "SC", "RS",
                                       "MS", "MT", "GO")))


# Importa tabela de dados municipais da publicacao

MunicipiosPub <- read_excel("C:/ACELERADOR/IBGE_Cidades/Apendice_3_MunicipiosPublicacao.xls", skip = 1)

ListaMunicipios <- M_BATER %>%
  group_by(GEO_MUN) %>%
  summarise("Domicílios em Área de Risco" = sum(M001), "População em Área de Risco" = sum(M004))
  
write_xlsx(ListaMunicipios, path = "C:/ACELERADOR/IBGE_Cidades/ListaMunicipiosRisco.xlsx")
