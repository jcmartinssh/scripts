library(sf)
library(data.table)
library(tidyverse)


Vars <- fread(file = "C:/ACELERADOR/EstPop/TabelaCalcFinal.csv", sep = ";", dec = ".")
Vars$ID_UNICO <- as.character(Vars$ID_UNICO)

ListaMunInt <- "'RIO DE JANEIRO', 'SÃO PAULO'"

query_mun <- str_glue("SELECT CD_GEOCODM, NM_MUNICIP, geom FROM 'BRMUE250GC_SIR_2010' WHERE NM_MUNICIP IN (", ListaMunInt, ")")


# Carrega camada dos municípios
Municipios <- st_read("C:/ACELERADOR/Bases/MUNICIPIOS.gpkg",
                      query = query_mun)
  
# Repara erro de topologia dos municípios
Municipios <- st_make_valid(Municipios)

# Cria a lista vazia para receber os data.frames / sf interseccionados, com áreas calculadas
GradeInt <- list()


# loop para intersecao das bases e calculo de areas dos municipios mapeados
for (i in Municipios$CD_GEOCODM) {
  
  # Seleciona e o município
  Mun <- Municipios %>%
    filter(CD_GEOCODM == i)
  
  # extrai a geometria do municipio e converte para texto
  Mun_wkt <- Mun %>%
    st_geometry() %>%
    st_as_text()
  
  # carrega a grade estatistica filtrando com a geometria do municipio
  grade_mun <- st_read("C:/ACELERADOR/Bases/GradeEstatistica.gpkg",
                   query = "SELECT ID_UNICO, POP, DOM_OCU, Shape_Area as AreaGrade, geom FROM GradeEstatistica",
                   wkt_filter = Mun_wkt)

  
  # insere na lista de tabelas
  GradeInt[[i]] = grade_mun
}

# consolida a tabela de calculo de municipios mapeados a partir da lista de tabelas
GradeInt <- bind_rows(GradeInt)

GradeInt <- GradeInt %>%
  left_join(Vars, by = "ID_UNICO")

st_write(GradeInt, dsn = "C:/ACELERADOR/GradeAnalise.gpkg", layer = "GradeInt", append= FALSE)  

