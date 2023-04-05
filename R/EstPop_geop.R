# library(tidyverse)
library(data.table)
library(sf)
# library(units)
# library(arrow)

# Sys.setlocale("LC_ALL", "English")
Sys.setenv(LANG = "English")


# Tem que ficar fora da função pra não rodar várias vezes num lapply

prAlbersBR <- 'PROJCS["Conica_Equivalente_de_Albers_Brasil",
                         GEOGCS["GCS_SIRGAS2000",
                                DATUM["D_SIRGAS2000",
                                      SPHEROID["Geodetic_Reference_System_of_1980",6378137,298.2572221009113]],
                                PRIMEM["Greenwich",0],
                                UNIT["Degree",0.017453292519943295]],
                         PROJECTION["Albers"],
                         PARAMETER["standard_parallel_1",-2],
                         PARAMETER["standard_parallel_2",-22],
                         PARAMETER["latitude_of_origin",-12],
                         PARAMETER["central_meridian",-54],
                         PARAMETER["false_easting",5000000],
                         PARAMETER["false_northing",10000000],
                         UNIT["Meter",1]]'

# Cria a lista de todos os municípios
ListaMun <- st_read("C:/ACELERADOR/Bases/MUNICIPIOS.gpkg",
                    query ="SELECT CD_GEOCODM FROM BRMUE250GC_SIR_2010") |>
  getElement("CD_GEOCODM")

# Cria lista dos municípios mapeados no Áreas urbanizadas
ListaMunMap <- st_read("C:/ACELERADOR/Bases/AreasUrbanizadas2015.gpkg",
                       query = "SELECT CD_MUN FROM Municipios_mapeados") |>
  getElement("CD_MUN")

# Cria lista dos municípios não mapeados
ListaMunNMap <- setdiff(ListaMun, ListaMunMap)

#########################
#########################
#########################

gbaseTrp <- function(codmun, gpkg, layer, vars, crs = prAlbersBR) {
  
  # Carrega camada dos municípios
  Municipios <- st_read("C:/ACELERADOR/Bases/MUNICIPIOS.gpkg",
                        layer = "BRMUE250GC_SIR_2010")
  ######PAREI AQUI
  # Repara erro de topologia dos municípios
  Municipios <- st_make_valid(Municipios)
  # Seleciona e o município
  Mun <- layermun[layermun$CD_GEOCODM == codmun]
  
  # extrai a geometria do municipio e converte para texto
  Mun_wkt <- Mun |>
    st_geometry() |>
    st_as_text()

  # constroi o query SQL pra grade  
  querygrade <- paste0(c("SELECT", paste0(vars, collapse = ", "), "FROM", layer), collapse = " ")
  
  # carrega a camada de interesse filtrando com a geometria do municipio
  grade <- st_read("C:/ACELERADOR/Suscetibilidade/Bases_Suscetibilidade.gpkg",
                   query = "SELECT INDICE_GRE, ID_GRE, geom FROM UsoCoberturaTerra2000a2018",
                   wkt_filter = Mun_wkt)
  ###/etapa
  
  # reprojeta a grade para coordenadas planas
  grade <- st_transform(grade, prAlbersBR)
  
  # cria a query dos setores
  query_setor <- str_c("SELECT CD_GEOCODI, CD_GEOCODS, CD_GEOCODD, CD_GEOCODM, NM_MUNICIP, UF, geom FROM 'SETORES CENSITÁRIOS' WHERE CD_GEOCODM = ", i)
  
  # carrega os setores censitarios do municipio
  setores <- st_read("C:/ACELERADOR/Bases/BASE_2010.gpkg",
                     query = query_setor)
  
  # reprojeta os setores para coordenadas planas
  setores <- st_transform(setores, prAlbersBR)
  
  # seleciona as áreas urbanizadas que interseccionem o município
  areas_urb <- AreasUrb2015[Mun, op = st_intersects]
  
  # reprojeta as áreas urbanizadas para coordenadas planas
  areas_urb <- st_transform(areas_urb, prAlbersBR)
  
  # interseciona a grade com os setores
  intersecao <- st_intersection(grade, setores)
  
  # interseciona a camada de grade+setores com as areas urbanizadas
  intersecao_urb <- st_intersection(intersecao, areas_urb)
  
  # cria a camada diferenca de grade+setores com as areas urbanizdas
  diferenca_urb <- st_difference(intersecao, st_union(areas_urb))
  
  # junta as duas camadas - grade+setores+areas urbanizadas e grade+setor-areas urbanizadas
  intersecao <- bind_rows(intersecao_urb, diferenca_urb)
  
  # calcula as áreas dos segmentos resultantes
  tabela_areas <- intersecao %>%
    mutate(Area_Inter = st_area(intersecao))
  
  # converte para numeros, sem unidade
  attributes(tabela_areas$Area_Inter) <- NULL
  
  # remove a geometria para liberar memoria
  st_geometry(tabela_areas) <- NULL
  gc()
  
  # insere na lista de tabelas
  tabelaCalcAreaMap[[i]] = tabela_areas
}
}







###############################################
#### DO SCRIPT DE CALCULO DOS COEFICIENTES ####
###############################################

### essa etapa terá que ser deslocada para outro momento
  # seleciona as celulas da grade que estejam completamente dentro do municipio
  grade <- grade[Mun, op = st_within]
###/etapa

### essa etapa deverá ser ajustada e realizada o quanto antes no processamento
# remove segmentos sem associacao com grade ou setor (problemas de topologia)
tabelaCalcAreaMap <- na.omit(tabelaCalcAreaMap, cols = c("CD_GEOCODI", "ID_UNICO"))
###/etapa

############################################
#### DO SCRIPT DE CALCULO DAS VARIAVEIS ####
############################################


# Cria a lista vazia para receber os data.frames / sf interseccionados, com áreas calculadas
tabelaCalcAreaMap <- list()

# loop para intersecao das bases e calculo de areas dos municipios mapeados
for (i in ListaMunMap) {


# consolida a tabela de calculo de municipios mapeados a partir da lista de tabelas
TabelaCalcMap <- bind_rows(tabelaCalcAreaMap)



# remove as unidades de área (m^2)
TabelaCalcMap <- drop_units(TabelaCalcMap)

setDT(TabelaCalcMap)
TabelaCalcMap <- (
  TabelaCalcMap
  [, ClasseUrb := fcase(Tipo == "Área urbanizada" & Densidade == "Densa", "UrDs",
                        Tipo == "Área urbanizada" & Densidade == "Pouco densa", "UrPd",
                        Tipo == "Outros equipamentos urbanos", "NRes",
                        Tipo == "Vazio intraurbano", "Vaz", is.na(Tipo), "Vaz")]
  [ClasseUrb == "UrDs", AreUrDsSet := sum(Area_Inter), by = CD_GEOCODI]
  [ClasseUrb == "UrPd", AreUrPdSet := sum(Area_Inter), by = CD_GEOCODI]
  [ClasseUrb == "NRes", AreNResSet := sum(Area_Inter), by = CD_GEOCODI]
  [ClasseUrb == "Vaz", AreaVazSet := sum(Area_Inter), by = CD_GEOCODI]
  [is.na(AreUrDsSet), AreUrDsSet := 0]
  [is.na(AreUrPdSet), AreUrPdSet := 0]
  [is.na(AreNResSet), AreNResSet := 0]
  [is.na(AreaVazSet), AreaVazSet := 0]
  [, AreUrDsSet := max(AreUrDsSet), by = CD_GEOCODI]
  [, AreUrPdSet := max(AreUrPdSet), by = CD_GEOCODI]
  [, AreNResSet := max(AreNResSet), by = CD_GEOCODI]
  [, AreaVazSet := max(AreaVazSet), by = CD_GEOCODI]
  [, map := fifelse(CD_GEOCODS %in% SubD_Map, TRUE, FALSE)]
  [map == FALSE, Area_TSet := sum(Area_Inter), by = CD_GEOCODI]
)




# Cria a lista vazia para receber os data.frames / sf interseccionados, com áreas calculadas
TabelaCalcNMap <- list()

# loop para intersecao das bases e calculo de areas dos municipios não mapeados
for (i in ListaMunNMap) {

  # Seleciona e o município
  Mun <- Municipios %>%
    filter(CD_GEOCODM == i)

  # extrai a geometria do municipio e converte para texto
  Mun_wkt <- Mun %>%
    st_geometry() %>%
    st_as_text()

  # carrega a grade estatistica filtrando com a geometria do municipio
  grade <- st_read("C:/ACELERADOR/Suscetibilidade/Bases_Suscetibilidade.gpkg",
                   query = "SELECT INDICE_GRE, ID_GRE, geom FROM UsoCoberturaTerra2000a2018",
                   wkt_filter = Mun_wkt)

  # reprojeta a grade para coordenadas planas
  grade <- st_transform(grade, prAlbersBR)

  # cria a query dos setores
  query_setor <- str_c("SELECT CD_GEOCODI, CD_GEOCODS, CD_GEOCODD, CD_GEOCODM, NM_MUNICIP, UF, geom FROM 'SETORES CENSITÁRIOS' WHERE CD_GEOCODM = ", i)

  # carrega os setores censitarios do municipio
  setores <- st_read("C:/ACELERADOR/Bases/BASE_2010.gpkg",
                     query = query_setor)

  # reprojeta os setores para coordenadas planas
  setores <- st_transform(setores, prAlbersBR)

  # interseciona a grade com os setores
  intersecao <- st_intersection(grade, setores)

  # calcula as áreas dos segmentos resultantes
  tabela_areas <- intersecao %>%
    mutate(Area_Inter = st_area(intersecao))

  # converte para numeros, sem unidade
  attributes(tabela_areas$Area_Inter) <- NULL

  # remove a geometria para liberar memoria
  st_geometry(tabela_areas) <- NULL
  gc()

  # insere na lista de tabelas
  TabelaCalcNMap[[i]] = tabela_areas
}

# consolida a tabela de calculo de municipios não mapeados a partir da lista de tabelas
TabelaCalcNMap <- bind_rows(TabelaCalcNMap)


# remove as unidades de área (m^2)
TabelaCalcNMap <- drop_units(TabelaCalcNMap)

setDT(TabelaCalcNMap)
TabelaCalcNMap <- (
  TabelaCalcNMap
  [, Area_TSet := sum(Area_Inter), by = CD_GEOCODI]
  [, map := FALSE]
)