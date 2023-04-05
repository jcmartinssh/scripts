library(tidyverse)
library(data.table)
library(sf)
library(units)
library(arrow)

# Sys.setlocale("LC_ALL", "English")
Sys.setenv(LANG = "English")

#
# CRS para cálculo de áreas - 
# A Diretoria de Geociências – DGC do IBGE através da Coordenação de Cartografia - CCAR, elaborou um estudo (FRANÇA e MARANHÃO, 2015)
# de geração de uma grade estatística para o Brasil buscando minimizar as distorções decorrentes da grande extensão territorial do país.
# Neste contexto, foi feita a opção pela utilização da Projeção Equivalente de Albers, que apresenta como característica principal 
# a equivalência em área. Essa projeção é formada por um cone ao redor do globo, sob o qual as feições da superfície terrestre são 
# projetadas. Este cone intercepta o globo em dois paralelos padrão, com os meridianos formando linhas retas com a origem em um ponto
# central na geratriz do cone, enquanto os paralelos formam círculos concêntricos em torno deste ponto. O datum horizontal adotado é o
# SIRGAS2000.
# 
# Os parâmetros da projeção adotada são:
# Meridiano Central -54º
# Latitude de Origem -12º
# 1º Paralelo Padrão -2º
# 2º Paralelo Padrão -22º
# Origem E: 5.000.000
# Origem N: 10.000.000
# Área de Abrangência canto inferior esquerdo (E,N):
#   2.800.000, 7.350.000
# canto superior direito (E,N):
#   8.210.000, 12.200.000
#
# https://spatialreference.org/ref/sr-org/albers-conical-equal-area-brazil-sirgas-2000/
#
# +proj=aea +lat_1=-2 +lat_2=-22 +lat_0=-12 +lon_0=-54 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs
#

# endereço do arquivo geopackage com as camadas de amostragem para calculo dos coeficientes dos municípios

Begin_time <- Sys.time()

Proj_IBGE_area <- 'PROJCS["Conica_Equivalente_de_Albers_Brasil",
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

# Carrega camada dos municípios
Municipios <- st_read("C:/ACELERADOR/Bases/MUNICIPIOS.gpkg",
                      layer = "BRMUE250GC_SIR_2010")

# Repara erro de topologia dos municípios
Municipios <- st_make_valid(Municipios)

# Cria lista dos municípios mapeados no Áreas urbanizadas
ListaMunMap <- st_read("C:/ACELERADOR/Bases/AreasUrbanizadas2015.gpkg",
                       query = "SELECT CD_MUN FROM Municipios_mapeados") %>%
  filter(CD_MUN %in% Municipios$CD_GEOCODM)

ListaMunMap <- ListaMunMap$CD_MUN

# Cria lista dos municípios não mapeados
ListaMunNMap <- setdiff(Municipios$CD_GEOCODM, ListaMunMap)

# Cria a lista vazia para receber os data.frames / sf interseccionados, com áreas calculadas
tabelaCalcAreaMap <- list()

# loop para intersecao das bases e calculo de areas dos municipios mapeados
for (i in ListaMunMap) {

  # Seleciona e o município
  Mun <- Municipios %>%
    filter(CD_GEOCODM == i)

  # extrai a geometria do municipio e converte para texto
  Mun_wkt <- Mun %>%
    st_geometry() %>%
    st_as_text()

### abstrair essa etapa pra poder usar com diferentes camadas
  # carrega a grade estatistica filtrando com a geometria do municipio
  grade <- st_read("C:/ACELERADOR/Suscetibilidade/Bases_Suscetibilidade.gpkg",
                   query = "SELECT INDICE_GRE, ID_GRE, geom FROM UsoCoberturaTerra2000a2018",
                   wkt_filter = Mun_wkt)
###/etapa

  # reprojeta a grade para coordenadas planas
  grade <- st_transform(grade, Proj_IBGE_area)

  # cria a query dos setores
  query_setor <- str_c("SELECT CD_GEOCODI, CD_GEOCODS, CD_GEOCODD, CD_GEOCODM, NM_MUNICIP, UF, geom FROM 'SETORES CENSITÁRIOS' WHERE CD_GEOCODM = ", i)

  # carrega os setores censitarios do municipio
  setores <- st_read("C:/ACELERADOR/Bases/BASE_2010.gpkg",
                     query = query_setor)

  # reprojeta os setores para coordenadas planas
  setores <- st_transform(setores, Proj_IBGE_area)

  # seleciona as áreas urbanizadas que interseccionem o município
  areas_urb <- AreasUrb2015[Mun, op = st_intersects]

  # reprojeta as áreas urbanizadas para coordenadas planas
  areas_urb <- st_transform(areas_urb, Proj_IBGE_area)

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
  grade <- st_transform(grade, Proj_IBGE_area)

  # cria a query dos setores
  query_setor <- str_c("SELECT CD_GEOCODI, CD_GEOCODS, CD_GEOCODD, CD_GEOCODM, NM_MUNICIP, UF, geom FROM 'SETORES CENSITÁRIOS' WHERE CD_GEOCODM = ", i)

  # carrega os setores censitarios do municipio
  setores <- st_read("C:/ACELERADOR/Bases/BASE_2010.gpkg",
                     query = query_setor)

  # reprojeta os setores para coordenadas planas
  setores <- st_transform(setores, Proj_IBGE_area)

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