library(sf)
library(tidyverse)
library(data.table)


##### Pesquisar depois e adaptar para utilizar o pacote geobr ao invés dos geopackages

Sys.setlocale("LC_ALL", "English")
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
#
# +proj=aea +lat_1=-2 +lat_2=-22 +lat_0=-12 +lon_0=-54 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs
Proj_IBGE_area <- 'PROJCS["Brazil_Albers_Equal_Area_Conic",
       GEOGCS["SIRGAS 2000",
              DATUM["Sistema_de_Referencia_Geocentrico_para_las_AmericaS_2000",
                    SPHEROID["GRS 1980",6378137,298.257222101,
                             AUTHORITY["EPSG","7019"]],
                    TOWGS84[0,0,0,0,0,0,0],
                    AUTHORITY["EPSG","6674"]],
              PRIMEM["Greenwich",0,
                     AUTHORITY["EPSG","8901"]],
              UNIT["degree",0.0174532925199433,
                   AUTHORITY["EPSG","9122"]],
              AUTHORITY["EPSG","4674"]],
       PROJECTION["Albers_Conic_Equal_Area"],
       PARAMETER["False_Easting",0],
       PARAMETER["False_Northing",0],
       PARAMETER["longitude_of_center",-54],
       PARAMETER["Standard_Parallel_1",-2],
       PARAMETER["Standard_Parallel_2",-22],
       PARAMETER["latitude_of_center",-12],
       UNIT["Meter",1],
       AUTHORITY["IBGE","55555"]]'



Begin_time <- Sys.time()

# Carrega camada do Areas urbanizadas
AreasUrb2015 <- st_read("C:/ACELERADOR/Bases/AreasUrbanizadas2015.gpkg",
                        layer = "AreasUrbanizadas2015_CorrecaoTopologia")

# Carrega camada dos municípios
Municipios <- st_read("C:/ACELERADOR/Bases/MUNICIPIOS.gpkg",
                      layer = "BRMUE250GC_SIR_2010") 

# Repara erro de topologia dos municípios
Municipios <- st_make_valid(Municipios)

# Seleciona os municípios que interseccionam a camada de áreas urbanizadas (mapeados)
Municipios_proc <- Municipios[AreasUrb2015, op = st_intersects]

# Remove a camada original de municípios para liberar memória
rm(Municipios)
gc()

# Cria a sequência de geocódigos dos municípios
ListaMun <- Municipios_proc$CD_GEOCODM
ListaMun <- ListaMun[1:2]

# Cria a lista vazia para receber os data.frames / sf interseccionados e com áreas calculadas
TabelaCalculo <- list()

# loop para intersecao e calculo de areas
for (i in ListaMun) {
  
  # Seleciona e o município
  Mun <- Municipios_proc %>%
    filter(CD_GEOCODM == i)
  
  # extrai a geometria e converte para texto
  Mun_wkt <- Mun %>%
    st_geometry() %>%
    st_as_text()
  
  # carrega a grade estatistica filtrando com a geometria do municipio e reprojeta
  grade <- st_read("C:/ACELERADOR/Bases/GradeEstatistica.gpkg",
                   layer = "GradeEstatistica",
                   wkt_filter = Mun_wkt)
  
  grade <- grade[Mun, op = st_within]
  
  grade <- st_transform(grade, Proj_IBGE_area)
  
  # cria a query dos setores
  query_setor <- str_c("SELECT * FROM 'SETORES CENSITÁRIOS' WHERE CD_GEOCODM = ", i)
  
  # carrega os setores censitarios do municipio e reprojeta
  setores <- st_read("C:/ACELERADOR/Bases/BASE_2010.gpkg",
                     query = query_setor)
  
  setores <- st_transform(setores, Proj_IBGE_area)
  
  # filtra as áreas urbanizadas do município e reprojeta
  areas_urb <- AreasUrb2015[Mun, op = st_intersects]
  
  areas_urb <- st_transform(areas_urb, Proj_IBGE_area)
  
  # interseciona os setores com a grade
  intersecao <- st_intersection(setores, grade)
  
  # interseciona a camada de intersecao grade / setores com as areas urbanizadas
  intersecao_urb <- st_intersection(intersecao, areas_urb)
  
  # cria a camada da intersecao grade / setores que não interseciona as areas urbanizadas
  diferenca_urb <- st_difference(intersecao, st_union(areas_urb))
  
  # junta as duas camadas - intersecao grade / setor em areas urbanizadas e intersecao grade / setor fora das areas urbanizadas
  intersecao <- bind_rows(intersecao_urb, diferenca_urb)
  
  # calcula as áreas
  tabela_areas <- intersecao %>%
    mutate(Area_Inter = st_area(intersecao))
  
  # remove a geometria para liberar memoria
  st_geometry(tabela_areas) <- NULL
  gc()
  
  # insere na tabela
  TabelaCalculo[[i]] = tabela_areas
}

# consolida a tabela de calculo
TabelaCalculo <- bind_rows(TabelaCalculo)

# limpa a memoria de objetos que já cumpriram sua função
rm(list = setdiff(ls(), c("TabelaCalculo", "Begin_time")))
gc()

# calcula as áreas totais dos setores por classe de densidade usando data.table
TabelaCalculo <- as.data.table(TabelaCalculo)
TabelaCalculo <- TabelaCalculo[, ClasseUrb := fcase(
  Tipo == "Área urbanizada" & Densidade == "Densa", "UrDs",
  Tipo == "Área urbanizada" & Densidade == "Pouco densa", "UrPd",
  Tipo == "Outros equipamentos urbanos", "NRes",
  Tipo == "Vazio intraurbano", "Vaz", is.na(Tipo), "Vaz")
  ][ClasseUrb == "UrDs", AreUrDsSet := sum(Area_Inter), .(CD_GEOCODI, ClasseUrb)
  ][ClasseUrb == "UrPd", AreUrPdSet := sum(Area_Inter), .(CD_GEOCODI, ClasseUrb)
  ][ClasseUrb == "NRes", AreNResSet := sum(Area_Inter), .(CD_GEOCODI, ClasseUrb)
  ][ClasseUrb == "Vaz", AreaVazSet := sum(Area_Inter), .(CD_GEOCODI, ClasseUrb)
  ][is.na(AreUrDsSet), AreUrDsSet := 0
  ][is.na(AreUrPdSet), AreUrPdSet := 0
  ][is.na(AreNResSet), AreNResSet := 0
  ][is.na(AreaVazSet), AreaVazSet := 0]

End_time <- Sys.time()

TempoTotal <- End_time - Begin_time







