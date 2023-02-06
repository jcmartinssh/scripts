library(tidyverse)
library(data.table)
library(sf)
library(units)
library(arrow)

Sys.setlocale("LC_ALL", "English")
Sys.setenv(LANG = "English")


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

Coefs_SubD_pop <- st_read("W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/_GPKG/EstPop.gpkg", layer = "Coefs_SubD_pop")
Coefs_SubD_dom <- st_read("W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/_GPKG/EstPop.gpkg", layer = "Coefs_SubD_dom")
Coefs_Dist_pop <- st_read("W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/_GPKG/EstPop.gpkg", layer = "Coefs_Dist_pop")
Coefs_Dist_dom <- st_read("W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/_GPKG/EstPop.gpkg", layer = "Coefs_Dist_dom")
Coefs_Mun_pop <- st_read("W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/_GPKG/EstPop.gpkg", layer = "Coefs_Mun_pop")
Coefs_Mun_dom <- st_read("W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/_GPKG/EstPop.gpkg", layer = "Coefs_Mun_dom")

amostra_min <- 10

Coefs_pop <- Coefs_SubD_pop %>%
  mutate(CD_GEOCODM = str_sub(CD_GEOCODS, start = 1, end = 7)) %>%
  left_join(Coefs_Mun_pop, by = "CD_GEOCODM") %>%
  mutate(pop_coefDS = pop_coefDS_Mun,
         pop_coefPD = pop_coefPD_Mun,
         pop_coefVZ = pop_coefVZ_Mun,
         pop_coefNR = pop_coefNR_Mun) %>%
  select(CD_GEOCODS, CD_GEOCODM, pop_coefDS, pop_coefPD, pop_coefVZ, pop_coefNR)

Coefs_dom <- Coefs_SubD_dom %>%
  mutate(CD_GEOCODM = str_sub(CD_GEOCODS, start = 1, end = 7)) %>%
  left_join(Coefs_Mun_dom, by = "CD_GEOCODM") %>%
  mutate(dom_coefDS = dom_coefDS_Mun,
         dom_coefPD = dom_coefPD_Mun,
         dom_coefVZ = dom_coefVZ_Mun,
         dom_coefNR = dom_coefNR_Mun) %>%
  select(CD_GEOCODS, CD_GEOCODM, dom_coefDS, dom_coefPD, dom_coefVZ, dom_coefNR)


setDT(Coefs_pop)
setkey(Coefs_pop, "CD_GEOCODS")
setDT(Coefs_dom)
setkey(Coefs_dom, "CD_GEOCODS")

end_time <- Sys.time()
Tempo_coefs <- end_time - Begin_time

#########################################################################################
#### SE FOR CARREGAR A TABELA DO GEOPROCESSAMENTO JÁ CALCULADA, PULAR PARA O PRÓXIMO ####
#########################################################################################

start_time <- Sys.time()

# Carrega camada dos municípios
Municipios <- st_read("C:/ACELERADOR/Bases/MUNICIPIOS.gpkg",
                      layer = "BRMUE250GC_SIR_2010")

# Repara erro de topologia dos municípios
Municipios <- st_make_valid(Municipios)

ListaMunMap <- unique(c(Coefs_pop$CD_GEOCODM, Coefs_dom$CD_GEOCODM))
ListaMunNMap <- setdiff(Municipios$CD_GEOCODM, ListaMunMap)

# para testar o script
# ListaMunMap <- ListaMunMap[341:352]
# ListaMunNMap <- ListaMunNMap[2500:2505]

end_time <- Sys.time()
Tempo_mun <- end_time - start_time
start_time <- Sys.time()

# Carrega camada do Areas urbanizadas
AreasUrb2015 <- st_read("C:/ACELERADOR/Bases/AreasUrbanizadas2015.gpkg",
                        query = "SELECT Densidade, Tipo, CodConcUrb, geom FROM AreasUrbanizadas2015_CorrecaoTopologia")

end_time <- Sys.time()
Tempo_areasurb <- end_time - start_time
start_time <- Sys.time()

# Cria a lista vazia para receber os data.frames / sf interseccionados, com áreas calculadas
TabelaCalcMap <- list()

# loop para intersecao das bases e calculo de areas dos municipios mapeados
for (i in ListaMunMap) {
  
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
  TabelaCalcMap[[i]] = tabela_areas
}

# consolida a tabela de calculo de municipios mapeados a partir da lista de tabelas
TabelaCalcMap <- bind_rows(TabelaCalcMap)

end_time <- Sys.time()
Tempo_geopmap <- end_time - start_time
start_time <- Sys.time()

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
  [, map := TRUE]
)

end_time <- Sys.time()
Tempo_procmap <- end_time - start_time
start_time <- Sys.time()

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

# consolida a tabela de calculo de municipios mapeados a partir da lista de tabelas
TabelaCalcNMap <- bind_rows(TabelaCalcNMap)

end_time <- Sys.time()
Tempo_geopnmap <- end_time - start_time
start_time <- Sys.time()

# remove as unidades de área (m^2)
TabelaCalcNMap <- drop_units(TabelaCalcNMap)

setDT(TabelaCalcNMap)
TabelaCalcNMap <- (
  TabelaCalcNMap
  [, Area_TSet := sum(Area_Inter), by = CD_GEOCODI]
  [, map := FALSE]
)

end_time <- Sys.time()
Tempo_procnmap <- end_time - start_time
start_time <- Sys.time()

# junta as tabelas ou...
TabelaCalcVar <- rbindlist(list(TabelaCalcMap, TabelaCalcNMap), fill = TRUE)

#########################################################################################
##### SE TIVER CARREGADO A TABELA DO GEOPROCESSAMENTO JÁ CALCULADA, CONTINUAR DAQUI #####
#########################################################################################

# carrega a tabela já calculada

# TabelaCalcVar <- fread(file = "C:/ACELERADOR/EstPop/TabelaCalcVar.csv", sep = ";", dec = ".")

# insere os coeficientes
TabelaCalcFinal <- Coefs_pop[TabelaCalcVar, on = "CD_GEOCODS"]
TabelaCalcFinal <- Coefs_dom[TabelaCalcFinal, on = "CD_GEOCODS"]

# remove segmentos sem associacao com grade ou setor (problemas de topologia)
TabelaCalcFinal <- na.omit(TabelaCalcFinal, cols = c("CD_GEOCODI", "INDICE_GRE"))

# Cria lista de setores
ListaSet <- unique(TabelaCalcFinal$CD_GEOCODI)
str_c(ListaSet, collapse = ", ")

# cria a query com os geocodigos dos setores
query_set <- str_glue("SELECT Cod_setor, V001, V002 FROM 'Basico' WHERE Cod_setor IN (", str_c(ListaSet, collapse = ", "), ")", collapse = "")
query_raca <- str_glue("SELECT CD_GEOCODI, branca, preta, amarela, parda, indigena FROM 'TabCorRaca' WHERE CD_GEOCODI IN (", str_c(ListaSet, collapse = ", "), ")", collapse = "")

# Carrega a tabela de variáveis dos setores
VarSetores <- st_read("C:/ACELERADOR/Bases/BASE_2010.gpkg",
                      query = query_set)

VarRaca <- st_read("C:/ACELERADOR/Bases/BASE_2010.gpkg",
                   query = query_raca)

# Converte as variaveis populacao e domicilio para numerico e renomeia a variavel de geocodigo do setor
setDT(VarSetores)
VarSetores <- VarSetores[, ':='(V001 = as.numeric(V001), V002 = as.numeric(V002), CD_GEOCODI = Cod_setor)]

# Converte as variaveis de cor ou raca para numerico
setDT(VarRaca)
VarRaca <- VarRaca[, ':='(CD_GEOCODI = as.character(CD_GEOCODI),
                          branca = as.numeric(branca),
                          preta = as.numeric(preta), 
                          amarela = as.numeric(amarela), 
                          parda = as.numeric(parda), 
                          indigena = as.numeric(indigena))]

# Associa as variaveis domicilios e moradores dos setores a tabela de calculo
TabelaCalcFinal <- VarSetores[TabelaCalcFinal, on = "CD_GEOCODI"]
TabelaCalcFinal <- VarRaca[TabelaCalcFinal, on = "CD_GEOCODI"]

# assigna o valor 0 para os setores sem valor nas variáveis V001 e V002
TabelaCalcFinal <- (
  TabelaCalcFinal
  [is.na(V001), V001 := 0]
  [is.na(V002), V002 := 0]
  [is.na(branca), branca := 0]
  [is.na(preta), preta := 0]
  [is.na(amarela), amarela := 0]
  [is.na(parda), parda := 0]
  [is.na(indigena), indigena := 0]
  [, ':=' (negra = preta + parda,
           outra = amarela + indigena)]
)

end_time <- Sys.time()
Tempo_preparo <- end_time - start_time
start_time <- Sys.time()

# calcula as variaveis pra cada segmento e agrega por célula da grade
TabelaCalcFinal <- (
  TabelaCalcFinal
  [map == TRUE, ':=' (DomEst = (V001 * Area_Inter) * fcase(ClasseUrb == "UrDs", dom_coefDS,
                                                           ClasseUrb == "UrPd", dom_coefPD,
                                                           ClasseUrb == "Vaz", dom_coefVZ,
                                                           ClasseUrb == "NRes", dom_coefNR)
                      / ((AreUrDsSet * dom_coefDS) + (AreUrPdSet * dom_coefPD) + (AreaVazSet * dom_coefVZ) + (AreNResSet * dom_coefNR)),
                      PopEst = (V002 * Area_Inter) * fcase(ClasseUrb == "UrDs", pop_coefDS,
                                                           ClasseUrb == "UrPd", pop_coefPD,
                                                           ClasseUrb == "Vaz", pop_coefVZ,
                                                           ClasseUrb == "NRes", pop_coefNR)
                      / ((AreUrDsSet * pop_coefDS) + (AreUrPdSet * pop_coefPD) + (AreaVazSet * pop_coefVZ) + (AreNResSet * pop_coefNR)),
                      popbranca = (branca * Area_Inter) * fcase(ClasseUrb == "UrDs", pop_coefDS,
                                                           ClasseUrb == "UrPd", pop_coefPD,
                                                           ClasseUrb == "Vaz", pop_coefVZ,
                                                           ClasseUrb == "NRes", pop_coefNR)
                      / ((AreUrDsSet * pop_coefDS) + (AreUrPdSet * pop_coefPD) + (AreaVazSet * pop_coefVZ) + (AreNResSet * pop_coefNR)),
                      popnegra = (negra * Area_Inter) * fcase(ClasseUrb == "UrDs", pop_coefDS,
                                                           ClasseUrb == "UrPd", pop_coefPD,
                                                           ClasseUrb == "Vaz", pop_coefVZ,
                                                           ClasseUrb == "NRes", pop_coefNR)
                      / ((AreUrDsSet * pop_coefDS) + (AreUrPdSet * pop_coefPD) + (AreaVazSet * pop_coefVZ) + (AreNResSet * pop_coefNR)),
                      popoutra = (outra * Area_Inter) * fcase(ClasseUrb == "UrDs", pop_coefDS,
                                                           ClasseUrb == "UrPd", pop_coefPD,
                                                           ClasseUrb == "Vaz", pop_coefVZ,
                                                           ClasseUrb == "NRes", pop_coefNR)
                      / ((AreUrDsSet * pop_coefDS) + (AreUrPdSet * pop_coefPD) + (AreaVazSet * pop_coefVZ) + (AreNResSet * pop_coefNR)))]
  
  [map == FALSE, ':=' (DomEst = V001 * Area_Inter / Area_TSet,
                       PopEst = V002 * Area_Inter / Area_TSet,
                       popbranca = branca * Area_Inter / Area_TSet,
                       popnegra = negra * Area_Inter / Area_TSet,
                       popoutra = outra * Area_Inter / Area_TSet)]
  [, .(DomEst = sum(DomEst),
       PopEst = sum(PopEst),
       popbranca = sum(popbranca),
       popnegra = sum(popnegra),
       popoutra = sum(popoutra)),
    by = INDICE_GRE]
)


end_time <- Sys.time()
Tempo_calculo <- end_time - start_time
start_time <- Sys.time()
Tempo_Total <- end_time - Begin_time

Tempo_coefs
Tempo_mun
Tempo_areasurb
Tempo_geopmap
Tempo_procmap
Tempo_geopnmap
Tempo_procnmap
Tempo_preparo
Tempo_calculo
Tempo_Total

TabelaCalcFinal[is.na(TabelaCalcFinal)] <- 0

write_parquet(TabelaCalcFinal, sink = "C:/ACELERADOR/EstPop/TabelaCalcFinal_1kmc.parquet", compression = "zstd", compression_level = 19)

fwrite(TabelaCalcVar, file = "C:/ACELERADOR/EstPop/TabelaCalcVar_1km.csv", append = FALSE, quote = TRUE, sep = ";", dec = ".")
fwrite(TabelaCalcFinal, file = "C:/ACELERADOR/EstPop/TabelaCalcFinal_1km.csv", append = FALSE, quote = TRUE, sep = ";", dec = ".")


