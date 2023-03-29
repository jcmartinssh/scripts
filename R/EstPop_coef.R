library(tidyverse)
library(data.table)
library(sf)
library(units)
library(arrow)


##### Pesquisar depois e adaptar para utilizar o pacote geobr ao invés dos geopackages

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

# Cria lista dos municípios mapeados no Áreas urbanizadas
ListaMun <- st_read("C:/ACELERADOR/Bases/AreasUrbanizadas2015.gpkg",
                       query = "SELECT CD_MUN FROM Municipios_mapeados")

ListaMun <- ListaMun$CD_MUN

#teste São paulo
# ListaMun <- '3550308'

# Carrega camada do Areas urbanizadas
AreasUrb2015 <- st_read("C:/ACELERADOR/Bases/AreasUrbanizadas2015.gpkg",
                        query = "SELECT Densidade, Tipo, CodConcUrb, geom FROM AreasUrbanizadas2015_CorrecaoTopologia")

end_time <- Sys.time()
Tempo_areasurb <- end_time - Begin_time
start_time <- Sys.time()


# Carrega camada dos municípios
Municipios <- st_read("C:/ACELERADOR/Bases/MUNICIPIOS.gpkg",
                      layer = "BRMUE250GC_SIR_2010") %>%
  filter(CD_GEOCODM %in% ListaMun)

# Repara erro de topologia dos municípios
Municipios <- st_make_valid(Municipios)

# Seleciona os municípios que interseccionam a camada de áreas urbanizadas (mapeados)
Municipios_proc <- Municipios[AreasUrb2015, op = st_intersects]

# Remove a camada original de municípios para liberar memória
rm(Municipios)
gc()

# Cria a sequência de geocódigos dos municípios
# ListaMun <- Municipios_proc$CD_GEOCODM
# ListaMun <- Municipios_proc$CD_GEOCODM[Municipios_proc$NM_MUNICIP == "RIO DE JANEIRO"]
# ListaMun <- ListaMun[1]
# ListaMun <- c("2307700", "2304202") # município que tá dando erro
# ListaMun <- c("3124104", "3136652", "3130101", "3106705", "3162922", "3109006", "3140159", "3165537", "3149309",
#               "3118601", "3154606", "3129806", "3106200", "3162955", "3144805", "3171204", "3117876", "3137601",
#               "3157807", "3156700", "3153905", "3154804", "3110004") ## BH

end_time <- Sys.time()
Tempo_municipios <- end_time - start_time
start_time <- Sys.time()

# Cria a lista vazia para receber os data.frames / sf interseccionados e com áreas calculadas
TabelaCalcCoef <- list()

# loop para intersecao das bases e calculo de areas
for (i in ListaMun) {

  # Seleciona e o município
  Mun <- Municipios_proc %>%
    filter(CD_GEOCODM == i)
  
  # extrai a geometria do municipio e converte para texto
  Mun_wkt <- Mun %>%
    st_geometry() %>%
    st_as_text()
  
  # carrega a grade estatistica filtrando com a geometria do municipio
  grade <- st_read("C:/ACELERADOR/Bases/GradeEstatistica.gpkg",
                   query = "SELECT ID_UNICO, POP, DOM_OCU, Shape_Area as AreaGrade, geom FROM GradeEstatistica",
                   wkt_filter = Mun_wkt)
  
  # seleciona as celulas da grade que estejam completamente dentro do municipio
  grade <- grade[Mun, op = st_within]
  
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
  
  # exporta para um geopackage para avaliação
  # st_write(st_transform(tabela_areas, crs = 4674), dsn = "C:/ACELERADOR/EsPop_coef.gpkg", layer = i, append = FALSE)
  
  # converte para numeros, sem unidade
  attributes(tabela_areas$Area_Inter) <- NULL
  
  # remove a geometria para liberar memoria
  st_geometry(tabela_areas) <- NULL
  gc()
  
  # insere na lista de tabelas
  TabelaCalcCoef[[i]] = tabela_areas
}

# consolida a tabela de calculo a partir da lista de tabelas
TabelaCalcCoef <- bind_rows(TabelaCalcCoef)

# limpa a memoria de objetos não mais necessarios
rm(list = setdiff(ls(), c("TabelaCalcCoef", "ListaMun", "Begin_time", "start_time", "Tempo_areasurb", "Tempo_municipios")))
gc()

end_time <- Sys.time()
Tempo_geoproc <- end_time - start_time
start_time <- Sys.time()

# remove as unidades de área (m^2)
TabelaCalcCoef <- drop_units(TabelaCalcCoef)

# soma as áreas totais dos setores por classe de densidade usando data.table
setDT(TabelaCalcCoef)
TabelaCalcCoef <- (
  TabelaCalcCoef
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
)

# remove segmentos sem associacao com grade ou setor (problemas de topologia)
TabelaCalcCoef <- na.omit(TabelaCalcCoef, cols = c("CD_GEOCODI", "ID_UNICO"))

# Cria lista de setores
ListaSet <- unique(TabelaCalcCoef$CD_GEOCODI)
str_c(ListaSet, collapse = ", ")

### MUDAR AQUI
VarSetoresDOM <- read_parquet("C:/ACELERADOR/DadosCenso/dados/Domicilio01.parquet", col_select = c("Cod_setor", "V002"), as_data_frame = TRUE) %>%
  filter(Cod_setor %in% ListaSet) %>%
  rename("CD_GEOCODI" = "Cod_setor", "V001" = "V002") 

VarSetoresPOP <- read_parquet("C:/ACELERADOR/DadosCenso/dados/Domicilio02.parquet", col_select = c("Cod_setor", "V002"), as_data_frame = TRUE) %>%
  filter(Cod_setor %in% ListaSet) %>%
  rename("CD_GEOCODI" = "Cod_setor")

VarSetores <- VarSetoresDOM[VarSetoresPOP, on = "CD_GEOCODI"]

# cria a query com os geocodigos dos setores
# query_set <- str_glue("SELECT Cod_setor, V001, V002 FROM 'Basico' WHERE Cod_setor IN (", str_c(ListaSet, collapse = ", "), ")", collapse = "")

# Carrega a tabela de variáveis dos setores
# VarSetores <- st_read("C:/ACELERADOR/Bases/BASE_2010.gpkg",
#                       query = query_set)

# Converte as variaveis populacao e domicilio para numerico e renomeia a variavel de geocodigo do setor
VarSetores <- as.data.table(VarSetores)
# VarSetores <- VarSetores[, ':='(V001 = as.numeric(V001),
#                                 V002 = as.numeric(V002),
#                                 CD_GEOCODI = Cod_setor)]

# Associa as variaveis domicilios e moradores dos setores a tabela de calculo
TabelaCalcCoef <- VarSetores[TabelaCalcCoef, on = "CD_GEOCODI"]

# assigna o valor 0 para os setores sem valor nas variáveis V001 e V002
TabelaCalcCoef <- (
  TabelaCalcCoef[is.na(V001), V001 := 0]
  [is.na(V002), V002 := 0]
)

# Cria variaveis de controle de amostra e seleciona as colunas relevantes
TabelaCalcCoef <- (
  TabelaCalcCoef
  [, NC_Cont := uniqueN(ClasseUrb), by = CD_GEOCODI]
  [, grd_amostra := fifelse(any(NC_Cont > 1), TRUE, FALSE), by = ID_UNICO]
  [, ':=' (N_SubD = uniqueN(CD_GEOCODS), N_Dist = uniqueN(CD_GEOCODD), N_Mun = uniqueN(CD_GEOCODM)),by = ID_UNICO]
  [, .(CD_GEOCODI, CD_GEOCODS, CD_GEOCODD, CD_GEOCODM, NM_MUNICIP, UF, ID_UNICO, NC_Cont, grd_amostra, N_SubD, N_Dist, N_Mun,
       POP, DOM_OCU, V001, V002, Area_Inter, ClasseUrb, AreUrDsSet, AreUrPdSet, AreNResSet, AreaVazSet)]
)

# ordena segundo o geocodigo do setor - crescente
setorder(TabelaCalcCoef, CD_GEOCODI)

# pivota a tabela, com o total das áreas por classe para cada segmento
TabelaCalcCoef <- dcast(TabelaCalcCoef, ... ~ ClasseUrb,
                        fun = sum,
                        value.var = "Area_Inter",
                        fill = 0)

# verifica a existência das colunas de áreas por classe dos segmentos, e cria as faltantes com valor 0
colunas <- c("UrDs", "UrPd", "NRes", "Vaz")
setDF(TabelaCalcCoef)
TabelaCalcCoef[colunas[!(colunas %in% colnames(TabelaCalcCoef))]] = 0  # semantica de Base R... devo tentar entender em algum momento
setDT(TabelaCalcCoef)

# separa segmentos que não pertencem à células da grade utilizadas na amostra - tabela final para calculo
# TabelaCalcCoef_NAmostra <- TabelaCalcCoef[grd_amostra == FALSE]
TabelaCalcCoef <- TabelaCalcCoef[grd_amostra == TRUE]

## Integrar a parte de calculo dos coeficientes

end_time <- Sys.time()
Tempo_tabela <- end_time - start_time
start_time <- Sys.time()

par.ini <- c(1, 1, 1, 1)
ubound <- c(10^9, 10^9, 10^9, 10^9) # testar pra ver se resolve os erros
lbound <- c(10^-9, 10^-9, 10^-9, 10^-9)
Subd <- unique(TabelaCalcCoef$CD_GEOCODS)
Dist <- unique(TabelaCalcCoef$CD_GEOCODD)
Mun <- unique(TabelaCalcCoef$CD_GEOCODM)

SubD_coef_dpp <- list()
Dist_coef_dpp <- list()
Mun_coef_dpp <- list()

# função usando o data.table
fnopt_pop <- function(par, data) {
  (
    data
    [, POPEstTrecho := V002 * ((UrDs * par[1]) + (UrPd * par[2]) + (Vaz * par[3]) + (NRes * par[4]))
      / ((AreUrDsSet * par[1]) + (AreUrPdSet * par[2]) + (AreaVazSet * par[3]) + (AreNResSet * par[4]))]
    [, .(POPEst = sum(POPEstTrecho), POPGde = first(POP)), by = "ID_UNICO"]
    [, Erro := abs(POPGde - POPEst)]
    [, .(total = sum(Erro))]
  )
}

fnopt_dom <- function(par, data) {
  (
    data
    [, DOMEstTrecho := V001 * ((UrDs * par[1]) + (UrPd * par[2]) + (Vaz * par[3]) + (NRes * par[4]))
      / ((AreUrDsSet * par[1]) + (AreUrPdSet * par[2]) + (AreaVazSet * par[3]) + (AreNResSet * par[4]))]
    [, .(DOMEst = sum(DOMEstTrecho), DOMGde = first(DOM_OCU)), by = "ID_UNICO"]
    [, Erro := abs(DOMGde - DOMEst)]
    [, .(total = sum(Erro))]
  )
}

end_time <- Sys.time()
Tempo_vars <- end_time - start_time
start_time <- Sys.time()

for (i in Subd) {
  selecao <- TabelaCalcCoef[N_SubD == 1 & CD_GEOCODS == i]
  amostra <- n_distinct(selecao$ID_UNICO)
  result_pop <- try(optim(par = par.ini, fn = fnopt_pop, data = selecao, method = "L-BFGS-B", lower = lbound))
  SubD_coef_dpp[[i]] = data.frame(i)
  SubD_coef_dpp[[i]]$CD_GEOCODS = i
  SubD_coef_dpp[[i]]$pop_coefDS_SubD = ifelse(class(result_pop) == "list", result_pop$par[1], NA)
  SubD_coef_dpp[[i]]$pop_coefPD_SubD = ifelse(class(result_pop) == "list", result_pop$par[2], NA)
  SubD_coef_dpp[[i]]$pop_coefVZ_SubD = ifelse(class(result_pop) == "list", result_pop$par[3], NA)
  SubD_coef_dpp[[i]]$pop_coefNR_SubD = ifelse(class(result_pop) == "list", result_pop$par[4], NA)
  SubD_coef_dpp[[i]]$amostra_SubD = amostra
}

Coefs_SubD_pop <- bind_rows(SubD_coef_dpp) %>%
  select(CD_GEOCODS, amostra_SubD, pop_coefDS_SubD, pop_coefPD_SubD, pop_coefVZ_SubD, pop_coefNR_SubD)

end_time <- Sys.time()
Tempo_subdist_pop <- end_time - start_time
start_time <- Sys.time()

for (i in Subd) {
  selecao <- TabelaCalcCoef[N_SubD == 1 & CD_GEOCODS == i]
  amostra <- n_distinct(selecao$ID_UNICO)
  result_dom <- try(optim(par = par.ini, fn = fnopt_dom, data = selecao, method = "L-BFGS-B", lower = lbound))
  SubD_coef_dpp[[i]] = data.frame(i)
  SubD_coef_dpp[[i]]$CD_GEOCODS = i
  SubD_coef_dpp[[i]]$dom_coefDS_SubD = ifelse(class(result_dom) == "list", result_dom$par[1], NA)
  SubD_coef_dpp[[i]]$dom_coefPD_SubD = ifelse(class(result_dom) == "list", result_dom$par[2], NA)
  SubD_coef_dpp[[i]]$dom_coefVZ_SubD = ifelse(class(result_dom) == "list", result_dom$par[3], NA)
  SubD_coef_dpp[[i]]$dom_coefNR_SubD = ifelse(class(result_dom) == "list", result_dom$par[4], NA)
  SubD_coef_dpp[[i]]$amostra_SubD = amostra
}

Coefs_SubD_dom <- bind_rows(SubD_coef_dpp) %>%
  select(CD_GEOCODS, amostra_SubD, dom_coefDS_SubD, dom_coefPD_SubD, dom_coefVZ_SubD, dom_coefNR_SubD)

end_time <- Sys.time()
Tempo_subdist_dom <- end_time - start_time
start_time <- Sys.time()

for (i in Dist) {
  selecao <- TabelaCalcCoef[N_Dist == 1 & CD_GEOCODD == i]
  amostra <- n_distinct(selecao$ID_UNICO)
  result_pop <- try(optim(par = par.ini, fn = fnopt_pop, data = selecao, method = "L-BFGS-B", lower = lbound))
  Dist_coef_dpp[[i]] = data.frame(i)
  Dist_coef_dpp[[i]]$CD_GEOCODD = i
  Dist_coef_dpp[[i]]$pop_coefDS_Dist = ifelse(class(result_pop) == "list", result_pop$par[1], NA)
  Dist_coef_dpp[[i]]$pop_coefPD_Dist = ifelse(class(result_pop) == "list", result_pop$par[2], NA)
  Dist_coef_dpp[[i]]$pop_coefVZ_Dist = ifelse(class(result_pop) == "list", result_pop$par[3], NA)
  Dist_coef_dpp[[i]]$pop_coefNR_Dist = ifelse(class(result_pop) == "list", result_pop$par[4], NA)
  Dist_coef_dpp[[i]]$amostra_Dist = amostra
}

Coefs_Dist_pop <- bind_rows(Dist_coef_dpp) %>%
  select(CD_GEOCODD, amostra_Dist, pop_coefDS_Dist, pop_coefPD_Dist, pop_coefVZ_Dist, pop_coefNR_Dist)

end_time <- Sys.time()
Tempo_dist_pop <- end_time - start_time
start_time <- Sys.time()

for (i in Dist) {
  selecao <- TabelaCalcCoef[N_Dist == 1 & CD_GEOCODD == i]
  amostra <- n_distinct(selecao$ID_UNICO)
  result_dom <- try(optim(par = par.ini, fn = fnopt_dom, data = selecao, method = "L-BFGS-B", lower = lbound))
  Dist_coef_dpp[[i]] = data.frame(i)
  Dist_coef_dpp[[i]]$CD_GEOCODD = i
  Dist_coef_dpp[[i]]$dom_coefDS_Dist = ifelse(class(result_dom) == "list", result_dom$par[1], NA)
  Dist_coef_dpp[[i]]$dom_coefPD_Dist = ifelse(class(result_dom) == "list", result_dom$par[2], NA)
  Dist_coef_dpp[[i]]$dom_coefVZ_Dist = ifelse(class(result_dom) == "list", result_dom$par[3], NA)
  Dist_coef_dpp[[i]]$dom_coefNR_Dist = ifelse(class(result_dom) == "list", result_dom$par[4], NA)
  Dist_coef_dpp[[i]]$amostra_Dist = amostra
}

Coefs_Dist_dom <- bind_rows(Dist_coef_dpp) %>%
  select(CD_GEOCODD, amostra_Dist, dom_coefDS_Dist, dom_coefPD_Dist, dom_coefVZ_Dist, dom_coefNR_Dist)

end_time <- Sys.time()
Tempo_dist_dom <- end_time - start_time
start_time <- Sys.time()

for (i in Mun) {
  selecao <- TabelaCalcCoef[N_Mun == 1 & CD_GEOCODM == i]
  amostra <- n_distinct(selecao$ID_UNICO)
  result_pop <- try(optim(par = par.ini, fn = fnopt_pop, data = selecao, method = "L-BFGS-B", lower = lbound))
  Mun_coef_dpp[[i]] = data.frame(i)
  Mun_coef_dpp[[i]]$CD_GEOCODM = i
  Mun_coef_dpp[[i]]$pop_coefDS_Mun = ifelse(class(result_pop) == "list", result_pop$par[1], NA)
  Mun_coef_dpp[[i]]$pop_coefPD_Mun = ifelse(class(result_pop) == "list", result_pop$par[2], NA)
  Mun_coef_dpp[[i]]$pop_coefVZ_Mun = ifelse(class(result_pop) == "list", result_pop$par[3], NA)
  Mun_coef_dpp[[i]]$pop_coefNR_Mun = ifelse(class(result_pop) == "list", result_pop$par[4], NA)
  Mun_coef_dpp[[i]]$amostra_Mun = amostra
}

Coefs_Mun_pop <- bind_rows(Mun_coef_dpp) %>%
  select(CD_GEOCODM, amostra_Mun, pop_coefDS_Mun, pop_coefPD_Mun, pop_coefVZ_Mun, pop_coefNR_Mun)

end_time <- Sys.time()
Tempo_mun_pop <- end_time - start_time
start_time <- Sys.time()

for (i in Mun) {
  selecao <- TabelaCalcCoef[N_Mun == 1 & CD_GEOCODM == i]
  amostra <- n_distinct(selecao$ID_UNICO)
  result_dom <- try(optim(par = par.ini, fn = fnopt_dom, data = selecao, method = "L-BFGS-B", lower = lbound))
  Mun_coef_dpp[[i]] = data.frame(i)
  Mun_coef_dpp[[i]]$CD_GEOCODM = i
  Mun_coef_dpp[[i]]$dom_coefDS_Mun = ifelse(class(result_dom) == "list", result_dom$par[1], NA)
  Mun_coef_dpp[[i]]$dom_coefPD_Mun = ifelse(class(result_dom) == "list", result_dom$par[2], NA)
  Mun_coef_dpp[[i]]$dom_coefVZ_Mun = ifelse(class(result_dom) == "list", result_dom$par[3], NA)
  Mun_coef_dpp[[i]]$dom_coefNR_Mun = ifelse(class(result_dom) == "list", result_dom$par[4], NA)
  Mun_coef_dpp[[i]]$amostra_Mun = amostra
}

Coefs_Mun_dom <- bind_rows(Mun_coef_dpp) %>%
  select(CD_GEOCODM, amostra_Mun, dom_coefDS_Mun, dom_coefPD_Mun, dom_coefVZ_Mun, dom_coefNR_Mun)

end_time <- Sys.time()
Tempo_mun_dom <- end_time - start_time
start_time <- Sys.time()
Finish_time <- Sys.time()

###
### TÔ mexendo daqui


# SubD_pop_NAmostra <- setdiff(unique(TabelaCalcCoef_NAmostra$CD_GEOCODS), unique(Coefs_SubD_pop$CD_GEOCODS))
# SubD_dom_NAmostra <- setdiff(unique(TabelaCalcCoef_NAmostra$CD_GEOCODS), unique(Coefs_SubD_dom$CD_GEOCODS))
# 
# Dist_pop_NAmostra <- setdiff(unique(TabelaCalcCoef_NAmostra$CD_GEOCODD), unique(Coefs_Dist_pop$CD_GEOCODD))
# Dist_dom_NAmostra <- setdiff(unique(TabelaCalcCoef_NAmostra$CD_GEOCODD), unique(Coefs_Dist_dom$CD_GEOCODD))
# 
# Mun_pop_NAmostra <- setdiff(unique(TabelaCalcCoef_NAmostra$CD_GEOCODM), unique(Coefs_Mun_pop$CD_GEOCODM))
# Mun_dom_NAmostra <- setdiff(unique(TabelaCalcCoef_NAmostra$CD_GEOCODM), unique(Coefs_Mun_dom$CD_GEOCODM))

# setdiff(Mun_NAmostra, unique(Coefs_Mun_dom$CD_GEOCODM))
# 
# length(Mun_dom_NAmostra)
# length(SubD_NAmostra)
# 
### Testando esse código abaixo

# AA_Coefs_Mun_dom_teste <- if_else(length(Mun_dom_NAmostra) != 0,
#                                bind_rows(list(Coefs_Mun_dom, data.frame(CD_GEOCODM = Mun_dom_NAmostra,
#                                                                  amostra_Mun = 0,
#                                                                  dom_coefDS_Mun = 1,
#                                                                  dom_coefPD_Mun = 1,
#                                                                  dom_coefVZ_Mun = 1,
#                                                                  dom_coefNR_Mun = 1))),
#                               Coefs_Mun_dom)
# 
# AA_Coefs_SubD_dom_teste <- if_else(length(SubD_dom_NAmostra) != 0,
#                               add_row(Coefs_SubD_dom, data.frame(CD_GEOCODS = SubD_dom_NAmostra,
#                                                                 amostra_SubD = 0,
#                                                                 dom_coefDS_SubD = 1,
#                                                                 dom_coefPD_SubD = 1,
#                                                                 dom_coefVZ_SubD = 1,
#                                                                 dom_coefNR_SubD = 1)),
#                               Coefs_SubD_dom)

# unique(Coefs_SubD_pop)
# 
# Coefs_SubD_pop_NAmostra <- tryCatch({}, data.frame(CD_GEOCODS = setdiff(SubD_NAmostra, unique(Coefs_SubD_pop$CD_GEOCODS)),
#                                       amostra_SubD = 0,
#                                       pop_coefDS_SubD = 1,
#                                       pop_coefPD_SubD = 1,
#                                       pop_coefVZ_SubD = 1,
#                                       pop_coefNR_SubD = 1))
# 
# Coefs_SubD_dom_NAmostra <- tryCatch({}, data.frame(CD_GEOCODS = setdiff(SubD_NAmostra, unique(Coefs_SubD_dom$CD_GEOCODS)),
#                                       amostra_SubD = 0,
#                                       dom_coefDS_SubD = 1,
#                                       dom_coefPD_SubD = 1,
#                                       dom_coefVZ_SubD = 1,
#                                       dom_coefNR_SubD = 1))
# 
# Coefs_Dist_pop_NAmostra <- tryCatch({}, data.frame(CD_GEOCODD = setdiff(Dist_NAmostra, unique(Coefs_Dist_pop$CD_GEOCODD)),
#                                       amostra_Dist = 0,
#                                       pop_coefDS_Dist = 1,
#                                       pop_coefPD_Dist = 1,
#                                       pop_coefVZ_Dist = 1,
#                                       pop_coefNR_Dist = 1))
# 
# Coefs_Dist_dom_NAmostra <- tryCatch({}, data.frame(CD_GEOCODD = setdiff(Dist_NAmostra, unique(Coefs_Dist_dom$CD_GEOCODD)),
#                                       amostra_Dist = 0,
#                                       dom_coefDS_Dist = 1,
#                                       dom_coefPD_Dist = 1,
#                                       dom_coefVZ_Dist = 1,
#                                       dom_coefNR_Dist = 1))
# 
# Coefs_Mun_pop_NAmostra <- tryCatch({}, data.frame(CD_GEOCODM = setdiff(Mun_NAmostra, unique(Coefs_Mun_pop$CD_GEOCODM)),
#                                      amostra_Mun = 0,
#                                      pop_coefDS_Mun = 1,
#                                      pop_coefPD_Mun = 1,
#                                      pop_coefVZ_Mun = 1,
#                                      pop_coefNR_Mun = 1), silent = TRUE)
# 
# Coefs_Mun_dom_NAmostra <- tryCatch({}, data.frame(CD_GEOCODM = setdiff(Mun_NAmostra, unique(Coefs_Mun_dom$CD_GEOCODM)),
#                                          amostra_Mun = 0,
#                                          dom_coefDS_Mun = 1,
#                                          dom_coefPD_Mun = 1,
#                                          dom_coefVZ_Mun = 1,
#                                          dom_coefNR_Mun = 1), silent = TRUE)
# 
# Coefs_SubD_pop <- bind_rows(list(Coefs_SubD_pop, Coefs_SubD_pop_NAmostra))
# 
# Coefs_SubD_dom <- bind_rows(list(Coefs_SubD_dom, Coefs_SubD_dom_NAmostra))
# 
# Coefs_Dist_pop <- bind_rows(list(Coefs_Dist_pop, Coefs_Dist_pop_NAmostra))
# 
# Coefs_Dist_dom <- bind_rows(list(Coefs_Dist_dom, Coefs_Dist_dom_NAmostra)))
# 
# Coefs_Mun_pop <- bind_rows(list(Coefs_Mun_pop, Coefs_Mun_pop_NAmostra)))
# 
# Coefs_Mun_dom <- bind_rows(list(Coefs_Mun_dom, Coefs_Mun_dom_NAmostra))

### Até aqui

Tempo_coefs <- Finish_time - start_time
Tempo_Total <- Finish_time - Begin_time

# rm(list = setdiff(ls(), c("Coefs_SubD_pop", "Coefs_SubD_dom", "Coefs_Dist_pop", "Coefs_Dist_dom", "Coefs_Mun_pop", "Coefs_Mun_dom",
#                           "TabelaCalcCoef", "TabelaCalcCoef_NAmostra", "Tempo_areasurb", "Tempo_municipios", "Tempo_geoproc", "Tempo_tabela", "Tempo_vars", "Tempo_subdist_pop",
#                           "Tempo_subdist_dom", "Tempo_dist_pop", "Tempo_dist_dom", "Tempo_mun_pop", "Tempo_mun_dom", "Tempo_coefs", "Tempo_Total")))

Tempo_areasurb
Tempo_municipios
Tempo_geoproc
Tempo_tabela
Tempo_vars
Tempo_subdist_pop
Tempo_subdist_dom
Tempo_dist_pop
Tempo_dist_dom
Tempo_mun_pop
Tempo_mun_dom
Tempo_coefs
Tempo_Total

SubD_pop_erro <- Coefs_SubD_pop$CD_GEOCODS[is.na(c(Coefs_SubD_pop$pop_coefDS_SubD))]
SubD_dom_erro <- Coefs_SubD_dom$CD_GEOCODS[is.na(c(Coefs_SubD_dom$dom_coefDS_SubD))]
Dist_pop_erro <- Coefs_Dist_pop$CD_GEOCODD[is.na(c(Coefs_Dist_pop$pop_coefDS_Dist))]
Dist_dom_erro <- Coefs_Dist_dom$CD_GEOCODD[is.na(c(Coefs_Dist_dom$dom_coefDS_Dist))]
Mun_pop_erro <- Coefs_Mun_pop$CD_GEOCODM[is.na(c(Coefs_Mun_pop$pop_coefDS_Mun))]
Mun_dom_erro <- Coefs_Mun_dom$CD_GEOCODM[is.na(c(Coefs_Mun_dom$dom_coefDS_Mun))]

# st_write(Coefs_SubD_pop, dsn = "W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/_GPKG/EstPop.gpkg", layer = "Coefs_SubD_pop", append = FALSE)
# st_write(Coefs_SubD_dom, dsn = "W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/_GPKG/EstPop.gpkg", layer = "Coefs_SubD_dom", append = FALSE)
# st_write(Coefs_Dist_pop, dsn = "W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/_GPKG/EstPop.gpkg", layer = "Coefs_Dist_pop", append = FALSE)
# st_write(Coefs_Dist_dom, dsn = "W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/_GPKG/EstPop.gpkg", layer = "Coefs_Dist_dom", append = FALSE)
# st_write(Coefs_Mun_pop, dsn = "W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/_GPKG/EstPop.gpkg", layer = "Coefs_Mun_pop", append = FALSE)
# st_write(Coefs_Mun_dom, dsn = "W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/_GPKG/EstPop.gpkg", layer = "Coefs_Mun_dom", append = FALSE)
