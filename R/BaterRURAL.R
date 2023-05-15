library(readr)
library(tidyverse)
library(stringr)
library(sf)
library(data.table)
library(utils)

# arquivos corruptos oriundos do ftp: 29, 31, 33, 35, 41, 43
# foram descompactados com o 7zip e recompactados

# CodUF <- c('24', '41')

CodUF <- c('11', '12', '13', '14', '15', '16', '17', '21', '22', '23', '24', '25', '26',
           '27', '28', '29', '31', '32', '33', '35', '41', '42', '43', '50', '51', '52', '53')

espec_end <- c('01', '02', '03', '04', '05', '12')

fcon <- function(c) {
  s = ifelse(str_sub(c, -1, -1) == "O" | str_sub(c, -1, -1) == "S", -1, 1)
  form = as.numeric(str_sub(c, 1, 2)) + 
    as.numeric(str_sub(ifelse(str_sub(c, 2, 2) == " ", paste("0", c, sep = ""), c), 4, 5)) / 60 + 
    as.numeric(str_sub(c, -9, -2)) / 3600
  s * form
}

TabelaAnalise <- list()

# CodUF <- "33"


for (i in CodUF) {
  unzip(str_glue("C:/ACELERADOR/ProcCNEFE/", i, ".zip"), exdir = "C:/ACELERADOR/ProcCNEFE/temp")
  arquivo <- (str_glue("C:/ACELERADOR/ProcCNEFE/temp/", i, ".txt"))
  tabela_enderecos <- read_fwf(arquivo, 
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
  file.remove(arquivo)
  gc()
  
  tabela_enderecos <- tabela_enderecos %>%
    mutate(across(everything(), str_trim)) %>%
    filter(LATITUDE != "" & LONGITUDE != "") %>%
    mutate(LATITUDE = fcon(LATITUDE), LONGITUDE = fcon(LONGITUDE))
  
  # aqui as coordenadas convertidas são transformadas em geometria no SIRGAS 2000 (EPSG 4674)
  pontos_enderecos <- st_as_sf(tabela_enderecos, coords = c("LONGITUDE", "LATITUDE"), crs = 4674)
  
  # aqui exporta os pontos para um arquivo shapefile com o codifo da uf no nome
  # st_write(pontos_enderecos, str_glue("C:/ACELERADOR/ProcCNEFE/pontos/pontos_", i, ".shp"))
  
  # cria a query dos setores 
  query_setor <- str_glue("SELECT CD_GEOCODI, CD_GEOCODS, CD_GEOCODD, CD_GEOCODM, NM_MUNICIP, geom FROM 'SETORES CENSITÁRIOS' WHERE substr(CD_GEOCODM, 1, 2) in ('", i, "')")
  
  # carrega os setores censitarios do municipio
  setores <- st_read("C:/ACELERADOR/Bases/BASE_2010.gpkg",
                     query = query_setor)
  
  setores <- st_make_valid(setores)
  
  # cria a query das baters
  query_bater <- str_glue("SELECT GEO_UF, GEO_MUN, GEO_BATER, ORIGEM, ACURACIA, geom FROM '4_Bater_Publicacao2018_CamposControle' WHERE GEO_UF in ('", i, "')")
  
  # carrega os setores censitarios do municipio
  baters <- st_read("C:/ACELERADOR/Bases/CEMADEN.gpkg",
                    query = query_bater)
  
  baters <- st_make_valid(baters)
  
  pontos_final <- pontos_enderecos %>%
    st_join(baters, join = st_intersects, left = TRUE) %>%
    st_join(setores, join = st_intersects, left = TRUE) %>%
    filter(!is.na(CD_GEOCODI) & ESPEC_ENDC %in% espec_end) %>%
    select(ESPEC_ENDC, GEO_BATER, CD_GEOCODI)
  
  st_geometry(pontos_final) <- NULL
  
  setDT(pontos_final)
  tabela_final <- (
    pontos_final
    [, ptsetor := .N, by = CD_GEOCODI]
    [!is.na(GEO_BATER), ptinter := .N, by = .(CD_GEOCODI, GEO_BATER)]
    [!is.na(GEO_BATER), ptsetRisco := .N, by = CD_GEOCODI]
    [!is.na(GEO_BATER), .(CD_GEOCODI, GEO_BATER, ptsetor, ptinter, ptsetRisco)]
  )
  
  tabela_final <- unique(tabela_final)
  
  query_basico <- str_glue("SELECT Cod_setor, V001, V002 FROM 'Basico' WHERE substr(Cod_setor, 1, 2) IN ('", i, "')")
  
  # Carrega a tabela de variáveis dos setores
  Varbasico <- st_read("C:/ACELERADOR/Bases/BASE_2010.gpkg",
                       query = query_basico)
  
  tabela_final <- tabela_final %>%
    left_join(Varbasico, by = c("CD_GEOCODI" = "Cod_setor")) %>%
    mutate(V001 = as.numeric(V001), V002 = as.numeric(V002))

  
  TabelaAnalise[[i]] = tabela_final
}

TabelaAnalise <- bind_rows(TabelaAnalise)

# cria a query das baters
query_bater <- str_glue("SELECT GEO_UF, GEO_MUN, GEO_BATER, ORIGEM, ACURACIA, geom FROM '4_Bater_Publicacao2018_CamposControle'")

# carrega os setores censitarios do municipio
baters <- st_read("C:/ACELERADOR/Bases/CEMADEN.gpkg",
                  query = query_bater)

TabelaAnalise <- TabelaAnalise %>%
  left_join(baters, by = "GEO_BATER") %>%
  filter(!is.na(V001)) %>%
  select(CD_GEOCODI, GEO_BATER, ORIGEM, ACURACIA, ptsetor, ptinter, ptsetRisco, V001, V002)


TabelaAnaliseSemAssoc <- TabelaAnalise %>%
  filter(is.na(ORIGEM))


# st_write(TabelaAnalise, dsn = "W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Rodrigo/Plano de trabalho 2021/dezembro2022/TabelaJoaquim_20221208/FeitoBrasiTodasBaters.xlsx", append = FALSE)
# st_write(TabelaAnaliseSemAssoc, dsn = "W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Rodrigo/Plano de trabalho 2021/dezembro2022/TabelaJoaquim_20221208/FeitoBrasilBaterSemAssoc.xlsx", append = FALSE)

  



