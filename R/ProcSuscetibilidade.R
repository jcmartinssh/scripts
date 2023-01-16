## Transposição dos dados de suscetibilidade a deslizamentos para a grade estatística - pelo QGIS tava dando pau. Aproveito para aprender a fazer como pacote sf.

library(tidyverse)
library(sf)

# evitar problema de loop na hora do spatial join
sf_use_s2(FALSE)

Sys.setlocale("LC_ALL", "English")
Sys.setenv(LANG = "English")

## carrega as camadas

# camada de uso e cobertura do solo
GrdUsoCob <- st_read("C:/ACELERADOR/Suscetibilidade/Bases_Suscetibilidade.gpkg",
                     layer = "UsoCoberturaTerra2000-2018") %>%
  mutate(INDICE_GRE = factor(INDICE_GRE))

# Grade estatística populacional com agregação do dado da grade estatística para as células de 1 km e eliminação de células não habitadas
GrdEstPOP <- st_read("C:/ACELERADOR/Suscetibilidade/GradeEstatistica.gpkg",
                     layer = "GradeEstatistica",
                     query = "select ID_UNICO, nome_1KM, nome_5KM, nome_10KM, nome_50KM, nome_100KM, nome_500KM, QUADRANTE, MASC, FEM, POP, DOM_OCU from \"GradeEstatistica\"") %>%
  mutate(nome_1KM = factor(nome_1KM)) %>%
  group_by(nome_1KM) %>%
  summarise(MASC = sum(MASC), FEM = sum(FEM), POP = sum(POP), DOM_OCU = sum(DOM_OCU)) %>%
  filter(POP != 0)

# carregamento do uso e cobertura da terra na grade estatística populacional
GrdEst_Susc <- GrdUsoCob %>%
  right_join(GrdEstPOP, by = c("INDICE_GRE" = "nome_1KM"))

# remove a grade estatística e uso e cobertura originais
rm(GrdEstPOP)
rm(GrdUsoCob)
gc()

# camada de Suscetibilidade a deslizamentos
GrdSuscet <- st_read("C:/ACELERADOR/Suscetibilidade/Bases_Suscetibilidade.gpkg",
                     layer = "SuscetibilidadeDeslizamentos")

# carrega o dado de suscetibilidade para a grade estatística.
GrdEst_Susc <- st_join(GrdEst_Susc, GrdSuscet, join = st_intersects, left = TRUE, largest = TRUE)

# remove a grade de suscetibilidade original.
rm(GrdSuscet)
gc()

# camada de municípios
Municipios <- st_read("C:/ACELERADOR/Suscetibilidade/MUNICIPIOS.gpkg", layer = "BRMUE250GC_SIR_2019") %>%
  mutate(GdReg = factor(as.numeric(substr(CD_MUN, 1, 1)), labels = c("N", "NE", "SE", "S", "CO")),
         CodUF = factor(as.numeric(substr(CD_MUN, 1, 2))),
         UF = factor(CodUF, labels = c("RO", "AC", "AM", "RR", "PA", "AP", "TO",
                                       "MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", "BA",
                                       "MG", "ES", "RJ", "SP", 
                                       "PR", "SC", "RS",
                                       "MS", "MT", "GO", "DF")))

# carrega o geocodigo dos municipios.
GrdEst_Susc <- st_join(GrdEst_Susc, Municipios, join = st_intersects, left = TRUE, largest = TRUE)

# # camada de arranjos populacionais
# ArranjosPop <- st_read("C:/ACELERADOR/Suscetibilidade/ArranjosPopulacionais_2ed.gpkg", layer = "BRMUE250GC_SIR_2019")

# remove da memória a camada de municipios
rm(Municipios)
gc()


# camada de áreas urbanizadas 2015
AreasUrb <- st_read("C:/ACELERADOR/Suscetibilidade/AreasUrbanizadas2015.gpkg",
                    layer = "AreasUrbanizadas2015_CorrecaoTopologia") %>%
  filter(Tipo %in% c("Área urbanizada", "Outros equipamentos urbanos")) %>%
  select(!(UF))

# classifica as areas artificias de acordo com o areas urbanizadas e limpa as colunas
GrdEst_Susc <- st_join(GrdEst_Susc, AreasUrb, join = st_intersects, left = TRUE, largest = TRUE) %>%
  select(INDICE_GRE, MASC, FEM, POP, DOM_OCU, USO2000, USO2010, USO2012, USO2014, USO2016, USO2018, susdsl_gr, susdsl_ds, CD_MUN, NM_MUN, UF, GdReg, Tipo, CodConcUrb, NomeConcUr, UF, geom)

# remove da memória as áreas urbanizadas 2015
rm(AreasUrb)
gc()

# salva o arquivo na rede
st_write(GrdEst_Susc, "C:/ACELERADOR/Suscetibilidade/EstudoSuscetibilidade.gpkg", layer = "ExposicaoSuscPOP")

# extrai a tabela sem geometria para facilitar a preparação de gráficos
TabUsoCob_Susc <- GrdEst_Susc
st_geometry(TabUsoCob_Susc) <- NULL

# grafico : qtde de células por grau suscetibilidade por Gde Região
TabUsoCob_Susc %>%
  drop_na(c(GdReg, susdsl_ds)) %>%
  mutate(Suscetibilidade = factor(susdsl_ds, levels = c("Muito Alta", "Alta", "Média", "Baixa", "Muito Baixa"))) %>%
  ggplot(aes(x = GdReg, fill = Suscetibilidade)) +
  geom_histogram(stat = "count") + 
  labs(title = "Distribuição de áreas habitadas por grau de suscetibilidade e Grande Região", x = "Grande Região", y = "Número de células habitadas da grade estatística de 1km") +
  scale_y_continuous(breaks = c(100000, 200000, 300000, 400000, 500000), labels = c("100K", "200K", "300K", "400K", "500K"))

# grafico: Total População por grau suscetibilidade por Gde Região
TabUsoCob_Susc %>%
  drop_na(c(GdReg, susdsl_ds)) %>%
  mutate(Suscetibilidade = factor(susdsl_ds, levels = c("Muito Alta", "Alta", "Média", "Baixa", "Muito Baixa"))) %>%
  group_by(GdReg, Suscetibilidade) %>%
  summarise(POP = sum(POP)) %>%
  ggplot(aes(x = GdReg, y = POP, fill = Suscetibilidade)) +
  geom_col(position = "stack") +
  labs(title = "População total por grau de suscetibilidade e Grande Região", x = "Grande Região", y = "População") +
  scale_y_continuous(breaks = c(10000000, 20000000, 30000000, 40000000, 50000000, 60000000), labels = c("10M", "20M", "30M", "40M", "50M", "60M"))




# camada REINDESC já processada no QGIS
# REINDESC <- st_read("W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/DESENVOLVIMENTO/MAPA RISCO DESLIZAMENTO BRASIL/produtos/Reindesc_Teste.shp")
# 
# REINDESC %>% 
#   drop_na(GdReg) %>%
#   ggplot(aes(x = IndRisco, fill = GdReg)) + 
#   geom_histogram() +
#   facet_wrap(~ GdReg, ncol = 5)
# 
# REINDESC %>% 
#   drop_na(GdReg) %>%
#   ggplot(aes(x = susdsl_gr, fill = GdReg)) + 
#   geom_histogram() +
#   geom_histogram(aes(x = susdsl_gr), data = GrdEst_Susc, fill = "black") +
#   facet_wrap(~ GdReg, ncol = 5)
# 
# REINDESC %>% 
#   drop_na(GdReg) %>%
#   ggplot(aes(x = POP, fill = GdReg)) + 
#   geom_histogram() +
#   facet_wrap(~ GdReg, ncol = 5)

