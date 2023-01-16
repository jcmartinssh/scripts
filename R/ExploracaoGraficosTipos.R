library(tidyverse)
library(readODS)
library(readxl)
library(dplyr)
library(ggplot2)

# Importa a tabela M_gMUNIC.xlsx e cria os campos de Códigos e Siglas das UFs e Regiões, usando fatores e ordenando segundo 
# padrão IBGE para os gráficos e tabelas

M_MUNIC <- read_excel("W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/ExploracaoDados/M_gMUNIC.xlsx",
                        col_types = c("text", "text", "numeric", 
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
  mutate(GdReg = factor(as.numeric(substr(COD_MUNICIPIO, 1, 1)), labels = c("N", "NE", "SE", "S", "CO")),
         CodUF = factor(as.numeric(substr(COD_MUNICIPIO, 1, 2))),
         UF = factor(CodUF, labels = c("RO", "AC", "AM", "RR", "PA", "AP", "TO",
                                       "MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", "BA",
                                       "MG", "ES", "RJ", "SP", 
                                       "PR", "SC", "RS",
                                       "MS", "MT", "GO", "DF")))

D_MUNIC <- read_excel("W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/ExploracaoDados/D_gMUNIC.xlsx", 
                         col_types = c("text", "text", "numeric", 
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
                         "numeric", "numeric", "numeric")) %>% 
  mutate(GdReg = factor(as.numeric(substr(COD_MUNICIPIO, 1, 1)), labels = c("N", "NE", "SE", "S", "CO")),
         CodUF = factor(as.numeric(substr(COD_MUNICIPIO, 1, 2))),
         UF = factor(CodUF, labels = c("RO", "AC", "AM", "RR", "PA", "AP", "TO",
                                       "MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", "BA",
                                       "MG", "ES", "RJ", "SP", 
                                       "PR", "SC", "RS",
                                       "MS", "MT", "GO", "DF")))

# Carrega as tabelas com as variáveis das BATERs.
# Observe que não existe BATER no Distrito Federal, por isso não tem "DF" na lista de UFs.

M_BATER <- read_excel("W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/ExploracaoDados/M_gBATER_Definitiva_modificadoNatal.xls",
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
  

D_BATER <- read_excel("W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/ExploracaoDados/D_gBATER_09042018_modificadoNatal.xls",
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
                                    "numeric", "numeric", "numeric")) %>%
  
  mutate(GdReg = factor(as.numeric(substr(GeoMunic, 1, 1)), labels = c("N", "NE", "SE", "S", "CO")),
         CodUF = factor(as.numeric(substr(GeoMunic, 1, 2))),
         UF = factor(CodUF, labels = c("RO", "AC", "AM", "RR", "PA", "AP", "TO",
                                       "MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", "BA",
                                       "MG", "ES", "RJ", "SP", 
                                       "PR", "SC", "RS",
                                       "MS", "MT", "GO")))



# Inserindo a tabela com o Dicionário
DIC <- read_ods(path = "W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/ExploracaoDados/PARBR2018_VariaveisMorador.ods" ,sheet = 1, col_names = TRUE)

## experimentando tipos de graficos

cor_risco <- "red"

# Distribuição da pop / renda / Grande Região

M_MUNIC %>% 
  mutate(CD_GEOM = COD_MUNICIPIO, NomeMun = DSC_MUNICIPIO, TotPop = M004, TotDom = M001 ,sM_0 = M069, SM_01_04 = M062, SM_01_2 = M063, SM_1 = M064, SM_2 = M065, SM_3 = M066, SM_5 = M067, SM_5_ = M068) %>%
  select(c(CD_GEOM, NomeMun, GdReg, CodUF, UF, TotPop, TotDom, sM_0 , SM_01_04, SM_01_2, SM_1, SM_2, SM_3, SM_5, SM_5_)) %>%
  pivot_longer(cols = c(sM_0 , SM_01_04, SM_01_2, SM_1, SM_2, SM_3, SM_5, SM_5_), names_to = "FaixaRenda", values_to = "Qtd" ) %>%
  mutate(FaixaRenda = factor(FaixaRenda, ordered = TRUE)) %>%
  group_by(UF, FaixaRenda, GdReg) %>% 
  summarise(Total = sum(Qtd)) %>%
  ggplot(aes(x = FaixaRenda, y = Total, fill = FaixaRenda)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ GdReg, ncol = 5)

M_BATER %>% 
  mutate(CD_GEOM = GEO_MUN, NomeMun = NOM_MUN, TotPop = M004, TotDom = M001 ,sM_0 = M069, SM_01_04 = M062, SM_01_2 = M063, SM_1 = M064, SM_2 = M065, SM_3 = M066, SM_5 = M067, SM_5_ = M068) %>%
  select(c(CD_GEOM, NomeMun, GdReg, CodUF, UF, TotPop, TotDom, sM_0 , SM_01_04, SM_01_2, SM_1, SM_2, SM_3, SM_5, SM_5_)) %>%
  pivot_longer(cols = c(sM_0 , SM_01_04, SM_01_2, SM_1, SM_2, SM_3, SM_5, SM_5_), names_to = "FaixaRenda", values_to = "Qtd" ) %>%
  mutate(FaixaRenda = factor(FaixaRenda, ordered = TRUE)) %>%
  group_by(UF, FaixaRenda, GdReg) %>% 
  summarise(Total = sum(Qtd)) %>%
  ggplot(aes(x = FaixaRenda, y = Total, fill = FaixaRenda)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ GdReg, ncol = 5)
  

TabRendaUF <- M_MUNIC %>% 
  mutate(CD_GEOM = COD_MUNICIPIO, NomeMun = DSC_MUNICIPIO, TotPop = M004, TotDom = M001 ,sM_0 = M069, SM_01_04 = M062, SM_01_2 = M063, SM_1 = M064, SM_2 = M065, SM_3 = M066, SM_5 = M067, SM_5_ = M068) %>%
  select(c(CD_GEOM, NomeMun, GdReg, CodUF, UF, TotPop, TotDom, sM_0 , SM_01_04, SM_01_2, SM_1, SM_2, SM_3, SM_5, SM_5_)) %>%
  pivot_longer(cols = c(sM_0 , SM_01_04, SM_01_2, SM_1, SM_2, SM_3, SM_5, SM_5_), names_to = "FaixaRenda", values_to = "Qtd" ) %>%
  mutate(FaixaRenda = factor(FaixaRenda, ordered = TRUE)) %>%
  group_by(UF, FaixaRenda, GdReg) %>% 
  summarise(Total = sum(Qtd))
  
 
TabRendaRisco <- M_BATER %>% 
  mutate(CD_GEOM = GEO_MUN, NomeMun = NOM_MUN, TotPop = M004, TotDom = M001 ,sM_0 = M069, SM_01_04 = M062, SM_01_2 = M063, SM_1 = M064, SM_2 = M065, SM_3 = M066, SM_5 = M067, SM_5_ = M068) %>%
  select(c(CD_GEOM, NomeMun, GdReg, CodUF, UF, TotPop, TotDom, sM_0 , SM_01_04, SM_01_2, SM_1, SM_2, SM_3, SM_5, SM_5_)) %>%
  pivot_longer(cols = c(sM_0 , SM_01_04, SM_01_2, SM_1, SM_2, SM_3, SM_5, SM_5_), names_to = "FaixaRenda", values_to = "Qtd" ) %>%
  mutate(FaixaRenda = factor(FaixaRenda, ordered = TRUE)) %>%
  group_by(UF, FaixaRenda, GdReg) %>% 
  summarise(Risco = sum(Qtd)) %>%
  select(UF, FaixaRenda, Risco)

TabRendaUFRisco <- TabRendaUF %>%
  left_join(TabRendaRisco, by = c("UF", "FaixaRenda")) %>%
  mutate(P_Risco = Risco / Total) %>%
  group_by(UF, GdReg, FaixaRenda) %>%
  summarize(Pop = sum(Total), PopRisco = sum(Risco), P_Risco_R = PopRisco / Pop) %>%
  mutate_all(~replace(., is.na(.), 0))

TabRendaReg <- TabRendaUFRisco %>%
  group_by(GdReg, FaixaRenda) %>%
  summarize(Pop = sum(Pop), PopRisco = sum(PopRisco), P_Risco_R = PopRisco / Pop)

TabRendaBR <- TabRendaUFRisco %>%
  group_by(FaixaRenda) %>%
  summarize(Pop = sum(Pop), PopRisco = sum(PopRisco), P_Risco_R = PopRisco / Pop)

TabRendaReg %>%
  ggplot(aes(x = FaixaRenda, y = Pop/1000, fill = FaixaRenda)) +
  scale_y_continuous(breaks = c(1000, 5000, 10000, 20000, 30000)) +
  geom_bar(stat = "identity") +
  geom_bar(aes(y = PopRisco/1000), fill = cor_risco, stat = "identity") +
  #geom_text(aes(label = scales::percent(signif(P_Risco_R, 7))), y = -100) +
  coord_flip() +
  theme(legend.position = "none") +
  facet_wrap(~GdReg, nrow = 5)

TabRendaBR %>%
  ggplot(aes(x = FaixaRenda, y = Pop/1000, fill = FaixaRenda)) +
  scale_y_continuous(breaks = c(1000, 5000, 10000, 20000, 30000)) +
  geom_bar(stat = "identity") +
  geom_bar(aes(y = PopRisco/1000), fill = cor_risco, stat = "identity") +
  #geom_text(aes(label = scales::percent(signif(P_Risco_R, 7))), y = -100) +
  coord_flip() +
  theme(legend.position = "none")
 

TabRendaUFRisco %>%
  ggplot(aes(x = FaixaRenda, y = Pop/1000, fill = FaixaRenda)) +
  scale_y_continuous(breaks = c(1000, 5000, 10000, 20000, 30000)) +
  geom_bar(stat = "identity") +
  geom_bar(aes(y = PopRisco/1000), fill = cor_risco, stat = "identity") +
  #geom_text(aes(label = scales::percent(signif(P_Risco_R, 1)))) +
  coord_flip() +
  theme(legend.position = "none") +
  facet_wrap(~UF, ncol = 4)

## filtro: municípios mapeados

M_BATER_MUN <- M_BATER %>%
  group_by(GEO_MUN) %>%
  summarise(PopRisco = sum(M004))
  
  
  
M_MUNIC_MAP <- M_MUNIC %>%
  left_join(M_BATER_MUN, by = c("COD_MUNICIPIO" = "GEO_MUN"))


MapTabRendaUF <- M_MUNIC_MAP %>% 
  filter(PopRisco >0) %>%
  mutate(CD_GEOM = COD_MUNICIPIO, NomeMun = DSC_MUNICIPIO, TotPop = M004, TotDom = M001 ,sM_0 = M069, SM_01_04 = M062, SM_01_2 = M063, SM_1 = M064, SM_2 = M065, SM_3 = M066, SM_5 = M067, SM_5_ = M068) %>%
  select(c(CD_GEOM, NomeMun, GdReg, CodUF, UF, TotPop, TotDom, sM_0 , SM_01_04, SM_01_2, SM_1, SM_2, SM_3, SM_5, SM_5_)) %>%
  pivot_longer(cols = c(sM_0 , SM_01_04, SM_01_2, SM_1, SM_2, SM_3, SM_5, SM_5_), names_to = "FaixaRenda", values_to = "Qtd" ) %>%
  mutate(FaixaRenda = factor(FaixaRenda, ordered = TRUE)) %>%
  group_by(UF, FaixaRenda, GdReg) %>% 
  summarise(Total = sum(Qtd))


MapTabRendaUFRisco <- MapTabRendaUF %>%
  left_join(TabRendaRisco, by = c("UF", "FaixaRenda")) %>%
  mutate(P_Risco = Risco / Total) %>%
  group_by(UF, GdReg, FaixaRenda) %>%
  summarize(Pop = sum(Total), PopRisco = sum(Risco), P_Risco_R = PopRisco / Pop) %>%
  mutate_all(~replace(., is.na(.), 0))

MapTabRendaReg <- MapTabRendaUFRisco %>%
  group_by(GdReg, FaixaRenda) %>%
  summarize(Pop = sum(Pop), PopRisco = sum(PopRisco), P_Risco_R = PopRisco / Pop)


MapTabRendaReg %>%
  ggplot(aes(x = FaixaRenda, y = Pop/1000, fill = FaixaRenda)) +
  scale_y_continuous(breaks = c(1000, 5000, 10000, 20000, 30000)) +
  geom_bar(stat = "identity") +
  geom_bar(aes(y = PopRisco/1000), fill = cor_risco, stat = "identity") +
  #geom_text(aes(label = scales::percent(signif(P_Risco_R, 7))), y = -100) +
  coord_flip() +
  theme(legend.position = "none") +
  facet_wrap(~GdReg, nrow = 5)

MapTabRendaUFRisco %>%
  ggplot(aes(x = FaixaRenda, y = Pop/1000, fill = FaixaRenda)) +
  scale_y_continuous(breaks = c(1000, 5000, 10000, 20000, 30000)) +
  geom_bar(stat = "identity") +
  geom_bar(aes(y = PopRisco/1000), fill = cor_risco, stat = "identity") +
  #geom_text(aes(label = scales::percent(signif(P_Risco_R, 1)))) +
  coord_flip() +
  theme(legend.position = "none") +
  facet_wrap(~UF, ncol = 4)

