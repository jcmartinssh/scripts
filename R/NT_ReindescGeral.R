library(tidyverse)
library(sf)

PastaSaida <- "W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/DESENVOLVIMENTO/REINDESC/NotaTecnica/Produtos/"

setwd(PastaSaida)

st_layers("W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/DESENVOLVIMENTO/REINDESC/NotaTecnica/Bases/BasesNotaTecnica.gpkg")

REINDESC <- st_read(
  "W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/DESENVOLVIMENTO/REINDESC/NotaTecnica/Bases/BasesNotaTecnica.gpkg", 
  layer = "REINDESC_Base")

BaseMunicipios <- st_read(
  "W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/DESENVOLVIMENTO/REINDESC/NotaTecnica/Bases/BasesNotaTecnica.gpkg", 
  layer = "BaseMunicipios")

PopMunic <- st_read(
  "W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/DESENVOLVIMENTO/REINDESC/NotaTecnica/Bases/BasesNotaTecnica.gpkg", 
  layer = "M_gMUNIC")


st_geometry(BaseMunicipios) <- NULL

REINDESC$TIPO[REINDESC$ID_OCORR == 3392] <- "Geo"
REINDESC$EVENTO[REINDESC$ID_OCORR == 3392] <- "Tremor"


## AGSN

AGSN_Var <- st_read(
  "W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/DESENVOLVIMENTO/REINDESC/NotaTecnica/Bases/BasesNotaTecnica.gpkg",
  layer = "AGSN_Variaveis")

AGSN_BME <- read.csv(
  "W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/DESENVOLVIMENTO/REINDESC/NotaTecnica/Bases/resultado_bmm_162997.csv",
  sep = ";", 
  fileEncoding = "UTF-8")

AGSN_Base <- st_read(
  "W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/DESENVOLVIMENTO/REINDESC/NotaTecnica/Bases/BasesNotaTecnica.gpkg",
  layer = "SetoresAGSN")


st_geometry(AGSN_Base) <- NULL


## BATER

BATER_Var <- st_read(
  "W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/DESENVOLVIMENTO/REINDESC/NotaTecnica/Bases/BasesNotaTecnica.gpkg",
  layer = "M_gBATER_Definitiva_modificadoNatal")

## Tipologia


Tipologia_Var <- st_read(
  "W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/DESENVOLVIMENTO/REINDESC/NotaTecnica/Bases/BasesNotaTecnica.gpkg",
  layer = "TipoIntraUrbESTATISTICAS2010_PorConcTipo")

Tipologia_Var <- Tipologia_Var %>%
  select(CodConcUrb, TipoIntraUrb, totpes) %>%
  drop_na(CodConcUrb)
 

## CNEFE

#FacesAssociadasEventos <- st_read(
#  "W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/DESENVOLVIMENTO/REINDESC/NotaTecnica/Bases/FacesREINDESC_SE.gpkg",
#  layer = "FacesAssociadasEventos")

EventosAssociadosFaces <- st_read(
  "W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/DESENVOLVIMENTO/REINDESC/NotaTecnica/Bases/FacesREINDESC_SE.gpkg",
  layer = "EventosAssociadosFaces")




### Cálculo da população das concentrações urbanas e inserção de grande região

BaseMunicipios <- BaseMunicipios %>%
  mutate(GdReg = factor(as.numeric(substr(CD_GEOCODM, 1, 1)), labels = c("N", "NE", "SE", "S", "CO"))) %>%
  mutate(CodConcUrb = ifelse(is.na(CodConcUrb), "0000000", CodConcUrb), NomeConcUr = ifelse(is.na(NomeConcUr), 'Fora de Concentração Urbana de médio ou grande porte.', NomeConcUr)) %>%
  mutate(COD_MUNICIPIO = as.integer(CD_GEOCODM)) %>%
  full_join(PopMunic, by = "COD_MUNICIPIO") %>%
  drop_na(M004) %>%
  mutate(PopM = M004) %>%
  group_by(GdReg, CodConcUrb)  %>%
  mutate(PopConUrb = sum(PopM), TotMunic = n()) %>%
  select(GdReg, CD_GEOCODM, NM_MUNICIP, PopM, CodConcUrb, NomeConcUr, PopConUrb, TotMunic)
 

### Número de ocorrências por tipo

REINDESC_Munic <- REINDESC
st_geometry(REINDESC_Munic) <- NULL

REINDESC_Munic <- REINDESC_Munic %>% 
  select(UF, ID_OCORR, ID_REIND, CD_GEOM, TIPO, MAGNITUDE, PREC_LOCAL, GEO_BATER, CD_AGSN, NM_AGSN, TipoSubtip) %>%
  mutate(TIPO = ifelse(
    TIPO %in% c("Hidro", "Hidro."), "Hidro", ifelse(
      TIPO %in% c("Geo", "Geo.", "Geo  Queda de muro de arrimo"), "Geo", ifelse(
        TIPO == "Hidro, Geo", "Misto", TIPO)))) %>%
  mutate(PREC_LOCAL = ifelse(
    PREC_LOCAL == "BAIXA PRECISÃO", "Baixa", ifelse(
      PREC_LOCAL == "MÉDIA PRECISÃO", "Media", ifelse(
        PREC_LOCAL == "ALTA PRECISÃO", "Alta", ifelse(
          PREC_LOCAL == "AUSENTE", "sem inf.", PREC_LOCAL))))) %>%
  replace_na(list(PREC_LOCAL = "sem inf.")) %>%
  left_join(BaseMunicipios, by = c("CD_GEOM" = "CD_GEOCODM"))

Munic_Ocor <- REINDESC_Munic %>%
  group_by(CD_GEOM, TIPO) %>%
  mutate(freq = n()) %>%
  pivot_wider(id_cols = c(CD_GEOM),
              names_from = TIPO,
              values_fill = 0,
              values_from = freq,
              values_fn = max) %>%
  mutate(Hidro = Hidro + Misto, Geo = Geo + Misto) %>%
  select(CD_GEOM, Hidro, Geo)
  
  REINDESC_ConcUrb <- REINDESC_Munic %>%
  group_by(GdReg, CodConcUrb, NomeConcUr, TIPO) %>%
  mutate(freq = n()) %>%
  pivot_wider(id_cols = c(GdReg, CodConcUrb, NomeConcUr),
              names_from = TIPO,
              values_fill = 0,
              values_from = freq,
              values_fn = max) %>%
  mutate(Hidro = Hidro + Misto, Geo = Geo + Misto) %>%
  select(GdReg, CodConcUrb, NomeConcUr, Hidro, Geo)

########
###
### RANKS AGSN E BATER
###
########



### Cria as tabelas de ranking de BATERs por número de ocorrências

Rank_Baters <- REINDESC_Munic %>%
  filter(!is.na(GEO_BATER) & PREC_LOCAL %in% c("Media", "Alta")) %>%
  group_by(GdReg, UF, NM_MUNICIP, GEO_BATER) %>%
  summarize(freq = n()) %>%
  arrange(desc(freq))


#st_write(Rank_Baters, paste(PastaSaida, "Rank_Bater.ods", sep = ""), append = FALSE)

### Cria as tabelas de ranking de AGSN por número de ocorrências

Rank_AGSN <- REINDESC_Munic %>%
  filter(!is.na(CD_AGSN) & PREC_LOCAL %in% c("Media", "Alta")) %>%
  group_by(GdReg, UF, NM_MUNICIP, CD_AGSN, NM_AGSN) %>%
  summarize(freq = n()) %>%
  arrange(desc(freq))


#st_write(Rank_AGSN, paste(PastaSaida, "Rank_AGSN.ods", sep = ""), append = FALSE)


########
###
### CONCENTRAÇÃO URBANA
###
########


### Tipo "misto" foi considerado no somatório de hidro e de geo 

REINDESC_ConcUrb <- REINDESC_Munic %>%
  group_by(GdReg, CodConcUrb, NomeConcUr, TIPO) %>%
  mutate(freq = n()) %>%
  pivot_wider(id_cols = c(GdReg, CodConcUrb, NomeConcUr),
              names_from = TIPO,
              values_fill = 0,
              values_from = freq,
              values_fn = max) %>%
  mutate(Hidro = Hidro + Misto, Geo = Geo + Misto) %>%
  select(GdReg, CodConcUrb, NomeConcUr, Hidro, Geo)
  
Tabela_ConcUrb <- BaseMunicipios %>%
  select(GdReg, CodConcUrb, NomeConcUr, PopConUrb, TotMunic) %>%
  group_by(GdReg, CodConcUrb, NomeConcUr, PopConUrb, TotMunic) %>%
  summarise() %>%
  left_join(REINDESC_ConcUrb, by = c("GdReg", "CodConcUrb")) %>%
  replace_na(list(Hidro = 0, Geo = 0)) %>%
  select(GdReg, CodConcUrb, NomeConcUr.x, TotMunic, PopConUrb, Hidro, Geo) %>%
  arrange(CodConcUrb)

SelecaoConcUrb <- Tabela_ConcUrb %>%
  filter(CodConcUrb != '0000000') %>%
  arrange(desc(Hidro + Geo)) %>%
  select(CodConcUrb, NomeConcUr.x)

ordemConcUrb <- SelecaoConcUrb$NomeConcUr.x

ConcUrbMaisImp <- head(SelecaoConcUrb$CodConcUrb, 10)


TabelaSimples_Concurb <- BaseMunicipios %>%
  filter(CodConcUrb %in% ConcUrbMaisImp) %>%
  select(GdReg, CodConcUrb, NomeConcUr, PopConUrb, TotMunic) %>%
  group_by(GdReg, CodConcUrb, NomeConcUr, PopConUrb, TotMunic) %>%
  summarise() %>%
  left_join(REINDESC_ConcUrb, by = c("GdReg", "CodConcUrb")) %>%
  replace_na(list(Hidro = 0, Geo = 0)) %>%
  select(GdReg, CodConcUrb, NomeConcUr.x, TotMunic, PopConUrb, Hidro, Geo) %>%
  mutate(NomeConcUr.x = factor(NomeConcUr.x, levels = ordemConcUrb, ordered = TRUE)) %>%
  arrange(NomeConcUr.x)

Tabela_MunicipiosConcUrb <- BaseMunicipios %>%
  filter(CodConcUrb %in% ConcUrbMaisImp) %>%
  mutate(NomeConcUr = factor(NomeConcUr, levels = ordemConcUrb, ordered = TRUE)) %>%
  group_by(CodConcUrb, NomeConcUr) %>%
  mutate(Munic = str_c(NM_MUNICIP, collapse = ", ")) %>%
  group_by(CodConcUrb, NomeConcUr, Munic) %>%
  summarise() %>%
  mutate(NomeConcUr = factor(NomeConcUr, levels = ordemConcUrb, ordered = TRUE)) %>%
  arrange(NomeConcUr)

Tabela_Municipios <- BaseMunicipios %>%
  filter(CodConcUrb %in% ConcUrbMaisImp) %>%
  left_join(Munic_Ocor, by = c("CD_GEOCODM" = "CD_GEOM")) %>%
  mutate(NomeConcUr = factor(NomeConcUr, levels = ordemConcUrb, ordered = TRUE)) %>%
  group_by(NM_MUNICIP, PopM, NomeConcUr) %>%
  replace_na(list("Hidro" = 0, "Geo" = 0)) %>%
  arrange(NomeConcUr, desc(Hidro + Geo)) %>%
  select(NM_MUNICIP, PopM, NomeConcUr, Hidro, Geo)

  

#st_write(TabelaSimples_Concurb, paste(PastaSaida, "TabelaSimples_Concurb.ods", sep = ""), append = FALSE)
#st_write(Tabela_MunicipiosConcUrb, paste(PastaSaida, "Tabela_MunicipiosConcUrb.ods", sep = ""), append = FALSE)
#st_write(Tabela_Municipios, paste(PastaSaida, "Tabela_Municipios.ods", sep = ""), append = FALSE)


ggplot(data = filter(Tabela_ConcUrb, CodConcUrb != "0000000")) +
  geom_point(aes(x = Geo, y = Hidro, size = PopConUrb, color = GdReg), position = "jitter") + 
  scale_x_sqrt() +
  scale_y_sqrt() + 
  geom_text(data = filter(Tabela_ConcUrb, CodConcUrb %in% ConcUrbMaisImp),
            aes(x = Geo, y = Hidro, label = NomeConcUr.x),
            size = 3,  
            stat = "identity",
            check_overlap = TRUE,
            nudge_y = 0.6)
  

# será que faz sentido esse rank?

Rank_ConcUrb <- REINDESC_ConcUrb %>%
  filter(CodConcUrb != "0000000") %>%
  mutate(N_Ocor = Hidro + Geo) %>%
  select(GdReg, CodConcUrb, NomeConcUr, N_Ocor) %>%
  arrange(desc(N_Ocor))

Rank_MunForaConcUrb <- REINDESC_Munic %>%
  filter(CodConcUrb == "0000000") %>%
  group_by(GdReg, UF, CD_GEOM, NM_MUNICIP) %>%
  summarise(freq = n()) %>%
  arrange(desc(freq))

#st_write(Rank_ConcUrb, paste(PastaSaida, "Rank_ConcUrb.ods", sep = ""), append = FALSE)

#st_write(Rank_MunForaConcUrb, paste(PastaSaida, "Rank_ForaConcUrb.ods", sep = ""), append = FALSE)

#st_write(Tabela_ConcUrb, paste(PastaSaida, "Tabela_ConcUrb.ods", sep = ""), append = FALSE)

########
###
### OPERAÇÃO
###
######## 

###  nº de eventos por classe de precisão da localização

Tabela_Operacao <- REINDESC_Munic %>%
  group_by(GdReg, CodConcUrb, NomeConcUr, PREC_LOCAL) %>%
  mutate(freq = n()) %>%
  pivot_wider(id_cols = c(GdReg, CodConcUrb, NomeConcUr), names_from = PREC_LOCAL, values_fill = 0, values_from = freq, values_fn = max) %>%
  select(GdReg, CodConcUrb, NomeConcUr, Alta, Media, Baixa, "sem inf.") %>%
  arrange(CodConcUrb)

TabelaSimples_Operacao <- REINDESC_Munic %>%
  filter(CodConcUrb %in% ConcUrbMaisImp) %>%
  group_by(GdReg, CodConcUrb, NomeConcUr, PREC_LOCAL) %>%
  mutate(freq = n()) %>%
  pivot_wider(id_cols = c(GdReg, CodConcUrb, NomeConcUr), names_from = PREC_LOCAL, values_fill = 0, values_from = freq, values_fn = max) %>%
  left_join(Tabela_ConcUrb, by = "CodConcUrb") %>%
  select(GdReg.x, CodConcUrb, NomeConcUr, Alta, Media, Baixa, "sem inf.", Hidro, Geo) %>%
  mutate(NomeConcUr = factor(NomeConcUr, levels = ordemConcUrb, ordered = TRUE)) %>%
  arrange(NomeConcUr)



  
#st_write(Tabela_Operacao, paste(PastaSaida, "Tabela_Operacao.ods", sep = ""), append = FALSE)
#st_write(TabelaSimples_Operacao, paste(PastaSaida, "TabelaSimples_Operacao.ods", sep = ""), append = FALSE)


########
###
### AGSN
###
########


### Criação do campo com geocódigo municipal

AGSN_Var <- AGSN_Var %>%
  mutate(CD_GEOCODI = COD_SETOR15) %>%
  left_join(AGSN_Base, by = "CD_GEOCODI") %>%
  select(CD_GEOCODM, CD_AGSN, NM_AGSN, CD_GEOCODI, M004) %>%
  group_by(CD_GEOCODM, CD_AGSN, NM_AGSN) %>%
  summarize(PopAGSN = sum(M004))

### Testando se a variável agregada dos setores AGSN é igual à variável AGSN do BME

Teste <- AGSN_BME %>%
  mutate(CD_AGSN = as.character(Código.do.Item.Geográfico)) %>%
  left_join(AGSN_Var, by = "CD_AGSN") %>%
  mutate(PopAgsn2 = Morador..número..operação..Soma.) %>%
  select(CD_AGSN, PopAGSN, PopAgsn2) %>%
  filter(PopAGSN == PopAgsn2)

# É tudo igual! trabalho à toa.... mas sabemos com certeza agora né. Vou continuar usando a agregada dos setores.
  
### Nº de eventos em AGSN e áreas normais por classe, população em AGSN com ocorrências e população total em AGSN.

AGSN_ConcUrb <- AGSN_Var %>%
  group_by(CD_GEOCODM) %>%
  summarise(PopMunAGSN = sum(PopAGSN)) %>%
  left_join(BaseMunicipios, by = "CD_GEOCODM") %>%
  group_by(GdReg, CodConcUrb, NomeConcUr, PopConUrb) %>%
  summarise(PopTotAGSN = sum(PopMunAGSN))
  

# Usando a tabela de população por AGSN obtida no BME, sem agregar dos setores


AGSN_Ocorr <- REINDESC_Munic %>%
  filter(PREC_LOCAL %in% c("Media", "Alta")) %>%
  replace_na(list(CD_AGSN = "00000000000")) %>%
  left_join(BaseMunicipios, by = c("CD_GEOM" = "CD_GEOCODM")) %>%
  group_by(GdReg.x, CodConcUrb.x) %>%
  mutate(Normal_Geo = if_else(CD_AGSN == "00000000000" & (TIPO == "Geo" | TIPO == "Misto"), as.integer(1), as.integer(0)),
         Normal_Hidro = if_else(CD_AGSN == "00000000000" & (TIPO == "Hidro" | TIPO == "Misto"), as.integer(1), as.integer(0)),
         AGSN_Geo = if_else(CD_AGSN != "00000000000" & (TIPO == "Geo" | TIPO == "Misto"), as.integer(1), as.integer(0)),
         AGSN_Hidro = if_else(CD_AGSN != "00000000000" & (TIPO == "Hidro" | TIPO == "Misto"), as.integer(1), as.integer(0))) %>%
  summarise(TotNorm_Geo = sum(Normal_Geo), TotNorm_Hidro = sum(Normal_Hidro), TotAGSN_Geo = sum(AGSN_Geo), TotAGSN_Hidro = sum(AGSN_Hidro)) 


AGSN_Sel <- unique(REINDESC$CD_AGSN)
  
Tabela_AGSN <- AGSN_Var %>%
  filter(CD_AGSN %in% AGSN_Sel) %>%
  left_join(BaseMunicipios, by = "CD_GEOCODM") %>%
  group_by(GdReg, CodConcUrb) %>%
  summarise(PopTotAGSN_Ocorr = sum(PopAGSN)) %>%
  left_join(AGSN_ConcUrb, by = c("GdReg",  "CodConcUrb")) %>%
  inner_join(AGSN_Ocorr, by = c("GdReg" = "GdReg.x", "CodConcUrb" = "CodConcUrb.x")) %>%
  select(GdReg, CodConcUrb, NomeConcUr, PopConUrb, PopTotAGSN, PopTotAGSN_Ocorr, TotNorm_Geo, TotNorm_Hidro, TotAGSN_Geo, TotAGSN_Hidro) %>%
  arrange(CodConcUrb)

TabelaSimples_AGSN <- AGSN_Var %>%
  filter(CD_AGSN %in% AGSN_Sel) %>%
  left_join(BaseMunicipios, by = "CD_GEOCODM") %>%
  group_by(GdReg, CodConcUrb) %>%
  summarise(PopTotAGSN_Ocorr = sum(PopAGSN)) %>%
  left_join(AGSN_ConcUrb, by = c("GdReg",  "CodConcUrb")) %>%
  inner_join(AGSN_Ocorr, by = c("GdReg" = "GdReg.x", "CodConcUrb" = "CodConcUrb.x")) %>%
  select(GdReg, CodConcUrb, NomeConcUr, PopConUrb, PopTotAGSN, PopTotAGSN_Ocorr, TotNorm_Geo, TotNorm_Hidro, TotAGSN_Geo, TotAGSN_Hidro) %>%
  filter(CodConcUrb %in% ConcUrbMaisImp) %>%
  mutate(NomeConcUr = factor(NomeConcUr, levels = ordemConcUrb, ordered = TRUE)) %>%
  arrange(NomeConcUr)

#st_write(Tabela_AGSN, paste(PastaSaida, "Tabela_AGSN.ods", sep = ""), append = FALSE)
#st_write(TabelaSimples_AGSN, paste(PastaSaida, "TabelaSimples_AGSN.ods", sep = ""), append = FALSE)


########
###
### BATER
###
########


BATER_Var <- BATER_Var %>%
  mutate(PopBATER = M004) %>%
  select(GEO_MUN, COD_BATER, PopBATER)

Bater_ConcUrb <- BATER_Var %>%
  group_by(GEO_MUN) %>%
  summarise(PopMunBATER= sum(PopBATER)) %>%
  left_join(BaseMunicipios, by = c("GEO_MUN" = "CD_GEOCODM")) %>%
  group_by(GdReg, CodConcUrb, NomeConcUr, PopConUrb) %>%
  summarise(PopTotBATER = sum(PopMunBATER))
  
BATER_Ocorr <- REINDESC_Munic %>%
  filter(PREC_LOCAL %in% c("Media", "Alta")) %>%
  replace_na(list(GEO_BATER = "00000000000")) %>%
  left_join(BaseMunicipios, by = c("CD_GEOM" = "CD_GEOCODM")) %>%
  group_by(GdReg.x, CodConcUrb.x) %>%
  mutate(Fora_Geo = if_else(GEO_BATER == "00000000000" & (TIPO == "Geo" | TIPO == "Misto"), as.integer(1), as.integer(0)),
         Fora_Hidro = if_else(GEO_BATER == "00000000000" & (TIPO == "Hidro" | TIPO == "Misto"), as.integer(1), as.integer(0)),
         BATER_Geo = if_else(GEO_BATER != "00000000000" & (TIPO == "Geo" | TIPO == "Misto"), as.integer(1), as.integer(0)),
         BATER_Hidro = if_else(GEO_BATER != "00000000000" & (TIPO == "Hidro" | TIPO == "Misto"), as.integer(1), as.integer(0))) %>%
  summarise(TotFora_Geo = sum(Fora_Geo), TotFora_Hidro = sum(Fora_Hidro), TotBATER_Geo = sum(BATER_Geo), TotBATER_Hidro = sum(BATER_Hidro)) 

BATER_Sel <- unique(REINDESC$GEO_BATER)

Tabela_BATER <- BATER_Var %>%
  filter(COD_BATER %in% BATER_Sel) %>%
  left_join(BaseMunicipios, by = c("GEO_MUN" = "CD_GEOCODM")) %>%
  group_by(GdReg, CodConcUrb) %>%
  summarise(PopTotBATER_Ocorr = sum(PopBATER)) %>%
  left_join(Bater_ConcUrb, by = c("GdReg",  "CodConcUrb")) %>%
  inner_join(BATER_Ocorr, by = c("GdReg" = "GdReg.x", "CodConcUrb" = "CodConcUrb.x")) %>%
  select(GdReg, CodConcUrb, NomeConcUr, PopConUrb, PopTotBATER, PopTotBATER_Ocorr, TotFora_Geo, TotFora_Hidro, TotBATER_Geo, TotBATER_Hidro) %>%
  arrange(CodConcUrb)

TabelaSimples_BATER <- BATER_Var %>%
  filter(COD_BATER %in% BATER_Sel) %>%
  left_join(BaseMunicipios, by = c("GEO_MUN" = "CD_GEOCODM")) %>%
  group_by(GdReg, CodConcUrb) %>%
  summarise(PopTotBATER_Ocorr = sum(PopBATER)) %>%
  left_join(Bater_ConcUrb, by = c("GdReg",  "CodConcUrb")) %>%
  inner_join(BATER_Ocorr, by = c("GdReg" = "GdReg.x", "CodConcUrb" = "CodConcUrb.x")) %>%
  select(GdReg, CodConcUrb, NomeConcUr, PopConUrb, PopTotBATER, PopTotBATER_Ocorr, TotFora_Geo, TotFora_Hidro, TotBATER_Geo, TotBATER_Hidro) %>%
  filter(CodConcUrb %in% ConcUrbMaisImp) %>%
  mutate(NomeConcUr = factor(NomeConcUr, levels = ordemConcUrb, ordered = TRUE)) %>%
  arrange(NomeConcUr)


#st_write(Tabela_BATER, paste(PastaSaida, "Tabela_BATER.ods", sep = ""), append = FALSE)
#st_write(TabelaSimples_BATER, paste(PastaSaida, "TabelaSimples_BATER.ods", sep = ""), append = FALSE)


########
###
### TIPOLOGIA
###
########


FiltroConcUrb <- unique(REINDESC_Munic$CodConcUrb[is.na(REINDESC_Munic$TipoSubtip)])

Tabela_Tipologia <- REINDESC_Munic %>%
  filter(PREC_LOCAL %in% c("Media", "Alta") & CodConcUrb != "0000000") %>%
  mutate(TipoSubtip = str_sub(TipoSubtip, 1, 1)) %>%
  replace_na(list(TipoSubtip = "X" )) %>%
  mutate(TipoSubtip = factor(TipoSubtip, levels = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "X"), ordered = TRUE)) %>%
  group_by(GdReg, CodConcUrb, TipoSubtip) %>%
  mutate(freq = n()) %>%
  pivot_wider(id_cols = c(GdReg, CodConcUrb, NomeConcUr),
              names_from = TipoSubtip,
              values_fill = 0,
              values_from = freq,
              values_fn = max) %>%
  mutate(total = A + B + C + D + E + F + G + H + I + J + K) %>%
  filter(CodConcUrb %in% FiltroConcUrb) %>%
  mutate(NomeConcUr = factor(NomeConcUr, levels = ordemConcUrb, ordered = TRUE)) %>%
  arrange(NomeConcUr) %>%
  select(GdReg, CodConcUrb, NomeConcUr, "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "X", total)

TabelaSimples_Tipologia <- REINDESC_Munic %>%
  filter(PREC_LOCAL %in% c("Media", "Alta") & CodConcUrb != "0000000") %>%
  mutate(TipoSubtip = str_sub(TipoSubtip, 1, 1)) %>%
  replace_na(list(TipoSubtip = "X" )) %>%
  mutate(TipoSubtip = factor(TipoSubtip, levels = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "X"), ordered = TRUE)) %>%
  group_by(GdReg, CodConcUrb, TipoSubtip) %>%
  mutate(freq = n()) %>%
  pivot_wider(id_cols = c(GdReg, CodConcUrb, NomeConcUr),
              names_from = TipoSubtip,
              values_fill = 0,
              values_from = freq,
              values_fn = max) %>%
  mutate(total = A + B + C + D + E + F + G + H + I + J + K) %>%
  filter(CodConcUrb %in% ConcUrbMaisImp) %>%
  mutate(NomeConcUr = factor(NomeConcUr, levels = ordemConcUrb, ordered = TRUE)) %>%
  arrange(NomeConcUr) %>%
  select(GdReg, CodConcUrb, NomeConcUr, "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "X", total)

REINDESC_Tipologia <- REINDESC_Munic %>%
  filter(PREC_LOCAL %in% c("Media", "Alta") & CodConcUrb != "0000000") %>%
  mutate(TipoSubtip = str_sub(TipoSubtip, 1, 1)) %>%
  replace_na(list(TipoSubtip = "X" )) %>%
  mutate(TipoSubtip = factor(TipoSubtip, levels = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "X"), ordered = TRUE)) %>%
  group_by(GdReg, CodConcUrb) %>%
  mutate(total = n()) %>%
  ungroup() %>%
  group_by(GdReg, CodConcUrb, TipoSubtip) %>%
  mutate(freq = n(), perc = freq/total) %>%
  ungroup() %>%
  select(GdReg, ID_OCORR, CodConcUrb, NomeConcUr, TipoSubtip, freq, total, perc) %>%
  pivot_wider(id_cols = c(CodConcUrb, NomeConcUr, total), 
              names_from = TipoSubtip,  
              values_from = perc, 
              values_fill = 0,
              values_fn = max) %>%
  select(CodConcUrb, NomeConcUr, "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", total) %>%
  mutate(NomeConcUr = factor(NomeConcUr, levels = ordemConcUrb, ordered = TRUE)) %>%
  arrange(NomeConcUr)
  
  
Pop_Tipologia <- Tipologia_Var %>%
  select(CodConcUrb, TipoIntraUrb, totpes) %>%
  left_join(BaseMunicipios, by = "CodConcUrb") %>%
  mutate(perc = totpes / PopConUrb) %>%
  pivot_wider(id_cols = c(CodConcUrb, PopConUrb, NomeConcUr), 
              values_from = perc,
              names_from = TipoIntraUrb,
              values_fill = 0,
              values_fn = max) %>%
  select(CodConcUrb, NomeConcUr, "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", PopConUrb) %>%
  mutate(NomeConcUr = factor(NomeConcUr, levels = ordemConcUrb, ordered = TRUE)) %>%
  arrange(NomeConcUr)  


## Gráficos Tipologia

Conc_Sel <- unique(Tabela_Tipologia$CodConcUrb)

Graf_PopTipo <- Pop_Tipologia %>%
  select(CodConcUrb, NomeConcUr, "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K") %>%
  filter(CodConcUrb %in% Conc_Sel) %>%
  pivot_longer(c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K"), names_to = "Tipo", values_to = "perc_pop")

Graf_OcorTipo <- REINDESC_Tipologia %>%
  select(CodConcUrb, NomeConcUr, "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K") %>%
  filter(CodConcUrb %in% Conc_Sel) %>%
  pivot_longer(c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K"), names_to = "Tipo", values_to = "perc_tipo") %>%
  left_join(Graf_PopTipo, by = c("CodConcUrb", "Tipo")) %>%
  select(NomeConcUr.x, Tipo, perc_pop, perc_tipo) %>%
  pivot_longer(c(perc_pop, perc_tipo), names_to = "Fracao", values_to = "Percentual") %>%
  mutate(Fracao = ifelse(Fracao == "perc_pop", "População", "Ocorrência"))
  
ggplot(data = Graf_OcorTipo, aes(x = Tipo, y = Percentual, group = Fracao, color = Fracao)) +
  geom_line(stat = "identity") + 
  facet_wrap(~NomeConcUr.x, nrow = 5)

GrafSimples_PopTipo <- Pop_Tipologia %>%
  select(CodConcUrb, NomeConcUr, "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K") %>%
  filter(CodConcUrb %in% ConcUrbMaisImp) %>%
  pivot_longer(c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K"), names_to = "Tipo", values_to = "perc_pop")

GrafSimples_OcorTipo <- REINDESC_Tipologia %>%
  select(CodConcUrb, NomeConcUr, "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K") %>%
  filter(CodConcUrb %in% ConcUrbMaisImp) %>%
  pivot_longer(c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K"), names_to = "Tipo", values_to = "perc_tipo") %>%
  left_join(Graf_PopTipo, by = c("CodConcUrb", "Tipo")) %>%
  select(NomeConcUr.x, Tipo, perc_pop, perc_tipo) %>%
  pivot_longer(c(perc_pop, perc_tipo), names_to = "Fracao", values_to = "Percentual") %>%
  mutate(Fracao = ifelse(Fracao == "perc_pop", "População", "Ocorrência"))

ggplot(data = GrafSimples_OcorTipo, aes(x = Tipo, y = Percentual, group = Fracao, color = Fracao)) +
  geom_line(stat = "identity") + 
  facet_wrap(~NomeConcUr.x, nrow = 5)


#st_write(Tabela_Tipologia, paste(PastaSaida, "Tabela_Tipologia.ods", sep = ""), append = FALSE)
#st_write(TabelaSimples_Tipologia, paste(PastaSaida, "TabelaSimples_Tipologia.ods", sep = ""), append = FALSE)




########
###
### CNEFE
###
########


## REGIÃO SUDESTE

Ocor_CNEFE <- REINDESC_Munic %>%
  filter(GdReg == "SE") %>%
  mutate(PREC_LOCAL = ifelse(PREC_LOCAL == "sem inf.", "sem_inf", PREC_LOCAL)) %>%
  group_by(CodConcUrb, PREC_LOCAL) %>%
  summarise(Ocorr = n()) %>%
  pivot_wider(id_cols = CodConcUrb, names_from = PREC_LOCAL, values_from = Ocorr, values_fill = 0) %>%
  mutate(AltaMedia = Alta + Media, Total_Ocorr = Alta + Media + Baixa + sem_inf) %>%
  select(CodConcUrb, AltaMedia, Total_Ocorr)
  
           

  

Tabela_CNEFE <- REINDESC_Munic %>%
  filter(GdReg == "SE" & PREC_LOCAL == "Media") %>%
  left_join(EventosAssociadosFaces, by = "ID_OCORR") %>%
  replace_na(list(NdeFaces = 0)) %>%
  mutate(ASSOC = ifelse(NdeFaces != 0, 1, 0), NASSOC = ifelse(NdeFaces == 0, 1, 0)) %>%
  group_by(CodConcUrb, NomeConcUr) %>%
  summarise(OcorAssoc = sum(ASSOC), OcorNAssoc = sum(NASSOC), TotalFaces = sum(NdeFaces, na.rm = TRUE), TotalResid = sum(T_Res_Ev, na.rm = TRUE), TotalEnd = sum(T_End_Ev, na.rm = TRUE)) %>%
  left_join(Ocor_CNEFE, by = "CodConcUrb") %>%
  select(CodConcUrb, NomeConcUr, OcorAssoc, OcorNAssoc, Total_Ocorr, TotalFaces, TotalResid, TotalEnd)

#st_write(Tabela_CNEFE, paste(PastaSaida, "Tabela_CNEFE.ods", sep = ""), append = FALSE)  





