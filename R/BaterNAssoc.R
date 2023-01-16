
library(tidyverse)
library(sf)
library(writexl)

endereco <- "W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Rodrigo/Plano de trabalho 2021/rural/Abril 2022/Cnefe32_bater_setor_1305.shp"
BATER32 <- "W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Rodrigo/Plano de trabalho 2021/rural/Abril 2022/BATER_32.shp"

Pontos32 <- st_read(endereco) 
BATER32 <- st_read(BATER32)

st_geometry(Pontos32) <- NULL

Pontos32 <- Pontos32 %>%
  mutate(ID_PONTO = as.character(ID_PONTO),
         ESPEC_ENDC = factor(ESPEC_ENDC, levels = c(str_pad(as.character(1:12), 2, side = "left", pad = "0"))),
         ID_SetBat = str_c(GEO_BATER, CD_SETOR)) %>%
  arrange(ESPEC_ENDC)

SetBat_32 <- Pontos32 %>%
  group_by(ID_SetBat, CD_SETOR, GEO_BATER, CD_GEOCODI, ESPEC_ENDC) %>%
  summarise(TotEnd = n_distinct(ID_PONTO)) %>%
  arrange(ESPEC_ENDC) %>%
  pivot_wider(id_cols = c("ID_SetBat", "CD_SETOR", "GEO_BATER", "CD_GEOCODI"), names_from = ESPEC_ENDC, values_from = TotEnd, values_fill = 0)

Set_32 <- Pontos32 %>%
  group_by(CD_SETOR, CD_GEOCODI, ESPEC_ENDC, V001, V002) %>%
  summarise(TotEnd = n_distinct(ID_PONTO)) %>%
  arrange(ESPEC_ENDC) %>%
  pivot_wider(id_cols = c("CD_SETOR", "CD_GEOCODI", "V001", "V002"), names_from = ESPEC_ENDC, values_from = TotEnd, values_fill = 0)

Bater_CNEFE32 <- BATER32 %>%
  left_join(SetBat_32, by = "GEO_BATER") %>%
  left_join(Set_32, by = "CD_SETOR", suffix = c("_sbt", "_set")) %>%
  select(c(2:9, !(10:321))) %>%
  select(!CD_GEOCODI_set) %>%
  rename(CD_GEOCODI = CD_GEOCODI_sbt)
  

#st_write(Bater_CNEFE32, "W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Rodrigo/Plano de trabalho 2021/rural/Abril 2022/Bater_CNEFE32.shp")
#write_xlsx(SetBat_32, "W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Rodrigo/Plano de trabalho 2021/rural/Abril 2022/SetBat32.xlsx")
#write_xlsx(Set_32, "W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Rodrigo/Plano de trabalho 2021/rural/Abril 2022/Set_32.xlsx")
