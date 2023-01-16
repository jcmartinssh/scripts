library(sf)
library(tidyverse)


GradeBH <- st_read("W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/_GPKG/Piloto_BH.gpkg", layer = "GradeCompUSetoresAreasUrb_BH")
SetoresBH <- st_read("W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/_GPKG/Piloto_BH.gpkg", layer = "Setores_BH")

Var <- c("V001", "V002", "V003")
RefVar <- "POP"
AnVar <- Var[2]

BH_Calculo <- GradeBH %>%
  mutate(ClasseUrb = ifelse(
    Tipo_Urb == "Outros equipamentos urbanos", "NRes", ifelse(
      Tipo_Urb == "Área urbanizada" & Densidade == "Densa", "UrDs", ifelse(
        Tipo_Urb == "Área urbanizada" & Densidade == "Pouco densa", "UrPd", ifelse(
          Tipo_Urb == "Vazio intraurbano", "Vaz", NA
        ))))) %>%
  replace_na(list(ClasseUrb = "Vaz")) %>%
  drop_na(CD_GEOCODI) %>%
  drop_na(ID_UNICO) %>%
  mutate(V001 = as.numeric(V001), V002 = as.numeric(V002), V003 = as.numeric(str_replace(V003, ",", "."))) %>%
  unite("CodGrupSet", c(ID_UNICO, CD_GEOCODI), remove = FALSE, sep = "" ) %>%
  mutate(CodGrupSet = factor(CodGrupSet), ClasseUrb = factor(ClasseUrb, levels = c("Vaz", "NRes", "UrPd", "UrDs"))) %>%
  select("CodGrupSet", "ID_UNICO", "DOM_OCU", "POP", "CD_GEOCODI", all_of(Var), "ClasseUrb", "AreUrDsSet", "AreUrPdSet", "AreNResSet", "AreaVazSet", "Area_Inter")

st_geometry(BH_Calculo) <- NULL

agregado <- BH_Calculo %>%
  pivot_wider(id_cols = c(CodGrupSet, ID_UNICO, DOM_OCU, POP, CD_GEOCODI, all_of(Var), AreUrDsSet, AreUrPdSet, AreNResSet, AreaVazSet),
              names_from = ClasseUrb,
              values_from = Area_Inter,
              values_fn = sum) %>%
  replace_na(list(Vaz = 0, UrPd = 0, UrDs = 0, NRes = 0)) 


fnopt <- function(data, par) {
  data %>%
    mutate(VarEstTrecho = (V002 * ((UrDs * par[1]) + (UrPd * par[2]) + (Vaz * par[3]) + (NRes * par[4])) / ((AreUrDsSet * par[1]) + (AreUrPdSet * par[2]) + (AreaVazSet * par[3]) + (AreNResSet * par[4])))) %>%
    group_by(ID_UNICO) %>%
    summarise(VarEst = sum(VarEstTrecho), VarGde = max(POP)) %>%
    mutate(Erro = (VarEst - VarGde)^2) %>%
    summarise(total = sum(Erro))
}

result <- optim(par = c(4, 2, 0.05, 0.05), fn = fnopt, data = agregado)


coefDS <- result$par[1]
coefPD <- result$par[2]
coefVZ <- result$par[3]
coefNR <- result$par[4]

BH_Final <- BH_Calculo %>%
  mutate(coef = ifelse(ClasseUrb == "UrDs", coefDS, ifelse(ClasseUrb == "UrPd", coefPD, ifelse(ClasseUrb == "Vaz", coefVZ, ifelse(ClasseUrb == "NRes", coefNR, 0))))) %>%
  mutate(VarCalc = (V002 * coef * Area_Inter) / ((AreUrDsSet * coefDS) + (AreUrPdSet * coefPD) + (AreaVazSet * coefVZ) + (AreNResSet * coefNR))) %>%
  group_by(ID_UNICO, DOM_OCU, POP) %>%
  summarise(VarTotal = sum(VarCalc), DOM_OCU = max(DOM_OCU), POP = max(POP)) %>%
  mutate(VarTotal = ifelse(VarTotal < 0, 0, round(VarTotal)), Erro = sqrt((POP - VarTotal)^2), ErroPerc = Erro / VarTotal, Xerro = Erro * ErroPerc)


#st_write(BH_Final, dsn = "W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/_GPKG/Piloto_BH.gpkg", layer = "BH_Final_POP", append = FALSE)

ErroTotalPOP <- sum(BH_Final$VarTotal) - sum(BH_Final$POP)
ErroPercPop <- ErroTotalPOP / sum(BH_Final$POP)

