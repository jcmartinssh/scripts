library(sf)
library(tidyverse)


#st_layers("W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/_GPKG/Piloto_BH.gpkg")
#Amostra <- st_read("W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/_GPKG/Piloto_BH.gpkg", layer = "AmostraUSetoresAreasUrb_BH")
#Amostra2 <- st_read("W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/_GPKG/Piloto_BH.gpkg", layer = "AmostraUSetoresAreasUrb_BH_2")
GradeBH <- st_read("W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/_GPKG/Piloto_BH.gpkg", layer = "GradeCompUSetoresAreasUrb_BH")

# fórmula para estimativa do número de domicílios por trecho :
# =IF(AND(Y2="Área urbanizada"; X2="Densa"); AI2*U2*$Controle.$B$3/((AE2*$Controle.$B$3)+(AF2*$Controle.$B$4)+(AG2*$Controle.$B$5)+(AH2*$Controle.$B$6)); IF(AND(Y2="Área urbanizada"; X2="Pouco densa"); AI2*U2*$Controle.$B$3/((AE2*$Controle.$B$3)+(AF2*$Controle.$B$4)+(AG2*$Controle.$B$5)+(AH2*$Controle.$B$6)); IF(OR(Y2="Vazio intraurbano"; ISBLANK(Y2)); AI2*U2*$Controle.$B$5/((AE2*$Controle.$B$3)+(AF2*$Controle.$B$4)+(AG2*$Controle.$B$5)+(AH2*$Controle.$B$6)); IF(Y2="Outros equipamentos urbanos"; AI2*U2*$Controle.$B$6/((AE2*$Controle.$B$3)+(AF2*$Controle.$B$4)+(AG2*$Controle.$B$5)+(AH2*$Controle.$B$6))))))

#st_geometry(Amostra) <- NULL
#st_geometry(Amostra2) <- NULL
#st_geometry(GradeBH) <- NULL

#tabela <- Amostra2 %>%
#  mutate(ClasseUrb = ifelse(
#    Tipo_Urb == "Outros equipamentos urbanos", "NRes", ifelse(
#      Tipo_Urb == "Área urbanizada" & Densidade == "Densa", "UrDs", ifelse(
#        Tipo_Urb == "Área urbanizada" & Densidade == "Pouco densa", "UrPd", ifelse(
#          Tipo_Urb == "Vazio intraurbano", "Vaz", NA
#      ))))) %>%
#  replace_na(list(ClasseUrb = "Vaz")) %>%
#  drop_na(CD_GEOCODI) %>%
#  mutate(V001 = as.numeric(V001), V003 = as.numeric(str_replace(V003, ",", "."))) %>%
#  unite("CodGrupSet", c(group_id, CD_GEOCODI), remove = FALSE, sep = "" ) %>%
#  mutate(CodGrupSet = factor(CodGrupSet), ClasseUrb = factor(ClasseUrb, levels = c("Vaz", "NRes", "UrPd", "UrDs"))) %>%
#  select("CodGrupSet", "group_id", "DOM_OCU", "POP", "CD_GEOCODI", "V001", "V003", "ClasseUrb", "AreUrDsSet", "AreUrPdSet", "AreNResSet", "AreaVazSet", "Area_Inter")
  

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
  mutate(V001 = as.numeric(V001), V003 = as.numeric(str_replace(V003, ",", "."))) %>%
  unite("CodGrupSet", c(ID_UNICO, CD_GEOCODI), remove = FALSE, sep = "" ) %>%
  mutate(CodGrupSet = factor(CodGrupSet), ClasseUrb = factor(ClasseUrb, levels = c("Vaz", "NRes", "UrPd", "UrDs"))) %>%
  select("CodGrupSet", "ID_UNICO", "DOM_OCU", "POP", "CD_GEOCODI", "V001", "V003", "ClasseUrb", "AreUrDsSet", "AreUrPdSet", "AreNResSet", "AreaVazSet", "Area_Inter")

st_geometry(BH_Calculo) <- NULL

#agregado <- tabela %>%
#  pivot_wider(id_cols = c(CodGrupSet, group_id, DOM_OCU, POP, CD_GEOCODI, V001, V003, AreUrDsSet, AreUrPdSet, AreNResSet, AreaVazSet),
#              names_from = ClasseUrb,
#              values_from = Area_Inter,
#              values_fn = sum) %>%
#  replace_na(list(Vaz = 0, UrPd = 0, UrDs = 0, NRes = 0))


agregado2 <- BH_Calculo %>%
  pivot_wider(id_cols = c(CodGrupSet, ID_UNICO, DOM_OCU, POP, CD_GEOCODI, V001, V003, AreUrDsSet, AreUrPdSet, AreNResSet, AreaVazSet),
              names_from = ClasseUrb,
              values_from = Area_Inter,
              values_fn = sum) %>%
  replace_na(list(Vaz = 0, UrPd = 0, UrDs = 0, NRes = 0)) 

#fnopt <- function(data, par) {
#  data %>%
#    mutate(DomEstTrecho = (V001 * ((UrDs * par[1]) + (UrPd * par[2]) + (Vaz * par[3]) + (NRes * par[4])) / ((AreUrDsSet * par[1]) + (AreUrPdSet * par[2]) + (AreaVazSet * par[3]) + (AreNResSet * par[4])))) %>%
#    group_by(group_id) %>%
#    summarise(DomEst = sum(DomEstTrecho), DomGde = max(DOM_OCU)) %>%
#    mutate(Erro = (DomEst - DomGde)^2) %>%
#    summarise(total = sum(Erro))
#}

fnopt2 <- function(data, par) {
  data %>%
    mutate(DomEstTrecho = (V001 * ((UrDs * par[1]) + (UrPd * par[2]) + (Vaz * par[3]) + (NRes * par[4])) / ((AreUrDsSet * par[1]) + (AreUrPdSet * par[2]) + (AreaVazSet * par[3]) + (AreNResSet * par[4])))) %>%
    group_by(ID_UNICO) %>%
    summarise(DomEst = sum(DomEstTrecho), DomGde = max(DOM_OCU)) %>%
    mutate(Erro = (DomEst - DomGde)^2) %>%
    summarise(total = sum(Erro))
}

#result <- optim(par = c(4, 2, 0.05, 0.05), fn = fnopt, data = agregado)
result2 <- optim(par = c(4, 2, 0.05, 0.05), fn = fnopt2, data = agregado2)

#coefDS <- result$par[1]
#coefPD <- result$par[2]
#coefVZ <- result$par[3]
#coefNR <- result$par[4]

#coefDS2 <- coefDS
#coefPD2 <- coefPD
#coefVZ2 <- coefVZ
#coefNR2 <- coefNR

coefDS2 <- result2$par[1]
coefPD2 <- result2$par[2]
coefVZ2 <- result2$par[3]
coefNR2 <- result2$par[4]

#tabAmostraFinal <- tabela %>%
#  mutate(coef = ifelse(ClasseUrb == "UrDs", coefDS, ifelse(ClasseUrb == "UrPd", coefPD, ifelse(ClasseUrb == "Vaz", coefVZ, ifelse(ClasseUrb == "NRes", coefNR, 0))))) %>%
#  mutate(DomCalc = (V001 * coef * Area_Inter) / ((AreUrDsSet * coefDS) + (AreUrPdSet * coefPD) + (AreaVazSet * coefVZ) + (AreNResSet * coefNR))) %>%
#  group_by(group_id, DOM_OCU, POP) %>%
#  summarise(DomTotal = sum(DomCalc), DOM_OCU = max(DOM_OCU), Erro = sqrt((DOM_OCU - DomTotal)^2), ErroPerc = Erro / DomTotal, Xerro = Erro * ErroPerc) 

BH_Final <- BH_Calculo %>%
  mutate(coef = ifelse(ClasseUrb == "UrDs", coefDS2, ifelse(ClasseUrb == "UrPd", coefPD2, ifelse(ClasseUrb == "Vaz", coefVZ2, ifelse(ClasseUrb == "NRes", coefNR2, 0))))) %>%
  mutate(DomCalc = (V001 * coef * Area_Inter) / ((AreUrDsSet * coefDS2) + (AreUrPdSet * coefPD2) + (AreaVazSet * coefVZ2) + (AreNResSet * coefNR2))) %>%
  group_by(ID_UNICO, DOM_OCU, POP) %>%
  summarise(DomTotal = sum(DomCalc), DOM_OCU = max(DOM_OCU)) %>%
  mutate(DomTotal = ifelse(DomTotal < 0, 0, round(DomTotal)), Erro = sqrt((DOM_OCU - DomTotal)^2), ErroPerc = Erro / DomTotal, Xerro = Erro * ErroPerc)


#st_write(BH_Final, dsn = "W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/_GPKG/Piloto_BH.gpkg", layer = "BH_Final_DOM", append = FALSE)

  


