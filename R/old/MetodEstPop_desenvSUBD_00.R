library(sf)
library(tidyverse)


GradeBH <- st_read("W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/_GPKG/Piloto_BH.gpkg", layer = "GradeCompUSetoresAreasUrb_BH")
SetoresBH <- st_read("W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/_GPKG/Piloto_BH.gpkg", layer = "Setores_BH")

st_geometry(GradeBH) <- NULL
st_geometry(SetoresBH) <- NULL

Var <- c("Total_POP", "Idade_Vul", "Renda_Vul", "Esgot_Vul", "V002")
RefVar <- "POP"
AnVar <- Var[1]

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
  unite("CodGrupSet", c(ID_UNICO, CD_GEOCODI), remove = FALSE, sep = "" ) %>%
  mutate(CodGrupSet = factor(CodGrupSet), ClasseUrb = factor(ClasseUrb, levels = c("Vaz", "NRes", "UrPd", "UrDs"))) %>%
  mutate(V001 = as.numeric(V001), V002 = as.numeric(V002), V003 = as.numeric(str_replace(V003, ",", "."))) %>%
  left_join(SetoresBH, by = "CD_GEOCODI") %>%
  mutate(CD_GEOCODS = CD_GEOCODS.y) %>%
  select("CodGrupSet", "ID_UNICO", "DOM_OCU", "POP", "CD_GEOCODI", "CD_GEOCODS", "V002", all_of(Var), "ClasseUrb", "AreUrDsSet", "AreUrPdSet", "AreNResSet", "AreaVazSet", "Area_Inter", "CompletamenteDentro")


BH_Amostra <- BH_Calculo %>%
  filter(CompletamenteDentro == "sim") %>%
  group_by(ID_UNICO) %>%
  mutate(N_SubD = n_distinct(CD_GEOCODS)) %>%
  filter(N_SubD == 1)

agregado <- BH_Amostra %>%
  pivot_wider(id_cols = c(CodGrupSet, CD_GEOCODS, ID_UNICO, DOM_OCU, POP, CD_GEOCODI, all_of(Var), AreUrDsSet, AreUrPdSet, AreNResSet, AreaVazSet),
              names_from = ClasseUrb,
              values_from = Area_Inter,
              values_fn = sum) %>%
  replace_na(list(Vaz = 0, UrPd = 0, UrDs = 0, NRes = 0)) 


#fnopt_tot <- function(data, par) {
#  data %>%
#    mutate(POPEstTrecho = (Total_POP * ((UrDs * par[1]) + (UrPd * par[2]) + (Vaz * par[3]) + (NRes * par[4])) / ((AreUrDsSet * par[1]) + (AreUrPdSet * par[2]) + (AreaVazSet * par[3]) + (AreNResSet * par[4])))) %>%
#    group_by(ID_UNICO) %>%
#    summarise(POPEst = sum(POPEstTrecho), POPGde = max(POP)) %>%
#    mutate(Erro = (POPEst - POPGde)^2) %>%
#    summarise(total = sum(Erro))
#}

fnopt_dpp <- function(data, par) {
  data %>%
    mutate(POPEstTrecho = (V002 * ((UrDs * par[1]) + (UrPd * par[2]) + (Vaz * par[3]) + (NRes * par[4])) / ((AreUrDsSet * par[1]) + (AreUrPdSet * par[2]) + (AreaVazSet * par[3]) + (AreNResSet * par[4])))) %>%
    group_by(ID_UNICO) %>%
    summarise(POPEst = sum(POPEstTrecho), POPGde = max(POP)) %>%
    mutate(Erro = (POPEst - POPGde)^2) %>%
    summarise(total = sum(Erro))
}

SubDists <- unique(agregado$CD_GEOCODS)

#SubD_coef_tot <- list()
SubD_coef_dpp <- list()

#for (i in SubDists) {
#  selecao <- filter(agregado, CD_GEOCODS == i)
#  result <- optim(par = c(4, 2, 0.05, 0.05), fn = fnopt_tot, data = selecao)
#  SubD_coef_tot[[i]] = data.frame(i)
#  SubD_coef_tot[[i]]$CD_GEOCODS = i
#  SubD_coef_tot[[i]]$coefDS = result$par[1]
#  SubD_coef_tot[[i]]$coefPD = result$par[2]
#  SubD_coef_tot[[i]]$coefVZ = result$par[3]
#  SubD_coef_tot[[i]]$coefNR = result$par[4]
#}

for (i in SubDists) {
  selecao <- filter(agregado, CD_GEOCODS == i)
  result <- optim(par = c(4, 2, 0.05, 0.05), fn = fnopt_dpp, data = selecao)
  SubD_coef_dpp[[i]] = data.frame(i)
  SubD_coef_dpp[[i]]$CD_GEOCODS = i
  SubD_coef_dpp[[i]]$coefDS = result$par[1]
  SubD_coef_dpp[[i]]$coefPD = result$par[2]
  SubD_coef_dpp[[i]]$coefVZ = result$par[3]
  SubD_coef_dpp[[i]]$coefNR = result$par[4]
}

#Tabela_Coefs_tot <- do.call(rbind, SubD_coef_tot) %>%
#  select(CD_GEOCODS, coefDS, coefPD, coefVZ, coefNR)

Tabela_Coefs_dpp <- do.call(rbind, SubD_coef_dpp) %>%
  select(CD_GEOCODS, coefDS, coefPD, coefVZ, coefNR)

#BH_Final_tot <- BH_Calculo %>%
#  left_join(Tabela_Coefs_tot, by = "CD_GEOCODS") %>%
#  mutate(POPCalc = (Total_POP * Area_Inter) * 
#           (ifelse(ClasseUrb == "UrDs", coefDS, 
#                   ifelse(ClasseUrb == "UrPd", coefPD, 
#                          ifelse(ClasseUrb == "Vaz", coefVZ, 
#                                 ifelse(ClasseUrb == "NRes", coefNR, 0))))) / ((AreUrDsSet * coefDS) + (AreUrPdSet * coefPD) + (AreaVazSet * coefVZ) + (AreNResSet * coefNR))) %>%
#  mutate(IdadeCalc = (Idade_Vul * Area_Inter) * 
#           (ifelse(ClasseUrb == "UrDs", coefDS, 
#                   ifelse(ClasseUrb == "UrPd", coefPD, 
#                          ifelse(ClasseUrb == "Vaz", coefVZ, 
#                                 ifelse(ClasseUrb == "NRes", coefNR, 0))))) / ((AreUrDsSet * coefDS) + (AreUrPdSet * coefPD) + (AreaVazSet * coefVZ) + (AreNResSet * coefNR))) %>%
#  mutate(RendaCalc = (Renda_Vul * Area_Inter) * 
#           (ifelse(ClasseUrb == "UrDs", coefDS, 
#                   ifelse(ClasseUrb == "UrPd", coefPD, 
#                          ifelse(ClasseUrb == "Vaz", coefVZ, 
#                                 ifelse(ClasseUrb == "NRes", coefNR, 0))))) / ((AreUrDsSet * coefDS) + (AreUrPdSet * coefPD) + (AreaVazSet * coefVZ) + (AreNResSet * coefNR))) %>%
#  mutate(EsgotCalc = (Esgot_Vul * Area_Inter) * 
#           (ifelse(ClasseUrb == "UrDs", coefDS, 
#                   ifelse(ClasseUrb == "UrPd", coefPD, 
#                          ifelse(ClasseUrb == "Vaz", coefVZ, 
#                                 ifelse(ClasseUrb == "NRes", coefNR, 0))))) / ((AreUrDsSet * coefDS) + (AreUrPdSet * coefPD) + (AreaVazSet * coefVZ) + (AreNResSet * coefNR))) %>%
#  group_by(ID_UNICO, DOM_OCU, POP) %>%
#  summarise(POPTotal = sum(POPCalc), IdadeTotal = sum(IdadeCalc), RendaTotal = sum(RendaCalc), EsgotTotal = sum(EsgotCalc), DOM_OCU = max(DOM_OCU), POP = max(POP)) %>%
#  mutate(POPTotal = ifelse(POPTotal < 0, 0, round(POPTotal)),
#         IdadeTotal = ifelse(IdadeTotal < 0, 0, round(IdadeTotal)),
#         RendaTotal = ifelse(RendaTotal < 0, 0, round(RendaTotal)),
#         EsgotTotal = ifelse(EsgotTotal < 0, 0, round(EsgotTotal)),
#         Erro = sqrt((POP - POPTotal)^2), ErroPerc = Erro / POPTotal, Xerro = Erro * ErroPerc) %>%
#  replace_na(list(POPTotal = 0, IdadeTotal = 0, RendaTotal = 0, EsgotTotal = 0)) %>%
#  ungroup()


BH_Final_dpp <- BH_Calculo %>%
  left_join(Tabela_Coefs_dpp, by = "CD_GEOCODS") %>%
  mutate(POPCalc = (V002 * Area_Inter) * 
           (ifelse(ClasseUrb == "UrDs", coefDS, 
                   ifelse(ClasseUrb == "UrPd", coefPD, 
                          ifelse(ClasseUrb == "Vaz", coefVZ, 
                                 ifelse(ClasseUrb == "NRes", coefNR, 0))))) / ((AreUrDsSet * coefDS) + (AreUrPdSet * coefPD) + (AreaVazSet * coefVZ) + (AreNResSet * coefNR))) %>%
  mutate(IdadeCalc = (Idade_Vul * Area_Inter) * 
           (ifelse(ClasseUrb == "UrDs", coefDS, 
                   ifelse(ClasseUrb == "UrPd", coefPD, 
                          ifelse(ClasseUrb == "Vaz", coefVZ, 
                                 ifelse(ClasseUrb == "NRes", coefNR, 0))))) / ((AreUrDsSet * coefDS) + (AreUrPdSet * coefPD) + (AreaVazSet * coefVZ) + (AreNResSet * coefNR))) %>%
  mutate(RendaCalc = (Renda_Vul * Area_Inter) * 
           (ifelse(ClasseUrb == "UrDs", coefDS, 
                   ifelse(ClasseUrb == "UrPd", coefPD, 
                          ifelse(ClasseUrb == "Vaz", coefVZ, 
                                 ifelse(ClasseUrb == "NRes", coefNR, 0))))) / ((AreUrDsSet * coefDS) + (AreUrPdSet * coefPD) + (AreaVazSet * coefVZ) + (AreNResSet * coefNR))) %>%
  mutate(EsgotCalc = (Esgot_Vul * Area_Inter) * 
           (ifelse(ClasseUrb == "UrDs", coefDS, 
                   ifelse(ClasseUrb == "UrPd", coefPD, 
                          ifelse(ClasseUrb == "Vaz", coefVZ, 
                                 ifelse(ClasseUrb == "NRes", coefNR, 0))))) / ((AreUrDsSet * coefDS) + (AreUrPdSet * coefPD) + (AreaVazSet * coefVZ) + (AreNResSet * coefNR))) %>%
  group_by(ID_UNICO, DOM_OCU, POP) %>%
  summarise(POPTotal = sum(POPCalc), IdadeTotal = sum(IdadeCalc), RendaTotal = sum(RendaCalc), EsgotTotal = sum(EsgotCalc), DOM_OCU = max(DOM_OCU), POP = max(POP)) %>%
  mutate(POPTotal = ifelse(POPTotal < 0, 0, POPTotal),
         IdadeTotal = ifelse(IdadeTotal < 0, 0, IdadeTotal),
         RendaTotal = ifelse(RendaTotal < 0, 0, RendaTotal),
         EsgotTotal = ifelse(EsgotTotal < 0, 0, EsgotTotal),
         Erro = sqrt((POP - POPTotal)^2), ErroPerc = Erro / POPTotal, Xerro = Erro * ErroPerc) %>%
  replace_na(list(POPTotal = 0, IdadeTotal = 0, RendaTotal = 0, EsgotTotal = 0)) %>%
  ungroup()

#ErroTotalPOP_tot <- sum(BH_Final_tot$POPTotal) - sum(BH_Final_tot$POP)
#ErroPercPop_tot <- ErroTotalPOP_tot / sum(BH_Final_tot$POP)

ErroTotalPOP_dpp <- sum(BH_Final_dpp$POPTotal) - sum(BH_Final_dpp$POP)
ErroPercPop_dpp <- ErroTotalPOP_dpp / sum(BH_Final_dpp$POP)

#PopEstTot_tot <- sum(BH_Final_tot$POPTotal)
#PopGdeTot_tot <- sum(BH_Final_tot$POP)

PopEstTot_dpp <- sum(BH_Final_dpp$POPTotal)
PopGdeTot_dpp <- sum(BH_Final_dpp$POP)

#BH_Final_comb <- BH_Final_tot %>%
#  mutate(Erro_tot = Erro, POPTotal_tot = POPTotal) %>%
#  ungroup() %>%
#  select(ID_UNICO, POPTotal_tot, Erro_tot) %>%
#  full_join(BH_Final_dpp, by = "ID_UNICO") %>%
#  mutate(Erro_dpp = Erro, POPTotal_dpp = POPTotal) %>%
#  select(ID_UNICO, POP, POPTotal_tot, POPTotal_dpp, Erro_tot, Erro_dpp)


#n_distinct(BH_Final_comb$ID_UNICO[BH_Final_comb$Erro_dpp > BH_Final_comb$Erro_tot])
#n_distinct(BH_Final_comb$ID_UNICO[BH_Final_comb$Erro_dpp < BH_Final_comb$Erro_tot])
#n_distinct(BH_Final_comb$ID_UNICO[BH_Final_comb$Erro_dpp == BH_Final_comb$Erro_tot])
#n_distinct(BH_Final_comb$ID_UNICO[BH_Final_comb$Erro_dpp == 0])
#n_distinct(BH_Final_comb$ID_UNICO[BH_Final_comb$Erro_tot == 0])
#n_distinct(BH_Final_comb$ID_UNICO[BH_Final_comb$Erro_dpp > 0])
#n_distinct(BH_Final_comb$ID_UNICO[BH_Final_comb$Erro_tot > 0])

#n_distinct(BH_Final_comb$ID_UNICO[BH_Final_comb$Erro_tot > 2 * (BH_Final_comb$Erro_dpp)])
#n_distinct(BH_Final_comb$ID_UNICO[BH_Final_comb$Erro_dpp > 2 * (BH_Final_comb$Erro_tot)])

mean(BH_Final_dpp$Erro, na.rm = TRUE)
median(BH_Final_dpp$Erro, na.rm = TRUE)
sum(BH_Final_dpp$Erro, na.rm = TRUE)
sd(BH_Final_dpp$Erro, na.rm = TRUE)
max(BH_Final_dpp$Erro, na.rm = TRUE)

#mean(BH_Final_comb$Erro_tot, na.rm = TRUE)
#median(BH_Final_comb$Erro_tot, na.rm = TRUE)
#sum(BH_Final_comb$Erro_tot, na.rm = TRUE)
#sd(BH_Final_comb$Erro_tot, na.rm = TRUE)
#max(BH_Final_comb$Erro_tot, na.rm = TRUE)

#ggplot(BH_Final_comb, aes(x = Erro_tot)) + 
#  geom_histogram(fill = "red", alpha = 0.5) +
#  geom_histogram(aes(x = Erro_dpp), fill = "blue", alpha = 0.5) +
#  scale_x_log10() + 
#  scale_y_sqrt()

#ggplot(BH_Final_comb, aes(x = Erro_tot)) + 
#  geom_freqpoly(color = "red", alpha = 0.5) +
#  geom_freqpoly(aes(x = Erro_dpp), color = "blue", alpha = 0.5) +
#  scale_x_log10() + 
#  scale_y_sqrt()

GradeBH_vars <- BH_Final_dpp %>%
  select(ID_UNICO, 4:10)
  

#preInOV_BH <- BH_Final %>%
#  filter(!is.na(POPTotal) & POPTotal != 0) %>%
#  mutate(ExpA = POPTotal / PopEstTot,
#         ExpCAPa = IdadeTotal / POPTotal,
#         ExpCAPb = RendaTotal / POPTotal,
#         ExpCAPc = EsgotTotal / POPTotal) %>%
#  select(1:7, ExpA, ExpCAPa, ExpCAPb, ExpCAPc)

#maxExpA <- max(preInOV_BH$ExpA)
#minExpA <- min(preInOV_BH$ExpA)
  
#maxExpCAPa <- max(preInOV_BH$ExpCAPa)
#minExpCAPa <- min(preInOV_BH$ExpCAPa)

#maxExpCAPb <- max(preInOV_BH$ExpCAPb)
#minExpCAPb <- min(preInOV_BH$ExpCAPb)

#maxExpCAPc <- max(preInOV_BH$ExpCAPc)
#minExpCAPc <- min(preInOV_BH$ExpCAPc)

#InOV_BH <- preInOV_BH %>%
#  mutate(N_ExpA = (ExpA - minExpA) / (maxExpA - minExpA),
#         N_ExpCAPa = (ExpCAPa - minExpCAPa) / (maxExpCAPa - minExpCAPa),
#         N_ExpCAPb = (ExpCAPb - minExpCAPb) / (maxExpCAPb - minExpCAPb),
#         N_ExpCAPc = (ExpCAPc - minExpCAPc) / (maxExpCAPc - minExpCAPc),
#         InOV = (N_ExpA * 2) + N_ExpCAPa + N_ExpCAPb + N_ExpCAPc,
#         Teste_InOV = (((N_ExpA * 2) + N_ExpCAPa + N_ExpCAPb + N_ExpCAPc) * ExpA
#                       )) %>%
#  filter(N_ExpA != 0)

#maxInOV <- max(InOV_BH$InOV)
#minInOV <- min(InOV_BH$InOV)

#maxTeste_InOV <- max(InOV_BH$Teste_InOV)
#minTeste_InOV <- min(InOV_BH$Teste_InOV)

#InOV_BH <- InOV_BH %>%
#  mutate(N_InOV = (InOV - minInOV) / (maxInOV - minInOV), Teste_N_InOV = (Teste_InOV - minTeste_InOV) / (maxTeste_InOV - minTeste_InOV))

#st_write(InOV_BH, dsn = "W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/_GPKG/Piloto_BH.gpkg", layer = "InOV_BH", append = FALSE)
#st_write(GradeBH_vars, dsn = "W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/_GPKG/Piloto_BH.gpkg", layer = "GradeBH_vars_2", append = FALSE)

