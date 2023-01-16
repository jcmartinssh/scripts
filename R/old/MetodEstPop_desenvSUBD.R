library(sf)
library(tidyverse)
library(ggridges)


GradeBH <- st_read("W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/_GPKG/Piloto_BH.gpkg", layer = "GradeCompUSetoresAreasUrb_BH")
SetoresBH <- st_read("W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/_GPKG/Piloto_BH.gpkg", layer = "Setores_BH")
IndRisco_BH <- st_read("W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/_GPKG/Piloto_BH.gpkg", layer = "IndRiscoUSetoresAreaUrb_BH", fid_column_name = "fid")

st_geometry(GradeBH) <- NULL
st_geometry(SetoresBH) <- NULL
st_geometry(IndRisco_BH) <- NULL

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
#Dists <- unique(agregado$CD_GEOCO)

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
  select(CD_GEOCODS, coefDS, coefPD, coefVZ, coefNR) %>%
  mutate(coefDS = ifelse(coefDS <0, 0, coefDS),
         coefPD = ifelse(coefPD <0, 0, coefPD),
         coefVZ = ifelse(coefVZ <0, 0, coefVZ),
         coefNR = ifelse(coefNR <0, 0, coefNR),)


#st_write(Tabela_Coefs_dpp, dsn = "W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/_GPKG/Piloto_BH.gpkg", layer = "Tabela_Coefs_dpp", append = FALSE)


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

minPOP <- min(BH_Final_dpp$POPTotal)
maxPOP <- max(BH_Final_dpp$POPTotal)

minRenda <- min(BH_Final_dpp$RendaTotal)
maxRenda <- max(BH_Final_dpp$RendaTotal)

minEsgot <- min(BH_Final_dpp$EsgotTotal)
maxEsgot <- max(BH_Final_dpp$EsgotTotal)

minIdade <- min(BH_Final_dpp$IdadeTotal)
maxIdade <- max(BH_Final_dpp$IdadeTotal)


GradeBH_vars <- BH_Final_dpp %>%
  select(ID_UNICO, 2:7) %>%
  mutate(erro = ifelse(IdadeTotal > POPTotal | RendaTotal > POPTotal | EsgotTotal > POPTotal, "sim", "nao")) %>%
  filter(erro == "nao") %>%
  mutate(p_renda = RendaTotal / POPTotal,
         p_esgot = EsgotTotal / POPTotal,
         p_idade = IdadeTotal / POPTotal,
         n_pop = (POPTotal - minPOP) / (maxPOP - minPOP),
         n_renda = (RendaTotal - minRenda) / (maxRenda - minRenda),
         n_esgot = (EsgotTotal - minEsgot) / (maxEsgot - minEsgot),
         n_idade = (IdadeTotal - minIdade) / (maxIdade - minIdade),
         rgb_renda = round((RendaTotal - minRenda) / (maxRenda - minRenda) * 255),
         rgb_esgot = round((EsgotTotal - minEsgot) / (maxEsgot - minEsgot) * 255),
         rgb_idade = round((IdadeTotal - minIdade) / (maxIdade - minIdade) * 255),
         Indic_somaVUL = RendaTotal + EsgotTotal + IdadeTotal,
         Indic_pondVUL = (RendaTotal * 3) + (EsgotTotal * 2) + IdadeTotal,
         Indic_expVUL = (RendaTotal * 3) + (EsgotTotal * 2) + IdadeTotal,
         InOV_grade = (n_pop * 2) + n_renda + n_esgot + n_idade,
         InOV_grade_n = (InOV_grade - min(InOV_grade)) / (max(InOV_grade) - min(InOV_grade)))

# Analise das variaveis

summary(GradeBH_vars$POPTotal)
summary(GradeBH_vars$RendaTotal)
summary(GradeBH_vars$EsgotTotal)
summary(GradeBH_vars$IdadeTotal)

summary(GradeBH_vars$p_renda)
summary(GradeBH_vars$p_esgot)
summary(GradeBH_vars$p_idade)


GradeBH_vars %>% 
  filter(POPTotal >= 1) %>%
  ggplot(aes(x = POPTotal)) +
  geom_histogram(bins = 500)

GradeBH_vars %>% 
  filter(POPTotal >= 1) %>%
  ggplot(aes(x = POPTotal)) +
  geom_density(aes(y = ..density..))

GradeBH_vars %>%
  pivot_longer(cols = c(p_renda, p_esgot, p_idade), names_to = "p_tipo", names_prefix = "p_", values_to = "razao") %>%
  ggplot(aes(x = razao, y = p_tipo, fill = p_tipo, group = p_tipo)) +
  geom_violin() + 
  theme(legend.position = "none")

GradeBH_vars %>%
  pivot_longer(cols = c(POPTotal, RendaTotal, EsgotTotal, IdadeTotal), names_to = "tipo", values_to = "qtde") %>%
  ggplot(aes(x = qtde, y = tipo, fill = tipo, group = tipo)) +
  geom_violin() +
  scale_x_continuous(trans = "log") + 
  theme(legend.position = "none")

GradeBH_vars %>%
  pivot_longer(cols = c(POPTotal, RendaTotal, EsgotTotal, IdadeTotal), names_to = "tipo", values_to = "qtde") %>%
  ggplot(aes(x = qtde, group = tipo, colour = tipo)) +
  stat_ecdf(position = "identity", geom = "step", na.rm = TRUE)

GradeBH_vars %>%
  pivot_longer(cols = c(n_pop, n_renda, n_esgot, n_idade), names_to = "tipo", names_prefix = "n_", values_to = "normal") %>%
  ggplot(aes(x = normal)) +
  stat_ecdf(aes(group = tipo, colour = tipo), position = "identity", geom = "step", na.rm = TRUE)


#st_write(GradeBH_vars, dsn = "W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/_GPKG/Piloto_BH.gpkg", layer = "GradeBH_vars_3", append = FALSE)


# TRABALHAR COM O ARQUIVO "IndRisco_BH" USANDO O CÓDIGO A PARTIR DO BH_Calculo

IndRisco_Calculo <- IndRisco_BH %>%
  mutate(ClasseUrb = ifelse(
    Tipo_Urb == "Outros equipamentos urbanos", "NRes", ifelse(
      Tipo_Urb == "Área urbanizada" & Densidade == "Densa", "UrDs", ifelse(
        Tipo_Urb == "Área urbanizada" & Densidade == "Pouco densa", "UrPd", ifelse(
          Tipo_Urb == "Vazio intraurbano", "Vaz", NA
        ))))) %>%
  replace_na(list(ClasseUrb = "Vaz")) %>%
  drop_na(CD_GEOCODI) %>%
  drop_na(ID_RISCO) %>%
  unite("CodGrupSet", c(ID_RISCO, CD_GEOCODI), remove = FALSE, sep = "" ) %>%
  mutate(CodGrupSet = factor(CodGrupSet), ClasseUrb = factor(ClasseUrb, levels = c("Vaz", "NRes", "UrPd", "UrDs"))) %>%
  mutate(V001 = as.numeric(V001), V002 = as.numeric(V002), V003 = as.numeric(str_replace(V003, ",", "."))) %>%
  left_join(SetoresBH, by = "CD_GEOCODI") %>%
  mutate(CD_GEOCODS = CD_GEOCODS.y) %>%
  select("CodGrupSet", "ID_RISCO", "CD_GEOCODI", "CD_GEOCODS", all_of(Var), "ClasseUrb", "AreUrDsSet", "AreUrPdSet", "AreNResSet", "AreaVazSet", "Area_Inter")

IndRisco_Final <- IndRisco_Calculo %>%
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
  group_by(ID_RISCO) %>%
  summarise(POPTotal = sum(POPCalc), IdadeTotal = sum(IdadeCalc), RendaTotal = sum(RendaCalc), EsgotTotal = sum(EsgotCalc)) %>%
  mutate(POPTotal = ifelse(POPTotal < 0, 0, POPTotal),
         IdadeTotal = ifelse(IdadeTotal < 0, 0, IdadeTotal),
         RendaTotal = ifelse(RendaTotal < 0, 0, RendaTotal),
         EsgotTotal = ifelse(EsgotTotal < 0, 0, EsgotTotal)) %>%
  replace_na(list(POPTotal = 0, IdadeTotal = 0, RendaTotal = 0, EsgotTotal = 0)) %>%
  ungroup()

minPOP_Risco <- min(IndRisco_Final$POPTotal)
maxPOP_Risco <- max(IndRisco_Final$POPTotal)

minRenda_Risco <- min(IndRisco_Final$RendaTotal)
maxRenda_Risco <- max(IndRisco_Final$RendaTotal)

minEsgot_Risco <- min(IndRisco_Final$EsgotTotal)
maxEsgot_Risco <- max(IndRisco_Final$EsgotTotal)

minIdade_Risco <- min(IndRisco_Final$IdadeTotal)
maxIdade_Risco <- max(IndRisco_Final$IdadeTotal)

IndRisco_vars <- IndRisco_Final %>%
  mutate(erro = ifelse(IdadeTotal > POPTotal | RendaTotal > POPTotal | EsgotTotal > POPTotal, "sim", "nao")) %>%
  filter(erro == "nao") %>%
  mutate(p_renda = RendaTotal / POPTotal,
         p_esgot = EsgotTotal / POPTotal,
         p_idade = IdadeTotal / POPTotal,
         n_pop = (POPTotal - minPOP_Risco) / (maxPOP_Risco - minPOP_Risco),
         n_renda = (RendaTotal - minRenda_Risco) / (maxRenda_Risco - minRenda_Risco),
         n_esgot = (EsgotTotal - minEsgot_Risco) / (maxEsgot_Risco - minEsgot_Risco),
         n_idade = (IdadeTotal - minIdade_Risco) / (maxIdade_Risco - minIdade_Risco),
         rgb_renda = round((RendaTotal - minRenda_Risco) / (maxRenda_Risco - minRenda_Risco) * 255),
         rgb_esgot = round((EsgotTotal - minEsgot_Risco) / (maxEsgot_Risco - minEsgot_Risco) * 255),
         rgb_idade = round((IdadeTotal - minIdade_Risco) / (maxIdade_Risco - minIdade_Risco) * 255),
         Indic_somaVUL = RendaTotal + EsgotTotal + IdadeTotal,
         Indic_pondVUL = (RendaTotal * 3) + (EsgotTotal * 2) + IdadeTotal,
         Indic_expVUL = (RendaTotal * 3) + (EsgotTotal * 2) + IdadeTotal,
         InOV_grade = (n_pop * 2) + n_renda + n_esgot + n_idade,
         InOV_grade_n = (InOV_grade - min(InOV_grade)) / (max(InOV_grade) - min(InOV_grade)))

#st_write(IndRisco_vars, dsn = "W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/_GPKG/Piloto_BH.gpkg", layer = "IndRisco_vars", append = FALSE)
