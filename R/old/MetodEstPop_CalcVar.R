library(sf)
library(tidyverse)

Sys.setenv(LANG = "English")


LayerUNION <- st_read("W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/_GPKG/Piloto_BH.gpkg", layer = "GradeCompUSetoresAreasUrb_BH_Completo")
SetoresBH <- st_read("W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/_GPKG/Piloto_BH.gpkg", layer = "Setores_BH")
UnDensidade <- st_read("W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/_GPKG/Piloto_BH.gpkg", layer = "Setores2010_AreasUrb_BH", fid_column_name = "fid")
IndRisco_BH <- st_read("W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/_GPKG/Piloto_BH.gpkg", layer = "IndRiscoUSetoresAreaUrb_BH", fid_column_name = "fid")

#Coefs_SubD <- st_read("W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/_GPKG/Piloto_BH.gpkg", layer = "Coefs_SubD")
#Coefs_Dist <- st_read("W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/_GPKG/Piloto_BH.gpkg", layer = "Coefs_Dist")
#Coefs_Mun <- st_read("W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/_GPKG/Piloto_BH.gpkg", layer = "Coefs_Mun")
CoefsFinal <- st_read("W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/_GPKG/Piloto_BH.gpkg", layer = "CoefsFinal_t")

st_geometry(LayerUNION) <- NULL
st_geometry(SetoresBH) <- NULL
st_geometry(UnDensidade) <- NULL
st_geometry(IndRisco_BH) <- NULL

Var <- c("Total_POP", "Idade_Vul", "Renda_Vul", "Esgot_Vul", "V002")
RefVar <- "POP"
AnVar <- Var[1]

LayerCALC <- LayerUNION %>%
  mutate(id = ID_UNICO,
         ClasseUrb = ifelse(Tipo_Urb == "Outros equipamentos urbanos", "NRes", 
                            ifelse(Tipo_Urb == "Área urbanizada" & Densidade == "Densa", "UrDs",
                                   ifelse(Tipo_Urb == "Área urbanizada" & Densidade == "Pouco densa", "UrPd", 
                                          ifelse(Tipo_Urb == "Vazio intraurbano", "Vaz", NA))))) %>%
  replace_na(list(ClasseUrb = "Vaz")) %>%
  drop_na(CD_GEOCODI) %>%
  drop_na(id) %>%
  unite("CodGrupSet", c(id, CD_GEOCODI), remove = FALSE, sep = "" ) %>%
  mutate(CodGrupSet = factor(CodGrupSet), ClasseUrb = factor(ClasseUrb, levels = c("Vaz", "NRes", "UrPd", "UrDs"))) %>%
  mutate(V001 = as.numeric(V001), V002 = as.numeric(V002), V003 = as.numeric(str_replace(V003, ",", "."))) %>%
  left_join(SetoresBH, by = "CD_GEOCODI") %>%
  mutate(CD_GEOCODS = CD_GEOCODS.y, CD_GEOCODD = CD_GEOCODD.y, CD_GEOCODM = CD_GEOCODM.y) %>%
  group_by(CD_GEOCODI) %>%
  select("CodGrupSet", "id", "DOM_OCU", "POP", "CD_GEOCODI", "CD_GEOCODS", "CD_GEOCODD", "CD_GEOCODM", "V002", all_of(Var), "ClasseUrb", "AreUrDsSet", "AreUrPdSet", "AreNResSet", "AreaVazSet", "Area_Inter", "Area_Orig", "AreaEQ")

segmentos <- LayerCALC %>%
  drop_na(id) %>%
  #add_count(id, CD_GEOCODI, name = "nset") %>%
  #add_count(id, ClasseUrb, name = "nclass") %>%
  group_by(id) %>%
  summarise(nseg = n(), nset = n_distinct(CD_GEOCODI), nclass = n_distinct(ClasseUrb))

SubDist_todos <- unique(LayerCALC$CD_GEOCODS)

SubDist_coef <- LayerCALC %>%
  filter(!(CD_GEOCODS %in% CoefsFinal$CD_GEOCODS)) %>%
  group_by(CD_GEOCODS) %>%
  summarise(coefDS = 1, coefPD = 1, coefVZ = 1, coefNR = 1) %>%
  bind_rows(CoefsFinal) %>%
  select(CD_GEOCODS, coefDS, coefPD, coefVZ, coefNR)

  
LayerVar <- LayerCALC %>%
  left_join(SubDist_coef, by = "CD_GEOCODS") %>%
  mutate(POPControle = V002 * Area_Inter / Area_Orig,
         POPCalc = (V002 * Area_Inter) *
           (ifelse(ClasseUrb == "UrDs", coefDS,
                   ifelse(ClasseUrb == "UrPd", coefPD,
                          ifelse(ClasseUrb == "Vaz", coefVZ,
                                 ifelse(ClasseUrb == "NRes", coefNR, 0))))) / ((AreUrDsSet * coefDS) + (AreUrPdSet * coefPD) + (AreaVazSet * coefVZ) + (AreNResSet * coefNR))) %>%
  mutate(IdadeControle = Idade_Vul * Area_Inter / Area_Orig,
         IdadeCalc = (Idade_Vul * Area_Inter) *
           (ifelse(ClasseUrb == "UrDs", coefDS,
                   ifelse(ClasseUrb == "UrPd", coefPD,
                          ifelse(ClasseUrb == "Vaz", coefVZ,
                                 ifelse(ClasseUrb == "NRes", coefNR, 0))))) / ((AreUrDsSet * coefDS) + (AreUrPdSet * coefPD) + (AreaVazSet * coefVZ) + (AreNResSet * coefNR))) %>%
  mutate(RendaControle = Renda_Vul * Area_Inter / Area_Orig,
         RendaCalc = (Renda_Vul * Area_Inter) *
           (ifelse(ClasseUrb == "UrDs", coefDS,
                   ifelse(ClasseUrb == "UrPd", coefPD,
                          ifelse(ClasseUrb == "Vaz", coefVZ,
                                 ifelse(ClasseUrb == "NRes", coefNR, 0))))) / ((AreUrDsSet * coefDS) + (AreUrPdSet * coefPD) + (AreaVazSet * coefVZ) + (AreNResSet * coefNR))) %>%
  mutate(EsgotControle = Esgot_Vul * Area_Inter / Area_Orig,
         EsgotCalc = (Esgot_Vul * Area_Inter) *
           (ifelse(ClasseUrb == "UrDs", coefDS,
                   ifelse(ClasseUrb == "UrPd", coefPD,
                          ifelse(ClasseUrb == "Vaz", coefVZ,
                                 ifelse(ClasseUrb == "NRes", coefNR, 0))))) / ((AreUrDsSet * coefDS) + (AreUrPdSet * coefPD) + (AreaVazSet * coefVZ) + (AreNResSet * coefNR)))


              
LayerFinal <- LayerVar %>%
  group_by(id, DOM_OCU, POP, AreaEQ) %>%
  summarise(POPTotal = sum(POPCalc), IdadeTotal = sum(IdadeCalc), RendaTotal = sum(RendaCalc), EsgotTotal = sum(EsgotCalc), 
            POPControle = sum(POPControle), IdadeControle = sum(IdadeControle), RendaControle = sum(RendaControle), EsgotControle = sum(EsgotControle),
            DOM_OCU = first(DOM_OCU), POP = first(POP), first(V002)) %>%
  mutate(Met_erro = (POP - POPTotal) / AreaEQ, Cont_erro = (POP - POPControle) / AreaEQ, Met_erroAbs = abs(POP - POPTotal) / AreaEQ, Cont_erroAbs = abs(POP - POPControle) / AreaEQ) %>%
  ungroup()

SetMC <- LayerVar %>%
  group_by(CD_GEOCODI) %>%
  filter(n_distinct(ClasseUrb) > 1) %>%
  ungroup()

Ver_Setores <- SetMC %>%
  group_by(CD_GEOCODI) %>%
  summarise(V002 = first(V002), POPTotal = sum(POPCalc), POPControle = sum(POPControle)) %>%
  ungroup() %>%
  summarise(V002 = sum(V002), POPTotal = sum(POPTotal), POPControle = sum(POPControle))

Ver_Grade <- LayerFinal %>%
  summarise(Grd_totpop = sum(POP), Met_totpop = sum(POPTotal), Cont_totpop = sum(POPControle),
            Met_toterro = sum(Met_erroAbs), Cont_toterro = sum(Cont_erroAbs), Met_perc = Met_toterro / sum(POP), Cont_perc = Cont_toterro / sum(POP),
            Met_mean = mean(Met_erro), Cont_mean = mean(Cont_erro), Met_sd = sd(Met_erro), Cont_sd = sd(Cont_erro), Met_medn = median(Met_erro), Cont_medn = median(Cont_erro),
            Grd_maxpop = max(POP / AreaEQ), Met_maxpop = max(POPTotal / AreaEQ), Cont_maxpop = max(POPControle / AreaEQ),
            Grd_minpop = min(POP / AreaEQ), Met_minpop = min(POPTotal / AreaEQ), Cont_minpop = min(POPControle / AreaEQ),
            Grd_meanpop = mean(POP / AreaEQ), Met_meanpop = mean(POPTotal / AreaEQ), Cont_meanpop = mean(POPControle / AreaEQ),
            Grd_sdpop = sd(POP / AreaEQ), Met_sdpop = sd(POPTotal / AreaEQ), Cont_sdpop = sd(POPControle / AreaEQ), 
            Grd_mednpop = median(POP / AreaEQ), Met_mednpop = median(POPTotal / AreaEQ), Cont_mednpop = median(POPControle / AreaEQ),
            Met_totrenda = sum(RendaTotal / AreaEQ), Cont_totrenda = sum(RendaControle / AreaEQ),
            Met_maxrenda = max(RendaTotal / AreaEQ), Cont_maxrenda = max(RendaControle / AreaEQ),
            Met_minrenda = min(RendaTotal / AreaEQ), Cont_minrenda = min(RendaControle / AreaEQ),
            Met_meanrenda = mean(RendaTotal / AreaEQ), Cont_meanrenda = mean(RendaControle / AreaEQ),
            Met_sdrenda = sd(RendaTotal / AreaEQ), Cont_sdrenda = sd(RendaControle / AreaEQ), 
            Met_mednrenda = median(RendaTotal / AreaEQ), Cont_mednrenda = median(RendaControle / AreaEQ),
            Met_totidade = sum(IdadeTotal / AreaEQ), Cont_totidade = sum(IdadeControle / AreaEQ),
            Met_maxidade = max(IdadeTotal / AreaEQ), Cont_maxidade = max(IdadeControle / AreaEQ),
            Met_minidade = min(IdadeTotal / AreaEQ), Cont_minidade = min(IdadeControle / AreaEQ),
            Met_meanidade = mean(IdadeTotal / AreaEQ), Cont_meanidade = mean(IdadeControle / AreaEQ),
            Met_sdidade = sd(IdadeTotal / AreaEQ), Cont_sdidade = sd(IdadeControle / AreaEQ), 
            Met_mednidade = median(IdadeTotal / AreaEQ), Cont_mednidade = median(IdadeControle / AreaEQ),
            Met_totesgot = sum(EsgotTotal / AreaEQ), Cont_totesgot = sum(EsgotControle / AreaEQ),
            Met_maxesgot = max(EsgotTotal / AreaEQ), Cont_maxesgot = max(EsgotControle / AreaEQ),
            Met_minesgot = min(EsgotTotal / AreaEQ), Cont_minesgot = min(EsgotControle / AreaEQ),
            Met_meanesgot = mean(EsgotTotal / AreaEQ), Cont_meanesgot = mean(EsgotControle / AreaEQ),
            Met_sdesgot = sd(EsgotTotal / AreaEQ), Cont_sdesgot = sd(EsgotControle / AreaEQ), 
            Met_mednesgot = median(EsgotTotal / AreaEQ), Cont_mednesgot = median(EsgotControle / AreaEQ))

# dados para análise da amostra (setores multiclasse)

Ver_Grade_amostra <- LayerFinal %>%
  filter(id %in% unique(SetMC$id)) %>%
  summarise(Grd_totpop = sum(POP), Met_totpop = sum(POPTotal), Cont_totpop = sum(POPControle),
            Met_toterro = sum(Met_erroAbs), Cont_toterro = sum(Cont_erroAbs), Met_perc = Met_toterro / sum(POP), Cont_perc = Cont_toterro / sum(POP),
            Met_mean = mean(Met_erro), Cont_mean = mean(Cont_erro), Met_sd = sd(Met_erro), Cont_sd = sd(Cont_erro), Met_medn = median(Met_erro), Cont_medn = median(Cont_erro),
            Met_max = max(Met_erro), Cont_max = max(Cont_erro), Met_min = min(Met_erro), Cont_min = min(Cont_erro),
            Grd_maxpop = max(POP / AreaEQ), Met_maxpop = max(POPTotal / AreaEQ), Cont_maxpop = max(POPControle / AreaEQ),
            Grd_minpop = min(POP / AreaEQ), Met_minpop = min(POPTotal / AreaEQ), Cont_minpop = min(POPControle / AreaEQ),
            Grd_meanpop = mean(POP / AreaEQ), Met_meanpop = mean(POPTotal / AreaEQ), Cont_meanpop = mean(POPControle / AreaEQ),
            Grd_sdpop = sd(POP / AreaEQ), Met_sdpop = sd(POPTotal / AreaEQ), Cont_sdpop = sd(POPControle / AreaEQ), 
            Grd_mednpop = median(POP / AreaEQ), Met_mednpop = median(POPTotal / AreaEQ), Cont_mednpop = median(POPControle / AreaEQ),
            Met_totrenda = sum(RendaTotal / AreaEQ), Cont_totrenda = sum(RendaControle / AreaEQ),
            Met_maxrenda = max(RendaTotal / AreaEQ), Cont_maxrenda = max(RendaControle / AreaEQ),
            Met_minrenda = min(RendaTotal / AreaEQ), Cont_minrenda = min(RendaControle / AreaEQ),
            Met_meanrenda = mean(RendaTotal / AreaEQ), Cont_meanrenda = mean(RendaControle / AreaEQ),
            Met_sdrenda = sd(RendaTotal / AreaEQ), Cont_sdrenda = sd(RendaControle / AreaEQ), 
            Met_mednrenda = median(RendaTotal / AreaEQ), Cont_mednrenda = median(RendaControle / AreaEQ),
            Met_totidade = sum(IdadeTotal / AreaEQ), Cont_totidade = sum(IdadeControle / AreaEQ),
            Met_maxidade = max(IdadeTotal / AreaEQ), Cont_maxidade = max(IdadeControle / AreaEQ),
            Met_minidade = min(IdadeTotal / AreaEQ), Cont_minidade = min(IdadeControle / AreaEQ),
            Met_meanidade = mean(IdadeTotal / AreaEQ), Cont_meanidade = mean(IdadeControle / AreaEQ),
            Met_sdidade = sd(IdadeTotal / AreaEQ), Cont_sdidade = sd(IdadeControle / AreaEQ), 
            Met_mednidade = median(IdadeTotal / AreaEQ), Cont_mednidade = median(IdadeControle / AreaEQ),
            Met_totesgot = sum(EsgotTotal / AreaEQ), Cont_totesgot = sum(EsgotControle / AreaEQ),
            Met_maxesgot = max(EsgotTotal / AreaEQ), Cont_maxesgot = max(EsgotControle / AreaEQ),
            Met_minesgot = min(EsgotTotal / AreaEQ), Cont_minesgot = min(EsgotControle / AreaEQ),
            Met_meanesgot = mean(EsgotTotal / AreaEQ), Cont_meanesgot = mean(EsgotControle / AreaEQ),
            Met_sdesgot = sd(EsgotTotal / AreaEQ), Cont_sdesgot = sd(EsgotControle / AreaEQ), 
            Met_mednesgot = median(EsgotTotal / AreaEQ), Cont_mednesgot = median(EsgotControle / AreaEQ))


# tabela com estatísticas básicas grade pop (setores multiclasse)

Estat_grade_amostra <- Ver_Grade_amostra %>%
  pivot_longer(everything(), names_to = c("Grupo", "Parametro"), names_sep = "_" ) %>%
  pivot_wider(names_from = Grupo, values_from = value)

# Histogramas de distribuição do erro

LayerFinal %>% 
  filter(id %in% unique(SetMC$id)) %>%
  ggplot() + 
  geom_histogram(aes(x = Met_erro), fill = "red", alpha = 0.5, binwidth = 5) +
  geom_histogram(aes(x = Cont_erro), fill = "blue", alpha = 0.5, binwidth = 5) +
  scale_y_sqrt()

LayerFinal %>% 
  filter(id %in% unique(SetMC$id)) %>%
  ggplot() + 
  geom_density(aes(x = Met_erro), fill = "red", alpha = 0.5) +
  geom_density(aes(x = Cont_erro), fill = "blue", alpha = 0.5) +
  scale_y_sqrt() + 
  scale_x_continuous(trans = "log2")
            
# tabela com setores multiclasse e número de classes


lista_setores_amostra <- SetMC %>%
  group_by(CD_GEOCODI) %>%
  summarise(N_classes = length(unique(ClasseUrb)))

# tabela com dados para análise da grade estimada e InOV se quiser

LayerUNION_vars <- LayerFinal %>%
  mutate(n_pop = ((POPTotal / AreaEQ) - Ver_Grade$Met_minpop) / (Ver_Grade$Met_maxpop - Ver_Grade$Met_minpop),
         n_renda = ((RendaTotal / AreaEQ) - Ver_Grade$Met_minrenda) / (Ver_Grade$Met_maxrenda - Ver_Grade$Met_minrenda),
         n_esgot = ((EsgotTotal / AreaEQ) - Ver_Grade$Met_minesgot) / (Ver_Grade$Met_maxesgot - Ver_Grade$Met_minesgot),
         n_idade = ((IdadeTotal / AreaEQ) - Ver_Grade$Met_minidade) / (Ver_Grade$Met_maxidade - Ver_Grade$Met_minidade),
         n_pop_cont = ((POPControle / AreaEQ) - Ver_Grade$Cont_minpop) / (Ver_Grade$Cont_maxpop - Ver_Grade$Cont_minpop),
         n_renda_cont = ((RendaControle / AreaEQ) - Ver_Grade$Cont_minrenda) / (Ver_Grade$Cont_maxrenda - Ver_Grade$Cont_minrenda),
         n_esgot_cont = ((EsgotControle / AreaEQ) - Ver_Grade$Cont_minesgot) / (Ver_Grade$Cont_maxesgot - Ver_Grade$Cont_minesgot),
         n_idade_cont = ((IdadeControle / AreaEQ) - Ver_Grade$Cont_minidade) / (Ver_Grade$Cont_maxidade - Ver_Grade$Cont_minidade),
         InOV_grade = (n_pop * 2) + n_renda + n_esgot + n_idade,
         InOV_grade_n = (InOV_grade - min(InOV_grade)) / (max(InOV_grade) - min(InOV_grade)),
         InOV_grade_cont = (n_pop_cont * 2) + n_renda_cont + n_esgot_cont + n_idade_cont,
         InOV_grade_n_cont = (InOV_grade_cont - min(InOV_grade_cont)) / (max(InOV_grade_cont) - min(InOV_grade_cont)),
         InOV_dif = InOV_grade_n - InOV_grade_n_cont)



## histograma da diferença no InOV (método - controle)

LayerUNION_vars %>%
  filter(id %in% unique(SetMC$id)) %>%
  ggplot() + 
  geom_histogram(aes(x = InOV_dif), fill = "red", binwidth = .01)



## tabela com estatísticas básicas InOV (setores multiclasse)

# Estat_inov_amostra <- LayerUNION_vars %>%
#   filter(id %in% unique(SetMC$id)) %>%
#   summarise(Grd_totpop = sum(POP), Met_totpop = sum(POPTotal), Cont_totpop = sum(POPControle),
#             Met_toterro = sum(Met_erroAbs), Cont_toterro = sum(Cont_erroAbs), Met_perc = Met_toterro / sum(POP), Cont_perc = Cont_toterro / sum(POP),
#             Met_mean = mean(Met_erro), Cont_mean = mean(Cont_erro), Met_sd = sd(Met_erro), Cont_sd = sd(Cont_erro), Met_medn = median(Met_erro), Cont_medn = median(Cont_erro),
#             Met_max = max(Met_erro), Cont_max = max(Cont_erro), Met_min = min(Met_erro), Cont_min = min(Cont_erro),)
#   
#   Estat_grade_amostra <- Ver_Grade_amostra %>%
#   pivot_longer(everything(), names_to = c("Grupo", "Parametro"), names_sep = "_" ) %>%
#   pivot_wider(names_from = Grupo, values_from = value)
#   
  
## Calculo da densidade na "unidade mínima homogênea"resel"

UnDens_Calculo <- UnDensidade %>%
  mutate(id = fid,
         ClasseUrb = ifelse(Tipo_Urb == "Outros equipamentos urbanos", "NRes", 
                            ifelse(Tipo_Urb == "Área urbanizada" & Densidade == "Densa", "UrDs", 
                                   ifelse(Tipo_Urb == "Área urbanizada" & Densidade == "Pouco densa", "UrPd", 
                                          ifelse(Tipo_Urb == "Vazio intraurbano", "Vaz", NA)))), 
         Area_Inter = Area_Trecho) %>%
  replace_na(list(ClasseUrb = "Vaz")) %>%
  drop_na(CD_GEOCODI) %>%
  drop_na(fid) %>%
  unite("CodGrupSet", c(fid, CD_GEOCODI), remove = FALSE, sep = "" ) %>%
  mutate(CodGrupSet = factor(CodGrupSet), ClasseUrb = factor(ClasseUrb, levels = c("Vaz", "NRes", "UrPd", "UrDs"))) %>%
  mutate(V001 = as.numeric(V001), V002 = as.numeric(V002), V003 = as.numeric(str_replace(V003, ",", "."))) %>%
  left_join(SetoresBH, by = "CD_GEOCODI") %>%
  mutate(CD_GEOCODS = CD_GEOCODS.y) %>%
  select("CodGrupSet", "id", "CD_GEOCODI", "CD_GEOCODS", all_of(Var), "ClasseUrb", "Area_Orig", "AreUrDsSet", "AreUrPdSet", "AreNResSet", "AreaVazSet", "Area_Inter")


UnDens_Var <- UnDens_Calculo %>%
  left_join(SubDist_coef, by = "CD_GEOCODS") %>%
  mutate(POPControle = V002 * Area_Inter / Area_Orig,
         POPCalc = (V002 * Area_Inter) *
           (ifelse(ClasseUrb == "UrDs", coefDS,
                   ifelse(ClasseUrb == "UrPd", coefPD,
                          ifelse(ClasseUrb == "Vaz", coefVZ,
                                 ifelse(ClasseUrb == "NRes", coefNR, 0))))) / ((AreUrDsSet * coefDS) + (AreUrPdSet * coefPD) + (AreaVazSet * coefVZ) + (AreNResSet * coefNR)))

UnDens_Final <- UnDens_Var %>%
  group_by(id) %>%
  summarise(POPTotal = sum(POPCalc), POPControle = sum(POPControle)) %>%
  ungroup()


# TRABALHAR COM O ARQUIVO "IndRisco_BH" USANDO O CÓDIGO A PARTIR DO LayerCALC

# IndRisco_Calculo <- IndRisco_BH %>%
#   mutate(ClasseUrb = ifelse(Tipo_Urb == "Outros equipamentos urbanos", "NRes", 
#                             ifelse(Tipo_Urb == "Área urbanizada" & Densidade == "Densa", "UrDs", 
#                                    ifelse(Tipo_Urb == "Área urbanizada" & Densidade == "Pouco densa", "UrPd", 
#                                           ifelse(Tipo_Urb == "Vazio intraurbano", "Vaz", NA))))) %>%
#   replace_na(list(ClasseUrb = "Vaz")) %>%
#   drop_na(CD_GEOCODI) %>%
#   drop_na(ID_RISCO) %>%
#   unite("CodGrupSet", c(ID_RISCO, CD_GEOCODI), remove = FALSE, sep = "" ) %>%
#   mutate(CodGrupSet = factor(CodGrupSet), ClasseUrb = factor(ClasseUrb, levels = c("Vaz", "NRes", "UrPd", "UrDs"))) %>%
#   mutate(V001 = as.numeric(V001), V002 = as.numeric(V002), V003 = as.numeric(str_replace(V003, ",", "."))) %>%
#   left_join(SetoresBH, by = "CD_GEOCODI") %>%
#   mutate(CD_GEOCODS = CD_GEOCODS.y) %>%
#   select("CodGrupSet", "ID_RISCO", "CD_GEOCODI", "CD_GEOCODS", all_of(Var), "ClasseUrb", "Area_Orig", "AreUrDsSet", "AreUrPdSet", "AreNResSet", "AreaVazSet", "Area_Inter")
# 
#                             
# IndRisco_Var <- IndRisco_Calculo %>%
#   left_join(SubDist_coef, by = "CD_GEOCODS") %>%
#   mutate(POPControle = V002 * Area_Inter / Area_Orig, 
#          POPCalc = (V002 * Area_Inter) *
#            (ifelse(ClasseUrb == "UrDs", coefDS,
#                    ifelse(ClasseUrb == "UrPd", coefPD,
#                           ifelse(ClasseUrb == "Vaz", coefVZ,
#                                  ifelse(ClasseUrb == "NRes", coefNR, 0))))) / ((AreUrDsSet * coefDS) + (AreUrPdSet * coefPD) + (AreaVazSet * coefVZ) + (AreNResSet * coefNR))) %>%
#   mutate(IdadeControle = Idade_Vul * Area_Inter / Area_Orig,
#          IdadeCalc = (Idade_Vul * Area_Inter) *
#            (ifelse(ClasseUrb == "UrDs", coefDS,
#                    ifelse(ClasseUrb == "UrPd", coefPD,
#                           ifelse(ClasseUrb == "Vaz", coefVZ,
#                                  ifelse(ClasseUrb == "NRes", coefNR, 0))))) / ((AreUrDsSet * coefDS) + (AreUrPdSet * coefPD) + (AreaVazSet * coefVZ) + (AreNResSet * coefNR))) %>%
#   mutate(RendaControle = Renda_Vul * Area_Inter / Area_Orig,
#          RendaCalc = (Renda_Vul * Area_Inter) *
#            (ifelse(ClasseUrb == "UrDs", coefDS,
#                    ifelse(ClasseUrb == "UrPd", coefPD,
#                           ifelse(ClasseUrb == "Vaz", coefVZ,
#                                  ifelse(ClasseUrb == "NRes", coefNR, 0))))) / ((AreUrDsSet * coefDS) + (AreUrPdSet * coefPD) + (AreaVazSet * coefVZ) + (AreNResSet * coefNR))) %>%
#   mutate(EsgotControle = Esgot_Vul * Area_Inter / Area_Orig,
#          EsgotCalc = (Esgot_Vul * Area_Inter) *
#            (ifelse(ClasseUrb == "UrDs", coefDS,
#                    ifelse(ClasseUrb == "UrPd", coefPD,
#                           ifelse(ClasseUrb == "Vaz", coefVZ,
#                                  ifelse(ClasseUrb == "NRes", coefNR, 0))))) / ((AreUrDsSet * coefDS) + (AreUrPdSet * coefPD) + (AreaVazSet * coefVZ) + (AreNResSet * coefNR)))
# 
# IndRisco_Final <- IndRisco_Var %>%
#   group_by(ID_RISCO) %>%
#   summarise(POPTotal = sum(POPCalc), IdadeTotal = sum(IdadeCalc), RendaTotal = sum(RendaCalc), EsgotTotal = sum(EsgotCalc),
#             POPControle = sum(POPControle), IdadeControle = sum(IdadeControle), RendaControle = sum(RendaControle), EsgotControle = sum(EsgotControle)) %>%
#   ungroup()
#  
# SetMC_IR <- IndRisco_Var %>% 
#   group_by(CD_GEOCODI) %>% 
#   filter(n_distinct(ClasseUrb) > 1) %>% 
#   ungroup()
# 
# Ver_IndRisco <- IndRisco_Final %>%
#   summarise(Met_totpop = sum(POPTotal), Cont_totpop = sum(POPControle),
#             Met_maxpop = max(POPTotal), Cont_maxpop = max(POPControle),
#             Met_minpop = min(POPTotal), Cont_minpop = min(POPControle),
#             Met_meanpop = mean(POPTotal), Cont_meanpop = mean(POPControle),
#             Met_sdpop = sd(POPTotal), Cont_sdpop = sd(POPControle), 
#             Met_mednpop = median(POPTotal), Cont_mednpop = median(POPControle),
#             Met_totrenda = sum(RendaTotal), Cont_totrenda = sum(RendaControle),
#             Met_maxrenda = max(RendaTotal), Cont_maxrenda = max(RendaControle),
#             Met_minrenda = min(RendaTotal), Cont_minrenda = min(RendaControle),
#             Met_meanrenda = mean(RendaTotal), Cont_meanrenda = mean(RendaControle),
#             Met_sdrenda = sd(RendaTotal), Cont_sdrenda = sd(RendaControle), 
#             Met_mednrenda = median(RendaTotal), Cont_mednrenda = median(RendaControle),
#             Met_totidade = sum(IdadeTotal), Cont_totidade = sum(IdadeControle),
#             Met_maxidade = max(IdadeTotal), Cont_maxidade = max(IdadeControle),
#             Met_minidade = min(IdadeTotal), Cont_minidade = min(IdadeControle),
#             Met_meanidade = mean(IdadeTotal), Cont_meanidade = mean(IdadeControle),
#             Met_sdidade = sd(IdadeTotal), Cont_sdidade = sd(IdadeControle), 
#             Met_mednidade = median(IdadeTotal), Cont_mednidade = median(IdadeControle),
#             Met_totesgot = sum(EsgotTotal), Cont_totesgot = sum(EsgotControle),
#             Met_maxesgot = max(EsgotTotal), Cont_maxesgot = max(EsgotControle),
#             Met_minesgot = min(EsgotTotal), Cont_minesgot = min(EsgotControle),
#             Met_meanesgot = mean(EsgotTotal), Cont_meanesgot = mean(EsgotControle),
#             Met_sdesgot = sd(EsgotTotal), Cont_sdesgot = sd(EsgotControle), 
#             Met_mednesgot = median(EsgotTotal), Cont_mednesgot = median(EsgotControle))
# 
# 
# 
# # dados para análise da amostra (setores multiclasse)
# 
# Ver_IndRisco_amostra <- IndRisco_Final %>%
#   filter(ID_RISCO %in% unique(SetMC_IR$ID_RISCO)) %>%
#   summarise(Met_totpop = sum(POPTotal), Cont_totpop = sum(POPControle),
#             Met_maxpop = max(POPTotal), Cont_maxpop = max(POPControle),
#             Met_minpop = min(POPTotal), Cont_minpop = min(POPControle),
#             Met_meanpop = mean(POPTotal), Cont_meanpop = mean(POPControle),
#             Met_sdpop = sd(POPTotal), Cont_sdpop = sd(POPControle), 
#             Met_mednpop = median(POPTotal), Cont_mednpop = median(POPControle),
#             Met_totrenda = sum(RendaTotal), Cont_totrenda = sum(RendaControle),
#             Met_maxrenda = max(RendaTotal), Cont_maxrenda = max(RendaControle),
#             Met_minrenda = min(RendaTotal), Cont_minrenda = min(RendaControle),
#             Met_meanrenda = mean(RendaTotal), Cont_meanrenda = mean(RendaControle),
#             Met_sdrenda = sd(RendaTotal), Cont_sdrenda = sd(RendaControle), 
#             Met_mednrenda = median(RendaTotal), Cont_mednrenda = median(RendaControle),
#             Met_totidade = sum(IdadeTotal), Cont_totidade = sum(IdadeControle),
#             Met_maxidade = max(IdadeTotal), Cont_maxidade = max(IdadeControle),
#             Met_minidade = min(IdadeTotal), Cont_minidade = min(IdadeControle),
#             Met_meanidade = mean(IdadeTotal), Cont_meanidade = mean(IdadeControle),
#             Met_sdidade = sd(IdadeTotal), Cont_sdidade = sd(IdadeControle), 
#             Met_mednidade = median(IdadeTotal), Cont_mednidade = median(IdadeControle),
#             Met_totesgot = sum(EsgotTotal), Cont_totesgot = sum(EsgotControle),
#             Met_maxesgot = max(EsgotTotal), Cont_maxesgot = max(EsgotControle),
#             Met_minesgot = min(EsgotTotal), Cont_minesgot = min(EsgotControle),
#             Met_meanesgot = mean(EsgotTotal), Cont_meanesgot = mean(EsgotControle),
#             Met_sdesgot = sd(EsgotTotal), Cont_sdesgot = sd(EsgotControle), 
#             Met_mednesgot = median(EsgotTotal), Cont_mednesgot = median(EsgotControle))
# 
# 
# IndRisco_vars <- IndRisco_Final %>%
#   mutate(n_pop = (POPTotal - Ver_IndRisco$Met_minpop) / (Ver_IndRisco$Met_maxpop - Ver_IndRisco$Met_minpop),
#          n_renda = (RendaTotal - Ver_IndRisco$Met_minrenda) / (Ver_IndRisco$Met_maxrenda - Ver_IndRisco$Met_minrenda),
#          n_esgot = (EsgotTotal - Ver_IndRisco$Met_minesgot) / (Ver_IndRisco$Met_maxesgot - Ver_IndRisco$Met_minesgot),
#          n_idade = (IdadeTotal - Ver_IndRisco$Met_minidade) / (Ver_IndRisco$Met_maxidade - Ver_IndRisco$Met_minidade),
#          n_pop_cont = (POPControle - Ver_IndRisco$Cont_minpop) / (Ver_IndRisco$Cont_maxpop - Ver_IndRisco$Cont_minpop),
#          n_renda_cont = (RendaControle - Ver_IndRisco$Cont_minrenda) / (Ver_IndRisco$Cont_maxrenda - Ver_IndRisco$Cont_minrenda),
#          n_esgot_cont = (EsgotControle - Ver_IndRisco$Cont_minesgot) / (Ver_IndRisco$Cont_maxesgot - Ver_IndRisco$Cont_minesgot),
#          n_idade_cont = (IdadeControle - Ver_IndRisco$Cont_minidade) / (Ver_IndRisco$Cont_maxidade - Ver_IndRisco$Cont_minidade),
#          InOV_grade = (n_pop * 2) + n_renda + n_esgot + n_idade,
#          InOV_grade_n = (InOV_grade - min(InOV_grade)) / (max(InOV_grade) - min(InOV_grade)),
#          InOV_grade_cont = (n_pop_cont * 2) + n_renda_cont + n_esgot_cont + n_idade_cont,
#          InOV_grade_n_cont = (InOV_grade_cont - min(InOV_grade_cont)) / (max(InOV_grade_cont) - min(InOV_grade_cont)))
# 
# 
# 
# Ver_InOV_amostra <- IndRisco_vars %>%
#   filter(ID_RISCO %in% unique(SetMC_IR$ID_RISCO)) %>%
#   summarise(Met_totinov = sum(InOV_grade_n), Cont_totinov = sum(InOV_grade_n_cont),
#             Met_maxinov = max(InOV_grade_n), Cont_maxinov = max(InOV_grade_n_cont),
#             Met_mininov = min(InOV_grade_n), Cont_mininov = min(InOV_grade_n_cont),
#             Met_meaninov = mean(InOV_grade_n), Cont_meaninov = mean(InOV_grade_n_cont),
#             Met_sdinov = sd(InOV_grade_n), Cont_sdinov = sd(InOV_grade_n_cont),
#             Met_medninov = median(InOV_grade_n), Cont_medninov = median(InOV_grade_n_cont)) %>%
#   bind_cols(Ver_IndRisco_amostra)
# 
#             
# # tabela com estatísticas básicas IndRisco (setores multiclasse)
# 
# Estat_inov_amostra <- Ver_InOV_amostra %>%
#   pivot_longer(everything(), names_to = c("Grupo", "Parametro"), names_sep = "_" ) %>%
#   pivot_wider(names_from = Grupo, values_from = value)
# 
# Municípios da Concentracao urbana de BH e populacao

# Municipios <- SetoresBH %>%
#   group_by(CD_GEOCODM, NM_MUNICIP) %>%
#   summarise(PopMun = sum(Total_POP))

# st_write(Estat_grade_amostra, dsn = "W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/_GPKG/Piloto_BH.gpkg", layer = "Estat_grade_amostra", append = FALSE)
# st_write(LayerUNION_vars, dsn = "W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/_GPKG/Piloto_BH.gpkg", layer = "LayerUNION_vars", append = FALSE)
# st_write(UnDens_Final, dsn = "W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/_GPKG/Piloto_BH.gpkg", layer = "UnDens_Final", append = FALSE)
# st_write(Estat_inov_amostra, dsn = "W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/_GPKG/Piloto_BH.gpkg", layer = "Estat_inov_amostra", append = FALSE)
# st_write(IndRisco_vars, dsn = "W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/_GPKG/Piloto_BH.gpkg", layer = "IndRisco_vars", append = FALSE)
# st_write(lista_setores_amostra, dsn = "W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/_GPKG/Piloto_BH.gpkg", layer = "lista_setores_amostra", append = FALSE)
# st_write(Municipios, dsn = "W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/_GPKG/Piloto_BH.gpkg", layer = "MunicipiosConcUrbBH", append = FALSE)
# st_write(segmentos, dsn = "W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/_GPKG/Piloto_BH.gpkg", layer = "N_segmentos", append = FALSE)


  

# LayerUNION_vars %>%
#   filter(POPTotal >= 1) %>%
#   ggplot(aes(x = POPTotal)) +
#   geom_histogram(bins = 500)
# 
# LayerUNION_vars %>%
#   filter(POPTotal >= 1) %>%
#   ggplot(aes(x = POPTotal)) +
#   geom_density(aes(y = ..density..))
# 
# LayerUNION_vars %>%
#   pivot_longer(cols = c(p_renda, p_esgot, p_idade), names_to = "p_tipo", names_prefix = "p_", values_to = "razao") %>%
#   ggplot(aes(x = razao, y = p_tipo, fill = p_tipo, group = p_tipo)) +
#   geom_violin() +
#   theme(legend.position = "none")
# 
# LayerUNION_vars %>%
#   pivot_longer(cols = c(POPTotal, RendaTotal, EsgotTotal, IdadeTotal), names_to = "tipo", values_to = "qtde") %>%
#   ggplot(aes(x = qtde, y = tipo, fill = tipo, group = tipo)) +
#   geom_violin() +
#   scale_x_continuous(trans = "log") +
#   theme(legend.position = "none")
# 
# LayerUNION_vars %>%
#   pivot_longer(cols = c(POPTotal, RendaTotal, EsgotTotal, IdadeTotal), names_to = "tipo", values_to = "qtde") %>%
#   ggplot(aes(x = qtde, group = tipo, colour = tipo)) +
#   stat_ecdf(position = "identity", geom = "step", na.rm = TRUE)
# 
# LayerUNION_vars %>%
#   pivot_longer(cols = c(n_pop, n_renda, n_esgot, n_idade), names_to = "tipo", names_prefix = "n_", values_to = "normal") %>%
#   ggplot(aes(x = normal)) +
#   stat_ecdf(aes(group = tipo, colour = tipo), position = "identity", geom = "step", na.rm = TRUE)