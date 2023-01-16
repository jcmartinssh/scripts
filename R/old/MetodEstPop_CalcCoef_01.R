library(sf)
library(tidyverse)

Sys.setlocale("LC_ALL", "English")
Sys.setenv(LANG = "English")

Begin_time <- Sys.time()

#GradeBH <- st_read("W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/_GPKG/Piloto_BH.gpkg", layer = "GradeCompUSetoresAreasUrb_BH")
GradeBH <- st_read("W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/_GPKG/Piloto_BH.gpkg", layer = "GradeCompUSetoresAreasUrb_BH_Completo")
SetoresBH <- st_read("W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/_GPKG/Piloto_BH.gpkg", layer = "Setores_BH")
#IndRisco_BH <- st_read("W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/_GPKG/Piloto_BH.gpkg", layer = "IndRiscoUSetoresAreaUrb_BH", fid_column_name = "fid")

end_time <- Sys.time()
tempoCarreg <- end_time - Begin_time

st_geometry(GradeBH) <- NULL
st_geometry(SetoresBH) <- NULL

Var <- c("Total_POP", "Idade_Vul", "Renda_Vul", "Esgot_Vul", "V002")
RefVar <- "POP"
AnVar <- Var[1]
amostra_min <- 20
#par.ini <- c(4, 2, 0.05, 0.05)
par.ini <- c(1, 1, 1, 1)


start_time <- Sys.time()

BH_Calculo <- GradeBH %>%
  mutate(ClasseUrb = ifelse(
    Tipo_Urb == "Outros equipamentos urbanos", "NRes", ifelse(
      Tipo_Urb == "Área urbanizada" & Densidade == "Densa", "UrDs", ifelse(
        Tipo_Urb == "Área urbanizada" & Densidade == "Pouco densa", "UrPd", ifelse(
          Tipo_Urb == "Vazio intraurbano", "Vaz", NA)))),) %>%
  replace_na(list(ClasseUrb = "Vaz")) %>%
  drop_na(CD_GEOCODI) %>%
  drop_na(ID_UNICO) %>%
  group_by(CD_GEOCODI) %>%
  mutate(NC_Cont = n_distinct(ClasseUrb)) %>%
  ungroup() %>%
  unite("CodGrupSet", c(ID_UNICO, CD_GEOCODI), remove = FALSE, sep = "" ) %>%
  mutate(CodGrupSet = factor(CodGrupSet), ClasseUrb = factor(ClasseUrb, levels = c("Vaz", "NRes", "UrPd", "UrDs"))) %>%
  mutate(V001 = as.numeric(V001), V002 = as.numeric(V002), V003 = as.numeric(str_replace(V003, ",", "."))) %>%
  left_join(SetoresBH, by = "CD_GEOCODI") %>%
  mutate(CD_GEOCODS = CD_GEOCODS.y, CD_GEOCODD = CD_GEOCODD.y, CD_GEOCODM = CD_GEOCODM.y) %>%
  select("CodGrupSet", "ID_UNICO", "DOM_OCU", "POP", "CD_GEOCODI", "NC_Cont", "CD_GEOCODS", "CD_GEOCODD", "CD_GEOCODM", "V002", "ClasseUrb", "Area_Orig", "AreUrDsSet", "AreUrPdSet", "AreNResSet", "AreaVazSet", "Area_Inter") %>%
  group_by(ID_UNICO) %>%
  mutate(grd_amostra = ifelse(any(NC_Cont >1), TRUE, FALSE),
         N_SubD = n_distinct(CD_GEOCODS), 
         N_Dist = n_distinct(CD_GEOCODD), 
         N_Mun = n_distinct(CD_GEOCODM)) %>%
  ungroup()

agregado <- BH_Calculo %>%
  pivot_wider(id_cols = c(CodGrupSet, CD_GEOCODI, CD_GEOCODS, CD_GEOCODD, CD_GEOCODM, N_SubD, N_Dist, N_Mun, ID_UNICO, DOM_OCU, POP, CD_GEOCODI, V002, AreUrDsSet, AreUrPdSet, AreNResSet, AreaVazSet, grd_amostra),
              names_from = ClasseUrb,
              values_from = Area_Inter,
              values_fn = sum) %>%
  replace_na(list(Vaz = 0, UrPd = 0, UrDs = 0, NRes = 0)) %>%
  filter(grd_amostra == TRUE) %>%
  select(CD_GEOCODS, CD_GEOCODD, CD_GEOCODM, N_SubD, N_Dist, N_Mun, ID_UNICO, POP, V002,  Vaz, UrPd, UrDs, NRes, AreUrDsSet, AreUrPdSet, AreNResSet, AreaVazSet)

end_time <- Sys.time()
tempoProcTab <- end_time - start_time

SubDists <- unique(agregado$CD_GEOCODS)
Dists <- unique(agregado$CD_GEOCODD)
Munic <- unique(agregado$CD_GEOCODM)

SubD_coef_dpp <- list()
Dist_coef_dpp <- list()
Mun_coef_dpp <- list()

fnopt_dpp <- function(par, data) {
  data %>%
    mutate(POPEstTrecho = V002 * ((UrDs * par[1]) + (UrPd * par[2]) + (Vaz * par[3]) + (NRes * par[4])) / 
             ((AreUrDsSet * par[1]) + (AreUrPdSet * par[2]) + (AreaVazSet * par[3]) + (AreNResSet * par[4]))) %>%
    group_by(ID_UNICO) %>%
    summarise(POPEst = sum(POPEstTrecho), POPGde = first(POP)) %>%
    mutate(Erro = abs(POPGde - POPEst)) %>%
    summarise(total = sum(Erro))
}

start_time <- Sys.time()

for (i in SubDists) {
  selecao <- filter(agregado, N_SubD == 1, CD_GEOCODS == i)
  amostra <- n_distinct(selecao$ID_UNICO)
  result <- optim(par = par.ini, fn = fnopt_dpp, data = selecao, method = "L-BFGS-B", lower = 10^-9)
  SubD_coef_dpp[[i]] = data.frame(i)
  SubD_coef_dpp[[i]]$CD_GEOCODS = i
  SubD_coef_dpp[[i]]$coefDS_Sd = result$par[1]
  SubD_coef_dpp[[i]]$coefPD_Sd = result$par[2]
  SubD_coef_dpp[[i]]$coefVZ_Sd = result$par[3]
  SubD_coef_dpp[[i]]$coefNR_Sd = result$par[4]
  SubD_coef_dpp[[i]]$amostra_Sd = amostra
  SubD_coef_dpp[[i]]$value = result$value
  SubD_coef_dpp[[i]]$counts_fn = result$counts[1]
  SubD_coef_dpp[[i]]$counts_gr = result$counts[2]
  SubD_coef_dpp[[i]]$convergence = result$convergence
  SubD_coef_dpp[[i]]$message = result$message
}


Coefs_SubD <- do.call(rbind, SubD_coef_dpp) %>%
  select(CD_GEOCODS, coefDS_Sd, coefPD_Sd, coefVZ_Sd, coefNR_Sd, amostra_Sd, value, counts_fn, counts_gr, convergence, message)

end_time <- Sys.time()
tempoSubdist <- end_time - start_time

start_time <- Sys.time()

for (i in Dists) {
  selecao <- filter(agregado, N_Dist == 1, CD_GEOCODD == i)
  amostra <- n_distinct(selecao$ID_UNICO)
  result <- optim(par = par.ini, fn = fnopt_dpp, data = selecao, method = "L-BFGS-B", lower = 10^-9)
  Dist_coef_dpp[[i]] = data.frame(i)
  Dist_coef_dpp[[i]]$CD_GEOCODD = i
  Dist_coef_dpp[[i]]$coefDS_D = result$par[1]
  Dist_coef_dpp[[i]]$coefPD_D = result$par[2]
  Dist_coef_dpp[[i]]$coefVZ_D = result$par[3]
  Dist_coef_dpp[[i]]$coefNR_D = result$par[4]
  Dist_coef_dpp[[i]]$amostra_D = amostra
  Dist_coef_dpp[[i]]$value = result$value
  Dist_coef_dpp[[i]]$counts_fn = result$counts[1]
  Dist_coef_dpp[[i]]$counts_gr = result$counts[2]
  Dist_coef_dpp[[i]]$convergence = result$convergence
  Dist_coef_dpp[[i]]$message = result$message
}

end_time <- Sys.time()

tempoDist <- end_time - start_time

Coefs_Dist <- do.call(rbind, Dist_coef_dpp) %>%
  select(CD_GEOCODD, coefDS_D, coefPD_D, coefVZ_D, coefNR_D, amostra_D, value, counts_fn, counts_gr, convergence, message)

start_time <- Sys.time()

for (i in Munic) {
  selecao <- filter(agregado, N_Mun == 1, CD_GEOCODM == i)
  amostra <- n_distinct(selecao$ID_UNICO)
  result <- optim(par = par.ini, fn = fnopt_dpp, data = selecao, method = "L-BFGS-B", lower = 10^-9)
  Mun_coef_dpp[[i]] = data.frame(i)
  Mun_coef_dpp[[i]]$CD_GEOCODM = i
  Mun_coef_dpp[[i]]$coefDS_M = result$par[1]
  Mun_coef_dpp[[i]]$coefPD_M = result$par[2]
  Mun_coef_dpp[[i]]$coefVZ_M = result$par[3]
  Mun_coef_dpp[[i]]$coefNR_M = result$par[4]
  Mun_coef_dpp[[i]]$amostra_M = amostra
  Mun_coef_dpp[[i]]$value = result$value
  Mun_coef_dpp[[i]]$counts_fn = result$counts[1]
  Mun_coef_dpp[[i]]$counts_gr = result$counts[2]
  Mun_coef_dpp[[i]]$convergence = result$convergence
  Mun_coef_dpp[[i]]$message = result$message
}

end_time <- Sys.time()

tempoMun <- end_time - start_time

Coefs_Mun <- do.call(rbind, Mun_coef_dpp) %>%
  select(CD_GEOCODM, coefDS_M, coefPD_M, coefVZ_M, coefNR_M, amostra_M, value, counts_fn, counts_gr, convergence, message)

CoefsFinal <- Coefs_SubD %>%
  mutate(CD_GEOCODD = str_sub(CD_GEOCODS, start = 1, end = 9), CD_GEOCODM = str_sub(CD_GEOCODS, start = 1, end = 7)) %>%
  left_join(Coefs_Dist, by = "CD_GEOCODD") %>%
  left_join(Coefs_Mun, by = "CD_GEOCODM") %>%
  mutate(coefDS = ifelse(amostra_Sd >= amostra_min, coefDS_Sd, ifelse(amostra_D >= amostra_min, coefDS_D, coefDS_M)),
         coefPD = ifelse(amostra_Sd >= amostra_min, coefPD_Sd, ifelse(amostra_D >= amostra_min, coefPD_D, coefPD_M)),
         coefVZ = ifelse(amostra_Sd >= amostra_min, coefVZ_Sd, ifelse(amostra_D >= amostra_min, coefVZ_D, coefVZ_M)),
         coefNR = ifelse(amostra_Sd >= amostra_min, coefNR_Sd, ifelse(amostra_D >= amostra_min, coefNR_D, coefNR_M)))
   
Finish_time <- Sys.time()

TempoTotal <- Finish_time - Begin_time


tempoCarreg
tempoProcTab
tempoSubdist
tempoDist
tempoMun
TempoTotal


# st_write(CoefsFinal, dsn = "W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/_GPKG/Piloto_BH.gpkg", layer = "CoefsFinal_t", append = FALSE)

Lista_amostra <- agregado %>%
  filter(N_SubD == 1) %>%
  select(ID_UNICO) %>%
  group_by(ID_UNICO) %>%
  summarise()

# st_write(Lista_amostra, dsn = "W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/_GPKG/Piloto_BH.gpkg", layer = "Lista_amostra", append = FALSE)




