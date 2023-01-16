##

# Script de processamento das tabelas do S2iD. 
# Por enquanto atua sobre a tabela com resumo de UF feita anteriormente.
# Futuramente ajustar para trabalhar com os arquivos direto do S2iD, iterativamente.


library(tidyverse)
library(readxl)
library(writexl)
library(sf)
library(fuzzyjoin)
library(clock)
#library(gridExtra)
library(ggthemes)
library(showtext)
library(RColorBrewer)
#library(extrafont)
#library(remotes)
#remotes::install_version("Rttf2pt1", version = "1.3.8")

showtext_auto()
showtext_opts(dpi = 300)

rm_accent <- function(str,pattern="all") {
  # Rotinas e funções úteis V 1.0
  # rm.accent - REMOVE ACENTOS DE PALAVRAS
  # Função que tira todos os acentos e pontuações de um vetor de strings.
  # Parâmetros:
  # str - vetor de strings que terão seus acentos retirados.
  # patterns - vetor de strings com um ou mais elementos indicando quais acentos deverão ser retirados.
  #            Para indicar quais acentos deverão ser retirados, um vetor com os símbolos deverão ser passados.
  #            Exemplo: pattern = c("´", "^") retirará os acentos agudos e circunflexos apenas.
  #            Outras palavras aceitas: "all" (retira todos os acentos, que são "´", "`", "^", "~", "¨", "ç")
  if(!is.character(str))
    str <- as.character(str)
  
  pattern <- unique(pattern)
  
  if(any(pattern=="Ç"))
    pattern[pattern=="Ç"] <- "ç"
  
  symbols <- c(
    acute = "áéíóúÁÉÍÓÚýÝ",
    grave = "àèìòùÀÈÌÒÙ",
    circunflex = "âêîôûÂÊÎÔÛ",
    tilde = "ãõÃÕñÑ",
    umlaut = "äëïöüÄËÏÖÜÿ",
    cedil = "çÇ"
  )
  
  nudeSymbols <- c(
    acute = "aeiouAEIOUyY",
    grave = "aeiouAEIOU",
    circunflex = "aeiouAEIOU",
    tilde = "aoAOnN",
    umlaut = "aeiouAEIOUy",
    cedil = "cC"
  )
  
  accentTypes <- c("´","`","^","~","¨","ç")
  
  if(any(c("all","al","a","todos","t","to","tod","todo")%in%pattern)) # opcao retirar todos
    return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))
  
  for(i in which(accentTypes%in%pattern))
    str <- chartr(symbols[i],nudeSymbols[i], str)
  
  return(str)
}

arq_excel <- "W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/AtlasNacional/S2iD/Danos_Informados_2013-2021.xlsx"
municipios <- "W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/AtlasNacional/BR_Municipios_2021/BR_Municipios_2021.shp"

saida <- "W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/AtlasNacional/S2iD/Danos_Informados_2013-2021_GEOCOD.xlsx"
saida_2 <- "W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/AtlasNacional/S2iD/TotMun.xlsx"
saida_3 <- "W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/AtlasNacional/S2iD/ranking.xlsx"

grafico_1a <- "W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/AtlasNacional/S2iD/grafico_1a.png"
grafico_1b <- "W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/AtlasNacional/S2iD/grafico_1b.png"

grafico_2a <- "W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/AtlasNacional/S2iD/grafico_2a.png"
grafico_2b <- "W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/AtlasNacional/S2iD/grafico_2b.png"
grafico_2c <- "W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/AtlasNacional/S2iD/grafico_2c.png"

planilhas <- excel_sheets(arq_excel)
planilhas <- subset(planilhas, substr(planilhas, 1, 2) == "20")

tipos <- c("11321 - Deslizamentos",
             "11331 - Corridas de Massa - Solo/Lama",
             "11313 - Quedas, Tombamentos e rolamentos - Matacões",
             "11332 - Corridas de Massa - Rocha/detrito",
             "11311 - Quedas, Tombamentos e rolamentos - Blocos",
             "11340 - Subsidências e colapsos", 
             "11312 - Quedas, Tombamentos e rolamentos - Lascas",
             "11314 - Quedas, Tombamentos e rolamentos - Lajes",
             "12100 - Inundações", "12300 - Alagamentos",
             "12200 - Enxurradas")

tipos_hidro <- c("12100 - Inundações", "12300 - Alagamentos",
                 "12200 - Enxurradas")

tipos_geo <- c("11321 - Deslizamentos",
               "11331 - Corridas de Massa - Solo/Lama",
               "11313 - Quedas, Tombamentos e rolamentos - Matacões",
               "11332 - Corridas de Massa - Rocha/detrito",
               "11311 - Quedas, Tombamentos e rolamentos - Blocos",
               "11340 - Subsidências e colapsos", 
               "11312 - Quedas, Tombamentos e rolamentos - Lascas",
               "11314 - Quedas, Tombamentos e rolamentos - Lajes")


# Esse método só usa o R básico, mas é difícil de entender.

#S2iD <- lapply(planilhas, read_excel, path = arq_excel, skip = 4)
#S2iD <- lapply(seq_along(planilhas), function(i) {
#  data.frame(sheet = I(planilhas[i]), S2iD[[i]])
#})
#S2iD <- do.call(rbind, S2iD)


# Esse outro método usa a biblioteca purrr do tidyverse.

S2iD <- set_names(planilhas) %>%
  map_df(~ read_excel(path = arq_excel, sheet = .x, skip = 4), .id = "AnoRef") %>%
  mutate(chave1 = str_c(UF, trimws(str_to_lower(rm_accent(Município))), sep = " ")) %>%
  mutate(chaveA = trimws(str_to_lower(rm_accent(Município)))) %>%
  rowid_to_column("ID")

munic_2021 <- st_read(municipios)

st_geometry(munic_2021) <- NULL

munic_2021 <- munic_2021 %>%
  mutate(chave2 = str_c(SIGLA, trimws(str_to_lower(rm_accent(NM_MUN))), sep = " ")) %>%
  mutate(chaveB = trimws(str_to_lower(rm_accent(NM_MUN))))

S2iD_Munic <- stringdist_left_join(S2iD, munic_2021, by = c("chave1" = "chave2"), max_dist = 0)

S2iD_assoc1 <- S2iD_Munic %>%
  filter(!is.na(chave2))

S2iD_nassoc1 <- S2iD_Munic %>%
  filter(is.na(chave2)) %>%
  select(1:57)

UF <- unique(munic_2021$SIGLA)

S2iD_assoc2 <- list()

for(i in UF) {
  eventos <- filter(S2iD_nassoc1, UF == i)
  munic <- filter(munic_2021, SIGLA == i)
  assoc <- stringdist_left_join(eventos, munic, by = c("chaveA" = "chaveB"), max_dist = 1)
  S2iD_assoc2[[i]] = assoc
}

S2iD_assoc2 <- do.call(rbind, S2iD_assoc2) 

S2iD_nassoc2 <- S2iD_assoc2 %>%
  filter(is.na(chaveB)) %>%
  select(1:57)

S2iD_assoc2 <- S2iD_assoc2 %>%
  filter(!is.na(chaveB)) %>%
  select(1:63)

S2iD_assoc3 <- list()

for(i in UF) {
  eventos <- filter(S2iD_nassoc2, UF == i)
  munic <- filter(munic_2021, SIGLA == i)
  assoc <- stringdist_left_join(eventos, munic, by = c("chaveA" = "chaveB"), max_dist = 2)
  S2iD_assoc3[[i]] = assoc
}

S2iD_assoc3 <- do.call(rbind, S2iD_assoc3)

S2iD_nassoc3 <- S2iD_assoc3 %>%
  filter(is.na(chaveB)) %>%
  select(1:57)

S2iD_assoc3 <- S2iD_assoc3 %>%
  filter(!is.na(chaveB)) %>%
  select(1:63)

S2iD_nassoc3 <- S2iD_nassoc3 %>%
  mutate(chaveA = ifelse(chaveA == "boa saude", "januario cicco",
                            ifelse(chaveA == "estancia turistica de sao roque", "sao roque",
                                   ifelse(chaveA == "fortaleza do tabocao", "tabocao", chaveA))))

S2iD_assoc4 <- list()

for(i in UF) {
  eventos <- filter(S2iD_nassoc3, UF == i)
  munic <- filter(munic_2021, SIGLA == i)
  assoc <- stringdist_left_join(eventos, munic, by = c("chaveA" = "chaveB"), max_dist = 1)
  S2iD_assoc4[[i]] = assoc
}

S2iD_assoc4 <- do.call(rbind, S2iD_assoc4)

S2iD_final <- bind_rows(S2iD_assoc1, S2iD_assoc2, S2iD_assoc3, S2iD_assoc4)

Tot_Mun <- S2iD_final %>%
  mutate(tipo = ifelse(COBRADE %in% tipos_hidro, "hidro", ifelse(COBRADE %in% tipos_geo, "geo", "outros"))) %>%
  group_by(UF, Município, CD_MUN, tipo) %>%
  summarise(Tot_Event = n()) %>%
  pivot_wider(names_from = tipo, values_from = Tot_Event, values_fill = 0)
  

Tot_GdReg_data <- S2iD_final %>%
  mutate(dataref = date_group(date_parse(Registro, format = "%d/%m/%Y"), "month"),
         GdReg = factor(as.numeric(substr(CD_MUN, 1, 1)), labels = c("Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste")),
         geologico = ifelse(COBRADE %in% tipos_geo, 1, 0),
         hidrologico = ifelse(COBRADE %in% tipos_hidro, 1, 0)) %>%
  group_by(GdReg, dataref) %>%
  summarise(Tot_geo = sum(geologico), Tot_hidro = sum(hidrologico), Tot_Event = Tot_geo + Tot_hidro)


Tot_GdReg_data2 <- S2iD_final %>%
  mutate(dataref = date_group(date_parse(Registro, format = "%d/%m/%Y"), "month"),
         mes = factor(date_format(dataref, format = "%m"), 
                      labels = c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")),
         GdReg = factor(as.numeric(substr(CD_MUN, 1, 1)), labels = c("Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste")),
         geologico = ifelse(COBRADE %in% tipos_geo, 1, 0),
         hidrologico = ifelse(COBRADE %in% tipos_hidro, 1, 0)) %>%
  group_by(GdReg, mes, dataref) %>%
  summarise(Tot_geo = sum(geologico), Tot_hidro = sum(hidrologico), Tot_Event = Tot_geo + Tot_hidro) %>%
  ungroup() %>%
  group_by(GdReg, mes) %>%
  summarise(Media_geo = mean(Tot_geo), Media_hidro = mean(Tot_hidro), Media_Event = mean(Tot_Event)) %>%
  pivot_longer(cols = starts_with("Media_"), names_to = "Tipo", values_to = "Media") %>%
  pivot_wider(names_from = c("mes", "Tipo"), values_from = Media, values_fill = 0) %>%
  pivot_longer(cols = !GdReg, names_to = c("mes", "Tipo"), names_pattern = "(.*)_Media_(.*)", values_to = "Media") %>%
  ungroup() %>%
  group_by(mes, Tipo) %>%
  mutate(mes = factor(mes, levels = c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")),
         Media_mes = sum(Media),
         pais = "Brasil", pais = factor(pais))

#graf1 <- Tot_GdReg_data %>%
#  ggplot(aes(x = dataref, y = Tot_Event, fill = GdReg, group = GdReg)) + 
#  scale_x_date(date_break = "6 months", date_labels = "%b/%Y", date_minor_breaks = "month") +
#  theme(axis.text.x = element_text(angle = 90)) +
#  geom_bar(stat = "identity") + 
#  labs(title = "Total de registros por grande região - 2013-2021", fill = "Grande Região") +
#  xlab("Mês / Ano") +
#  ylab("Total de registros") 
  
#graf1

#graf1 %>% ggsave(grafico_1a, width = 24.83, height = 9, units = "cm")
#graf1 %>% ggsave(grafico_1b, width = 26.22, height = 11.29, units = "cm")

#graf2 <- Tot_GdReg_data2 %>%
#  filter(Tipo %in% c("geo", "hidro")) %>%
#  #ggplot(aes(x = mes, y = Media, fill = GdReg, group = factor(GdReg, levels = rev(levels(GdReg))))) +
#  ggplot(aes(x = mes, y = Media, fill = GdReg, group = GdReg)) +
#  theme(axis.text.x = element_text(angle = 90)) +
#  geom_area(stat = "identity") + 
#  labs(title = "Média mensal de registros por grande região - 2013-2021",
#       fill = "Grande \nRegião") +
#  xlab("Mês") +
#  ylab("Média de registros") +
#  facet_wrap(~ Tipo)

#graf2

#graf3 <- Tot_GdReg_data2 %>%
#  filter(Tipo %in% c("geo", "hidro")) %>%
#  ggplot(aes(x = mes, y = Media, fill = GdReg, color = GdReg, group = GdReg)) +
#  theme(axis.text.x = element_text(angle = 90)) +
#  geom_line(stat = "identity", position = "identity", size = 0.8) +
#  geom_line(aes(y = Media_mes), color = "black", size = 1.2, linetype = "dashed") +
#  labs(title = "Média mensal de registros por grande região - 2013-2021",
#       fill = "Grande \nRegião") +
#  xlab("Mês") +
#  ylab("Média de registros") +
#  facet_wrap(~ Tipo) 

#graf3

#graf2 %>% ggsave(grafico_2a, width = 10.21, height = 9, units = "cm")
#graf2 %>% ggsave(grafico_2b, width = 12.73, height = 11.23, units = "cm")
#graf2 %>% ggsave(grafico_2c, width = 12.78, height = 11.29, units = "cm")

#rank <- Tot_Mun %>%
#  ungroup() %>%
#  arrange(desc(hidro + geo)) %>%
#  select(UF, Município, hidro, geo) %>%
#  head(20)


#write_xlsx(S2iD_final, path = saida)
#write_xlsx(Tot_Mun, path = saida_2)
#write_xlsx(rank, path = saida_3)

#RegistrosInverno <- S2iD_final %>%
#  filter(str_sub(Registro, 4,5) %in% c("05", "06", "07", "08") & COBRADE %in% tipos)

# Gráficos finais para publicação

arq_hidro <- "W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/AtlasNacional/S2iD/graf_reg_hidro.png"
arq_geo <- "W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/AtlasNacional/S2iD/graf_reg_geo.png"

# definir tema padrão dos gráficos 
# paths = "D:/Users/joaquim.cemaden/AppData/Local/Microsoft/Windows/Fonts"

#font_import()
font_add(family = "univers", regular = "C:/Windows/Fonts/univer.TTF")
cores <- brewer.pal(5, "Set1")
cores[6] <- "#000000"
names(cores) <- c(levels(Tot_GdReg_data2$GdReg), "Brasil")
colScale <- scale_color_manual(name = "GdReg", values = cores)
lineScale <- scale_linetype_manual(name = "GdReg", values = c(rep(1, 6)))

theme_set(
  theme_igray() + 
  theme(plot.title = element_text(family = "univers", face = "bold", size = 11, hjust = 0.5, vjust = 0.5, margin = margin(12, 0, 12, 0)),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.background = element_rect(fill = "white"),
        legend.text = element_text(family = "univers", face = "plain", size = 9),
        #legend
        #legend.key.width = unit(2.25, "cm"),
        plot.background = element_rect(fill = "#d0cece"),
        plot.margin = margin(t = 0, r = 40, b = 0, l = 10 ),
        axis.title = element_text(family = "univers", face = "plain", size = 9),
        axis.text = element_text(family = "univers", face = "plain", size = 7)))

graf_hidro <- Tot_GdReg_data2 %>%
  filter(Tipo == "hidro") %>%
  ggplot(aes(x = mes)) +
  #theme(axis.text.x = element_text(angle = 90)) +
  geom_line(stat = "identity", position = "identity", size = 0.8, aes(y = Media, fill = GdReg, color = GdReg, group = GdReg, linetype = GdReg)) +
  geom_line(size = 0.8, aes(y = Media_mes, colour = pais)) +
  #geom_area(aes(y = Media_mes, fill = pais)) +
  labs(title = "Média mensal de registros de desastres \npor grande região - 2013-2021") +
  xlab("Mês") +
  ylab("Registros") +
  colScale #+
  #lineScale
  #scale_linetype_manual(values = c(2, 1, 1, 1, 1, 1), labels = c("Brasil", "Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste"))# +
  #scale_colour_manual(values = c(1, 1, 1, 1, 1, 2), labels = c("Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste", "Brasil"))
  #scale_color_brewer(type = "qual", palette = "Set1")

# Accent, Dark2, Paired, Pastel1, Pastel2, Set1, Set2, Set3
graf_hidro
ggsave(arq_hidro, graf_hidro, width = 12.78, height = 11.29, units = "cm", dpi = 300)
#graf_geo %>% ggsave(arq_geo, width = 12.78, height = 11.29, units = "cm")  
