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
library(ggthemes)
library(showtext)
library(RColorBrewer)

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

arq_excel <- "W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/AtlasNacional/Alertas/AlertasCemaden2012-2021.xlsx"
municipios <- "W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/AtlasNacional/BR_Municipios_2021/BR_Municipios_2021.shp"


planilha <- read_excel(arq_excel)

Tot_Mun <- planilha %>%
  group_by(`Código IBGE`, Evento) %>%
  summarise(Tot_Event = n()) %>%
  mutate(Evento = ifelse(Evento == "Geo/Hidro", "Hidrogeo", Evento)) %>%
  pivot_wider(names_from = Evento, values_from = Tot_Event, values_fill = 0) %>%
  mutate(tot_geo = Geo + Hidrogeo, tot_hidro = Hidro + Hidrogeo)
  
Med_GdReg_data <- planilha %>%
  mutate(dataref = date_group(as.Date(planilha$Data), "month"),
         mes = factor(date_format(dataref, format = "%m"), 
                      labels = c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")),
         GdReg = factor(as.numeric(substr(`Código IBGE`, 1, 1)), labels = c("N", "NE", "SE", "S", "CO")),
         geologico = ifelse(Evento %in% c("Geo", "Geo/Hidro"), 1, 0),
         hidrologico = ifelse(Evento %in% c("Hidro", "Geo/Hidro"), 1, 0)) %>%
  group_by(GdReg, mes, dataref) %>%
  summarise(Tot_Event = n(), Tot_geo = sum(geologico), Tot_hidro = sum(hidrologico)) %>%
  ungroup() %>%
  group_by(GdReg, mes) %>%
  summarise(Media_geo = mean(Tot_geo), Media_hidro = mean(Tot_hidro), Media_Event = mean(Tot_Event)) %>%
  # manobra esquisita para arrumar a tabela, rever esse código adiante de pivotagem
  pivot_longer(cols = starts_with("Media_"), names_to = "Tipo", values_to = "Media") %>%
  pivot_wider(names_from = c("mes", "Tipo"), values_from = Media, values_fill = 0) %>%
  pivot_longer(cols = !GdReg, names_to = c("mes", "Tipo"), names_pattern = "(.*)_Media_(.*)", values_to = "Media") %>%
  # até aqui.
  ungroup() %>%
  group_by(Tipo, mes) %>%
  mutate(mes = factor(mes, levels = c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")))


Med_Pais_geral <- Med_GdReg_data %>%
  group_by(Tipo, mes) %>%
  summarise(Media = sum(Media)) %>%
  mutate(GdReg = "Brasil") %>%
  select(GdReg, mes, Tipo, Media)

Med_Geral <- Med_GdReg_data %>%
  bind_rows(Med_Pais_geral) %>%
  mutate(GdReg = factor(GdReg, levels = c("Brasil", "N", "NE", "SE", "S", "CO")))


# Gráficos finais para publicação

arq_hidro <- "W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/AtlasNacional/Alertas/graf_alerta_hidro.png"
arq_geo <- "W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/AtlasNacional/Alertas/graf_alerta_geo.png"
arq_geral <- "W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/AtlasNacional/Alertas/graf_alerta_geral.png"

# definir tema padrão dos gráficos 

font_add(family = "univers", regular = "C:/Windows/Fonts/univer.TTF")

theme_set(
  theme_igray() + 
    theme(plot.title = element_text(family = "univers", face = "bold", size = 11, hjust = 0.5, vjust = 0.5, lineheight = 1.1, margin = margin(12, 0, 12, 0)),
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.background = element_rect(fill = "white"),
          legend.text = element_text(family = "univers", face = "plain", size = 9, margin = margin(0, 15, 0, 5)),
          legend.key.width = unit(1.5, "cm"),
          plot.background = element_rect(fill = "#d0cece"),
          plot.margin = margin(t = 0, r = 40, b = 0, l = 10 ),
          axis.title = element_text(family = "univers", face = "plain", size = 9),
          axis.text = element_text(family = "univers", face = "plain", size = 7)))

cores <- c("#000000", brewer.pal(5, "Set1"))
names(cores) <- c("Brasil", levels(Med_GdReg_data$GdReg))
colScale <- scale_color_manual(name = "GdReg", labels = c("Brasil", "Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste"), values = cores)
lineScale <- scale_linetype_manual(name = "GdReg", labels = c("Brasil", "Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste"), values = c(2, 1, 1, 1, 1, 1))

graf_hidro <- Med_Geral %>%
  filter(Tipo == "hidro") %>%
  ggplot(aes(x = mes, y = Media, colour = GdReg, group = GdReg, linetype = GdReg)) +
  geom_line(stat = "identity", position = "identity", size = 0.8, ) +
  labs(title = "Média mensal de alertas de risco hidrológico,\npor Grandes Regiões - 2012-2021") +
  xlab("Mês") +
  ylab("Alertas") +
  lineScale +
  colScale +
  theme(legend.margin = margin(0, 40, 0, 40))

graf_geo <- Med_Geral %>%
  filter(Tipo == "geo") %>%
  ggplot(aes(x = mes, y = Media, colour = GdReg, group = GdReg, linetype = GdReg)) +
  geom_line(stat = "identity", position = "identity", size = 0.8, ) +
  labs(title = "Média mensal de alertas de risco geológico,\npor Grandes Regiões - 2012-2021") +
  xlab("Mês") +
  ylab("Alertas") +
  lineScale +
  colScale +
  theme(legend.margin = margin(0, 40, 0, 40))

graf_geral <- Med_Geral %>%
  filter(Tipo == "Event") %>%
  arrange(GdReg) %>%
  ggplot(aes(x = mes, y = Media, colour = GdReg, group = GdReg, linetype = GdReg)) +
  geom_line(size = 0.5) +
  labs(title = "Média mensal de alertas de risco geohidrológico,\npor Grandes Regiões - 2012-2021") +
  xlab("Mês") +
  ylab("Alertas") +
  lineScale +
  colScale +
  theme(legend.margin = margin(0, 40, 0, 40)) +
  facet_wrap(vars(GdReg), nrow = 1)
  
graf_hidro
graf_geo
graf_geral
  
  

#ggsave(arq_hidro, graf_hidro, width = 12.78, height = 11.29, units = "cm", dpi = 300)
#ggsave(arq_geo, graf_geo, width = 12.78, height = 11.29, units = "cm", dpi = 300)
#ggsave(arq_geral, graf_geral, width = 42, height = 10, units = "cm", dpi = 300)

