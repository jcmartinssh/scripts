library(tidyverse)
library(readODS)
library(gridExtra)

# carrega a tabela exportada do qGIS

Analise_REINDESC <- read_ods("//Ibge.gov.br/dgc-cgema/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/DESENVOLVIMENTO/REINDESC/REINDESC_AnalisePreliminar_ATUALIZADA_2.ods", col_names = TRUE)

# altera a tabela inserindo a coluna com nome da Grande Região e ordenando a UF para seguir a ordem dos códigos do IBGE

Analise_REINDESC <- Analise_REINDESC %>% 
                    mutate(GdReg = factor(as.numeric(substr(CD_GEOM, 1, 1)), labels = c("N", "NE", "SE", "S", "CO")),
                           UF = factor(UF, levels = c("RO", "AC", "AM", "RR", "PA", "AP", "TO",
                                                      "MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", "BA",
                                                      "MG", "ES", "RJ", "SP", 
                                                      "PR", "SC", "RS",
                                                      "MS", "MT", "GO", "DF")))
         

# Cria a tabela agregada por UF com as colunas de interesse para analisar

grafs <- Analise_REINDESC %>%
         group_by(UF, GdReg, PREC_LOCAL) %>%
         summarize(Total = n(),
                   Bater = sum(!is.na(GEO_BATER)),
                   Agsn = sum(!is.na(CD_AGSN)),
                   Bater_alta = sum(!is.na(GEO_BATER) & PREC_LOCAL == "ALTA PRECISÃO"),
                   Agsn_alta = sum(!is.na(CD_AGSN) & PREC_LOCAL == "ALTA PRECISÃO")) %>%
         arrange(UF)


# Cria os gráficos por UF 

gBATER_Alta_UF <- grafs %>% filter(PREC_LOCAL == "ALTA PRECISÃO") %>%
  
             ggplot(aes(x=UF)) +
  
             geom_bar(aes(y=Total,
                          fill = GdReg),
                      position = "stack",
                      stat = "identity") + 
  
             geom_bar(aes(y=Bater),
                      fill = "black",
                      alpha = 0.3,
                      position = "dodge",
                      stat = "identity") +
  
             labs(x = "Unidade da Federação",
                  y = "Total de Ocorrências", 
                  fill = "Grande Região",
                  title = "Ocorrências com Alta Precisão de Localização")

gBATER_Media_UF <- grafs %>% filter(PREC_LOCAL == "MÉDIA PRECISÃO") %>%
  
  ggplot(aes(x=UF)) +
  
  geom_bar(aes(y=Total,
               fill = GdReg),
           position = "stack",
           stat = "identity") + 
  
  geom_bar(aes(y=Bater),
           fill = "black",
           alpha = 0.3,
           position = "dodge",
           stat = "identity") +
  
  labs(x = "Unidade da Federação",
       y = "Total de Ocorrências", 
       fill = "Grande Região",
       title = "Ocorrências com Média Precisão de Localização")

gBATER_Baixa_UF <- grafs %>% filter(PREC_LOCAL == "BAIXA PRECISÃO") %>%
  
  ggplot(aes(x=UF)) +
  
  geom_bar(aes(y=Total,
               fill = GdReg),
           position = "stack",
           stat = "identity") + 
  
  geom_bar(aes(y=Bater),
           fill = "black",
           alpha = 0.3,
           position = "dodge",
           stat = "identity") +
  
  labs(x = "Unidade da Federação",
       y = "Total de Ocorrências", 
       fill = "Grande Região",
       title = "Ocorrências com Baixa Precisão de Localização")

# Cria as tabelas de ranking de BATERs por número de ocorrências

Baters <- Analise_REINDESC %>%
  filter(!is.na(GEO_BATER)) %>%
  group_by(GEO_BATER) %>%
  summarize(freq = n()) %>%
  arrange(desc(freq)) %>%
  head(5)

Baters_AltaPrec <- Analise_REINDESC %>%
  filter(!is.na(GEO_BATER) & PREC_LOCAL == "ALTA PRECISÃO") %>%
  group_by(GEO_BATER) %>%
  summarize(freq = n()) %>%
  arrange(desc(freq)) %>%
  head(5)

Rank_Baters <- Analise_REINDESC %>%
  filter(!is.na(GEO_BATER) & PREC_LOCAL != "BAIXA PRECISÃO" & M004 > 50) %>%
  group_by(GEO_BATER, UF, GdReg, M004) %>%
  summarize(freq = n()) %>%
  arrange(desc(freq))

# Cria os gráficos de ranking de BATERs

gRank <- Rank_Baters %>%
  ggplot(aes(UF,
             freq,
             label = UF,
             col = GdReg,
             size = M004/1000)) +
  
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  
  labs(x = "Unidades da Federação", 
       y = "Nº de Ocorrências", 
       col = "Grande\nRegião",
       size = "Pop. em\nMilhares") +
  
  ggtitle("BATERs - População e Número de ocorrências", subtitle = "Média e Alta Precisão de Localização") +
  
  geom_point(position = "jitter")


print(gRank)
print(gBATER_Alta_UF)
print(gBATER_Media_UF)
print(gBATER_Baixa_UF)
print(head(Rank_Baters$GEO_BATER, 6))

grid.arrange(tableGrob(Baters_AltaPrec, cols = c("GEO_BATER", "Nº de Ocorrências")))
grid.arrange(tableGrob(Baters, cols = c("GEO_BATER", "Nº de Ocorrências")))
grid.arrange(tableGrob(head(Rank_Baters, 29), cols = c("GEO_BATER", "UF", "Gd Região", "População", "Nº de Ocorrências")))
