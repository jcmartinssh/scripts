library(tidyverse)
library(readODS)
library(readxl)
library(fuzzyjoin)
library(openxlsx)



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
# População em Áreas de Risco por Unidade da Federação em 2010

# colunas: População Total; População Total dos Municípios Monitorados; População em Risco nos Municípios Monitorados

# Importa a tabela M_gMUNIC.xlsx e cria os campos de Códigos e Siglas das UFs e Regiões, usando fatores e ordenando segundo 
# padrão IBGE para os gráficos e tabelas

M_MUNIC <- read_excel("//ibge.gov.br/dgc-cgema/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/AnuarioEstatistico/M_gMUNIC.xlsx",
                        col_types = c("text", "text", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric")) %>% 
  mutate(GdReg = factor(as.numeric(substr(COD_MUNICIPIO, 1, 1)), labels = c("N", "NE", "SE", "S", "CO")),
         CodUF = factor(as.numeric(substr(COD_MUNICIPIO, 1, 2))),
         UF = factor(CodUF, labels = c("RO", "AC", "AM", "RR", "PA", "AP", "TO",
                                       "MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", "BA",
                                       "MG", "ES", "RJ", "SP", 
                                       "PR", "SC", "RS",
                                       "MS", "MT", "GO", "DF"))) %>%
  mutate(PopVulMun = M005 + M006 + M009 + M010)

# Carrega as tabelas com as variáveis das BATERs.
# Observe que não existe BATER no Distrito Federal, por isso não tem "DF" na lista de UFs.

M_BATER <- read_excel("//ibge.gov.br/dgc-cgema/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/AnuarioEstatistico/M_gBATER_Definitiva_modificadoNatal.xls",
                      col_types = c("text", "text", "text",
                                    "text", "text", "numeric", 
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric")) %>%
  mutate(GdReg = factor(as.numeric(substr(GEO_MUN, 1, 1)), labels = c("N", "NE", "SE", "S", "CO")),
         CodUF = factor(as.numeric(substr(GEO_MUN, 1, 2))),
         UF = factor(CodUF, labels = c("RO", "AC", "AM", "RR", "PA", "AP", "TO",
                                       "MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", "BA",
                                       "MG", "ES", "RJ", "SP", 
                                       "PR", "SC", "RS",
                                       "MS", "MT", "GO"))) %>%
  mutate(PopVul = M005 + M006 + M009 + M010)
  
M_AGSN <- read_excel("//ibge.gov.br/dgc-cgema/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/AnuarioEstatistico/872Municipios_TabelaMorador_editada.xls") %>%
  mutate(CodMunic = as.character(CodMunic), Pop_ReA = M_R_AGSN, Crianca_ReA = Cr_R_AGSN, Idoso_ReA = Id_R_AGSN) %>%
  select(CodMunic, Pop_ReA, Crianca_ReA, Idoso_ReA) 
  
  
  
# Inserindo a tabela com o Dicionário
DIC <- read_ods(path = "//ibge.gov.br/dgc-cgema/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/AnuarioEstatistico/PARBR2018_VariaveisMorador.ods" ,sheet = 1, col_names = TRUE)

# Tabela com lista dos municípios monitorados ou mapeados

MunMon <- read_excel("//ibge.gov.br/dgc-cgema/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/BOLSISTAS/Joaquim/AnuarioEstatistico/MunicPublicacao.xlsx") 

ListaMunicMonit <- unique(MunMon$GEO_MUN)
  


## filtro: municípios mapeados

M_BATER_MUN <- M_BATER %>%
  filter(GEO_MUN %in% ListaMunicMonit) %>%
  group_by(GEO_MUN, NOM_MUN) %>%
  summarise(PopRisco = sum(M004), PopVulRisco = sum(PopVul)) %>%
  left_join(M_MUNIC, by = c("GEO_MUN" = "COD_MUNICIPIO")) %>%
  mutate(pop_mun = M004, pop_risco_perc = PopRisco / pop_mun, pop_vul_risco_perc = PopVulRisco / pop_mun) %>%
  select("GEO_MUN", "NOM_MUN", "pop_mun", "PopRisco", "PopVulRisco", "pop_risco_perc", "pop_vul_risco_perc")

PopTotalUF <- M_MUNIC %>%
  group_by(GdReg, CodUF, UF) %>%
  summarise(PopTotal = sum(M004))

M_MUNIC_MAP <- M_MUNIC %>%
  filter(COD_MUNICIPIO %in% ListaMunicMonit) %>%
  left_join(M_BATER_MUN, by = c("COD_MUNICIPIO" = "GEO_MUN")) %>%
  left_join(M_AGSN, by = c("COD_MUNICIPIO" = "CodMunic")) %>%
  mutate(PopMun = M004, PopVulRiscoAGSN = Crianca_ReA + Idoso_ReA) %>%
  replace_na(list(PopRisco = 0, PopVulMun = 0, PopVulRisco = 0, PopVulRiscoAGSN = 0)) %>%
  group_by(CodUF) %>%
  summarise(PopMonit = sum(PopMun), PopVulMonit = sum(PopVulMun), PopRisco = sum(PopRisco), PopVulRisco = sum(PopVulRisco), PopVulRiscoAGSN = sum(PopVulRiscoAGSN))

TabelaFinal <- PopTotalUF %>%
  left_join(M_MUNIC_MAP, by = "CodUF") %>%
  replace_na(list(PopMonit = 0, PopVulMonit = 0, PopRisco = 0, PopVulRisco = 0, PopVulRiscoAGSN = 0)) %>%
  arrange(CodUF)


# Dados para nota sobre desastre em Petropolis

# variaveis de interesse : Dom = M001, Pop = M004, Crianca = M005 + M006, Idoso = M009 + M010, Homem = M011, Mulher = M018, Renda_meioSM = M062 + M063, PopTot = M149, MorProp = M154, AguaInad = M026 + M027 + M028 + M029 + M030 + M031 + M032, EsgInad = M035 + M036 + M037 + M038, LixoInad = M043 + M044 + M045 + M046 + M047

Dados_Petropolis <- M_MUNIC %>%
  filter(COD_MUNICIPIO == "3303906") %>%
  mutate(Nome = "Petrópolis", Dom = M001, Pop = M004, Crianca = M005 + M006, Idoso = M009 + M010, Homem = M011, Mulher = M018, Renda_meioSM = M062 + M063, PopTot = M149, MorProp = M154, AguaInad = M026 + M027 + M028 + M029 + M030 + M031 + M032, EsgInad = M035 + M036 + M037 + M038, LixoInad = M043 + M044 + M045 + M046 + M047) %>%
  select(Nome, Dom, Pop, Crianca, Idoso, Homem, Mulher, Renda_meioSM, PopTot, MorProp, AguaInad, EsgInad, LixoInad)
  
  
Dados_Risco_Petropolis <- M_BATER %>%
  filter(GEO_MUN == "3303906") %>%
  mutate(Nome = "População em Área de Risco", Dom = M001, Pop = M004, Crianca = M005 + M006, Idoso = M009 + M010, Homem = M011, Mulher = M018, Renda_meioSM = M062 + M063, PopTot = M149, MorProp = M154, AguaInad = M026 + M027 + M028 + M029 + M030 + M031 + M032, EsgInad = M035 + M036 + M037 + M038, LixoInad = M043 + M044 + M045 + M046 + M047) %>%
  group_by(Nome) %>%
  summarise(Dom = sum(Dom), Pop = sum(Pop), Crianca = sum(Crianca), Idoso = sum(Idoso), Homem = sum(Homem), Mulher = sum(Mulher), Renda_meioSM = sum(Renda_meioSM), PopTot = sum(PopTot), MorProp = sum(MorProp), AguaInad = sum(AguaInad), EsgInad = sum(EsgInad), LixoInad = sum(LixoInad)) %>%
  select(Nome, Dom, Pop, Crianca, Idoso, Homem, Mulher, Renda_meioSM, PopTot, MorProp, AguaInad, EsgInad, LixoInad)

  
Dados_Oficina <- M_BATER %>%
  filter(COD_BATER == "33039060052") %>%
  mutate(Nome = "Morro da Oficina", Dom = M001, Pop = M004, Crianca = M005 + M006, Idoso = M009 + M010, Homem = M011, Mulher = M018, Renda_meioSM = M062 + M063, PopTot = M149, MorProp = M154, AguaInad = M026 + M027 + M028 + M029 + M030 + M031 + M032, EsgInad = M035 + M036 + M037 + M038, LixoInad = M043 + M044 + M045 + M046 + M047) %>%
  select(Nome, Dom, Pop, Crianca, Idoso, Homem, Mulher, Renda_meioSM, PopTot, MorProp, AguaInad, EsgInad, LixoInad)

Tabela_Petropolis <- rbind(Dados_Petropolis, Dados_Risco_Petropolis, Dados_Oficina) %>%
  mutate(P_Crianca = Crianca / Pop, P_Idoso = Idoso / Pop, P_Homem = Homem / Pop, P_Mulher = Mulher / Pop, P_Renda_meioSM = Renda_meioSM / Pop, P_MorProp = MorProp / Pop, P_AguaInad = AguaInad / Pop, P_EsgInad = EsgInad / Pop, P_LixoInad = LixoInad / Pop)

#write.xlsx(Tabela_Petropolis, "//ibge.gov.br/dgc-cgema/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/Petropolis15_02_2022/R_Tabelas/Tabela_Petropolis.xlsx" )


