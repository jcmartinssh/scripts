
library(fuzzyjoin)
library(fastLink)
library(sf)
library(tidyverse)

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

normalizar_tipo <- function(str) {
  str <- str_replace_all(str, coll("av."), "avenida")
  str <- str_replace_all(str, coll("av "), "avenida")
  str <- str_replace_all(str, coll("r."), "rua")
  str <- str_replace_all(str, coll("r "), "rua")
  str <- str_replace_all(str, coll("tv."), "travessa")
  str <- str_replace_all(str, coll("tv "), "travessa")
  str <- str_replace_all(str, coll("est."), "estrada")
  str <- str_replace_all(str, coll("est "), "estrada")
  str <- str_replace_all(str, coll("dr."), "doutor")
  str <- str_replace_all(str, coll("dr "), "doutor")
  return(str)
}
  
  
  
REINDESC <- st_read(
  "W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/DESENVOLVIMENTO/REINDESC/NotaTecnica/Bases/BasesNotaTecnica.gpkg", 
  query = "select * from REINDESC_Base where substr(CD_GEOM, 1, 1) = '3' and PREC_LOCAL like 'M%DIA PRECIS%O'"
  )

REINDESC_COMPLETO <- st_read(
  "W:/DGC_ACERVO_CGEO/PROJETOS_EM_ANDAMENTO/Cemaden/DESENVOLVIMENTO/REINDESC/NotaTecnica/Bases/BasesNotaTecnica.gpkg", 
  layer = "REINDESC_Base"
)

Municipios <- unique(REINDESC$CD_GEOM)

Faces <- st_read(
  "C:/ACELERADOR/Bases/Faces.gpkg", 
  query = paste("select CD_GEO, CD_MUN, CD_SETOR, NM_TIP_LOG, NM_TIT_LOG, NM_LOG, TOT_RES, TOT_GERAL from Faces_2019 where substr(CD_MUN, 1, 1) = '3' and CD_MUN in (",
                paste(Municipios, collapse = ", "), 
                ") and CONT = 1"
                )
  )

unique(Faces$NM_TIP_LOG)

ind1 <- c("rua", "av", "av.", "travessa", "avenida", "beco", "alameda", "largo", "praca", "rodovia", "estrada", "viaduto", "r.")

separadores <- "[[:punct:]]|\\se\\s"


PROC_REINDESC <- REINDESC %>%
  mutate(LOCAL_INFO = trimws(str_to_lower(rm_accent(LOCAL_INFO)))) %>%
  mutate(LOCAL_INFO = normalizar_tipo(LOCAL_INFO)) %>%
  separate(LOCAL_INFO, c(paste("end", 1:50)), sep = separadores) %>%
  pivot_longer(cols = c(paste("end", 1:50)), names_to = "campo", values_to = "logradouro", values_drop_na = TRUE )

PROC_REINDESC <- PROC_REINDESC %>%
  filter(word(logradouro) %in% ind1) %>%
  mutate(logradouro = str_c(CD_GEOM, logradouro, sep = " ")) %>%
  select(1:32, logradouro) 

Faces <- Faces %>%
  unite(col = logradouro, NM_TIP_LOG, NM_TIT_LOG, NM_LOG, sep = " ", remove = FALSE, na.rm = TRUE) %>%
  mutate(logradouro = str_to_lower(logradouro)) %>%
  filter(!is.na(logradouro) & logradouro != "" & logradouro != " ") %>%
  select(CD_MUN, CD_GEO, logradouro, TOT_RES, TOT_GERAL) %>%
  mutate(logradouro = str_c(CD_MUN, logradouro, sep = " "))


 
FacesAssociadasEventos <- stringdist_inner_join(Faces, PROC_REINDESC, by = "logradouro", max_dist = 1) %>%
  mutate(logradouro = trimws(str_sub(logradouro.x, 9, -1))) %>%
  select(CD_MUN, CD_GEO, logradouro, TOT_RES, TOT_GERAL, ID_OCORR, ID_REIND)


EventosAssociadosFaces <- FacesAssociadasEventos %>%
  group_by(ID_OCORR) %>%
  summarize(NdeFaces = n(), T_Res_Ev = sum(TOT_RES), T_End_Ev = sum(TOT_GERAL), ID_REIND = max(ID_REIND), CD_MUN = max(CD_MUN)) %>%
  select(ID_OCORR, ID_REIND, CD_MUN, NdeFaces, T_Res_Ev, T_End_Ev)

#st_write(FacesAssociadasEventos, dsn = "C:/ACELERADOR/ProjetoREINDESC_CNEFE/ConcUrbCapitais/FacesREINDESC_SE.gpkg", layer = "FacesAssociadasEventos", append = FALSE)

#st_write(EventosAssociadosFaces, dsn = "C:/ACELERADOR/ProjetoREINDESC_CNEFE/ConcUrbCapitais/FacesREINDESC_SE.gpkg", layer = "EventosAssociadosFaces", append = FALSE)
