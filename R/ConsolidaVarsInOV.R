library(data.table)
library(arrow)


setDTthreads(threads = 0)

listaTabs <- c(
  "Basico",
  "Domicilio01",
  "Domicilio02",
  "DomicilioRenda",
  "Entorno01",
  "Entorno02",
  "Entorno03",
  "Entorno04",
  "Entorno05",
  "Pessoa01",
  "Pessoa02",
  "Pessoa03",
  "Pessoa04",
  "Pessoa05",
  "Pessoa06",
  "Pessoa07",
  "Pessoa08",
  "Pessoa09",
  "Pessoa10",
  "Pessoa11",
  "Pessoa12",
  "Pessoa13",
  "PessoaRenda",
  "Responsavel01",
  "Responsavel02",
  "ResponsavelRenda"
  )

  for (i in listaTabs) {
    setwd(str_flatten(c(
      "C:/ACELERADOR/DadosCenso/dados/", i, "/"
    ), collapse = ""))
    getwd()
    uf <-
      gsub(".*[_]([^.]+)[.].*", "\\1", list.files(pattern = "*.csv"))
    listaUF <- list()
    for (j in 1:length(uf)) {
      arq <- list.files(pattern = str_c(".+", uf[j], ".csv"))
      temp <- (fread(arq, encoding = "Latin-1", dec = ",")
                       [, Filter(function(x) any(!is.na(x)), .SD)])
      cols <- colnames(temp)
      vars <- cols[substr(cols, 1, 1) == "V"]
      listaUF[[j]] <- (temp
                       [, replace(.SD, .SD == "X", NA)]
                       [, (vars) := lapply(.SD, as.numeric), .SDcols = vars]
                       [, (setdiff(cols, vars)) := lapply(.SD, as.character), .SDcols = (setdiff(cols, vars))]
                       )
    }
    tabela <- rbindlist(listaUF, fill = TRUE)
    assign(i, tabela)
    write_parquet(get(i), str_c(c("../", i, ".parquet"), collapse = ""))
    gc()
  }


for (i in listaTabs) {
  arq <- paste0(c("C:/ACELERADOR/DadosCenso/dados/", i, ".parquet" ), collapse = "")
  tabela <- read_parquet(arq)
  setDT(tabela)
  tabela_erro <- tabela[nchar(trimws(Cod_setor)) != 15]
  tabela <- tabela[nchar(trimws(Cod_setor)) == 15]
  erro <- paste0(c(i, "_erro"), collapse = "")
  assign(erro, tabela_erro)
}



###################################################################
############# TABELAS COM PROBLEMAS EM ALGUNS SETORES #############
###################################################################
####  TABELAS NORMAIS: 310120 setores                             #
# Tabelas com problemas:                                          #
#                                                                 #
# Basico - 310107 setores                                         #
# Entorno02 - 310120 setores / 72521 c/ prob no geocodigo         #
# Entorno03 - 310120 setores / 72521 c/ prob no geocodigo         #
# Entorno04 - 310120 setores / 72521 c/ prob no geocodigo         #
# Entorno05 - 310114 setores / 72521 c/ prob no geocodigo         #
# Pessoa04 - 310099 setores                                       #
# Pessoa07 - 296894 setores                                       #
# Pessoa08 - 357853 setores / 2346 c/ prob no geocodigo           #
# Responsavel01 - 310114 setores                                  # 
# Responsavel02 310114 setores                                    #
#                                                                 #
###################################################################
###################################################################
#                                                                 #
# Tabelas disponíveis: Basico, Domicilio01, Domicilio02,          #
# DomicilioRenda, Entorno01, Pessoa01, Pessoa02, Pessoa03,        #
# Pessoa04, Pessoa05, Pessoa06, Pessoa09, Pessoa10, Pessoa11,     #
# Pessoa12, Pessoa13, PessoaRenda, Responsavel01, Responsavel02,  #
# ResponsavelRenda                                                #
#                                                                 #
###################################################################

# setEntorno05 <- Entorno05$Cod_setor
# setPessoa04 <- Pessoa04$Cod_setor
# setPessoa07 <- Pessoa07$Cod_setor
# setPessoa08 <- Pessoa08$Cod_setor
# setResponsavel01 <- Responsavel01$Cod_setor
# setResponsavel02 <- Responsavel02$Cod_setor
# 
# setEntorno05_ <- lapply(setEntorno05, trimws)
# setResponsavel01_ <- lapply(setResponsavel01, trimws)
# setResponsavel02_ <- lapply(setResponsavel02, trimws)
# 
# setdiff(unlist(setResponsavel01_), unlist(setResponsavel02_))
# setdiff(unlist(setResponsavel01_), unlist(setEntorno05_))
# setdiff(unlist(setEntorno05_), unlist(setResponsavel02_))

