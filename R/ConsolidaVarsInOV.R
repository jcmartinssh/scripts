library(data.table)
library(stringr)
library(arrow)

setDTthreads(threads = 0)
# consolidar tabelas censo
# Variávies para construção do InOV:

# População total: Domicilio_02 - V002
# População com idade vulnerável* - até 5 anos / igual ou acima de 60 anos: Pessoa13 - V022 + V035:V039 + V094:V134 -- Não se limita aos domicílios particulares permanentes, ao contrário das demais variáveis
# População com renda inferior a 1/2 salário mínimo: PessoaRenda - V067
# População em domicílios sem esgotamento sanitário adequado - rede ou fossa séptica: Domicilio02 - V019:V022 -- Não inclue domicílios sem sanitário de uso exclusivo dos moradores

# teste <- fread("C:/ACELERADOR/DadosCenso/dados/Basico/Basico_RS.csv", nrows = 11500) #22333 total

rm(list = ls())
listaTabs <- c("Basico", "Pessoa03", "Pessoa13", "PessoaRenda", "Domicilio01", "Domicilio02")

# teste
# listaTabs <- listaTabs[5]
# i <- listaTabs

for (i in listaTabs) {
  
  setwd(str_flatten(c("C:/ACELERADOR/DadosCenso/dados/", i, "/"), collapse = ""))
  getwd()
  uf <- gsub(".*[_]([^.]+)[.].*", "\\1", list.files(pattern = "*.csv"))
  # uf <- uf[28]
  listaUF <- list()
  for (j in 1:length(uf)) {
  
    arq <- list.files(pattern = str_c(".+", uf[j], ".csv"))
    cols <- colnames(fread(arq, nrows = 2, fill = TRUE, encoding = "Latin-1", dec = ","))
    vars <- cols[substr(cols, 1, 1) == "V"]
    listaUF[[j]] <- (fread(arq, encoding = "Latin-1", dec = ",")
    [, replace(.SD, .SD == "X", NA)]
    [, (vars) := lapply(.SD, as.numeric), .SDcols = vars]
    [, (setdiff(cols, vars)) := lapply(.SD, as.character), .SDcols = (setdiff(cols, vars))]
    )
  }
   
  tabela <- rbindlist(listaUF, fill = TRUE)
  assign(i, tabela)
  write_parquet(get(i), str_c(c("../", i, ".parquet"), collapse = ""))
}
