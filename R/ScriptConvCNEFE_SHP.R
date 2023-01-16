
### Script para conversão do arquivo da base do CNEFE 2010 para shapefile de pontos dos domicílios com coordenadas.
## o caractere "#" é comentário, não fazendo parte do código.

#### PARA RODAR, CLIQUE EM "SOURCE" NO CANTO SUPERIOR DIREITO DESTA JANELA OU UTILIZE O ATALHO "ctrl-shift-s" OU "ctrl-shift-enter"

# os procedimentos abaixo instalam os pacotes necessários para executar o Script. Só é necessário instalá-los uma vez.
# após a primeira instalação desses pacotes, comente as linhas com # no início das mesmas para que não reinstale na reexecução

install.packages("readr")
install.packages("tidyverse")
install.packages("stringr")
install.packages("sf")

# esses comandos carregam os pacotes, devendo ser necessário rodar sempre na primeira sessão, após abrir o script.

library(readr)
library(tidyverse)
library(stringr)
library(sf)

# O CodUF deve ser ajustado para o arquivo que será convertido. Basta mudar o número abaixo para o código da UF do arquivo, que deve se
# chamar "coduf".txt (é necessário descompactar o mesmo)

CodUF <- 35

# Ajuste a linha abaixo para o endereço da pasta de trabalho. Pode copiar e colar, mas é necessário trocar as contrabarras "\" 
# utilizadas no windows por barras normais "/" usadas em todos os outros sistemas operacionais e no mundo civilizado em geral.
# É recomendável realizar a operação na máquina local, de preferência em discos de estado sólido (SSD), com altas taxas de leitura e escrita.
# Os arquivos do CNEFE são bem grandes, com milhões de linhas e preenchidos com caracteres tipo "espaço" em seus campos vazios.
# Nos computadpres do IBGE, crie uma pasta na partição C: e use como pasta de trabalho.

pastaRaiz <- "C:/ACELERADOR/ProcCNEFE/"

# Daqui em diante não é necessário mudar nada, só passar um cafezinho e relaxar.... vai levar alguns minutos.
# É recomendável abrir o gerenciador de tarefas do windows, na aba "Desempenho - memória" e monitorar o uso de memória RAM.
# SP é bem pesado, foi necessário inserir uma "quebra" no código (o "View(tabela_enderecos)") para liberar no meio do caminho, 
#várias vezes deu um erro misterioso, e era pouca memória...dos 16 GB disponíveis! mas SP ne, outras UFs devem ser mais suaves.

# os comandos abaixo definem o nome e endereço dos arquivos de entrada e saída conforme as variáveis editadas acima

arquivo_entrada <- paste(pastaRaiz, as.character(CodUF), ".txt", sep = "")
arquivo_saida <- paste(pastaRaiz, as.character(CodUF), ".shp", sep = "")

# cria a função que transforma a notação da latitude e longitude para algo utilizável, de graus - minutos - segundos e formatação 
# ruim para grau decimal

fcon <- function(c) {
  s = ifelse(str_sub(c, -1, -1) == "O" | str_sub(c, -1, -1) == "S", -1, 1)
  form = as.numeric(str_sub(c, 1, 2)) + 
    as.numeric(str_sub(ifelse(str_sub(c, 2, 2) == " ", paste("0", c, sep = ""), c), 4, 5)) / 60 + 
    as.numeric(str_sub(c, -9, -2)) / 3600
  s * form
}

# abaixo é executada a leitura do arquivo, formatação das colunas com indicação da largura dos campos e títulos (ajustados pra virar shp)
# e também filtragem dos elementos sem dados nas coordenadas. Os cinco primeiros campos originais do CNEFE foram unidos no geocódigo do setor.

tabela_enderecos <- read_fwf(arquivo_entrada, 
                fwf_widths(c(15,
                             1, 20, 30, 60, 8,
                             7, 20, 10, 20, 10,
                             20, 10, 20, 10, 20,
                             10, 20, 10, 15, 15,
                             60, 60, 2, 40, 1,
                             30, 3, 3, 8),
                           col_names = c("CD_SETOR",
                                         "SIT_SETOR", "TIP_LOG", "TIT_LOG", "NOM_LOG", "NUM_LOG",
                                         "MOD_NUM", "COMPLTO", "ELEMENTO1", "VALOR1", "ELEMENTO2",
                                         "VALOR2", "ELEMENTO3", "VALOR3", "ELEMENTO4", "VALOR4",
                                         "ELEMENTO5", "VALOR5", "ELEMENTO6", "LATITUDE", "LONGITUDE",
                                         "LOCALIDADE", "NULO", "ESPEC_ENDC", "ID_ESTAB", "IND_ENDC",
                                         "ID_DOM_COL", "QUADRA", "FACE", "CEP")),
                trim_ws = FALSE,
                na = character()) %>%
  filter(LATITUDE != "               " & LONGITUDE != "               ")

# aqui é uma quebra pro interpretador fumar um cigarrinho e refrescar a memória

View(tabela_enderecos)

# voltamos, em seguida a eliminação dos espaços nas colunas, refiltragem de elementos com coordenadas e conversão das mesmas utilizando
# a função "fcon"

tabela_enderecos <- tabela_enderecos %>%
  mutate(across(everything(), str_trim)) %>%
  filter(LATITUDE != "" & LONGITUDE != "") %>%
  mutate(LATITUDE = fcon(LATITUDE), LONGITUDE = fcon(LONGITUDE))
  
# aqui as coordenadas convertidas são transformadas em geometria no SIRGAS 2000 (EPSG 4674)

pontos_enderecos <- st_as_sf(tabela_enderecos, coords = c("LONGITUDE", "LATITUDE"), crs = 4674)

# aqui os pontos são exportados para shapefile na pasta de trabalho, com o nome = códifo da UF

st_write(pontos_enderecos, arquivo_saida, delete_layer = TRUE)

## FIM :)